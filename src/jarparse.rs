//! Parse the binary contents of jar files.

use std::collections::{HashSet, HashMap};
use std::fs;
use std::io;
use std::io::{Read, Write};
use std::ops::{Deref, DerefMut, Range};
use std::sync::{Arc, mpsc, Mutex};

use crypto;
use crypto::digest::Digest;
use futures;
use zip;
use shellexpand;

use crate::bc::javastd::JavaStdLib;
use crate::bc::pubapi::{ApiParser, JavaClass, PublicApi};
use crate::bc::refer;
use crate::bc::refer::{JavaReference, Reference, ReferenceWalker};
use crate::bc::refer::Reference::FieldReference;
use crate::jarparse::ReferenceValidationError::{NoSuchClass, NoSuchField};
use crate::sconst::{IndexedString, StringConstants};
use std::sync::mpsc::Sender;
use std::error::Error;

pub struct JarContents {
    pub path: String,
    pub entries: Vec<JarEntry>,
    pub string_constants: Arc<Mutex<StringConstants>>,
    standard_classes: HashMap<IndexedString, JavaClass>,
    class_name_to_entry_index: HashMap<IndexedString, usize>,
}

pub struct CombinedJarContents {
    pub path: String,
    pub api: PublicApi,
    pub references: Vec<Reference>,
    pub string_constants: StringConstants,
}

#[derive(Clone)]
pub struct JarEntry {
    pub resource_name: String,
    pub sha: String,
    pub api: Option<PublicApi>,
    pub references: Vec<refer::Reference>,
}

impl JarContents {
    pub fn load_jars(
        jar_paths: Vec<&str>,
        parallelism: usize,
        parse_references: bool,
    ) -> io::Result<JarContents> {
        let mut canonical_jar_paths = vec![];
        for jar_path in &jar_paths {
            let canonical = canonicalize(&shellexpand::tilde(jar_path));
            if let Err(e) = canonical {
                return Err(io::Error::new(
                    e.kind(),
                    format!("Error canonicalizing path: {}: {}", e.description(), jar_path),
                ));
            }
            let canonical = canonical.unwrap();
            if !std::path::Path::new(&canonical).is_file() {
                return Err(io::Error::new(
                    io::ErrorKind::NotFound,
                    format!("Jar file does not exist: {}", canonical),
                ));
            }
            canonical_jar_paths.push(canonical);
        }

        let mut string_constants = StringConstants::new();
        let java_standard_library_classes;
        {
            let mut java_standard_library = JavaStdLib::new()?;
            println!("Loading standard library.");
            let names = java_standard_library.get_all_java_class_names();
            println!(
                "Need to read {} java classes from the standard library.",
                names.len()
            );
            for name in names {
                print!("Read {}              \r", &name);
                io::stdout().flush();
                // Force the class to load.
                java_standard_library.get(&name, &mut string_constants);
            }
            println!("Done.");
            java_standard_library_classes = java_standard_library.get_classes()
                .into_iter()
                .map(|c| (c.class_name, c))
                .collect();
        }

        let (px, rx) = mpsc::channel::<JarEntry>();

        let string_constants = Mutex::new(string_constants);
        let string_constants = Arc::new(string_constants);

        let mut futures = vec![];

        for canonical_jar_path in &canonical_jar_paths {
            let archive = read_zip_archive(&canonical_jar_path);
            if let Err(e) = archive {
                return Err(io::Error::new(
                    io::ErrorKind::NotFound,
                    format!("Unable to read_zip_archive({}): {}",
                        &canonical_jar_path,
                        e.description()),
                ));
            }
            let entry_count = archive?.len();

            for i in 0..parallelism {
                let start = i * entry_count / parallelism;
                let end = (i + 1) * entry_count / parallelism;
                let slice = start..end;

                let zip_path = canonical_jar_path.clone();
                let px = px.clone();
                let slice_index = i;
                let string_constants = string_constants.clone();
                futures.push(async move {
                    Self::process_jar_entries_slice(
                        zip_path,
                        slice,
                        px,
                        string_constants,
                        slice_index,
                        entry_count,
                        parse_references,
                    );
                });
            }
        }

        drop(px);

        let pool = futures::executor::ThreadPool::new()?;
        futures.into_iter().for_each(|f| pool.spawn_ok(f));
        let mut entries: Vec<JarEntry> = rx.iter().collect();

        let mut class_name_to_entry_index = HashMap::new();
        for (index, entry) in entries.iter().enumerate() {
            if let Some(api) = &entry.api {
                for class in &api.classes {
                    class_name_to_entry_index.insert(class.class_name, index);
                }
            }
        }

        println!("Done analyzing jar contents.");
        Ok(JarContents {
            path: canonical_jar_paths.join(":"),
            entries,
            string_constants,
            standard_classes: java_standard_library_classes,
            class_name_to_entry_index,
        })
    }

    fn process_jar_entries_slice(
        zip_path: String,
        slice: Range<usize>,
        px: Sender<JarEntry>,
        string_constants: Arc<Mutex<StringConstants>>,
        slice_index: usize,
        entry_count: usize,
        parse_references: bool) {
        println!("Starting slice {:#?}", slice);
        let zip = read_zip_archive(&zip_path);
        if zip.is_err() {
            return;
        }
        let mut zip = zip.unwrap();
        for i in slice.clone() {
            let entry = zip.by_index(i);
            if entry.is_err() {
                println!("Unable to process {:#?}", entry.err());
                continue;
            }
            let mut entry = entry.unwrap();

            if !entry.is_file() {
                continue;
            }

            let mut bytes: Vec<u8> = vec![];
            let mut sha1 = crypto::sha1::Sha1::new();
            let mut buffer = [0; 4096];
            while let Ok(read) = entry.read(&mut buffer) {
                if read == 0 {
                    break;
                }
                sha1.input(&buffer[0..read]);
                bytes.extend_from_slice(&buffer[0..read]);
            }

            let sha = sha1.result_str();

            if (i + 1) % 1000 == 0 {
                println!(
                    "Slice {} is processing {}/{} ({}%): {} {}\r",
                    slice_index,
                    i + 1,
                    entry_count,
                    (100 * (i - slice.start + 1) / (slice.end - slice.start)),
                    sha,
                    entry.name(),
                );
            }

            let mut content: EntryContent = EntryContent::Bytes(&bytes);
            if entry.name().ends_with(".class") {
                if let Ok(class) = noak::reader::Class::new(&bytes) {
                    content = EntryContent::Class(class);
                }
            }

            let references;
            let api;
            {
                let mut string_constants = string_constants.lock().unwrap();
                let string_constants = string_constants.deref_mut();

                api = Self::create_api_entry(string_constants, entry.name(), &mut content);

                references = if parse_references {
                    Self::parse_references(string_constants, &mut content).unwrap_or(vec![])
                } else {
                    vec![]
                };
            }

            if let Err(e) = px.send(JarEntry {
                resource_name: entry.name().to_string(),
                sha,
                api,
                references,
            }) {
                println!("Error sending jar entry back: {:#?}", e);
                return;
            }
        }
    }

    fn parse_references<'a, 'b>(
        constants: &mut StringConstants,
        content: &'a mut EntryContent<'a, 'b>,
    ) -> Option<Vec<Reference>> {
        if let EntryContent::Class(class) = content {
            let result = ReferenceWalker::collect_all_references(class, constants);
            if let Ok(references) = result {
                return Some(references);
            }
        }
        None
    }

    fn create_api_entry(
        constants: &mut StringConstants,
        entry_name: &str,
        content: &mut EntryContent,
    ) -> Option<PublicApi> {
        match content {
            EntryContent::Class(class) => {
                let mut parser = ApiParser::new(constants);
                parser.add_class(class);
                Some(parser.get_api())
            }
            EntryContent::Bytes(bytes) => {
                if entry_name.starts_with("META-INF/services") {
                    let interface_name = &entry_name["META-INF/services/".len()..];
                    if interface_name.len() > 0 {
                        let mut implementations = vec![];
                        for line in String::from_utf8_lossy(&bytes).lines() {
                            let line = line.trim();
                            if line.len() > 0 && !line.starts_with("#") {
                                implementations.push(line.to_string());
                            }
                        }
                        let mut api = PublicApi::new();
                        api.service_interfaces
                            .add_all(interface_name, implementations);
                        return Some(api);
                    }
                }
                None
            }
        }
    }

    pub fn validate_reference(
        &self,
        reference: &Reference,
        string_constants: &StringConstants,
    ) -> Result<(), ReferenceValidationError> {
        let no_such_class = Err(NoSuchClass(
            String::from("No such class."),
            reference.class_name().clone()));
        match reference {
            Reference::ClassReference(_class) => {
                if self.find_class_by_name(reference.class_name()).is_some() {
                    Ok(())
                } else {
                    no_such_class
                }
            }
            Reference::DynamicClassReference(_class) => {
                if self.find_class_by_name(reference.class_name()).is_some() {
                    Ok(())
                } else {
                    no_such_class
                }
            }
            Reference::MethodReference(method) => {
                let mut class_exists = false;
                let mut checked_classes = vec![];
                for class in SuperclassIter::new(self, method.class_name) {
                    class_exists = true;
                    if class
                        .methods
                        .iter()
                        .any(|m| m.method_name == method.method_name)
                    {
                        return Ok(());
                    }
                    if let Some(name) = class.class_name.get_str(&string_constants) {
                        checked_classes.push(name);
                    }
                }
                if class_exists {
                    Err(ReferenceValidationError::NoSuchMethod(
                        format!(
                            "Method not defined by any of [{}]",
                            checked_classes.join(", ")
                        ), method.method_name))
                } else {
                    no_such_class
                }
            }
            Reference::FieldReference(field) => {
                let mut class_exists = false;
                for class in SuperclassIter::new(self, field.class_name) {
                    class_exists = true;
                    for f in &class.fields {
                        if f.field_name != field.field_name {
                            continue;
                        }
                        if f.field_type != field.field_type {
                            return Err(NoSuchField(format!(
                                "Field referenced with wrong type (declared type is {}).",
                                f.field_type.get_str(&string_constants).unwrap()
                            ), field.field_name));
                        }
                        return Ok(());
                    }
                }
                if class_exists {
                    Err(ReferenceValidationError::NoSuchField(String::from(
                        "Field not defined by class.",
                    ), field.field_name))
                } else {
                    no_such_class
                }
            }
        }
    }

    pub fn find_class_by_name(&self, name: &IndexedString) -> Option<&JavaClass> {
        if let Some(api) = self.class_name_to_entry_index.get(name)
            .and_then(|index| self.entries.get(*index))
            .and_then(|entry| entry.api.as_ref()) {
            for class in &api.classes {
                if &class.class_name == name {
                    return Some(class);
                }
            }
            // Maybe this should panic? The map lied to us.
            return None;
        }

        self.standard_classes.get(name)
    }

    pub fn into_combined(self) -> CombinedJarContents {
        let string_constants = self.string_constants.lock().unwrap();
        // This is expensive. Would be nice to avoid. :|
        let string_constants = string_constants.clone();

        let mut api = PublicApi::new();
        let mut references: HashSet<Reference> = HashSet::new();
        for entry in self.entries {
            if let Some(entry_api) = entry.api {
                api.service_interfaces
                    .merge_from(&entry_api.service_interfaces);
                for class in entry_api.classes {
                    api.classes.push(class);
                }
            }
            for reference in entry.references {
                references.insert(reference);
            }
        }

        let references: Vec<Reference> = references.into_iter().collect();

        CombinedJarContents {
            path: self.path,
            api,
            references,
            string_constants,
        }
    }

    pub fn find_missing_references(self: Arc<Self>) -> MissingReferenceSummary {
        let (px, rx) = std::sync::mpsc::channel();
        let mut threadpool = futures::executor::ThreadPoolBuilder::new()
            .create()
            .expect("Unable to create threadpool.");

        let string_constants;
        {
            let sc = self.string_constants.lock().unwrap();
            let sc = sc.deref();
            string_constants = Arc::new(sc.clone());
        }
        println!("Computing invalid references:");

        let chunk_size = 32;
        for (chunk_index, entry_chunk) in self.entries.chunks(chunk_size).enumerate() {
            let entry_chunk: Vec<JarEntry> = entry_chunk.iter().map(|e| e.clone()).collect();
            let px = px.clone();
            let string_constants = Arc::clone(&string_constants);
            let jar = Arc::clone(&self);
            threadpool.spawn_ok(async move {
                for (i, jar_entry) in entry_chunk.iter().enumerate() {
                    for reference in &jar_entry.references {
                        let is_stdlib = reference.is_java_stdlib(&string_constants);
                        if is_stdlib {
                            continue;
                        }
                        if let Err(error) = jar.validate_reference(
                            reference,
                            &string_constants) {
                            px.send((chunk_index * chunk_size + i, reference.clone(), error));
                        }
                    }
                }
            });
        }
        drop(px);

        let mut missing_dynamic_references: HashMap<String, HashSet<String>> = HashMap::new();

        // Map of missing classes to lists of classes that invoke them.
        let mut missing_classes: HashMap<String, HashSet<String>> = HashMap::new();

        // Map of missing methods to lists of classes that invoke them.
        let mut missing_methods: HashMap<String, HashSet<String>> = HashMap::new();

        // Map of missing fields to lists of classes that invoke them.
        let mut missing_fields: HashMap<String, HashSet<String>> = HashMap::new();

        let mut invalid_reference_total = 0;
        for (entry_index, reference, error) in rx {
            invalid_reference_total += 1;
            print!("Processed {} invalid references (last entry {} /{})          \r",
                   invalid_reference_total,
                   entry_index,
                   &self.entries.len());
            std::io::stdout().flush();

            let reference_name;
            let source_class_name;
            let target_class_name;
            {
                reference_name = reference.to_string(&string_constants);
                source_class_name = reference
                    .source_class_name()
                    .get_str(&string_constants)
                    .unwrap_or("?")
                    .to_string();
                target_class_name = reference.class_name().get_str(&string_constants)
                    .unwrap_or("?")
                    .to_string();
            }

            if let Reference::DynamicClassReference(class_ref) = reference {
                let target_name = class_ref.class_name.get_string(&string_constants)
                    .expect("No string in string pool for potential dynamic class reference.");
                if !missing_dynamic_references.contains_key(&target_name) {
                    missing_dynamic_references.insert(target_name.clone(), HashSet::new());
                }
                missing_dynamic_references.get_mut(&target_name).unwrap()
                    .insert(source_class_name.clone());
            } else {
                match error {
                    ReferenceValidationError::NoSuchClass(_, class_name) => {
                        if !missing_classes.contains_key(&target_class_name) {
                            missing_classes.insert(target_class_name.clone(), HashSet::new());
                        }
                        missing_classes.get_mut(&target_class_name)
                            .unwrap()
                            .insert(source_class_name.clone());
                    }
                    ReferenceValidationError::NoSuchMethod(_, method_name) => {
                        let method_name = method_name.get_str(&string_constants)
                            .unwrap_or("?")
                            .to_string();
                        let key = format!("{}#{}", &target_class_name, &method_name);
                        if !missing_methods.contains_key(&key) {
                            missing_methods.insert(key.clone(), HashSet::new());
                        }
                        missing_methods.get_mut(&key)
                            .unwrap()
                            .insert(source_class_name.clone());
                    }
                    ReferenceValidationError::NoSuchField(_, field_name) => {
                        let field_name = field_name.get_str(&string_constants)
                            .unwrap_or("?")
                            .to_string();
                        let key = format!("{}#{}", &target_class_name, &field_name);
                        if !missing_fields.contains_key(&key) {
                            missing_fields.insert(key.clone(), HashSet::new());
                        }
                        missing_fields.get_mut(&key)
                            .unwrap()
                            .insert(source_class_name.clone());
                    }
                }
            }
        }

        let mut declared_packages: HashSet<String> = HashSet::new();
        for entry in &self.entries {
            if let Some(api) = &entry.api {
                // We only check one class, because all classes in a single .class file will have
                // the same package.
                if let Some(class) = api.classes.get(0) {
                    if let Some(package) = class.get_package_name(&string_constants) {
                        for package in Self::get_package_and_parents(&package) {
                            declared_packages.insert(package);
                        }
                    }
                }
            }
        }

        for class in self.standard_classes.values() {
            if let Some(package) = class.get_package_name(&string_constants) {
                for package in Self::get_package_and_parents(&package) {
                    declared_packages.insert(package);
                }
            }
        }

        let mut packages_with_missing_classes: HashMap<String, HashSet<String>> = HashMap::new();
        for class in missing_classes.keys() {
            let package = JavaClass::parse_package(class);
            if package.is_none() {
                continue;
            }
            let package = package.unwrap();

            // Roll up to the highest-level package which isn't actually defined.
            let package_hierarchy = Self::get_package_and_parents(&package);
            let mut lowest_index = None;
            for (i, package) in package_hierarchy.iter().enumerate() {
                if packages_with_missing_classes.contains_key(package) {
                    lowest_index = None;
                    packages_with_missing_classes.get_mut(package)
                        .unwrap()
                        .insert(class.clone());
                    break; // Already covered.
                }
                if declared_packages.contains(package) {
                    break; // This package is not actually missing.
                }
                lowest_index = Some(i);
            }
            if let Some(index) = lowest_index {
                let key = &package_hierarchy[index];
                packages_with_missing_classes.insert(key.clone(), HashSet::new());
                packages_with_missing_classes.get_mut(key)
                    .unwrap()
                    .insert(class.clone());
            }
        }

        println!("Done processing invalid references.                               ");
        MissingReferenceSummary {
            classes: missing_classes,
            methods: missing_methods,
            fields: missing_fields,
            dynamic: missing_dynamic_references,
            packages: packages_with_missing_classes,
        }
    }

    fn get_package_and_parents(package: &str) -> Vec<String> {
        let mut results = vec![];
        let parts: Vec<_> = package.split(".").collect();
        for i in (0..parts.len()).rev() {
            let pkg = parts[0..(i + 1)].join(".");
            results.push(pkg)
        }
        results
    }
}

pub struct MissingReferenceSummary {
    /** Map of invalid class references to the classes that reference them. */
    pub classes: HashMap<String, HashSet<String>>,

    /** Map of invalid method references to the classes that reference them. */
    pub methods: HashMap<String, HashSet<String>>,

    /** Map of invalid field references to the classes that reference them. */
    pub fields: HashMap<String, HashSet<String>>,

    /**
     * Invalid dynamic references (string constants which look like classes, but aren't).
     *
     * Most, if not all, of these are probably going to be false-positives.
     */
    pub dynamic: HashMap<String, HashSet<String>>,

    /**
     * Missing package references.
     */
    pub packages: HashMap<String, HashSet<String>>,
}

impl MissingReferenceSummary {
    pub fn difference(&self, other: &MissingReferenceSummary) -> MissingReferenceSummary {
        Self {
            classes: Self::map_diff(&self.classes, &other.classes),
            methods: Self::map_diff(&self.methods, &other.methods),
            fields: Self::map_diff(&self.fields, &other.fields),
            dynamic: Self::map_diff(&self.dynamic, &other.dynamic),
            packages: Self::map_diff(&self.packages, &other.packages),
        }
    }

    fn map_diff(
        one: &HashMap<String, HashSet<String>>,
        two: &HashMap<String, HashSet<String>>) -> HashMap<String, HashSet<String>> {
        let mut result = HashMap::new();
        for two_key in two.keys() {
            let two_values = two.get(two_key).unwrap();
            if let Some(one_values) = one.get(two_key) {
                let mut set_diff = HashSet::new();
                two_values.iter()
                    .filter(|v| !one_values.contains(v.clone()))
                    .for_each(|v| {
                        set_diff.insert(v.clone());
                    });

                if set_diff.len() > 0 {
                    result.insert(two_key.clone(), set_diff);
                }
            } else {
                result.insert(two_key.clone(), two_values.clone());
            }
        }
        result
    }
}

#[derive(Debug)]
pub enum ReferenceValidationError {
    NoSuchClass(String, IndexedString),
    NoSuchMethod(String, IndexedString),
    NoSuchField(String, IndexedString),
}

struct SuperclassIter<'a> {
    jar_contents: &'a JarContents,
    frontier: Vec<IndexedString>,
    visited: HashSet<IndexedString>,
}

impl<'a> SuperclassIter<'a> {
    pub fn new(jar_contents: &'a JarContents, class: IndexedString) -> Self {
        Self {
            jar_contents,
            frontier: vec![class],
            visited: HashSet::new(),
        }
    }
}

impl<'a> Iterator for SuperclassIter<'a> {
    type Item = &'a JavaClass;

    fn next(&mut self) -> Option<Self::Item> {
        let mut next = self.frontier.pop();
        while next.is_some() && self.visited.contains(&next.unwrap()) {
            next = self.frontier.pop();
        }
        if let Some(class_name) = next {
            self.visited.insert(class_name);
            if let Some(class) = self.jar_contents.find_class_by_name(&class_name) {
                for interface in &class.interfaces {
                    self.frontier.push(interface.clone());
                }
                self.frontier.push(class.superclass);
                return Some(class);
            }
        }
        None
    }
}

enum EntryContent<'a, 'b> {
    Class(noak::reader::Class<'a>),
    Bytes(&'b [u8]),
}

fn read_zip_archive(path: &str) -> zip::result::ZipResult<zip::ZipArchive<fs::File>> {
    let file = fs::File::open(path)?;
    zip::ZipArchive::new(file)
}

fn canonicalize(path: &str) -> io::Result<String> {
    let canonical_path = fs::canonicalize(path)?;
    let canonical_path = canonical_path.to_str();
    match canonical_path {
        None => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Could not parse jar path.",
        )),
        Some(path) => Ok(path.to_string()),
    }
}
