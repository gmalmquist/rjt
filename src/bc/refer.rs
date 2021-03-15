//! Parses the references made by classes from their compiled bytcode.

use noak;
use noak::descriptor::{MethodDescriptor, TypeDescriptor};
use noak::reader::cpool;
use noak::reader::cpool::Item;
use noak::reader::AttributeContent;
use regex;
use lazy_static::lazy_static;

use crate::bc::javastd::JavaStdLib;
use crate::bc::{get_type_name, to_real_string};
use crate::sconst::{IndexedString, StringConstants};
use std::collections::HashSet;
use noak::reader::attributes::annotations::{ElementValue, TypeAnnotation, Annotation};

lazy_static! {
    // We decide a string looks like a class if it's a sequence of three or more valid identifiers
    // delimited by periods. This will therefore not notice any classes that are referred to by their
    // simple name, and it will not notice classes in top-level packages (which are unconventional in
    // java anyway).
    static ref LOOKS_LIKE_A_CLASS: regex::Regex = regex::Regex::new(r"^([a-zA-Z_](\w)*)+([.]([a-zA-Z_](\w)*)){2,}$")
        .expect("Static LOOKS_LIKE_A_CLASS regex didn't compile.");
}

#[derive(Clone, Debug, Eq, Ord, PartialOrd, PartialEq, Hash)]
pub struct ClassReference {
    pub source_class: IndexedString,
    pub class_name: IndexedString,
}

#[derive(Clone, Debug, Eq, Ord, PartialOrd, PartialEq, Hash)]
pub struct MethodReference {
    pub source_class: IndexedString,
    pub class_name: IndexedString,
    pub method_name: IndexedString,
    pub return_type: Option<IndexedString>,
    pub is_static: bool,
}

#[derive(Clone, Debug, Eq, Ord, PartialOrd, PartialEq, Hash)]
pub struct FieldReference {
    pub source_class: IndexedString,
    pub class_name: IndexedString,
    pub field_name: IndexedString,
    pub field_type: Option<IndexedString>,
    pub is_static: bool,
}

pub trait JavaReference {
    fn class_name(&self) -> &IndexedString;

    fn source_class_name(&self) -> &IndexedString;

    fn is_java_stdlib(&self, constants: &StringConstants) -> bool {
        if let Some(name) = self.class_name().get_str(constants) {
            return name.starts_with("java/")
                || name.starts_with("javax/")
                || JavaStdLib::is_standard_library_class(name);
        }
        false
    }

    fn is_self_reference(&self, constants: &StringConstants) -> bool {
        if self.class_name() == self.source_class_name() {
            return true;
        }
        let mut me = self.class_name().get_str(constants).unwrap_or("");
        let mut other = self.source_class_name().get_str(constants).unwrap_or("");

        if let Some(dollar) = me.find("$") {
            me = &me[..dollar];
        }

        if let Some(dollar) = other.find("$") {
            other = &other[..dollar];
        }

        me == other
    }

    fn is_sun_internal(&self, constants: &StringConstants) -> bool {
        let name = self.class_name().get_str(constants).unwrap_or("");
        name == "sun/misc/Unsafe"
    }

    fn to_string(&self, constants: &StringConstants) -> String;
}

impl JavaReference for ClassReference {
    fn class_name(&self) -> &IndexedString {
        &self.class_name
    }
    fn source_class_name(&self) -> &IndexedString {
        &self.source_class
    }

    fn to_string(&self, constants: &StringConstants) -> String {
        self.class_name
            .get_string(constants)
            .expect("Expected class name in string pool.")
    }
}

impl JavaReference for MethodReference {
    fn class_name(&self) -> &IndexedString {
        &self.class_name
    }

    fn source_class_name(&self) -> &IndexedString {
        &self.source_class
    }

    fn to_string(&self, constants: &StringConstants) -> String {
        let class_name = self
            .class_name
            .get_string(constants)
            .expect("Expected class name in string pool.");
        let method_name = self
            .method_name
            .get_string(constants)
            .expect("Expected method name in string pool.");
        let return_type = self
            .return_type
            .map(|rt| rt.get_string(constants)
                .expect("Expected return type in string pool."))
            .unwrap_or("n/a".to_string());
        format!("{}#{}(..): {}", class_name, method_name, return_type)
    }
}

impl JavaReference for FieldReference {
    fn class_name(&self) -> &IndexedString {
        &self.class_name
    }

    fn source_class_name(&self) -> &IndexedString {
        &self.source_class
    }

    fn to_string(&self, constants: &StringConstants) -> String {
        let class_name = self
            .class_name
            .get_string(constants)
            .expect("Expected class name in string pool.");
        let field_name = self
            .field_name
            .get_string(constants)
            .expect("Expected field name in string pool.");
        let field_type = self
            .field_type
            .map(|ft| ft.get_string(constants)
                .expect("Expected field type in string pool."))
            .unwrap_or("n/a".to_string());
        format!("{}#{}: {}", class_name, field_name, field_type)
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialOrd, PartialEq, Hash)]
pub enum Reference {
    ClassReference(ClassReference),
    DynamicClassReference(ClassReference),
    MethodReference(MethodReference),
    FieldReference(FieldReference),
}

impl JavaReference for Reference {
    fn class_name(&self) -> &IndexedString {
        match &self {
            Reference::ClassReference(c) => c.class_name(),
            Reference::DynamicClassReference(c) => c.class_name(),
            Reference::MethodReference(m) => m.class_name(),
            Reference::FieldReference(f) => f.class_name(),
        }
    }

    fn source_class_name(&self) -> &IndexedString {
        match &self {
            Reference::ClassReference(c) => c.source_class_name(),
            Reference::DynamicClassReference(c) => c.source_class_name(),
            Reference::MethodReference(m) => m.source_class_name(),
            Reference::FieldReference(f) => f.source_class_name(),
        }
    }

    fn to_string(&self, constants: &StringConstants) -> String {
        match &self {
            Reference::ClassReference(c) => c.to_string(constants),
            Reference::DynamicClassReference(c) => c.to_string(constants),
            Reference::MethodReference(m) => m.to_string(constants),
            Reference::FieldReference(f) => f.to_string(constants),
        }
    }
}

pub struct ReferenceWalker<'a, 'b, 'c> {
    /// String constants parsed from the class file bytecode.
    cpool: &'a cpool::ConstantPool<'a>,

    /// References parsed from the byte code.
    references: &'b mut HashSet<Reference>,

    /// String constants pool to store any parsed strings. Separate from the cpool, because the
    /// cpool just refers to the read-only bytecode for this class. The constants pool may be
    /// populated with strings aggregated from bytecode spanning an entire shaded jar.
    constants: &'c mut StringConstants,
}

impl<'cpool, 'references, 'constants> ReferenceWalker<'cpool, 'references, 'constants> {
    fn new(
        cpool: &'cpool cpool::ConstantPool<'cpool>,
        references: &'references mut HashSet<Reference>,
        constants: &'constants mut StringConstants,
    ) -> Self {
        Self {
            cpool,
            references,
            constants,
        }
    }

    pub fn collect_all_references<'a, 'b>(
        class: &'a mut noak::reader::Class<'a>,
        constants: &'b mut StringConstants,
    ) -> Result<Vec<Reference>, noak::error::DecodeError> {
        let mut references = HashSet::new();

        let source_class = if let Ok(class_name) = class.this_class_name() {
            String::from_utf8_lossy(class_name.as_bytes()).to_string()
        } else {
            String::from("-")
        };
        let source_class = constants.put(&source_class);

        if let Ok(attrs) = class.attribute_indices() {
            let mut walker = ReferenceWalker::new(class.pool()?, &mut references, constants);
            walker.walk_attributes(source_class, attrs);
        }

        if let Ok(methods) = class.methods() {
            for method in methods {
                if let Ok(method) = method {
                    let mut walker =
                        ReferenceWalker::new(class.pool()?, &mut references, constants);
                    walker.walk_attributes(source_class, method.attribute_indices());
                }
            }
        }

        if let Ok(fields) = class.fields() {
            for field in fields {
                if let Ok(field) = field {
                    let mut walker =
                        ReferenceWalker::new(class.pool()?, &mut references, constants);
                    walker.walk_attributes(source_class, field.attribute_indices());
                }
            }
        }

        return Ok(references.into_iter().collect());
    }

    fn record_class_ref(&mut self, r: ClassReference) {
        if r.is_self_reference(&self.constants) || r.is_sun_internal(&self.constants) {
            return;
        }
        self.references.insert(Reference::ClassReference(r));
    }

    fn record_dynamic_class_reference(&mut self, r: ClassReference) {
        if r.is_self_reference(&self.constants) || r.is_sun_internal(&self.constants) {
            return;
        }
        self.references.insert(Reference::DynamicClassReference(r));
    }

    fn record_method_ref(&mut self, r: MethodReference) {
        if r.is_self_reference(&self.constants) || r.is_sun_internal(&self.constants) {
            return;
        }

        self.references.insert(Reference::MethodReference(r));
    }
    fn record_field_ref(&mut self, r: FieldReference) {
        if r.is_self_reference(&self.constants) || r.is_sun_internal(&self.constants) {
            return;
        }

        self.references.insert(Reference::FieldReference(r));
    }

    fn put(&mut self, string: &str) -> IndexedString {
        self.constants.put(string)
    }

    fn walk_attributes(
        &mut self,
        source_class: IndexedString,
        attributes: noak::reader::AttributeIter<'cpool>,
    ) -> () {
        for attr in attributes {
            if attr.is_err() {
                continue;
            }
            let attr = attr.unwrap();

            let content = attr.read_content(self.cpool);
            if content.is_err() {
                continue;
            }
            let content = content.unwrap();

            self.walk_attribute(source_class, &content);
        }
    }

    fn walk_attribute(
        &mut self,
        source_class: IndexedString,
        attribute: &noak::reader::AttributeContent<'cpool>,
    ) -> () {
        match attribute {
            AttributeContent::AnnotationDefault(_) => {}
            AttributeContent::BootstrapMethods(_) => {}
            AttributeContent::Code(code) => {
                for instruction in code.raw_instructions() {
                    if let Ok((_, instruction)) = instruction {
                        self.walk_instruction(source_class, &instruction);
                    }
                }
            }
            AttributeContent::ConstantValue(c) => {
                if let Ok(item) = self.cpool.get(c.value()) {
                    match item {
                        Item::Class(class) => {
                            if let Ok(class_name) = self.cpool.get(class.name).map(to_real_string) {
                                let class_name = self.constants.put(&class_name);
                                self.record_class_ref(ClassReference {
                                    source_class,
                                    class_name,
                                });
                            }
                        }
                        Item::FieldRef(field) => {
                            let class_name_type =
                                self.get_class_name_type(&field.class, &field.name_and_type);
                            if let Some(reference) = class_name_type {
                                let class_name = self.constants.put(&reference.class_name);
                                let field_name = self.constants.put(&reference.member_name);
                                let field_type = self.constants.put(&reference.member_type);
                                let field_type = Some(field_type);
                                self.record_field_ref(FieldReference {
                                    source_class,
                                    class_name,
                                    field_name,
                                    field_type,
                                    is_static: true,
                                });
                            }
                        }
                        Item::MethodRef(method) => {
                            let class_name_type =
                                self.get_class_name_type(&method.class, &method.name_and_type);
                            if let Some(reference) = class_name_type {
                                let class_name = self.constants.put(&reference.class_name);
                                let method_name = self.constants.put(&reference.member_name);
                                let return_type = self.constants.put(&reference.member_type);
                                let return_type = Some(return_type);
                                self.record_method_ref(MethodReference {
                                    source_class,
                                    class_name,
                                    method_name,
                                    return_type,
                                    is_static: true,
                                });
                            }
                        }
                        Item::InterfaceMethodRef(_) => {}
                        Item::String(s) => {
                            if let Ok(s) = self.cpool.get(s.string).map(to_real_string) {
                                if LOOKS_LIKE_A_CLASS.is_match(&s) {
                                    let class_name = self.constants.put(&s);
                                    self.record_dynamic_class_reference(ClassReference {
                                        source_class,
                                        class_name,
                                    });
                                }
                            }
                        }
                        Item::Integer(_) => {}
                        Item::Long(_) => {}
                        Item::Float(_) => {}
                        Item::Double(_) => {}
                        Item::NameAndType(name_and_type) => {
                            if let Ok(desc) = self
                                .cpool
                                .get(name_and_type.descriptor)
                                .and_then(|utf8| TypeDescriptor::parse(utf8.content))
                            {
                                let type_name = get_type_name(&desc);
                                let class_name = self.constants.put(&type_name);
                                self.record_class_ref(ClassReference {
                                    source_class,
                                    class_name,
                                });
                            }
                        }
                        Item::Utf8(_) => {}
                        Item::MethodHandle(_) => {}
                        Item::MethodType(method_type) => {
                            if let Ok(method) = self
                                .cpool
                                .get(method_type.descriptor)
                                .and_then(|utf8| MethodDescriptor::parse(utf8.content))
                            {
                                if let Some(return_type) = method.return_type() {
                                    let class_name =
                                        self.constants.put(&get_type_name(&return_type));
                                    self.record_class_ref(ClassReference {
                                        source_class,
                                        class_name,
                                    });
                                }
                                for param in method.parameters() {
                                    let class_name = self.constants.put(&get_type_name(&param));
                                    self.record_class_ref(ClassReference {
                                        source_class,
                                        class_name,
                                    });
                                }
                            }
                        }
                        Item::Dynamic(_d) => {}
                        Item::InvokeDynamic(_) => {}
                        Item::Module(_) => {}
                        Item::Package(_) => {}
                    }
                }
            }
            AttributeContent::Deprecated => {}
            AttributeContent::EnclosingMethod(_) => {}
            AttributeContent::Exceptions(_) => {}
            AttributeContent::InnerClasses(_) => {}
            AttributeContent::LineNumberTable(_) => {}
            AttributeContent::LocalVariableTable(_) => {}
            AttributeContent::LocalVariableTypeTable(_) => {}
            AttributeContent::MethodParameters(_) => {}
            AttributeContent::Module(_) => {}
            AttributeContent::ModuleMainClass(_) => {}
            AttributeContent::ModulePackages(_) => {}
            AttributeContent::NestHost(_) => {}
            AttributeContent::NestMembers(_) => {}
            AttributeContent::RuntimeInvisibleAnnotations(annotations) => {
                for annotation in annotations.iter() {
                    if let Ok(annotation) = annotation {
                        self.process_annotation(&annotation, source_class);
                    }
                }
            }
            AttributeContent::RuntimeInvisibleParameterAnnotations(_) => {}
            AttributeContent::RuntimeInvisibleTypeAnnotations(_) => {}
            AttributeContent::RuntimeVisibleAnnotations(annotations) => {
                for annotation in annotations.iter() {
                    if let Ok(annotation) = annotation {
                        self.process_annotation(&annotation, source_class);
                    }
                }
            }
            AttributeContent::RuntimeVisibleParameterAnnotations(_) => {}
            AttributeContent::RuntimeVisibleTypeAnnotations(_) => {}
            AttributeContent::Signature(_) => {}
            AttributeContent::SourceDebugExtension(_) => {}
            AttributeContent::SourceFile(_sf) => {
                // This is the literal name of the source file.
            }
            AttributeContent::StackMapTable(_) => {}
            AttributeContent::Synthetic => {}
        }
    }

    fn process_annotation(&mut self, annotation: &Annotation, source_class: IndexedString) {
        let class_name = annotation.r#type();
        if let Some(class_name) = self.get_class_name_from_utf8(class_name) {
            let class_name = self.constants.put(&class_name);
            self.record_class_ref(ClassReference {
                source_class,
                class_name,
            });
        }

        for pair in annotation.pairs() {
            if let Ok(pair) = pair {
                self.process_annotation_element(pair.value(), source_class);
            }
        }
    }

    fn process_annotation_element(&mut self, element: &ElementValue, source_class: IndexedString) {
        match element {
            ElementValue::Boolean(_) => {}
            ElementValue::Byte(_) => {}
            ElementValue::Short(_) => {}
            ElementValue::Int(_) => {}
            ElementValue::Long(_) => {}
            ElementValue::Float(_) => {}
            ElementValue::Double(_) => {}
            ElementValue::Char(_) => {}
            ElementValue::String(s) => {
                if let Ok(s) = self.cpool.get(*s).map(to_real_string) {
                    if !LOOKS_LIKE_A_CLASS.is_match(&s) {
                        return;
                    }
                    let class_name = self.constants.put(&s);
                    self.record_dynamic_class_reference(ClassReference {
                        source_class,
                        class_name,
                    });
                }
            }
            ElementValue::Class(class_name) => {
                if let Some(class_name) = self.get_class_name_from_utf8(*class_name) {
                    let class_name = self.constants.put(&class_name);
                    self.record_class_ref(ClassReference {
                        source_class,
                        class_name,
                    });
                }
            }
            ElementValue::Enum { type_name, const_name } => {
                if let Some(class_name) = self.get_class_name_from_utf8(*type_name) {
                    let class_name = self.constants.put(&class_name);
                    self.record_class_ref(ClassReference {
                        source_class,
                        class_name,
                    });
                }
            }
            ElementValue::Annotation(annotation) => {
                self.process_annotation(annotation, source_class);
            }
            ElementValue::Array(array) => {
                for item in array.iter() {
                    if let Ok(item) = item {
                        self.process_annotation_element(&item, source_class);
                    }
                }
            }
        }
    }

    fn walk_instruction(
        &mut self,
        source_class: IndexedString,
        instruction: &noak::reader::attributes::RawInstruction<'cpool>,
    ) -> () {
        match instruction {
            noak::reader::attributes::RawInstruction::CheckCast { index } => {
                if let Some(class) = self.get_class_name(index) {
                    let class_name = self.constants.put(&class);
                    self.record_class_ref(ClassReference {
                        source_class,
                        class_name,
                    });
                }
            }
            noak::reader::attributes::RawInstruction::ANewArray { index } => {
                if let Some(class) = self.get_class_name(index) {
                    let class_name = self.constants.put(&class);
                    self.record_class_ref(ClassReference {
                        source_class,
                        class_name,
                    });
                }
            }
            noak::reader::attributes::RawInstruction::GetField { index } => {
                self.handle_field(source_class, false, index);
            }
            noak::reader::attributes::RawInstruction::GetStatic { index } => {
                self.handle_field(source_class, true, index);
            }
            noak::reader::attributes::RawInstruction::InstanceOf { index } => {
                if let Some(class) = self.get_class_name(index) {
                    let class_name = self.constants.put(&class);
                    self.record_class_ref(ClassReference {
                        source_class,
                        class_name,
                    });
                }
            }
            noak::reader::attributes::RawInstruction::InvokeDynamic { index } => {
                if let Ok(dynamic) = self.cpool.get(*index) {
                    if let Some(NameAndType { name: _, base_type }) =
                    self.get_name_and_type(&dynamic.name_and_type)
                    {
                        // We don't know what object this invocation is acting on, but if we can at
                        // least report on its type.
                        let _class_name = self.constants.put(&base_type);
                    }
                }
            }
            noak::reader::attributes::RawInstruction::InvokeInterface { index, count: _ } => {
                if let Ok(interface) = self.cpool.get(*index) {
                    if let Some(reference) =
                    self.get_class_name_type(&interface.class, &interface.name_and_type)
                    {
                        let class_name = self.constants.put(&reference.class_name);
                        let method_name = self.constants.put(&reference.member_name);
                        let return_type = self.constants.put(&reference.member_type);
                        let return_type = Some(return_type);
                        self.record_method_ref(MethodReference {
                            source_class,
                            class_name,
                            method_name,
                            return_type,
                            is_static: false,
                        });
                    }
                }
            }
            noak::reader::attributes::RawInstruction::InvokeVirtual { index } => {
                if let Ok(method_ref) = self.cpool.get(*index) {
                    if let Some(reference) =
                    self.get_class_name_type(&method_ref.class, &method_ref.name_and_type)
                    {
                        let class_name = self.constants.put(&reference.class_name);
                        let method_name = self.constants.put(&reference.member_name);
                        let return_type = self.constants.put(&reference.member_type);
                        let return_type = Some(return_type);
                        self.record_method_ref(MethodReference {
                            source_class,
                            class_name,
                            method_name,
                            return_type,
                            is_static: false,
                        });
                    }
                }
            }
            noak::reader::attributes::RawInstruction::MultiANewArray {
                index,
                dimensions: _,
            } => {
                if let Some(class_name) = self.get_class_name(index) {
                    let class_name = self.constants.put(&class_name);
                    self.record_class_ref(ClassReference {
                        source_class,
                        class_name,
                    });
                }
            }
            noak::reader::attributes::RawInstruction::New { index } => {
                if let Some(class_name) = self.get_class_name(index) {
                    let class_name = self.constants.put(&class_name);
                    self.record_class_ref(ClassReference {
                        source_class,
                        class_name,
                    });
                }
            }
            noak::reader::attributes::RawInstruction::NewArray { atype: _ } => {
                // These are only for primitive arrays, so we don't care about them.
            }
            noak::reader::attributes::RawInstruction::PutField { index } => {
                if let Ok(field) = self.cpool.get(*index) {
                    if let Some(reference) =
                    self.get_class_name_type(&field.class, &field.name_and_type)
                    {
                        let class_name = self.constants.put(&reference.class_name);
                        let field_name = self.constants.put(&reference.member_name);
                        let field_type = self.constants.put(&reference.member_type);
                        let field_type = Some(field_type);
                        self.record_field_ref(FieldReference {
                            source_class,
                            class_name,
                            field_name,
                            field_type,
                            is_static: false,
                        });
                    }
                }
            }
            noak::reader::attributes::RawInstruction::PutStatic { index } => {
                if let Ok(field) = self.cpool.get(*index) {
                    if let Some(reference) =
                    self.get_class_name_type(&field.class, &field.name_and_type)
                    {
                        let class_name = self.constants.put(&reference.class_name);
                        let field_name = self.constants.put(&reference.member_name);
                        let field_type = self.constants.put(&reference.member_type);
                        let field_type = Some(field_type);
                        self.record_field_ref(FieldReference {
                            source_class,
                            class_name,
                            field_name,
                            field_type,
                            is_static: true,
                        });
                    }
                }
            }
            _ => {}
        }
    }

    fn handle_field(
        &mut self,
        source_class: IndexedString,
        is_static: bool,
        index: &cpool::Index<cpool::FieldRef>,
    ) -> () {
        if let Ok(field) = self.cpool.get(*index) {
            if let Some(reference) = self.get_class_name_type(&field.class, &field.name_and_type) {
                let class_name = self.constants.put(&reference.class_name);
                let field_name = self.constants.put(&reference.member_name);
                let field_type = self.constants.put(&reference.member_type);
                let field_type = Some(field_type);
                self.record_field_ref(FieldReference {
                    source_class,
                    class_name,
                    field_name,
                    field_type,
                    is_static,
                });
            }
        }
    }

    fn get_class_name_type(
        &self,
        class_index: &cpool::Index<cpool::Class>,
        name_and_type_index: &cpool::Index<cpool::NameAndType>,
    ) -> Option<ClassNameType> {
        let class_name = self.get_class_name(class_index);
        if class_name.is_none() {
            return None;
        }
        let class_name = class_name.unwrap();

        let name_and_type = self.get_name_and_type(name_and_type_index);
        if name_and_type.is_none() {
            return None;
        }
        let name_and_type = name_and_type.unwrap();

        Some(ClassNameType::new(class_name, name_and_type))
    }

    fn get_name_and_type(&self, index: &cpool::Index<cpool::NameAndType>) -> Option<NameAndType> {
        if let Ok(name_and_type) = self.cpool.get(*index) {
            let mut the_name = String::from("-");
            let mut the_type = String::from("-");
            if let Ok(name) = self.cpool.get(name_and_type.name).map(to_real_string) {
                the_name = name;
            }
            if let Ok(descriptor) = self
                .cpool
                .get(name_and_type.descriptor)
                .map(|encoded_descriptor| encoded_descriptor.content)
                .and_then(TypeDescriptor::parse)
            {
                the_type = get_type_name(&descriptor);
            }
            return Some(NameAndType {
                name: the_name,
                base_type: the_type,
            });
        }
        None
    }

    fn get_class_name(&self, index: &cpool::Index<cpool::Class>) -> Option<String> {
        if let Ok(class) = self.cpool.get(*index) {
            return self.get_class_name_from_utf8(class.name);
        }
        None
    }

    fn get_class_name_from_utf8(&self, class_name: cpool::Index<cpool::Utf8>) -> Option<String> {
        let name = self.cpool.get(class_name);
        if name.is_err() {
            return None;
        }
        let name = name.unwrap();
        let mut name_str = to_real_string(name);
        if name_str.starts_with("[") {
            if let Ok(descriptor) = TypeDescriptor::parse(name.content) {
                return Some(get_type_name(&descriptor));
            }
        }
        if name_str.starts_with("L") {
            name_str = name_str[1..].to_string();
        }
        if name_str.ends_with(";") {
            name_str = name_str[..name_str.len() - 1].to_string();
        }
        Some(name_str)
    }
}

struct ClassNameType {
    class_name: String,
    member_name: String,
    member_type: String,
}

impl ClassNameType {
    fn new(class_name: String, name_and_type: NameAndType) -> Self {
        Self {
            class_name,
            member_name: name_and_type.name,
            member_type: name_and_type.base_type,
        }
    }
}

struct NameAndType {
    name: String,
    base_type: String,
}
