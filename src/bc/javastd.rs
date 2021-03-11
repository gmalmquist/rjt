use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::io;
use std::io::{BufRead, Read, Write};
use std::path;
use std::path::PathBuf;
use std::process;
use std::sync::mpsc;
use std::thread;
use std::future;

use reqwest;
use tempfile;
use regex;
use tokio;

use crate::bc::pubapi::{JavaClass, JavaField, JavaMethod};
use crate::sconst::{IndexedString, StringConstants};

#[macro_use]
use lazy_static::lazy_static;
use std::process::Stdio;
use futures::task::Spawn;

const JAVA_CLASS_LIST_URL: &'static str = "https://docs.oracle.com/en/java/javase/11/docs/api/allclasses.html";

lazy_static! {
    static ref JAVA_CLASS_LIST: HashSet<String> = load_java_class_name_set();
}

fn load_java_class_name_set() -> HashSet<String> {
    let mut runtime = tokio::runtime::Runtime::new()
        .expect("Could not initialize tokio runtime.");
    let result = runtime.block_on(load_java_classes(JAVA_CLASS_LIST_URL));
    let hash_set: HashSet<String>;
    match result {
        Ok(result) => {
            hash_set = result;
        }
        Err(e) => {
            panic!("Error: {:#?}", e);
        }
    }
    hash_set
}

async fn load_java_classes(url: &str) -> Result<HashSet<String>, Box<dyn std::error::Error>> {
    println!("Loading java class list from {}", url);
    let package_pattern = "(?P<package>([a-zA-Z_0-9]+)([.][a-zA-Z_0-9]+)+)";
    let class_name_pattern = "(?P<class>([a-zA-Z_0-9]+)([/]([a-zA-Z_0-9]+))+)";
    let re = regex::Regex::new(&format!(
        "href=\"{}/{}[.]html\"",
        package_pattern,
        class_name_pattern))?;

    let body = reqwest::get(url)
        .await?
        .text()
        .await?;

    let mut set = HashSet::new();

    for cap in re.captures_iter(&body) {
        let package = cap.name("package");
        if package.is_none() {
            continue;
        }

        let class = cap.name("class");
        if class.is_none() {
            continue;
        }
        let class = class.unwrap();
        let class = class.as_str();
        set.insert(class.to_string());
    }

    if set.len() == 0 {
        return Err(Box::new(io::Error::new(io::ErrorKind::Other, "Found no classes.")));
    }

    println!("Loaded {} classes names for java standard library.", &set.len());
    Ok(set)
}

pub struct JavaStdLib {
    loaded_classes: HashMap<String, JavaClass>,
    workdir: tempfile::TempDir,
    request_sender: mpsc::Sender<String>,
    response_receiver: mpsc::Receiver<Option<ClassData>>,
    java_class_list: &'static HashSet<String>,
}

impl JavaStdLib {
    pub fn new() -> io::Result<Self> {
        let workdir = tempfile::tempdir()?;
        Self::write_java_code(&workdir.path())?;
        Self::compile_java_code(&workdir.path())?;
        let (send, receive) = Self::run_java_inspector(&workdir.path())?;
        Ok(Self {
            loaded_classes: HashMap::new(),
            workdir,
            request_sender: send,
            response_receiver: receive,
            java_class_list: &JAVA_CLASS_LIST,
        })
    }

    pub fn is_standard_library_class(class_name: &str) -> bool {
        return JAVA_CLASS_LIST.contains(class_name);
    }

    pub fn get_all_java_class_names(&self) -> Vec<String> {
        self.java_class_list.iter().map(|s| s.to_string()).collect()
    }

    pub fn get_classes(&self) -> Vec<JavaClass> {
        let mut results = vec![];
        for value in self.loaded_classes.values() {
            results.push(value.clone());
        }
        return results;
    }

    pub fn get(&mut self, class_name: &str, constants: &mut StringConstants) -> Option<&JavaClass> {
        if self.loaded_classes.contains_key(class_name) {
            return self.loaded_classes.get(class_name);
        }
        if let Some(java_class) = self.load(class_name, constants) {
            self.loaded_classes
                .insert(class_name.to_string(), java_class);
            return self.loaded_classes.get(class_name);
        }
        None
    }

    fn load(&mut self, class_name: &str, constants: &mut StringConstants) -> Option<JavaClass> {
        self.request_sender.send(class_name.to_string());
        let response = self.response_receiver.recv();
        if let Ok(class_data) = response {
            if class_data.is_none() {
                return None;
            }
            let class_data = class_data.unwrap();
            let class_name = constants.put(&class_data.name);
            let fields = class_data
                .fields
                .iter()
                .map(|f| {
                    let field_name = constants.put(&f.name);
                    let field_type = constants.put(&f.type_name);
                    JavaField {
                        field_name,
                        field_type,
                    }
                })
                .collect();
            let methods = class_data
                .methods
                .iter()
                .map(|m| {
                    let method_name = constants.put(&m.name);
                    let return_type = constants.put(&m.return_type);
                    JavaMethod {
                        method_name,
                        param_types: vec![],
                        return_type: Some(return_type),
                    }
                })
                .collect();
            let superclass = constants.put(&class_data.superclass);
            let interfaces = class_data
                .interfaces
                .iter()
                .map(|i| constants.put(i))
                .collect();
            return Some(JavaClass {
                class_name,
                interfaces,
                superclass,
                fields,
                methods,
            });
        }
        None
    }

    fn run_java_inspector(
        workdir: &path::Path,
    ) -> io::Result<(mpsc::Sender<String>, mpsc::Receiver<Option<ClassData>>)> {
        let java = java_home_file("bin/java")?;
        let (req_sender, req_receiver) = mpsc::channel::<String>();
        let (resp_sender, resp_receiver) = mpsc::channel::<Option<ClassData>>();

        let workdir = workdir.to_str().unwrap().to_string();
        thread::spawn(move || {
            let proc = process::Command::new(java)
                .arg("JavaStdLib")
                .current_dir(&workdir)
                .stdout(Stdio::piped())
                .stdin(Stdio::piped())
                .stderr(Stdio::inherit())
                .spawn();
            if let Err(e) = proc {
                println!("Running java code: {:#?}", e);
                return;
            }
            let proc = proc.unwrap();
            if proc.stdout.is_none() {
                println!("Process has no stdout pipe.");
                println!("Workdir exists? {}", path::Path::new(&workdir).is_dir());
                return;
            }

            let mut stdout = io::BufReader::new(proc.stdout.unwrap());

            if proc.stdin.is_none() {
                println!("Process has no stdin pipe.");
                return;
            }
            let mut stdin = proc.stdin.unwrap();

            for class_name in req_receiver {
                let request_string = format!("{}\n", class_name.replace("/", "."));
                if let Err(e) = stdin.write_all(request_string.as_bytes()) {
                    break;
                }
                let mut current_class = ClassData {
                    name: "".to_string(),
                    superclass: "".to_string(),
                    interfaces: vec![],
                    methods: vec![],
                    fields: vec![],
                };
                let mut current_method = MethodData {
                    name: "".to_string(),
                    return_type: "".to_string(),
                };
                let mut current_field = FieldData {
                    name: "".to_string(),
                    type_name: "".to_string(),
                };
                loop {
                    let mut line = String::new();
                    let read = stdout.read_line(&mut line);
                    if let Err(e) = read {
                        break;
                    }
                    if let Ok(0) = read {
                        break;
                    }
                    let line = line.trim();
                    if let Some(class_name) = remove_prefix(&line, "ClassName: ") {
                        current_class = ClassData {
                            name: class_name.replace(".", "/").to_string(),
                            superclass: "java/lang/Object".to_string(),
                            interfaces: vec![],
                            methods: vec![],
                            fields: vec![],
                        };
                    } else if let Some(superclass) = remove_prefix(&line, "SuperClass: ") {
                        current_class.superclass = superclass.replace(".", "/").to_string();
                    } else if let Some(interface) = remove_prefix(&line, "Implements: ") {
                        current_class
                            .interfaces
                            .push(interface.replace(".", "/").to_string());
                    } else if let Some(method_name) = remove_prefix(&line, "MethodName: ") {
                        current_method = MethodData {
                            name: method_name.to_string(),
                            return_type: "".to_string(),
                        }
                    } else if let Some(method_type) = remove_prefix(&line, "MethodReturnType: ") {
                        current_method.return_type = method_type.replace(".", "/").to_string();
                    } else if line == "EndMethod" {
                        current_class.methods.push(current_method.clone());
                    } else if let Some(field_name) = remove_prefix(&line, "FieldName: ") {
                        current_field = FieldData {
                            name: field_name.to_string(),
                            type_name: "".to_string(),
                        };
                    } else if let Some(field_type) = remove_prefix(&line, "FieldType: ") {
                        current_field.type_name = field_type.replace(".", "/").to_string();
                    } else if line == "EndField" {
                        current_class.fields.push(current_field.clone());
                    } else if line == "EndClass" {
                        resp_sender.send(Some(current_class.clone()));
                        break;
                    } else if line == "NoClass" {
                        resp_sender.send(None);
                        break;
                    }
                }
            }
            println!("Done listening for classes.");
            stdin.write_all("END\n".as_bytes());
        });
        Ok((req_sender, resp_receiver))
    }

    fn compile_java_code(workdir: &path::Path) -> io::Result<()> {
        let javac = java_home_file("bin/javac")?;
        let mut proc = process::Command::new(javac)
            .arg("JavaStdLib.java")
            .current_dir(workdir)
            .spawn()?;
        let exit = proc.wait()?;
        if exit.success() {
            Ok(())
        } else {
            let mut stdout = String::new();
            let mut stderr = String::new();

            if proc.stdout.is_some() {
                proc.stdout.unwrap().read_to_string(&mut stdout);
            }
            if proc.stderr.is_some() {
                proc.stderr.unwrap().read_to_string(&mut stderr);
            }
            Err(io::Error::new(
                io::ErrorKind::Other,
                format!("{}\n\n{}", stdout, stderr),
            ))
        }
    }

    fn write_java_code(workdir: &path::Path) -> io::Result<()> {
        let java_file_path = workdir.join("JavaStdLib.java");
        let mut java_file = fs::File::create(java_file_path)?;
        java_file.write_all(include_str!("JavaStdLib.java").as_bytes());
        Ok(())
    }
}

#[derive(Clone)]
struct ClassData {
    name: String,
    superclass: String,
    interfaces: Vec<String>,
    methods: Vec<MethodData>,
    fields: Vec<FieldData>,
}

#[derive(Clone)]
struct MethodData {
    name: String,
    return_type: String,
}

#[derive(Clone)]
struct FieldData {
    name: String,
    type_name: String,
}

fn remove_prefix<'a>(text: &'a str, prefix: &str) -> Option<&'a str> {
    if text.starts_with(prefix) {
        return Some(&text[prefix.len()..]);
    }
    None
}

fn java_home_file(subpath: &str) -> Result<PathBuf, io::Error> {
    env::var("JAVA_HOME")
        .map(|home| path::Path::new(&home).join(subpath))
        .map_err(|_e| io::Error::new(io::ErrorKind::Other, "Unable to read JAVA_HOME."))
}
