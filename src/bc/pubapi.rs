//! Parses the public API of classes from their bytecode.
use std::collections::HashMap;

use noak;
use noak::descriptor::MethodDescriptor;
use noak::error::DecodeError;

use noak::reader::cpool::ConstantPool;
use serde::{Deserialize, Serialize};

use crate::bc::get_type_name;

use crate::sconst::{IndexedString, StringConstants};

#[derive(Clone)]
pub struct PublicApi {
    pub classes: Vec<JavaClass>,
    pub service_interfaces: ServiceInterfaces,
}

impl PublicApi {
    pub fn new() -> Self {
        Self {
            classes: vec![],
            service_interfaces: ServiceInterfaces::new(),
        }
    }
}

pub struct ApiParser<'pool> {
    classes: Vec<JavaClass>,
    service_interfaces: ServiceInterfaces,
    constants: &'pool mut StringConstants,
}

impl<'pool> ApiParser<'pool> {
    pub fn new(constants: &'pool mut StringConstants) -> Self {
        Self {
            classes: vec![],
            service_interfaces: ServiceInterfaces::new(),
            constants,
        }
    }

    pub fn get_api(self) -> PublicApi {
        PublicApi {
            classes: self.classes,
            service_interfaces: self.service_interfaces,
        }
    }

    pub fn add_class(
        &mut self,
        class: &mut noak::reader::Class,
    ) -> Result<(), noak::error::DecodeError> {
        if class.access_flags()?.contains(noak::AccessFlags::PRIVATE) {
            return Ok(()); // Class isn't public.
        }

        let class = self.parse_class(class)?;
        self.classes.push(class);
        Ok(())
    }

    fn parse_class(&mut self, class: &mut noak::reader::Class) -> Result<JavaClass, DecodeError> {
        let class_name = String::from_utf8_lossy(class.this_class_name()?.as_bytes());
        let mut api_methods: Vec<JavaMethod> = vec![];
        let mut api_fields: Vec<JavaField> = vec![];
        let mut interfaces: Vec<IndexedString> = vec![];
        let mut superclass = self.constants.put("java/lang/Object");

        if let Ok(interface_names) = class.interface_names() {
            for interface_name in interface_names {
                if let Ok(interface_name) = interface_name {
                    let interface_name = String::from_utf8_lossy(interface_name.as_bytes());
                    let interface_name = self.constants.put(&interface_name);
                    interfaces.push(interface_name);
                }
            }
        }

        if let Ok(superclass_name) = class.super_class_name() {
            if let Some(superclass_name) = superclass_name {
                let superclass_name = String::from_utf8_lossy(superclass_name.as_bytes());
                superclass = self.constants.put(&superclass_name);
            }
        }

        if let Ok(methods) = class.methods() {
            for method in methods {
                if let Ok(method) = method {
                    // if method.access_flags().contains(noak::AccessFlags::PRIVATE) {
                    //     continue;
                    // }

                    // Parse for public api.
                    if let Ok(method) = self.parse_method(class.pool()?, &method) {
                        api_methods.push(method);
                    }
                }
            }
        }

        if let Ok(fields) = class.fields() {
            for field in fields {
                if let Ok(field) = field {
                    // if field.access_flags().contains(noak::AccessFlags::PRIVATE) {
                    //     continue;
                    // }

                    // Parse for public api.
                    if let Ok(field) = self.parse_field(class.pool()?, &field) {
                        api_fields.push(field);
                    }
                }
            }
        }

        Ok(self.new_class(
            class_name.to_string(),
            api_methods,
            api_fields,
            interfaces,
            superclass,
        ))
    }

    fn parse_method(
        &mut self,
        pool: &ConstantPool,
        method: &noak::reader::Method,
    ) -> Result<JavaMethod, DecodeError> {
        let method_name = String::from_utf8_lossy(pool.get(method.name())?.content.as_bytes());
        let descriptor = MethodDescriptor::parse(pool.get(method.descriptor())?.content)?;
        let return_type = descriptor.return_type().map(|r| get_type_name(&r));
        let param_types: Vec<String> = descriptor.parameters().map(|p| get_type_name(&p)).collect();
        Ok(self.new_method(method_name.to_string(), param_types, return_type))
    }

    fn parse_field(
        &mut self,
        pool: &ConstantPool,
        field: &noak::reader::Field,
    ) -> Result<JavaField, DecodeError> {
        let field_name = String::from_utf8_lossy(pool.get(field.name())?.content.as_bytes());
        let descriptor =
            noak::descriptor::TypeDescriptor::parse(pool.get(field.descriptor())?.content)?;
        Ok(self.new_field(field_name.to_string(), get_type_name(&descriptor)))
    }

    pub fn is_empty(&self) -> bool {
        self.constants.size() == 0
    }

    pub fn new_class(
        &mut self,
        name: String,
        methods: Vec<JavaMethod>,
        fields: Vec<JavaField>,
        interfaces: Vec<IndexedString>,
        superclass: IndexedString,
    ) -> JavaClass {
        JavaClass {
            class_name: self.constants.put(&name),
            methods,
            fields,
            interfaces,
            superclass,
        }
    }

    pub fn new_method(
        &mut self,
        name: String,
        param_types: Vec<String>,
        return_type: Option<String>,
    ) -> JavaMethod {
        JavaMethod {
            method_name: self.constants.put(&name),
            param_types: param_types
                .into_iter()
                .map(|s| self.constants.put(&s))
                .collect(),
            return_type: return_type.map(|s| self.constants.put(&s)),
        }
    }

    pub fn new_field(&mut self, field_name: String, field_type: String) -> JavaField {
        JavaField {
            field_name: self.constants.put(&field_name),
            field_type: self.constants.put(&field_type),
        }
    }
}

#[derive(Clone)]
pub struct JavaClass {
    pub class_name: IndexedString,
    pub interfaces: Vec<IndexedString>,
    pub superclass: IndexedString,
    pub fields: Vec<JavaField>,
    pub methods: Vec<JavaMethod>,
}

#[derive(Clone)]
pub struct JavaField {
    pub field_name: IndexedString,
    pub field_type: IndexedString,
}

#[derive(Clone)]
pub struct JavaMethod {
    pub method_name: IndexedString,
    pub param_types: Vec<IndexedString>,
    pub return_type: Option<IndexedString>,
}

impl JavaClass {
    pub fn class_name<'a>(&self, pool: &'a StringConstants) -> &'a str {
        self.class_name
            .get_str(pool)
            .expect("Class name not in constants.")
    }

    pub fn get_package_name(&self, pool: &StringConstants) -> Option<String> {
        self.class_name.get_str(pool)
            .and_then(|name| Self::parse_package(name))
    }

    pub fn parse_package(class_name: &str) -> Option<String> {
        if let Some(slash) = class_name.rfind("/") {
            return Some(class_name[0..slash].replace("/", "."));
        }
        None
    }
}

impl JavaField {
    pub fn field_name<'a>(&self, pool: &'a StringConstants) -> &'a str {
        self.field_name
            .get_str(pool)
            .expect("Field name not in constants.")
    }

    pub fn field_type<'a>(&self, pool: &'a StringConstants) -> &'a str {
        self.field_type
            .get_str(pool)
            .expect("Field type not in constants.")
    }
}

impl JavaMethod {
    pub fn method_name<'a>(&self, pool: &'a StringConstants) -> &'a str {
        self.method_name
            .get_str(pool)
            .expect("Method name not in constants.")
    }

    pub fn param_types<'a>(&self, pool: &'a StringConstants) -> Vec<&'a str> {
        self.param_types
            .iter()
            .map(|index| -> &str {
                index
                    .get_str(pool)
                    .expect("Parameter name not in constants.")
            })
            .collect()
    }

    pub fn return_type<'a>(&self, pool: &'a StringConstants) -> Option<&'a str> {
        match &self.return_type {
            Some(i) => i.get_str(pool),
            None => None,
        }
    }

    pub fn signature(&self, pool: &StringConstants) -> String {
        let mut text = String::new();
        text.push_str(self.method_name(pool));
        text.push('(');
        for (i, p) in self.param_types(pool).iter().enumerate() {
            if i > 0 {
                text.push_str(", ");
            }
            text.push_str(p);
        }
        text.push(')');
        if let Some(ret) = self.return_type(pool) {
            text.push_str(": ");
            text.push_str(ret);
        }
        text
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct ServiceInterfaces {
    mapping: HashMap<String, Vec<String>>,
}

impl ServiceInterfaces {
    pub fn new() -> Self {
        Self {
            mapping: HashMap::new(),
        }
    }

    pub fn add(&mut self, interface: &str, implementation: &str) {
        if !self.mapping.contains_key(interface) {
            self.mapping
                .insert(String::from(interface), vec![String::from(implementation)]);
            return;
        }
        self.mapping
            .get_mut(interface)
            .unwrap()
            .push(String::from(implementation));
    }

    pub fn add_all(&mut self, interface: &str, implementations: Vec<String>) {
        if !self.mapping.contains_key(interface) {
            self.mapping
                .insert(String::from(interface), implementations);
            return;
        }
        self.mapping
            .get_mut(interface)
            .unwrap()
            .extend(implementations);
    }

    pub fn interfaces(&self) -> Vec<&str> {
        return self.mapping.keys().map(|s| -> &str { s }).collect();
    }

    pub fn implementations(&self, interface: &str) -> Vec<&str> {
        match self.mapping.get(interface) {
            None => vec![],
            Some(classes) => classes.iter().map(|s| -> &str { s }).collect(),
        }
    }

    pub fn merge_from(&mut self, services: &ServiceInterfaces) -> () {
        for interface in services.interfaces() {
            self.add_all(
                interface,
                services
                    .implementations(interface)
                    .iter()
                    .map(|s| String::from(*s))
                    .collect(),
            );
        }
    }
}
