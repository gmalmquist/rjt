use noak::descriptor::{BaseType, TypeDescriptor};
use noak::reader::cpool;

pub mod javastd;
pub mod pubapi;
pub mod refer;

fn get_type_name(t: &TypeDescriptor) -> String {
    match t.base() {
        BaseType::Boolean => "java/lang/Boolean".to_string(),
        BaseType::Byte => "java/lang/Byte".to_string(),
        BaseType::Short => "java/lang/Short".to_string(),
        BaseType::Integer => "java/lang/Integer".to_string(),
        BaseType::Long => "java/lang/Long".to_string(),
        BaseType::Float => "java/lang/Float".to_string(),
        BaseType::Double => "java/lang/Double".to_string(),
        BaseType::Char => "java/lang/Char".to_string(),
        BaseType::Object(name) => String::from_utf8_lossy(name.as_bytes()).to_string(),
    }
}

fn to_real_string(utf8: &cpool::Utf8) -> String {
    String::from_utf8_lossy(utf8.content.as_bytes()).to_string()
}
