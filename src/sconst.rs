use std::collections::HashMap;
use std::fmt::Formatter;

#[derive(Clone, Copy, Eq, Ord, PartialOrd, PartialEq, Hash, Debug)]
pub struct IndexedString {
    index: usize,
}

impl std::fmt::Display for IndexedString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "IndexedString({})", self.index)
    }
}

impl IndexedString {
    pub fn new(index: usize) -> Self {
        Self { index }
    }

    pub fn from(string: &str, constants: &mut StringConstants) -> Self {
        constants.put(string)
    }

    pub fn get_str<'a>(&self, constants: &'a StringConstants) -> Option<&'a str> {
        constants.get_constant(self.index)
    }

    pub fn get_string(&self, constants: &StringConstants) -> Option<String> {
        self.get_str(constants).map(String::from)
    }
}

#[derive(Clone)]
pub struct StringConstants {
    constants_to_indices: HashMap<String, usize>,
    indices_to_constants: HashMap<usize, String>,
}

impl StringConstants {
    pub fn new() -> Self {
        Self {
            constants_to_indices: HashMap::new(),
            indices_to_constants: HashMap::new(),
        }
    }

    pub fn get_index(&self, constant: &str) -> Option<usize> {
        return self.constants_to_indices.get(constant).map(|i| *i);
    }

    pub fn get_constant(&self, index: usize) -> Option<&str> {
        return self.indices_to_constants.get(&index).map(|s| -> &str { s });
    }

    pub fn put(&mut self, constant: &str) -> IndexedString {
        if let Some(index) = self.constants_to_indices.get(constant) {
            return IndexedString::new(*index);
        }
        let index = self.size();
        self.constants_to_indices
            .insert(constant.to_string(), index);
        self.indices_to_constants
            .insert(index, constant.to_string());
        IndexedString { index }
    }

    /// Take the given string constant, and return a new string constant that points to a string
    /// stored in this pool.
    pub fn get_ref_for<'a>(&'a mut self, constant: &str) -> &'a str {
        let index = self.put(constant);
        return self
            .indices_to_constants
            .get(&index.index)
            .expect("Didn't contain index we just inserted! This makes no sense.");
    }

    pub fn size(&self) -> usize {
        self.indices_to_constants.len()
    }
}
