use std::env;
use std::fs;
use std::fs::File;
use std::io;
use std::io::{Read, Write};
use std::path;

use json;
use shellexpand;

const DEFAULT_CACHE_DIR: &'static str = "~/.cache/rjt";

pub struct Cache {
    cache_dir: String,
}

impl Cache {
    pub fn new() -> io::Result<Self> {
        let mut cache_dir = if let Ok(cache_dir) = env::var("CACHE_DIR") {
            println!(
                "Using cache from environment variable CACHE_DIR={}",
                cache_dir
            );
            cache_dir
        } else {
            println!("Using hardcoded cache location {}", DEFAULT_CACHE_DIR);
            String::from(DEFAULT_CACHE_DIR)
        };
        cache_dir = String::from(shellexpand::tilde(&cache_dir));
        let path = path::Path::new(&cache_dir);
        if path.exists() && !path.is_dir() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("Cache dir '{}' already exists and is a file.", &cache_dir),
            ));
        }
        if !path.exists() {
            fs::create_dir_all(&cache_dir)?;
        }
        Ok(Cache { cache_dir })
    }

    pub fn load<T: CacheData>(&self, path: &str) -> io::Result<T> {
        let mut full_path = path::PathBuf::from(&self.cache_dir);
        full_path.push(&path);
        let mut file = fs::File::open(full_path)?;
        CacheData::read(&mut file)
    }

    pub fn save<T: CacheData>(&self, path: &str, cache_data: &T) -> io::Result<()> {
        let mut full_path = path::PathBuf::from(&self.cache_dir);
        full_path.push(&path);
        let parent = full_path.parent();
        if let None = parent {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Invalid path, no parent directory.",
            ));
        }
        let parent = parent.unwrap();
        if !parent.is_dir() {
            fs::create_dir_all(parent)?;
        }
        let mut file = fs::File::create(full_path)?;
        cache_data.write(&mut file)
    }
}

pub trait CacheData {
    fn read(file: &mut fs::File) -> io::Result<Self>
    where
        Self: std::marker::Sized;

    fn write(&self, file: &mut fs::File) -> io::Result<()>;
}

impl CacheData for String {
    fn read(file: &mut File) -> io::Result<Self> {
        let mut string = String::new();
        file.read_to_string(&mut string)?;
        Ok(string)
    }

    fn write(&self, file: &mut fs::File) -> io::Result<()> {
        file.write_all(self.as_bytes())
    }
}

impl CacheData for json::JsonValue {
    fn read(file: &mut File) -> io::Result<Self> {
        let mut string = String::new();
        file.read_to_string(&mut string)?;
        match json::parse(&string) {
            Ok(value) => Ok(value),
            Err(e) => Err(io::Error::new(io::ErrorKind::InvalidData, e.to_string())),
        }
    }

    fn write(&self, file: &mut fs::File) -> io::Result<()> {
        self.write(file)
    }
}
