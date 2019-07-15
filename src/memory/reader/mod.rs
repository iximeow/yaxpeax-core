use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

use memory::repr::FlatMemoryRepr;
use memory::repr::process::ModuleData;

use std::fs::OpenOptions;
use std::path::Path;

pub mod hex;

pub enum FileRepr {
    IntelHEX(HashMap<u8, FlatMemoryRepr>),
    Executable(ModuleData),
    Flat(FlatMemoryRepr)
}

pub fn load_from_path(path: &Path) -> Result<FileRepr, String> {
    let mut file = OpenOptions::new().read(true).open(path).unwrap();

    let metadata = match file.metadata() {
        Ok(metadata) => metadata,
        Err(e) => { return Err(e.to_string()); }
    };
    let mut bytes = vec![0; metadata.len() as usize];
    match file.read(&mut bytes) {
        Ok(_) => { },
        Err(e) => { return Err(e.to_string()); }
    };

    match hex::from_hex(&bytes) {
        Ok(repr) => Ok(FileRepr::IntelHEX(repr)),
        Err(e) => {
            println!("{}", e);
            match ModuleData::load_from(&bytes, format!("{}", path.display())) {
                Some(data) => { Ok(FileRepr::Executable(data)) }
                None => {
                    Ok(FileRepr::Flat(FlatMemoryRepr::of(bytes)))
                }
            }
        }
    }
}
