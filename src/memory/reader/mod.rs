use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

use memory::repr::{FlatMemoryRepr, ProcessMemoryRepr};
use memory::repr::process::ModuleData;

pub mod hex;

pub enum FileRepr {
    IntelHEX(HashMap<u8, FlatMemoryRepr>),
    Executable(ModuleData),
    Flat(FlatMemoryRepr)
}

pub fn load_from_file(file: &mut File) -> Result<FileRepr, String> {
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
            match ModuleData::load_from(&bytes) {
                Some(data) => { Ok(FileRepr::Executable(data)) }
                None => {
                    Ok(FileRepr::Flat(FlatMemoryRepr::of(bytes)))
                }
            }
        }
    }
}
