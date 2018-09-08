use std::io::Read;
use std::fs::File;
use std::collections::HashMap;

fn read_byte<'a, T: Iterator<Item=&'a u8>>(data: T) -> Result<u8, String> {
    let hex = data.take(2).map(|x| *x).collect::<Vec<u8>>();
    if hex.len() != 2 {
        return Err("Not enough data.".to_owned());
    }

    match String::from_utf8(hex) {
        Ok(hexstr) => {
            match u8::from_str_radix(&hexstr, 16) {
                Ok(value) => Ok(value),
                Err(e) => Err(format!("{}", e).to_owned())
            }
        },
        Err(e) => Err(format!("{}", e).to_owned())
    }
}

fn from_hex(data: &Vec<u8>) -> Result<MemoryRepr, String> {
    let lines = data.split(|x| *x == ('\n' as u8));
    let parsed_lines: Result<Vec<(u8, u16, Vec<u8>)>, String> = lines.enumerate().filter(|(_, line)| line.len() != 0).map(|(idx, line)| {
        if line.len() < 3 {
            return Err(format!("Invalid line: {}", idx).to_owned());
        }

        if line[0] != ':' as u8 {
            return Err(format!("Line {} does not begin with ':'", idx).to_owned());
        }

        let mut bytesiter = line[1..].into_iter();

        let length = match read_byte(bytesiter.by_ref()) {
            Ok(value) => value,
            Err(e) => return Err(e)
        };

        let address = match (read_byte(bytesiter.by_ref()), read_byte(bytesiter.by_ref())) {
            (Ok(high), Ok(low)) => ((high as u16) << 8) | (low as u16),
            _ => return Err(format!("Line {} was invalid", idx).to_owned())
        };

        let tag = match read_byte(bytesiter.by_ref()) {
            Ok(value) => value,
            Err(e) => return Err(e)
        };

        let data: Vec<u8> = match (0..length).map(|_| {
            read_byte(bytesiter.by_ref())
        }).collect() {
            Ok(data) => data,
            Err(e) => return Err(e)
        };
        Ok((tag, address, data))
    }).collect();

    match parsed_lines {
        Ok(lines) => {
            let mut section = 0u8;
            let mut end = false;
            let mut repr = MemoryRepr::empty();

            let _: Result<Vec<()>, String> = lines.iter().map(|(tag, address, data)| {
                if end {
                    return Err("Malformed data: data after end of file.".to_owned());
                }
                // TODO: check checksums
                match tag {
                    0 => {
                        // data
                        let mut buf = repr.sections.entry(section).or_insert_with(|| Vec::new());

                        let end = (*address) as usize + data.len();

                        if end > buf.len() {
                            let len_before = buf.len();
                            buf.extend(vec![0; end - len_before]);
                        }

                        for i in 0..data.len() {
                            buf[(*address) as usize + i] = data[i];
                        }
                        Ok(())
                    },
                    1 => {
                        // end of file
                        end = true;
                        Ok(())
                    },
                    4 => {
                        // data should be two bytes
                        if data.len() != 2 {
                            // TODO: what line
                            return Err("Invalid length of data line".to_owned());
                        }

                        if data[0] != 0 {
                            // TODO: what line, what section. this error should go.
                            return Err("Section too large".to_owned());
                        }

                        section = data[1];
                        Ok(())
                    }
                    _ => { panic!("unrecognized tag type: {}", tag); }
                }
            }).collect();

            Ok(repr)
        },
        Err(e) => Err(e)
    }
}

pub struct MemoryRepr {
    pub sections: HashMap<u8, Vec<u8>>
}

impl MemoryRepr {
    pub fn empty() -> MemoryRepr {
        let map = HashMap::new();
        MemoryRepr {
            sections: map
        }
    }
    pub fn flat(data: Vec<u8>) -> MemoryRepr {
        let mut map = HashMap::new();
        map.insert(0u8, data);
        MemoryRepr {
            sections: map
        }
    }
}

pub fn load_from_file(file: &mut File) -> Result<MemoryRepr, String> {
    match file.metadata() {
        Ok(metadata) => {
            let mut result = vec![0; metadata.len() as usize];
            match file.read(&mut result) {
                Ok(_) => {
                    match from_hex(&result) {
                        Ok(repr) => Ok(repr),
                        Err(e) => {
                            println!("{}", e);
                            // maybe one day return some more abstract data format
                            // and use From and all that fancy rust stuff
                            Ok(MemoryRepr::flat(result))
                        }
                    }
                },
                Err(e) => {
                    Err(e.to_string())
                }
            }
        },
        Err(e) => Err(e.to_string())
    }
}
