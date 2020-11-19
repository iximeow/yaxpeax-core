use goblin;
use goblin::Object;

use arch::ISA;

use yaxpeax_arch::{Address, AddressDisplay};
use memory::repr::FlatMemoryRepr;
use memory::repr::{ReadCursor, UnboundedCursor};
use memory::{LayoutError, MemoryRange, MemoryRepr, Named, PatchyMemoryRepr};
use std::ops::Range;

#[derive(Debug)]
pub struct PESymbol {
    pub name: String,
    pub va: u64
}
#[derive(Debug)]
pub struct PEReloc {}
#[derive(Debug)]
pub struct PEImport {
    pub name: String,
    pub dll: String,
    ordinal: u16,
    pub offset: usize,
    pub rva: usize,
    size: usize
}
impl <'a, 'b> From<&'b goblin::pe::import::Import<'a>> for PEImport {
    fn from(imp: &'b goblin::pe::import::Import<'a>) -> PEImport {
        match imp {
            goblin::pe::import::Import {
                name,
                dll,
                ordinal,
                offset,
                rva,
                size
            } => {
                PEImport {
                    name: name.to_string(),
                    dll: dll.to_string(),
                    ordinal: *ordinal,
                    offset: *offset,
                    rva: *rva,
                    size: *size
                }
            }
        }
    }
}
#[derive(Debug)]
pub enum Reexport {
    DLLName(String, String),
    DLLOrdinal(String, usize)
}
impl <'a, 'b> From<&'b goblin::pe::export::Reexport<'a>> for Reexport {
    fn from(reexp: &'b goblin::pe::export::Reexport<'a>) -> Reexport {
        match reexp {
            goblin::pe::export::Reexport::DLLName {
                export,
                lib
            } => { Reexport::DLLName(lib.to_string(), export.to_string()) },
            goblin::pe::export::Reexport::DLLOrdinal {
                // TODO: goblin claims that dllordinal reexports specify the ordinal is for the dll
                // name or something? need to look into this..
                export,
                ordinal
            } => { Reexport::DLLOrdinal(export.to_string(), *ordinal) }
        }
    }
}
#[derive(Debug)]
pub struct PEExport {
    pub name: Option<String>,
    pub offset: usize,
    pub rva: usize,
    size: usize,
    reexport: Option<Reexport>
}
impl <'a, 'b> From<&'b goblin::pe::export::Export<'a>> for PEExport {
    fn from(exp: &'b goblin::pe::export::Export<'a>) -> PEExport {
        match exp {
            goblin::pe::export::Export {
                name,
                offset,
                rva,
                size,
                reexport
            } => {
                PEExport {
                    name: name.map(|x| x.to_string()),
                    offset: *offset,
                    rva: *rva,
                    size: *size,
                    reexport: reexport.as_ref().map(|x| x.into())
                }
            }
        }
    }
}
#[derive(Debug)]
pub struct ELFSymbol {
    pub name: String,
    pub section_index: usize,
    pub addr: u64
}
#[derive(Debug)]
pub struct ELFReloc {}
#[derive(Debug)]
pub struct ELFImport {
    pub name: String,
    pub section_index: usize,
    pub value: u64
}
#[derive(Debug)]
pub struct ELFExport {
    pub name: String,
    pub section_index: usize,
    pub addr: u64,
}

#[derive(Debug)]
pub struct Segment {
    start: usize,
    data: Vec<u8>,
    name: String
}

impl Segment {
    fn contains<A: Address>(&self, addr: A) -> bool {
        let linear = addr.to_linear();
        if linear < self.start {
            false
        } else if linear - self.start >= self.data.len() {
            false
        } else {
            true
        }
    }
    pub fn start(&self) -> usize {
        self.start
    }
    pub fn end(&self) -> usize {
        self.start + self.data.len()
    }
}

impl Named for Segment {
    fn name(&self) -> &str {
        &self.name
    }
}

impl <A: Address> MemoryRepr<A> for Segment {
    fn read(&self, addr: A) -> Option<u8> {
        if self.contains(addr) {
            Some(self.data[addr.to_linear() - self.start])
        } else {
            None
        }
    }
    fn to_flat(self) -> Option<FlatMemoryRepr> {
        None
    }
    fn module_info(&self) -> Option<&ModuleInfo> { None }
    fn module_for(&self, addr: A) -> Option<&dyn MemoryRepr<A>> {
        if self.contains(addr) {
            Some(self)
        } else {
            None
        }
    }
    fn size(&self) -> Option<u64> {
        Some(self.data.len() as u64)
    }
    fn start(&self) -> Option<u64> {
        Some(self.start as u64)
    }
}

#[derive(Debug)]
pub enum ISAHint {
    Hint(crate::arch::ISA),
    Unknown(String)
}

#[derive(Debug)]
pub struct ELFSection {
    pub name: String,
    pub start: u64,
    pub size: u64,
}

#[derive(Debug)]
pub enum ModuleInfo {
    PE(ISAHint, goblin::pe::header::Header, Vec<goblin::pe::section_table::SectionTable>, u64, Vec<PEReloc>, Vec<PEImport>, Vec<PEExport>, Vec<PESymbol>),
    ELF(ISAHint, goblin::elf::header::Header, Vec<goblin::elf::program_header::ProgramHeader>, Vec<ELFSection>, u64, Vec<ELFReloc>, Vec<ELFImport>, Vec<ELFExport>, Vec<ELFSymbol>)
    /*
     * One day, also MachO, .a, .o, .class, .jar, ...
     */
}

impl ModuleInfo {
    pub fn isa_hint(&self) -> &ISAHint {
        match self {
            ModuleInfo::PE(hint, _, _, _, _, _, _, _) |
            ModuleInfo::ELF(hint, _, _, _, _, _, _, _, _) => {
                hint
            }
        }
    }
}

mod elf {
    pub(crate) mod program_header {
        pub(crate) fn type_to_str(machine: u16, tpe: u32) -> String {
            match tpe {
                goblin::elf::program_header::PT_NULL => "NULL".to_string(),
                goblin::elf::program_header::PT_LOAD => "LOAD".to_string(),
                goblin::elf::program_header::PT_DYNAMIC => "DYNAMIC".to_string(),
                goblin::elf::program_header::PT_INTERP => "INTERP".to_string(),
                goblin::elf::program_header::PT_NOTE => "NOTE".to_string(),
                goblin::elf::program_header::PT_SHLIB => "SHLIB".to_string(),
                goblin::elf::program_header::PT_PHDR => "PHDR".to_string(),
                goblin::elf::program_header::PT_TLS => "TLS".to_string(),
                other => {
                    if other >= goblin::elf::program_header::PT_LOOS && other <= goblin::elf::program_header::PT_HIOS {
                        // best-effort, try to match any number
                        match other {
                            goblin::elf::program_header::PT_GNU_EH_FRAME => {
                                "GNU_EH_FRAME".to_string()
                            }
                            // not in goblin
                            // goblin::elf::program_header::PT_SUNW_UNWIND => {
                            0x6464e550 => {
                                "SUNW_UNWIND".to_string()
                            }
                            goblin::elf::program_header::PT_GNU_STACK => {
                                "GNU_STACK".to_string()
                            }
                            goblin::elf::program_header::PT_GNU_RELRO => {
                                "GNU_RELRO".to_string()
                            }
                            // not in goblin
                            // goblin::elf::program_header::PT_OPENBSD_RANDOMIZE => {
                            0x65a3dbe6 => {
                                "OPENBSD_RANDOMIZE".to_string()
                            }
                            // goblin::elf::program_header::PT_OPENBSD_WXNEEDED => {
                            0x65a3dbe7 => {
                                "OPENBSD_WXNEEDED".to_string()
                            }
                            // goblin::elf::program_header::PT_OPENBSD_BOOTDATA => {
                            0x65a41be6 => {
                                "OPENBSD_BOOTDATA".to_string()
                            }
                            _ => {
                                format!("unknown OS section type: {:#x}", other)
                            }
                        }
                    } else if other >= goblin::elf::program_header::PT_LOPROC && other <= goblin::elf::program_header::PT_HIPROC {
                        match (machine, other) {
                            (goblin::elf::header::EM_ARM, 0x70000000) => {
                                "ARM_ARCHEXT".to_string()
                            }
                            (goblin::elf::header::EM_ARM, 0x70000001) => {
                                // These contain stack unwind tables.
                                "ARM_UNWIND".to_string()
                            }
                            (goblin::elf::header::EM_MIPS, 0x70000000) => {
                                // Register usage information.
                                "MIPS_REGINFO".to_string()
                            }
                            (goblin::elf::header::EM_MIPS, 0x70000001) => {
                                // Runtime procedure table.
                                "MIPS_RTPROC".to_string()
                            }
                            (goblin::elf::header::EM_MIPS, 0x70000002) => {
                                // Options segment.
                                "MIPS_OPTIONS".to_string()
                            }
                            (goblin::elf::header::EM_MIPS, 0x70000003) => {
                                // Abiflags segment.
                                "MIPS_ABIFLAGS".to_string()
                            }
                            (machine, other) => {
                                format!("unknown machine-dependent section type, machine: {:#x} p_type: {:#x}", machine, other)
                            }
                        }
                    } else {
                        format!("unknown elf section type: {:#x}", other)
                    }
                }
            }
        }
    }
}

fn map_elf_machine(machine: u16) -> ISAHint {
    match machine {
        3 => {
            // IMAGE_FILE_MACHINE_I386
            ISAHint::Hint(ISA::x86)
        },
        20 => {
            ISAHint::Hint(ISA::PowerPC)
        },
        40 => {
            // ELF doesn't hint at *what kind* of ARM..
            ISAHint::Hint(ISA::ARM)
        }
        41 => {
            // IMAGE_FILE_MACHINE_ALPHA
            ISAHint::Hint(ISA::Alpha)
        },
        /*
         * The SuperH chips are interesting because ELF only hints SH, not what revision...
        42 => {
            // IMAGE_FILE_MACHINE_SH3
            ISAHint::Hint(ISA::SH3)
        },
        0x01a3 => {
            // IMAGE_FILE_MACHINE_SH3DSP
            ISAHint::Hint(ISA::SH3DSP)
        },
        0x01a4 => {
            // IMAGE_FILE_MACHINE_SH3E
            ISAHint::Hint(ISA::SH3E)
        },
        0x01a6 => {
            // IMAGE_FILE_MACHINE_SH4
            ISAHint::Hint(ISA::SH4)
        },
        0x01a8 => {
            // IMAGE_FILE_MACHINE_SH5
            ISAHint::Unknown("SuperH-5 never had shipping hardware...".to_string())
        },
        */
        44 => {
            // IMAGE_FILE_MACHINE_TRICORE
            ISAHint::Hint(ISA::Tricore)
        },
        50 => {
            // IMAGE_FILE_MACHINE_IA64
            ISAHint::Hint(ISA::IA64)
        },
        62 => {
            // IMAGE_FILE_MACHINE_AMD64
            ISAHint::Hint(ISA::x86_64)
        },
        /*
        0x0162 => {
            // IMAGE_FILE_MACHINE_R3000
            // this is specifically MIPS-1?
            ISAHint::Hint(ISA::MIPS)
        },
        0x0166 => {
            // IMAGE_FILE_MACHINE_R4000
            // specifically, MIPS-3?
            ISAHint::Hint(ISA::MIPS)
        },
        0x0168 => {
            // IMAGE_FILE_MACHINE_R10000
            // specifically, MIPS-4?
            ISAHint::Hint(ISA::MIPS)
        },
        0x0169 => {
            // IMAGE_FILE_MACHINE_WCEMIPSV2
            // ???
            ISAHint::Hint(ISA::MIPS)
        },
        */
        183 => { // this is from osdev.org, because i can't find the actual header!
            ISAHint::Hint(ISA::AArch64)
        },
        magic @ _ => {
            ISAHint::Unknown(format!("Unknown machine magic: {:#x}", magic))
        }
    }
}
fn map_pe_machine(machine: u16) -> ISAHint {
    match machine {
        0x0000 => {
            // IMAGE_FILE_MACHINE_UNKNOWN
            ISAHint::Unknown("PE machine field is 0".to_string())
        },
        0x014d => {
            // IMAGE_FILE_MACHINE_I860
            ISAHint::Unknown("Okay, I know this is an Intel I860, but I don't know what to do with that.".to_string())
        },
        0x014c => {
            // IMAGE_FILE_MACHINE_I386
            ISAHint::Hint(ISA::x86)
        },
        0x0162 => {
            // IMAGE_FILE_MACHINE_R3000
            // this is specifically MIPS-1?
            ISAHint::Hint(ISA::MIPS)
        },
        0x0166 => {
            // IMAGE_FILE_MACHINE_R4000
            // specifically, MIPS-3?
            ISAHint::Hint(ISA::MIPS)
        },
        0x0168 => {
            // IMAGE_FILE_MACHINE_R10000
            // specifically, MIPS-4?
            ISAHint::Hint(ISA::MIPS)
        },
        0x0169 => {
            // IMAGE_FILE_MACHINE_WCEMIPSV2
            // ???
            ISAHint::Hint(ISA::MIPS)
        },
        0x0184 => {
            // IMAGE_FILE_MACHINE_ALPHA
            ISAHint::Hint(ISA::Alpha)
        },
        0x01a2 => {
            // IMAGE_FILE_MACHINE_SH3
            ISAHint::Hint(ISA::SH3)
        },
        0x01a3 => {
            // IMAGE_FILE_MACHINE_SH3DSP
            ISAHint::Hint(ISA::SH3DSP)
        },
        0x01a4 => {
            // IMAGE_FILE_MACHINE_SH3E
            ISAHint::Hint(ISA::SH3E)
        },
        0x01a6 => {
            // IMAGE_FILE_MACHINE_SH4
            ISAHint::Hint(ISA::SH4)
        },
        0x01a8 => {
            // IMAGE_FILE_MACHINE_SH5
            ISAHint::Unknown("SuperH-5 never had shipping hardware...".to_string())
        },
        0x01c0 => {
            // IMAGE_FILE_MACHINE_ARM
            ISAHint::Hint(ISA::ARM)
        },
        0x01c2 => {
            // IMAGE_FILE_MACHINE_THUMB
            ISAHint::Hint(ISA::ARM)
        },
        0x01c4 => {
            // IMAGE_FILE_MACHINE_ARMNT -- windows for ARM Thumb-2 Little-Endian
            ISAHint::Hint(ISA::ARM)
        },
        0x01d3 => {
            // IMAGE_FILE_MACHINE_AM33
            ISAHint::Unknown("AM33?".to_string())
        },
        0x01f0 => {
            // IMAGE_FILE_MACHINE_POWERPC
            ISAHint::Hint(ISA::PowerPC)
        },
        0x01f1 => {
            // IMAGE_FILE_MACHINE_POWERPCFP
            ISAHint::Hint(ISA::PowerPC)
        },
        0x0200 => {
            // IMAGE_FILE_MACHINE_IA64
            ISAHint::Hint(ISA::IA64)
        },
        0x0266 => {
            // IMAGE_FILE_MACHINE_MIPS16
            ISAHint::Hint(ISA::MIPS)
        },
        0x0284 => {
            // IMAGE_FILE_MACHINE_ALPHA64 // aka: AXP64
            ISAHint::Hint(ISA::Alpha64)
        },
        0x0366 => {
            // IMAGE_FILE_MACHINE_MIPSFPU
            ISAHint::Hint(ISA::MIPS)
        },
        0x0466 => {
            // IMAGE_FILE_MACHINE_MIPSFPU16
            ISAHint::Hint(ISA::MIPS)
        },
        0x0520 => {
            // IMAGE_FILE_MACHINE_TRICORE
            ISAHint::Hint(ISA::Tricore)
        },
        0x0cef => {
            // IMAGE_FILE_MACHINE_CEF
            // https://blogs.msdn.microsoft.com/mikehall/2004/11/28/remember-cef-common-executable-format/
            ISAHint::Unknown("Common Executable Format? Sounds fake".to_string())
        },
        0x0ebc => {
            // IMAGE_FILE_MACHINE_EBC
            ISAHint::Unknown("EBC is unknown".to_string())
        },
        0x8664 => {
            // IMAGE_FILE_MACHINE_AMD64
            ISAHint::Hint(ISA::x86_64)
        },
        0x9041 => {
            // IMAGE_FILE_MACHINE_M32R
            ISAHint::Unknown("M32R is unknown".to_string())
        },
        0xc0ee => {
            // IMAGE_FILE_MACHINE_CEE
            ISAHint::Unknown("CEE is unknown".to_string())
        },
        0x01c5 => {
            // IMAGE_FILE_MACHINE_ARM64 // WINE ONLY
            ISAHint::Hint(ISA::AArch64)
        },
        0xaa64 => {
            // IMAGE_FILE_MACHINE_ARM64 // Windows 8.1+
            ISAHint::Hint(ISA::AArch64)
        },
        magic @ _ => {
            ISAHint::Unknown(format!("Unknown machine magic: {:#x}", magic))
        }
    }
}

impl ModuleInfo {
    pub fn from_goblin(obj: &goblin::Object, _data: &[u8]) -> Option<ModuleInfo> {
        match obj {
            Object::PE(pe) => {
                // From "winnt.h" .. kind of.
                // https://github.com/Alexpux/mingw-w64/blob/master/mingw-w64-tools/widl/include/winnt.h
                // also
                // https://docs.microsoft.com/en-us/windows/desktop/sysinfo/image-file-machine-constants
                // note they disagree on AArch64..
                let isa = map_pe_machine(pe.header.coff_header.machine);

    // PE(ISAHint, goblin::pe::header::Header, Vec<goblin::pe::section_table::SectionTable>, Vec<PEReloc>, Vec<PEImport>, Vec<PEExport>),
                Some(ModuleInfo::PE(
                    isa,
                    pe.header,
                    vec![],
                    pe.header.optional_header.map(|x| x.windows_fields.image_base as usize).unwrap_or(0x400000) as u64,
                    vec![],
                    pe.imports.iter().map(|x| x.into()).collect(),
                    pe.exports.iter().map(|x| x.into()).collect(),
                    vec![]
                ))
            }
            Object::Elf(elf) => {
                let imports: Vec<ELFImport> = Vec::new();
                let mut exports: Vec<ELFExport> = Vec::new();
                let mut syms: Vec<ELFSymbol> = Vec::new();
                let mut sections: Vec<ELFSection> = Vec::new();
                for section in elf.section_headers.iter() {
                    if section.sh_name == 0 {
                        continue;
                    }
                    sections.push(ELFSection {
                        name: elf.shdr_strtab.get(section.sh_name).unwrap().unwrap().to_string(),
                        start: section.sh_addr,
                        size: section.sh_size,
                    });
                }
                for sym in elf.syms.iter() {
                    syms.push(ELFSymbol {
                        name: elf.strtab.get(sym.st_name).unwrap().unwrap().to_string(),
                        section_index: sym.st_shndx,
                        addr: sym.st_value
                    })
                }

                for dynsym in elf.dynsyms.iter() {
                    // these are dynamically resolved symbols.
                    // this is what i'll call an 'import'
                    //
                    // if sy_value == 0 it's probably (not necessarily?) a symbol to be referenced
                    // by a reloc for a .got entry
                    if dynsym.st_bind() == 1 /* global */ && dynsym.st_type() == 2 /* func */ && dynsym.st_value != 0 /* bad check for "is it a rexport or actually local" */ {
                        // we have ourselves a bona fide export symbol, maybe? try it on for size.
                        syms.push(ELFSymbol {
                            name: elf.dynstrtab.get(dynsym.st_name).unwrap().unwrap().to_string(),
                            section_index: dynsym.st_shndx,
                            addr: dynsym.st_value
                        });
                        exports.push(ELFExport {
                            name: elf.dynstrtab.get(dynsym.st_name).unwrap().unwrap().to_string(),
                            section_index: dynsym.st_shndx,
                            addr: dynsym.st_value
                        });
                    } else {
                        // got entry, figure this out.
                    }
                }

                let isa = map_elf_machine(elf.header.e_machine);

    // PE(ISAHint, goblin::pe::header::Header, Vec<goblin::pe::section_table::SectionTable>, Vec<PEReloc>, Vec<PEImport>, Vec<PEExport>),
                Some(ModuleInfo::ELF(
                    isa,
                    elf.header,
                    vec![],
                    sections,
                    elf.entry,
                    vec![],
                    imports,
                    exports,
                    //elf.exports.iter().map(|x| x.into()).collect()
                    syms
                ))
            }
            _ => {
                None
            }
        }
    }
}

#[derive(Debug)]
pub struct ModuleData {
    pub segments: Vec<Segment>,
    pub module_info: ModuleInfo,
    pub name: String
//    pub headers: Option<goblin::Object<'a>>
}

impl ModuleData {
    pub fn load_from(data: &[u8], name: String) -> Option<ModuleData> {
        match Object::parse(data) {
            Ok(obj @ Object::Elf(_)) => {
                let mut module = ModuleData {
                    segments: vec![],
                    module_info: ModuleInfo::from_goblin(&obj, data).unwrap(),
                    name
                };

                let elf = match obj {
                    Object::Elf(elf) => elf,
                    _ => panic!()
                };
/*
                println!("Parsed ELF: {:?}", elf);

                for sym in elf.dynsyms.iter() {
                    println!("dynsym: {:?}", sym);
                    println!("Name: {}", elf.dynstrtab.get(sym.st_name).unwrap().unwrap());
                }
                for sym in elf.syms.iter() {
                    println!("sym: {:?}", sym);
                    println!("Name: {}", elf.strtab.get(sym.st_name).unwrap().unwrap());
                }
*/
                for (i, section) in elf.program_headers.iter().enumerate() {
                    // TODO: look into cow handles into the actual data
                    // TODO: actually respect ELF loader behavior w.r.t overlays
                    // TODO: does this do stuff with alignment or what
                    let mut section_data = vec![0; section.p_memsz as usize];
//                    println!("virtual size: {:#x}, size of raw data: {:#x}", section.p_memsz, section.p_filesz);
//                    println!("{:?}", section);
                    let physical_copy_end = (section.p_offset as usize) + std::cmp::min(section.p_filesz as usize, section.p_memsz as usize);
                    let copy_size = if physical_copy_end > data.len() {
                        if (section.p_offset as usize) < data.len() {
                            data.len() - section.p_offset as usize
                        } else {
                            0
                        }
                    } else {
                        std::cmp::min(section.p_filesz as usize, section.p_memsz as usize)
                    };

                    //println!("mapping section {} by copying {:#x} bytes starting from {:#x}", i, copy_size, section.p_offset);
                    //println!("virtual size is {:#x}", section_data.len());
                    for i in 0..copy_size {
                        section_data[i] = data[(section.p_offset as usize) + i];
                    }

                    let new_section = Segment {
                        start: section.p_vaddr as usize,
                        data: section_data,
                        name: elf::program_header::type_to_str(elf.header.e_machine, section.p_type),
                    };
                    //println!("mapped section {} to [{}, {})",
                    //    i,
                    //    new_section.start.show(),
                    //    (new_section.start as u64 + new_section.data.len() as u64).show()
                    //);
                    module.segments.push(new_section);
                }
                Some(module)
            },
            Ok(obj @ Object::PE(_)) => {
                let mut module = ModuleData {
                    segments: vec![],
                    module_info: ModuleInfo::from_goblin(&obj, data).unwrap(),
                    name
                };

                let pe = match obj {
                    Object::PE(pe) => pe,
                    _ => { unreachable!(); }
                };
//                println!("Parsed PE: {:?}", pe);

                for section in pe.sections.iter() {
                    // TODO: look into cow handles into the actual data
                    // TODO: actually respect PE loader behavior w.r.t overlays
                    // TODO: does this do stuff with alignment or what
                    let mut section_data = vec![0; section.virtual_size as usize];
//                    println!("virtual size: {:#x}, size of raw data: {:#x}", section.virtual_size, section.size_of_raw_data);
//                    println!("{:?}", section);
                    let physical_copy_end = (section.pointer_to_raw_data as usize) + std::cmp::min(section.size_of_raw_data as usize, section.virtual_size as usize);
                    let copy_size = if physical_copy_end > data.len() {
                        if (section.pointer_to_raw_data as usize) < data.len() {
                            data.len() - section.pointer_to_raw_data as usize
                        } else {
                            0
                        }
                    } else {
                        std::cmp::min(section.size_of_raw_data as usize, section.virtual_size as usize)
                    };

                    println!("mapping section \"{}\" by copying {:#x} bytes starting from {:#x}", std::str::from_utf8(&section.name[..]).unwrap(), copy_size, section.pointer_to_raw_data);
                    println!("virtual size is {:#x}", section_data.len());
                    for i in 0..copy_size {
                        section_data[i] = data[(section.pointer_to_raw_data as usize) + i];
                    }

                    let new_section = Segment {
                        start: section.virtual_address as usize + pe.header.optional_header.map(|x| x.windows_fields.image_base as usize).unwrap_or(0x400000),
                        data: section_data,
                        name: std::str::from_utf8(&section.name[..]).unwrap().to_string()
                    };
                    println!("mapped {} to [{}, {})",
                        std::str::from_utf8(&section.name[..]).unwrap(),
                        new_section.start.show(),
                        (new_section.start as u64 + new_section.data.len() as u64).show()
                    );
                    module.segments.push(new_section);
                }
                match &module.module_info {
                    ModuleInfo::PE(_, _, _, _, _, ref imports, ref _exports, _) => {
                        for _i in imports.iter() {
//                            println!("import: {:?}", i);
                        }
                    }
                    _ => { }
                }
                Some(module)
            },
            Ok(Object::Mach(_mach)) => {
                panic!("UHHHHH  IM SCARED");
            },
            Ok(Object::Archive(_archive)) => {
                panic!("u hhh h h hh h  H H H H H");
            },
            Ok(Object::Unknown(magic)) => {
                println!("goblin found unknown magic: {:#x}", magic);
                None
            },
            Err(e) => {
                println!("goblin error: {:?}", e);
                None
            }
        }
    }
    fn segment_for<A: Address>(&self, addr: A) -> Option<&Segment> {
        for segment in self.segments.iter() {
            if segment.contains(addr) {
                return Some(segment);
            }
        }
        None
    }
    #[allow(dead_code)]
    fn segment_after(&self, _segment: &Segment) -> Option<&Segment> {
        unreachable!()
    }
}

impl <A: Address> MemoryRepr<A> for ModuleData {
    fn read(&self, addr: A) -> Option<u8> {
        self.segment_for(addr).and_then(|segment| segment.read(addr))
    }
    fn to_flat(self) -> Option<FlatMemoryRepr> {
        None
    }
    fn module_info(&self) -> Option<&ModuleInfo> { Some(&self.module_info) }
    fn module_for(&self, addr: A) -> Option<&dyn MemoryRepr<A>> {
        if self.segment_for(addr).is_some() {
            Some(self)
        } else {
            None
        }
    }
    fn size(&self) -> Option<u64> {
        match ((self as &dyn MemoryRepr<A>).end(), (self as &dyn MemoryRepr<A>).start()) {
            (Some(end), Some(start)) => {
                Some(end - start)
            }
            _ => None
        }
    }
    fn end(&self) -> Option<u64> {
        self.segments.iter().map(|s| s.end()).max().map(|x| x as u64)
    }
    fn start(&self) -> Option<u64> {
        self.segments.iter().map(|s| s.start()).min().map(|x| x as u64)
    }
}

impl Named for ModuleData {
    fn name(&self) -> &str {
        &self.name
    }
}

impl <A: Address> MemoryRange<A> for ModuleData {
    fn range<'a>(&'a self, range: Range<A>) -> Option<ReadCursor<'a, A, Self>> {
        self.segment_for(range.start).and_then(|section| {
            if section.contains(range.end) {
                // TODO: return the section itself to avoid double lookups
                Some(ReadCursor::from(self, range))
            } else {
                // TODO: this range may intersect with multiple sections
                None
            }
        })
    }
    fn range_from<'a>(&'a self, start: A) -> Option<UnboundedCursor<'a, A, Self>> {
        self.segment_for(start).map(|_segment| UnboundedCursor::from(self, start))
    }
}

#[derive(Debug)]
pub struct ProcessMemoryRepr {
    pub modules: Vec<ModuleData>
}

impl <A: Address> MemoryRepr<A> for ProcessMemoryRepr {
    fn read(&self, addr: A) -> Option<u8> {
        // TODO: Overlap is not considered correctly here.
        for module in self.modules.iter() {
            match module.read(addr) {
                Some(data) => { return Some(data); },
                None => ()
            }
        }
        return None
    }
    fn to_flat(self) -> Option<FlatMemoryRepr> {
        None
    }
    fn module_info(&self) -> Option<&ModuleInfo> { /* TODO: how to get one specific moduleinfo? or should all of them get merged? */ None }
    fn module_for(&self, addr: A) -> Option<&dyn MemoryRepr<A>> {
        for module in self.modules.iter() {
            if module.segment_for(addr).is_some() {
                return Some(module)
            }
        }
        None
    }
    fn size(&self) -> Option<u64> {
        Some(self.modules.iter().map(|m| <ModuleData as MemoryRepr<A>>::size(m).unwrap()).sum())
    }
}

impl Named for ProcessMemoryRepr {
    fn name(&self) -> &str {
        "process"
    }
}

impl <A: Address> PatchyMemoryRepr<A> for ProcessMemoryRepr {
    fn add(&mut self, _data: Vec<u8>, _addr: A) -> Result<(), LayoutError> {
        Err(LayoutError::Unsupported)
    }
}

impl <A: Address> MemoryRange<A> for ProcessMemoryRepr {
    // TODO:
    fn range<'a>(&'a self, range: Range<A>) -> Option<ReadCursor<'a, A, Self>> {
//        if range.start.to_linear() < self.data.len() && range.end.to_linear() < self.data.len() && range.start < range.end {
            Some(ReadCursor::from(self, range))
                /*
            Some(ReadCursor {
                data: self,
                start: range.start,
                end: range.end
            })
            */
//        } else {
//            None
//        }
    }
    // TODO:
    fn range_from<'a>(&'a self, start: A) -> Option<UnboundedCursor<'a, A, Self>> {
//        if range.start.to_linear() < self.data.len() && range.end.to_linear() < self.data.len() && range.start < range.end {
            Some(UnboundedCursor::from(self, start))
//        } else {
//            None
//        }
    }
}

