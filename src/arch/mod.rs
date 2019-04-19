pub mod arm;
pub mod pic17;
pub mod pic18;
pub mod pic24;
pub mod msp430;

pub mod x86_64;

pub mod display;
pub mod interface;

use std::collections::VecDeque;

use std::fmt::{Debug, Display};

use yaxpeax_arch::{Address, Decodable, LengthedInstruction};

use memory::{MemoryRange, MemoryRepr};

#[derive(Debug)]
pub struct Function {
    name: String,
    arguments: Vec<String>,
    returns: Vec<String>
}

impl Function {
    pub fn of(name: String, args: Vec<String>, rets: Vec<String>) -> Function {
        Function {
            name: name,
            arguments: args,
            returns: rets
        }
    }
}

#[derive(Debug)]
pub enum Library {
    Name(String),
    This
}

#[derive(Debug)]
pub struct Symbol(pub Library, pub String);

impl Symbol {
    fn to_function(sym: &Symbol) -> Option<Function> {
        match sym {
            Symbol(Library::Name(library), f) if library == "kernel32.dll" && f == "GetProcAddress" => {
//                Some(Function::of("kernel32.dll!GetProcAddress", vec![Types::ptr, Types::ptr], vec![Types::ptr]))
                Some(Function::of("kernel32.dll!GetProcAddress".to_string(), vec!["void*".to_string(), "void*".to_string()], vec!["void*".to_string()]))
            }
            _ => {
                None
            }
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.0 {
            Library::Name(ref lib) => {
                write!(f, "{}!{}", lib, self.1)
            },
            Library::This => {
                write!(f, "{}", self.1)
            }
        }
    }
}

#[derive(Debug)]
pub enum BaseUpdate<T> {
    AddDataComment(String),
    AddCodeComment(String),
    DefineSymbol(Symbol),
    DefineFunction(Function),
    Specialized(T)
}

// The type param is only to thread PartialInstructionContext through.. not a huge fan.
pub trait OperandDefinitions<C> {
    type Update;
    type Dependence;

    fn updates(&self, ctx: &C) -> Vec<Self::Update>;
    fn dependencies(&self, ctx: &C) -> Vec<Self::Dependence>;
}

#[derive(Debug)]
pub enum Device {
    PIC24(pic24::CPU),
    PIC18(pic18::cpu::CPU),
    PIC17(pic17::cpu::CPU),
    MSP430(msp430::cpu::CPU),
    x86(x86_64::cpu::CPU),
    x86_64(x86_64::cpu::CPU),
    ARM
}

impl From<Device> for ISA {
    fn from(d: Device) -> ISA {
        match d {
            Device::PIC24(_) => { ISA::PIC24 }
            Device::PIC18(_) => { ISA::PIC18 }
            Device::PIC17(_) => { ISA::PIC17 }
            Device::MSP430(_) => { ISA::MSP430 }
            Device::x86(_) => { ISA::x86 }
            Device::x86_64(_) => { ISA::x86_64 }
            Device::ARM => { ISA::ARM }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ISA {
    PIC17,
    PIC18,
    PIC18e,
    PIC24,
    MSP430,
    Alpha,
    AArch64,
    ARC,
    ARM,
    Alpha64,
    C6X,
    Csky,
    H8300,
    Hexagon,
    IA64,
    M86k,
    MIPS,
    Microblaze,
    NDS32,
    NIOS2,
    OpenRISC,
    PARISC,
    PowerPC,
    RISCV,
    S390,
    SH3,
    SH3DSP,
    SH3E,
    SH4,
    SH5,
    SPARC,
    Tricore,
    Unicore32,
    Xtensa,
    x86,
    x86_64
}

impl ISA {
    pub fn try_from_str(s: &str) -> Option<ISA> {
        match s {
            "pic17" => Some(ISA::PIC17),
            "pic18" => Some(ISA::PIC18),
            "pic18e" => Some(ISA::PIC18e),
            "pic24" => Some(ISA::PIC24),
            "msp430" => Some(ISA::MSP430),
            "alpha" => Some(ISA::Alpha),
            "aarch64" => Some(ISA::AArch64),
            "arc" => Some(ISA::ARC),
            "arm" => Some(ISA::ARM),
            "alpha64" => Some(ISA::Alpha64),
            "c6x" => Some(ISA::C6X),
            "csky" => Some(ISA::Csky),
            "h8300" => Some(ISA::H8300),
            "hexagon" => Some(ISA::Hexagon),
            "ia64" => Some(ISA::IA64),
            "m86k" => Some(ISA::M86k),
            "mips" => Some(ISA::MIPS),
            "microblaze" => Some(ISA::Microblaze),
            "nds32" => Some(ISA::NDS32),
            "nios2" => Some(ISA::NIOS2),
            "openrisc" => Some(ISA::OpenRISC),
            "parisc" => Some(ISA::PARISC),
            "powerpc" => Some(ISA::PowerPC),
            "riscv" => Some(ISA::RISCV),
            "s390" => Some(ISA::S390),
            "sh3" => Some(ISA::SH3),
            "sh3dsp" => Some(ISA::SH3DSP),
            "sh3e" => Some(ISA::SH3E),
            "sh4" => Some(ISA::SH4),
            "sh5" => Some(ISA::SH5),
            "sparc" => Some(ISA::SPARC),
            "tricore" => Some(ISA::Tricore),
            "unicore32" => Some(ISA::Unicore32),
            "xtensa" => Some(ISA::Xtensa),
            "x86" => Some(ISA::x86),
            "x86_64" => Some(ISA::x86_64),
            _ => None
        }
    }
}

pub struct InstructionIteratorSpanned<'a, Addr: Address, M: MemoryRepr<Addr> + ?Sized, Instr> {
    data: &'a M,
    current: Addr,
    end: Addr,
    elem: Option<Instr>
}

pub trait InstructionSpan<'a, Addr: Address> where Self: MemoryRepr<Addr> {
    fn instructions_spanning<Instr: Decodable>(&'a self, start: Addr, end: Addr) -> InstructionIteratorSpanned<'a, Addr, Self, Instr>;
}

impl <'a, Addr: Address, M: MemoryRepr<Addr>> InstructionSpan<'a, Addr> for M {
    fn instructions_spanning<Instr: Decodable>(&'a self, start: Addr, end: Addr) -> InstructionIteratorSpanned<'a, Addr, M, Instr> {
        InstructionIteratorSpanned {
            data: self,
            current: start,
            end: end,
            elem: None
        }
    }
}

pub enum ControlFlowEffect<Addr> {
    FollowingInstruction,
    Relative(Addr),
    Absolute(Addr),
    Multiple(Vec<Addr>),
    Indirect
}

pub trait ControlFlowDeterminant {
    fn control_flow<T, Addr>(&self, &T) -> ControlFlowEffect<Addr>;
}

trait MCU {
    type Addr;
    type Instruction: Decodable;
    fn emulate(&mut self) -> Result<(), String>;
    fn decode(&self) -> Result<Self::Instruction, String>;
}

// TODO: streaming_iterator crate
pub trait SimpleStreamingIterator {
    type Item;

    fn next<'b>(&mut self) -> Option<&'b Self::Item>;
}

impl <'a, Addr: Address, M: MemoryRepr<Addr> + MemoryRange<Addr>, Instr> InstructionIteratorSpanned<'a, Addr, M, Instr> where Instr: Decodable + LengthedInstruction<Unit=Addr> {
    pub fn next<'b>(&mut self) -> Option<(Addr, &Instr)> {
        if self.elem.is_some() {
            let instr: &mut Instr = self.elem.as_mut().unwrap();
            //  TODO: check for wrappipng..
            match Some(self.current.add(instr.len())) {
                Some(next) => {
                    if next <= self.end {
                        self.current = next;
                        let decode_result = instr.decode_into(self.data.range_from(self.current).unwrap());
                        match decode_result {
                            Some(_) => {
                                Some((self.current, instr))
                            },
                            None => None
                        }
                    } else {
                        None
                    }
                },
                None => None
            }
        } else {
            if self.current <= self.end {
                self.elem = Instr::decode(self.data.range_from(self.current).unwrap());
                match self.elem {
                    Some(ref instr) => {
                        Some((self.current, &instr))
                    },
                    None => None
                }
            } else {
                None
            }
        }
        /*
            None => {
            }
        }*/
    }
}
