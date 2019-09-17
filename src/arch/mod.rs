pub mod arm;
pub mod pic17;
pub mod pic18;
pub mod pic24;
pub mod msp430;

pub mod x86_64;

pub mod display;
pub mod interface;

use std::fmt::{Display, Write};
use std::collections::HashMap;

use crate::data::types;
use data::ValueLocations;

use yaxpeax_arch::{Address, Decodable, LengthedInstruction};

use memory::{MemoryRange, MemoryRepr};

pub trait FunctionQuery<A: Address> {
    type Function: FunctionRepr;
    fn function_at(&self, addr: A) -> Option<&Self::Function>;
}

pub trait SymbolQuery<A: Address> {
    fn symbol_for(&self, addr: A) -> Option<&Symbol>;
    fn symbol_addr(&self, sym: &Symbol) -> Option<A>;
}

pub trait AddressNamer<A: Address> {
    fn address_name(&self, addr: A) -> Option<String>;
}

impl <T, A: Address> AddressNamer<A> for T where T: FunctionQuery<A> + SymbolQuery<A> {
    fn address_name(&self, addr: A) -> Option<String> {
        self.function_at(addr).map(|func| func.name().to_owned())
            .or_else(|| { self.symbol_for(addr).map(|sym| sym.to_string()) })
    }
}

pub trait CommentQuery<A: Address> {
    fn comment_for(&self, addr: A) -> Option<&str>;
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct Parameter {
    name: Option<String>,
    ty: Option<types::TypeSpec>
}

impl Default for Parameter {
    fn default() -> Self {
        Parameter {
            name: None,
            ty: None
        }
    }
}

impl Parameter {
    pub fn of(name: &str) -> Self {
        Parameter {
            name: Some(name.to_owned()),
            ty: None
        }
    }

    pub fn typed(mut self, ty: types::TypeSpec) -> Self {
        self.ty = Some(ty);
        self
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct Function {
    name: String,
    arguments: Vec<Parameter>,
    returns: Vec<Parameter>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct FunctionImpl<Loc> {
    name: String,
    arguments: Vec<(Loc, Parameter)>,
    returns: Vec<(Loc, Parameter)>,
    return_address: Option<Loc>,
}

// TODO:
// impl <T: Display> FunctionRepr for FunctionImpl<T> {
impl <T: std::fmt::Debug> FunctionRepr for FunctionImpl<T> {
    fn decl_string(&self, show_locations: bool) -> String {
        let mut res = self.name.clone();
        res.push('(');
        for (i, (loc, param)) in self.arguments.iter().enumerate() {
            if i > 0 {
                res.push_str(", ");
            }
            // TODO: figure out default naming strategy better than arg_n?
            if let Some(name) = param.name.as_ref() {
                res.push_str(name);
            } else {
                res.push_str(&format!("arg_{}", i));
            }
            if show_locations {
                res.push_str(" -> ");
                write!(res, "{:?}", loc);
            }
        }
        res.push(')');
        match self.returns.len() {
            0 => {},
            1 => {
                if show_locations {
                    write!(res, "{:?}", self.returns[0].0);
                    res.push_str(" -> ");
                }
                if let Some(name) = self.returns[0].1.name.as_ref() {
                    res.push_str(name);
                } else {
                    res.push_str("return_0");
                }
            },
            _ => {
                res.push_str(" -> ");
                for (i, (loc, ret)) in self.returns.iter().enumerate() {
                    if i > 0 {
                        res.push_str(", ");
                    }
                    if show_locations {
                        write!(res, "{:?}", loc).unwrap();
                        res.push_str(" -> ");
                    }
                    if let Some(name) = ret.name.as_ref() {
                        res.push_str(name);
                    } else {
                        res.push_str(&format!("return_{}", i));
                    }
                }
            }
        }
        res
    }
    fn name(&self) -> &str {
        &self.name
    }
}

trait FunctionAbi<A: ValueLocations> {
    fn argument_loc(&self, idx: usize) -> A::Location;
    fn return_loc(&self, idx: usize) -> A::Location;
    fn return_address(&self) -> A::Location;
}

impl Function {
    fn implement_for<A: ValueLocations>(&self, abi: &Box<dyn FunctionAbi<A>>) -> FunctionImpl<A::Location> {
        let arguments = self.arguments.iter().enumerate().map(|(idx, arg)| (abi.argument_loc(idx), arg.to_owned())).collect();
        let returns = self.returns.iter().enumerate().map(|(idx, arg)| (abi.return_loc(idx), arg.to_owned())).collect();
        FunctionImpl {
            name: self.name.clone(),
            arguments,
            returns,
            return_address: Some(abi.return_address()),
        }
    }

    fn unimplemented<A: ValueLocations>(&self) -> FunctionImpl<A::Location> {
        // TODO: include aref to self? or something else to eventually map this function to real
        // locations in the program?
        FunctionImpl {
            name: self.name.clone(),
            arguments: vec![],
            returns: vec![],
            return_address: None
        }
    }
}

pub trait FunctionRepr {
    fn decl_string(&self, show_locations: bool) -> String;
    fn name(&self) -> &str;
}

impl Function {
    pub fn of(name: String, args: Vec<Parameter>, rets: Vec<Parameter>) -> Function {
        Function {
            name: name,
            arguments: args,
            returns: rets
        }
    }
}

impl FunctionRepr for Function {
    // TODO: is there a way to sho locations for abstract functions? don't think so..
    fn decl_string(&self, _show_locations: bool) -> String {
        let mut res = self.name.clone();
        res.push('(');
        for (i, param) in self.arguments.iter().enumerate() {
            if i > 0 {
                res.push_str(", ");
            }
            if let Some(name) = param.name.as_ref() {
                res.push_str(name);
            } else {
                res.push_str(&format!("arg_{}", i));
            }
        }
        res.push(')');
        match self.returns.len() {
            0 => {},
            1 => {
                if let Some(name) = self.returns[0].name.as_ref() {
                    res.push_str(name);
                } else {
                    res.push_str("return_0");
                }
            },
            _ => {
                for (i, ret) in self.returns.iter().enumerate() {
                    if i > 0 {
                        res.push_str(", ");
                    }
                    if let Some(name) = ret.name.as_ref() {
                        res.push_str(name);
                    } else {
                        res.push_str(&format!("return_{}", i));
                    }
                }
            }
        }
        res
    }
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub enum Library {
    Name(String),
    This,
    Unknown
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct Symbol(pub Library, pub String);

/*
 * FARPROC GetProcAddress(
 *   HMODULE hModule,
 *   LPCSTR  lpProcName
 * )
 */
impl Symbol {
    fn to_function(sym: &Symbol) -> Option<Function> {
        match sym {
            Symbol(Library::Name(library), f) if library == "kernel32.dll" && f == "GetProcAddress" => {
//                Some(Function::of("kernel32.dll!GetProcAddress", vec![Types::ptr, Types::ptr], vec![Types::ptr]))
                Some(
                    Function::of(
                        "GetProcAddress".to_string(),
                        vec![
                            Parameter::of("hModule").typed(
//                                Types::by_name("HMODULE")
                                types::TypeSpec::Top
                            ),
                            Parameter::of("lpProcName").typed(
//                                Types::by_name("LPCSTR")
                                types::TypeSpec::Top
                            ),
                        ],
                        vec![
                            Parameter::of("proc").typed(
//                                Types::by_name("FARPROC")
                                types::TypeSpec::Top
                            )
                        ]
                    )
                )
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
            },
            Library::Unknown => {
                write!(f, "<unknown>!{}", self.1)
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
#[allow(non_camel_case_types)]
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

#[derive(Debug, Copy, Clone, Deserialize, Serialize)]
#[allow(non_camel_case_types)]
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
                        if let Some(range) = self.data.range_from(self.current) {
                            match instr.decode_into(range) {
                                Some(_) => {
                                    Some((self.current, instr))
                                },
                                None => None
                            }
                        } else {
                            println!("BUG: No data available for {}", self.current.stringy());
                            None
                        }
                    } else {
                        None
                    }
                },
                None => None
            }
        } else {
            if self.current <= self.end {
                if let Some(range) = self.data.range_from(self.current) {
                    self.elem = Instr::decode(range);
                    match self.elem {
                        Some(ref instr) => {
                            Some((self.current, &instr))
                        },
                        None => None
                    }
                } else {
                    println!("BUG: No data available for {}", self.current.stringy());
                    None
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
