pub mod arm;
pub mod pic17;
pub mod pic18;
// pub mod pic24;
pub mod msp430;

pub mod x86_64;

pub mod display;
pub mod interface;

use std::cell::{Ref, RefCell, RefMut};
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::fmt::{Display, Write};

use crate::data::types;
use data::ValueLocations;
use data::Direction;
use data::modifier::Precedence;

use analyses::static_single_assignment::{NoValueDescriptions, ValueDescriptionQuery};

use yaxpeax_arch::{Address, Decoder, LengthedInstruction};

use memory::{MemoryRange, MemoryRepr};
use memory::repr::cursor::ReadCursor;

use serde::{Deserialize, Serialize};

pub trait DecodeFrom<T: MemoryRepr<Self> + ?Sized>: yaxpeax_arch::Arch {
    fn decode_from<'mem>(t: &ReadCursor<'mem, Self, T>) -> Result<Self::Instruction, Self::DecodeError> {
        Self::decode_with_decoder(&Self::Decoder::default(), t)
    }
    fn decode_with_decoder<'mem>(decoder: &Self::Decoder, t: &ReadCursor<'mem, Self, T>) -> Result<Self::Instruction, Self::DecodeError> {
        let mut inst = Self::Instruction::default();
        Self::decode_with_decoder_into(decoder, t, &mut inst)?;
        Ok(inst)
    }
    fn decode_with_decoder_into<'mem>(decoder: &Self::Decoder, t: &ReadCursor<'mem, Self, T>, instr: &mut Self::Instruction) -> Result<(), Self::DecodeError>;
}

pub trait FunctionQuery<A: Address> {
    type Function;
    fn function_at(&self, addr: A) -> Option<&Self::Function>;
    fn all_functions(&self) -> Vec<&Self::Function>;
}

impl<Addr: Address, Loc: AbiDefaults> FunctionQuery<Addr> for std::collections::HashMap<Addr, FunctionImpl<Loc>> {
    type Function = FunctionImpl<Loc>;

    fn function_at(&self, addr: Addr) -> Option<&Self::Function> {
        self.get(&addr)
    }

    fn all_functions(&self) -> Vec<&Self::Function> {
        self.values().collect()
    }
}

pub trait SymbolQuery<A: Address> {
    fn symbol_for(&self, addr: A) -> Option<&Symbol>;
    fn symbol_addr(&self, sym: &Symbol) -> Option<A>;
}

pub trait AddressNamer<A: Address> {
    fn address_name(&self, addr: A) -> Option<String>;
}

impl <'a, T, A: Address, F: FunctionRepr> AddressNamer<A> for T where T: FunctionQuery<A, Function=F> + SymbolQuery<A> {
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
    arguments: Vec<Option<Parameter>>,
    returns: Vec<Option<Parameter>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionImpl<Loc: AbiDefaults> {
    names: Function,
    layout: Rc<RefCell<FunctionLayout<Loc>>>,
}

pub struct FunctionImplDescription<'a, Loc: AbiDefaults, V: ValueDescriptionQuery<Loc>> {
    f: &'a FunctionImpl<Loc>,
    values: Option<V>
}

// PartialEq, Eq

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FunctionLayout<Loc: AbiDefaults> {
    pub arguments: Vec<Option<Loc>>,
    pub(crate) returns: Vec<Option<Loc>>,
    pub(crate) clobbers: Vec<Option<Loc>>,
    pub(crate) return_address: Option<Loc>,
    defaults: Loc::AbiDefault,
}

impl <Loc: AbiDefaults> FunctionLayout<Loc> {
    fn for_abi(default: Loc::AbiDefault) -> Self {
        FunctionLayout {
            arguments: Vec::new(),
            returns: Vec::new(),
            clobbers: Vec::new(),
            return_address: None,
            defaults: default
        }
    }
}

pub trait AbiDefaults: Clone + Debug {
    type AbiDefault: FunctionAbiReference<Self> + Debug + Serialize + for<'de> Deserialize<'de> + Default;
}

impl <Loc: Hash + AbiDefaults> Hash for FunctionLayout<Loc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.arguments.hash(state);
        self.returns.hash(state);
        self.clobbers.hash(state);
        self.return_address.hash(state);
    }
}

fn insert_at<Loc: Clone>(vs: &mut Vec<Option<Loc>>, el: Option<Loc>, i: usize) {
    if i >= vs.len() {
        vs.resize(i + 1, None);
    }

    vs[i] = el;
}

impl <Loc: Clone + Debug + AbiDefaults> FunctionAbiReference<Loc> for FunctionLayout<Loc> {
    fn argument_at(&mut self, i: usize) -> Option<Loc> {
        if self.arguments.get(i).is_none() {
            insert_at(&mut self.arguments, self.defaults.argument_at(i), i);
        }

        self.arguments[i].clone()
    }
    fn return_at(&mut self, i: usize) -> Option<Loc> {
        if self.returns.get(i).is_none() {
            insert_at(&mut self.returns, self.defaults.return_at(i), i);
        }

        self.returns[i].clone()
    }
    fn return_address(&mut self) -> Option<Loc> {
        if self.return_address.is_none() {
            self.return_address = self.defaults.return_address();
        }

        self.return_address.clone()
    }
    fn clobber_at(&mut self, i: usize) -> Option<Loc> {
        if self.clobbers.get(i).is_none() {
            insert_at(&mut self.clobbers, self.defaults.clobber_at(i), i);
        }

        self.clobbers[i].clone()
    }
}

// TODO: this is an insufficient api - some calling convention/architecture combinations pick
// locations based on the type of value at the index in question. x86_64 ABIs typically use xmm
// registers for floating point values but 64-bit general purpose registers for arguments in the
// same index if the value is an integer.
pub trait FunctionAbiReference<Loc: Debug + Clone>: Debug {
    fn argument_at(&mut self, i: usize) -> Option<Loc>;
    fn return_at(&mut self, i: usize) -> Option<Loc>;
    fn return_address(&mut self) -> Option<Loc>;
    fn clobber_at(&mut self, i: usize) -> Option<Loc>;
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
struct NilAbi {}

impl <Loc: Debug + Clone> FunctionAbiReference<Loc> for NilAbi {
    fn argument_at(&mut self, _: usize) -> Option<Loc> { None }
    fn return_at(&mut self, _: usize) -> Option<Loc> { None }
    fn return_address(&mut self) -> Option<Loc> { None }
    fn clobber_at(&mut self, _: usize) -> Option<Loc> { None }
}

impl <Loc: AbiDefaults + PartialEq> FunctionImpl<Loc> {
    pub fn new(name: String) -> Self {
        Function::of(name, vec![], vec![])
            .unimplemented()
    }

    pub fn rename(&mut self, new_name: String) {
        self.names.name = new_name;
    }

    pub fn append_arg(&mut self, new_arg: (Option<Loc>, Parameter)) {
        self.names.arguments.push(Some(new_arg.1));
        let arg_idx = self.names.arguments.len() - 1;
        let mut layout_mut = self.layout.borrow_mut();
        let arg = new_arg.0.or_else(|| { layout_mut.argument_at(arg_idx) });
        while arg_idx >= layout_mut.arguments.len() {
            layout_mut.arguments.push(None);
        }
        layout_mut.arguments[arg_idx] = arg;
    }

    pub fn has_arg_at(&self, loc: Loc) -> bool {
        self.layout.borrow().arguments.contains(&Some(loc))
    }

    pub fn append_ret(&mut self, new_ret: (Option<Loc>, Parameter)) {
        self.names.returns.push(Some(new_ret.1));
        let ret_idx = self.names.returns.len() - 1;
        let ret = new_ret.0.or_else(|| { self.layout.borrow_mut().return_at(ret_idx) });
        self.layout.borrow_mut().returns[ret_idx] = ret;
    }

    pub fn layout(&self) -> Ref<FunctionLayout<Loc>> {
        self.layout.borrow()
    }

    pub fn layout_mut(&self) -> RefMut<FunctionLayout<Loc>> {
        self.layout.borrow_mut()
    }

    pub fn with_value_names<V: ValueDescriptionQuery<Loc>>(&self, value_descs: Option<V>) -> FunctionImplDescription<Loc, V> {
        FunctionImplDescription {
            f: self,
            values: value_descs,
        }
    }
}

impl <T: AbiDefaults + Debug, V: ValueDescriptionQuery<T>> FunctionRepr for FunctionImplDescription<'_, T, V> {
    fn decl_string(&self, show_locations: bool) -> String {
        let mut res = self.f.names.name.clone();
        res.push('(');
        // Arguments are shown like this:
        // <arg_name>[ -> <location>]: <value_location>[[ (= <value_description>)]]
        //
        // square brackets indicate text selected by `show_locations`.
        // double square brackets are only set if a value is known for the argument.
        //
        // TODO: if `<arg_name>` == `<value_location>`, just fold them together.
        for (i, name) in self.f.names.arguments.iter().enumerate() {
            if i > 0 {
                res.push_str(", ");
            }
            // TODO: figure out default naming strategy better than arg_n?
            if let Some(name) = name.as_ref().and_then(|n| n.name.as_ref()) {
                res.push_str(name);
            } else {
                res.push_str(&format!("arg_{}", i));
            }
            let argument = self.f.layout.borrow_mut().argument_at(i);
            if show_locations {
                res.push_str(" -> ");
                write!(res, "{:?}", argument).unwrap();
            }
            if let (Some(argument), Some(values)) = (argument, self.values.as_ref()) {
                if let Some(name) = values.modifier_name(argument.clone(), Direction::Read, Precedence::After) {
                    write!(res, ": {}", name).unwrap();
                }
                if let Some(value) = values.modifier_value(argument.clone(), Direction::Read, Precedence::After) {
                    write!(res, " (= {})", value).unwrap();
                }
            }
        }
        res.push(')');
        match self.f.names.returns.len() {
            0 => {},
            1 => {
                res.push_str(" -> ");
                let ret = self.f.layout.borrow_mut().return_at(0);
                if show_locations {
                    write!(res, "{:?}", ret).unwrap();
                    res.push_str(" -> ");
                }
                if let Some(name) = self.f.names.returns.get(0).and_then(|x| x.as_ref()).and_then(|n| n.name.as_ref()) {
                    res.push_str(name);
                } else {
                    res.push_str("return_0");
                }
                if let (Some(ret), Some(values)) = (ret, self.values.as_ref()) {
                    if let Some(name) = values.modifier_name(ret.clone(), Direction::Write, Precedence::After) {
                        write!(res, ": {}", name).unwrap();
                    }
                    if let Some(value) = values.modifier_value(ret.clone(), Direction::Write, Precedence::After) {
                        write!(res, " (= {})", value).unwrap();
                    }
                }
            },
            _ => {
                res.push_str(" -> ");
                for (i, name) in self.f.names.returns.iter().enumerate() {
                    if i > 0 {
                        res.push_str(", ");
                    }
                    let ret = self.f.layout.borrow_mut().return_at(i);
                    if show_locations {
                        write!(res, "{:?}", ret).unwrap();
                        res.push_str(" -> ");
                    }
                    if let Some(name) = name.as_ref().and_then(|n| n.name.as_ref()) {
                        res.push_str(name);
                    } else {
                        res.push_str(&format!("return_{}", i));
                    }
                    if let (Some(ret), Some(values)) = (ret, self.values.as_ref()) {
                        if let Some(name) = values.modifier_name(ret.clone(), Direction::Write, Precedence::After) {
                            write!(res, ": {}", name).unwrap();
                        }
                        if let Some(value) = values.modifier_value(ret.clone(), Direction::Write, Precedence::After) {
                            write!(res, " (= {})", value).unwrap();
                        }
                    }
                }
            }
        }
        res
    }

    fn name(&self) -> &str {
        &self.f.names.name
    }
}

// TODO:
// impl <T: Display> FunctionRepr for FunctionImpl<T> {
impl <T: AbiDefaults + Debug + PartialEq> FunctionRepr for FunctionImpl<T> {
    fn decl_string(&self, show_locations: bool) -> String {
        self.with_value_names(Some(NoValueDescriptions)).decl_string(show_locations)
    }
    fn name(&self) -> &str {
        &self.names.name
    }
}

pub trait FunctionAbi<A: ValueLocations> {
    fn argument_loc(&self, idx: usize) -> A::Location;
    fn return_loc(&self, idx: usize) -> A::Location;
    fn return_address(&self) -> A::Location;
}

impl Function {
    fn implement_for<Loc: AbiDefaults>(self, layout: FunctionLayout<Loc>) -> FunctionImpl<Loc> {
        FunctionImpl {
            names: self,
            layout: Rc::new(RefCell::new(layout))
        }
    }

    fn unimplemented<Loc: AbiDefaults>(self) -> FunctionImpl<Loc> {
        self
            .implement_for(
                FunctionLayout::for_abi(
                    Loc::AbiDefault::default()
                )
            )
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
            arguments: args.into_iter().map(|x| Some(x)).collect(),
            returns: rets.into_iter().map(|x| Some(x)).collect(),
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
            if let Some(name) = param.as_ref().and_then(|p| p.name.as_ref()) {
                res.push_str(name);
            } else {
                res.push_str(&format!("arg_{}", i));
            }
        }
        res.push(')');
        match self.returns.len() {
            0 => {},
            1 => {
                if let Some(name) = self.returns[0].as_ref().and_then(|r| r.name.as_ref()) {
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
                    if let Some(name) = ret.as_ref().and_then(|r| r.name.as_ref()) {
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
//    PIC24(pic24::CPU),
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
//            Device::PIC24(_) => { ISA::PIC24 }
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

pub struct InstructionIteratorSpanned<'a, A, M: MemoryRepr<A> + ?Sized> where
    A: ?Sized + yaxpeax_arch::Arch + DecodeFrom<M>
{
    data: &'a M,
    decoder: A::Decoder,
    current: A::Address,
    end: A::Address,
    elem: Option<A::Instruction>
}

pub trait InstructionSpan<M: MemoryRepr<Self> + ?Sized> where
    Self: yaxpeax_arch::Arch + DecodeFrom<M>
{
    fn instructions_spanning<'a>(data: &'a M, start: Self::Address, end: Self::Address) -> InstructionIteratorSpanned<'a, Self, M>;
}

impl<A, M: MemoryRepr<Self> + ?Sized> InstructionSpan<M> for A where
    A: yaxpeax_arch::Arch + DecodeFrom<M>
{
    fn instructions_spanning<'a>(data: &'a M, start: Self::Address, end: Self::Address) -> InstructionIteratorSpanned<'a, Self, M> {
        InstructionIteratorSpanned {
            data,
            decoder: Self::Decoder::default(),
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
    fn control_flow<T, Addr>(&self, _ctx: &T) -> ControlFlowEffect<Addr>;
}

trait MCU {
    type Addr;
    type Instruction: Default;
    fn emulate(&mut self) -> Result<(), String>;
    fn decode(&self) -> Result<Self::Instruction, String>;
}

// TODO: streaming_iterator crate
pub trait SimpleStreamingIterator {
    type Item;

    fn next<'b>(&mut self) -> Option<&'b Self::Item>;
}

impl <'a, A, M> InstructionIteratorSpanned<'a, A, M> where
    A: yaxpeax_arch::Arch + DecodeFrom<M>,
    M: MemoryRepr<A> + MemoryRange<A> + ?Sized
{
    pub fn next<'b>(&mut self) -> Option<(A::Address, &A::Instruction)> {
        if self.elem.is_some() {
            let instr: &mut A::Instruction = self.elem.as_mut().unwrap();
            //  TODO: check for wrappipng..
            match Some(self.current + instr.len()) {
                Some(next) => {
                    if next <= self.end {
                        self.current = next;
                        if let Some(range) = self.data.range_from(self.current) {
                            match A::decode_with_decoder_into(&self.decoder, &range, instr) {
                                Ok(()) => {
                                    Some((self.current, instr))
                                },
                                Err(_) => None
                            }
                        } else {
//                            tracing::warn!("BUG: No data available for {}", self.current.show());
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
                    self.elem = A::decode_with_decoder(&self.decoder, &range).ok();
                    match self.elem {
                        Some(ref instr) => {
                            Some((self.current, &instr))
                        },
                        None => None
                    }
                } else {
//                    tracing::warn!("BUG: No data available for {}", self.current.show());
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
