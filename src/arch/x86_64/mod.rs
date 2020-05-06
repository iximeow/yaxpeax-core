use yaxpeax_arch::{Address, AddressBase, AddressDiff, Arch};
use analyses::control_flow;
use analyses::static_single_assignment::SSA;
use analyses::xrefs;
use self::analyses::data_flow::DefaultCallingConvention;

use petgraph::graphmap::GraphMap;

use std::collections::HashMap;
use std::collections::HashSet;
use yaxpeax_x86::x86_64;
use yaxpeax_x86::long_mode::{Instruction, Opcode, Operand};
use std::rc::Rc;
use std::cell::RefCell;
use std::cell::Ref;
use std::fmt;
use num_traits::Zero;

use arch::{BaseUpdate, CommentQuery, FunctionLayout, FunctionImpl, FunctionQuery, Symbol, SymbolQuery, Library};
use data::{Direction, ValueLocations};
use data::modifier::InstructionModifiers;
use timing::Timings;

use ContextRead;
use ContextWrite;

pub mod analyses;
pub mod cpu;
pub mod debug;
pub mod display;
pub mod semantic;

#[derive(Serialize, Deserialize)]
#[allow(non_camel_case_types)]
pub struct x86_64Data {
    #[serde(skip)]
    pub timings: Timings<<x86_64 as Arch>::Address>,
    pub preferred_addr: <x86_64 as Arch>::Address,
    pub contexts: MergedContextTable,
    pub cfg: control_flow::ControlFlowGraph<<x86_64 as Arch>::Address>,
    pub ssa: HashMap<<x86_64 as Arch>::Address, (
        control_flow::ControlFlowGraph<<x86_64 as Arch>::Address>,
        SSA<x86_64>
    )>,
}

pub struct DisplayCtx<'a> {
    functions: Ref<'a, HashMap<<x86_64 as Arch>::Address, FunctionImpl<<x86_64 as ValueLocations>::Location>>>,
    comments: &'a HashMap<<x86_64 as Arch>::Address, String>,
    symbols: &'a HashMap<<x86_64 as Arch>::Address, Symbol>,
}

impl FunctionQuery<<x86_64 as Arch>::Address> for x86_64Data {
    type Function = FunctionImpl<<x86_64 as ValueLocations>::Location>;
    fn function_at(&self, addr: <x86_64 as Arch>::Address) -> Option<&Self::Function> {
        panic!("self.contexts.functions.get(&addr)");
    }
    fn all_functions(&self) -> Vec<&Self::Function> {
        panic!("TODO");
        // self.values().collect::<Vec<_>>()
    }
}

impl CommentQuery<<x86_64 as Arch>::Address> for MergedContextTable {
    fn comment_for(&self, addr: <x86_64 as Arch>::Address) -> Option<&str> {
        self.comments.get(&addr).map(String::as_ref)
    }
}

impl FunctionQuery<<x86_64 as Arch>::Address> for MergedContextTable {
    type Function = FunctionImpl<<x86_64 as ValueLocations>::Location>;
    fn function_at(&self, addr: <x86_64 as Arch>::Address) -> Option<&Self::Function> {
        panic!("TODO");
//        self.get(&addr)
    }
    fn all_functions(&self) -> Vec<&Self::Function> {
        panic!("TODO");
        // self.values().collect::<Vec<_>>()
    }
}

impl SymbolQuery<<x86_64 as Arch>::Address> for x86_64Data {
    fn symbol_for(&self, addr: <x86_64 as Arch>::Address) -> Option<&Symbol> {
        self.contexts.symbols.get(&addr)
    }
    fn symbol_addr(&self, sym: &Symbol) -> Option<<x86_64 as Arch>::Address> {
        for (k, v) in self.contexts.symbols.iter() {
            if v == sym {
                return Some(*k);
            }
        }

        None
    }
}

impl<'a> FunctionQuery<<x86_64 as Arch>::Address> for DisplayCtx<'a> {
    type Function = FunctionImpl<<x86_64 as ValueLocations>::Location>;
    fn function_at(&self, addr: <x86_64 as Arch>::Address) -> Option<&Self::Function> {
        self.functions.function_at(addr)
    }
    fn all_functions(&self) -> Vec<&Self::Function> {
        self.functions.all_functions()
    }
}

impl<'a> CommentQuery<<x86_64 as Arch>::Address> for DisplayCtx<'a> {
    fn comment_for(&self, addr: <x86_64 as Arch>::Address) -> Option<&str> {
        self.comments.get(&addr).map(String::as_ref)
    }
}

impl<'a> SymbolQuery<<x86_64 as Arch>::Address> for DisplayCtx<'a> {
    fn symbol_for(&self, addr: <x86_64 as Arch>::Address) -> Option<&Symbol> {
        self.symbols.get(&addr)
    }
    fn symbol_addr(&self, sym: &Symbol) -> Option<<x86_64 as Arch>::Address> {
        for (k, v) in self.symbols.iter() {
            if v == sym {
                return Some(*k);
            }
        }

        None
    }
}

impl Default for x86_64Data {
    fn default() -> Self {
        x86_64Data {
            timings: Timings::new(),
            preferred_addr: <x86_64 as Arch>::Address::zero(),
            contexts: MergedContextTable::create_empty(),
            cfg: control_flow::ControlFlowGraph::new(),
            ssa: HashMap::new(),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct MergedContextTable {
    pub user_contexts: HashMap<<x86_64 as Arch>::Address, Rc<()>>,
    pub computed_contexts: HashMap<<x86_64 as Arch>::Address, Rc<()>>,
    pub comments: HashMap<<x86_64 as Arch>::Address, String>,
    #[serde(skip)]
    pub call_graph: GraphMap<<x86_64 as Arch>::Address, (), petgraph::Directed>,
    #[serde(skip)]
    pub xrefs: xrefs::XRefCollection<<x86_64 as Arch>::Address>,
    pub symbols: HashMap<<x86_64 as Arch>::Address, Symbol>,
    #[serde(skip)]
    pub reverse_symbols: HashMap<Symbol, <x86_64 as Arch>::Address>,
    pub functions: Rc<RefCell<HashMap<<x86_64 as Arch>::Address, FunctionImpl<<x86_64 as ValueLocations>::Location>>>>,
    pub function_data: HashMap<<x86_64 as Arch>::Address, RefCell<InstructionModifiers<x86_64>>>,
    pub function_hints: Vec<<x86_64 as Arch>::Address>,
    functions_hinted: HashSet<<x86_64 as Arch>::Address>,
    pub default_abi: Option<DefaultCallingConvention>,
}

#[derive(Debug)]
pub struct MergedContext {
    pub computed: Option<Rc<()>>,
    pub user: Option<Rc<()>>
}

impl Default for MergedContextTable {
    fn default() -> Self {
        MergedContextTable::create_empty()
    }
}

impl MergedContextTable {
    pub fn fn_count(&self) -> usize {
        self.functions.borrow().len()
    }
    pub fn create_empty() -> MergedContextTable {
        MergedContextTable {
            user_contexts: HashMap::new(),
            computed_contexts: HashMap::new(),
            comments: HashMap::new(),
            call_graph: GraphMap::new(),
            xrefs: xrefs::XRefCollection::new(),
            functions: Rc::new(RefCell::new(HashMap::new())),
            function_hints: Vec::new(),
            functions_hinted: HashSet::new(),
            symbols: HashMap::new(),
            reverse_symbols: HashMap::new(),
            function_data: HashMap::new(),
            default_abi: Some(DefaultCallingConvention::Microsoft), //None,
        }
    }

    pub fn display_ctx(&self) -> DisplayCtx {
        DisplayCtx {
            functions: self.functions.borrow(),
            symbols: &self.symbols,
            comments: &self.comments,
        }
    }
}

/*
pub enum Value {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u64, u64),
    U256(u64, u64, u64, u64),
    U512(u64, u64, u64, u64, u64, u64, u64, u64)
}

trait PartialInstructionContext {
    pub fn reg_value(reg: RegSpec) -> Option<Value>;
    pub fn mem_value(addr: u64, width: u8) -> Option<Value>;
}
*/

pub type Update = BaseUpdate<x86Update>;

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum x86Update {
    AddXRef(xrefs::RefType, xrefs::RefAction, <x86_64 as Arch>::Address),
    RemoveXRef(xrefs::RefType, xrefs::RefAction, <x86_64 as Arch>::Address),
    FunctionHint
}

impl ContextRead<x86_64, MergedContext> for MergedContextTable {
    fn at(&self, address: &<x86_64 as Arch>::Address) -> MergedContext {
        MergedContext {
            user: self.user_contexts.get(address).map(|v| Rc::clone(v)),
            computed: self.computed_contexts.get(address).map(|v| Rc::clone(v))
        }
    }
}

impl ContextWrite<x86_64, Update> for MergedContextTable {
    fn put(&mut self, address: <x86_64 as Arch>::Address, update: Update) {
        // println!("Applying update: {} -> {:?}", address.stringy(), update);
        match update {
            BaseUpdate::Specialized(x86Update::FunctionHint) => {
                if !self.functions.borrow().contains_key(&address) && !self.functions_hinted.contains(&address) {
//                    println!("Function hint: {}", address.stringy());
                    self.functions_hinted.insert(address);
                    self.function_hints.push(address)
                }
            },
            BaseUpdate::Specialized(x86Update::AddXRef(tpe, action, dest)) => {
                // TODO: xrefs from non-code sources
                self.xrefs.insert_from_code(tpe, action, address, dest);
            },
            BaseUpdate::Specialized(x86Update::RemoveXRef(tpe, action, dest)) => {
                self.xrefs.delete_from_code(tpe, action, address, dest);
            }
            BaseUpdate::DefineSymbol(sym) => {
                //println!("address of {:?} recorded at {}", sym, address.stringy());
                if !self.symbols.contains_key(&address) {
                    match Symbol::to_function(&sym) {
                        Some(f) => {
                            if let Some(abi) = self.default_abi {
                                self.functions.borrow_mut().insert(address, f.implement_for(FunctionLayout::for_abi(abi)));
                            } else {
                                // TODO: indicate that the function should have been defined, but
                                // was not because we don't know an ABI to map it to?
                                self.functions.borrow_mut().insert(address, f.unimplemented());
                            }
                        }
                        None => { }
                    }
                    self.symbols.insert(address, sym.clone());
                    self.reverse_symbols.insert(sym, address);
                }
            }
            BaseUpdate::DefineFunction(f) => {
                if !self.functions.borrow().contains_key(&address) {
                    self.symbols.insert(address, Symbol(Library::This, f.name.clone()));
                    self.reverse_symbols.insert(Symbol(Library::This, f.name.clone()), address);
                    if let Some(abi) = self.default_abi {
                        self.functions.borrow_mut().insert(address, f.implement_for(FunctionLayout::for_abi(abi)));
                    } else {
                        // TODO: indicate that the function should have been defined, but was not
                        // because we don't know an ABI to map it to?
                        self.functions.borrow_mut().insert(address, f.unimplemented());
                    }
                }
            }
            BaseUpdate::AddCodeComment(comment) => {
                self.comments.insert(address, comment);
            }
            _ => { /* todo: the rest */ }
        }
    }
}

use analyses::Value;
use analyses::ValueRes;
use analyses::DFG;

struct ControlFlowAnalysis<A: Address + fmt::Debug> {
    effect: control_flow::Effect<A>,
}

impl <A: Address + fmt::Debug> ControlFlowAnalysis<A> {
    fn new() -> Self {
        Self {
            effect: control_flow::Effect::cont(),
        }
    }
}

impl From<AddressDiff<u64>> for control_flow::Effect<u64> {
    fn from(item: AddressDiff<u64>) -> Self {
        control_flow::Effect::cont_and(
            control_flow::Target::Relative(item)
        )
    }
}

pub trait ToAddrDiff: yaxpeax_arch::AddressDiffAmount {
    fn translate_offset(from: u64) -> AddressDiff<Self>;
}

impl ToAddrDiff for u64 {
    fn translate_offset(from: u64) -> AddressDiff<Self> {
        AddressDiff::from_const(from)
    }
}

impl ToAddrDiff for u32 {
    fn translate_offset(from: u64) -> AddressDiff<Self> {
        AddressDiff::from_const(from as u32)
    }
}

impl <A: Address + ToAddrDiff + fmt::Debug> Value for control_flow::Effect<A> {
    fn unknown() -> Self {
        control_flow::Effect::stop()
    }

    fn from_const(c: u64) -> Self {
        control_flow::Effect::stop_and(
            control_flow::Target::Relative(A::translate_offset(c))
        )
    }

    fn from_set(effects: &[Self]) -> Self {
        use self::control_flow::Effect;
        use self::control_flow::Target;

        debug_assert!(effects.len() != 0);
        let mut stop_after = true;
        let mut target: Option<Target<A>> = None;

        for effect in effects {
            stop_after &= effect.is_stop();

            let merged_target = match (target, effect.dest.as_ref()) {
                (None, None) => {
                    None
                }
                (None, Some(o)) => {
                    Some(o.clone())
                }
                (Some(o), None) => {
                    Some(o)
                }
                // welllll this ought to be deduplicated...
                /*
                (Some(Target::Multiple(ref l)), Some(Target::Multiple(r))) => {
                    let mut vec = l.clone();
                    vec.extend_from_slice(&r);
                    Some(Target::Multiple(vec))
                }
                (Some(Target::Multiple(l)), Some(r)) => {
                    let mut vec = l.clone();
                    vec.push(r.clone());
                    Some(Target::Multiple(vec))
                }
                (Some(ref l), Some(Target::Multiple(r))) => {
                    let mut vec = r.clone();
                    vec.push(l.clone());
                    Some(Target::Multiple(vec))
                }
                (Some(l), Some(r)) => {
                    if &l == r {
                        Some(l)
                    } else {
                        Some(Target::Multiple(vec![l, r.clone()]))
                    }
                }
                */
                _ => {
                    unsafe {
                        std::hint::unreachable_unchecked();
                    }
                }
            };
            target = merged_target;
        }

        Effect {
            stop_after,
            dest: target,
        }
    }

    fn to_const(&self) -> Option<u64> {
        None
    }

    #[inline(always)]
    fn add(&self, other: &Self) -> ValueRes<Self> {
        if (self.stop_after == true && self.dest.is_none()) ||
            (other.stop_after == true && other.dest.is_none()) {

            return ValueRes::literal(Self::unknown());
        }

        match (self.dest.as_ref(), other.dest.as_ref()) {
            (None, Some(control_flow::Target::Relative(rel))) |
            (Some(control_flow::Target::Relative(rel)), None) => {
                ValueRes::literal(control_flow::Effect {
                    stop_after: self.stop_after || other.stop_after,
                    dest: Some(control_flow::Target::Relative(*rel))
                })
            },
            (Some(control_flow::Target::Relative(l)), Some(control_flow::Target::Relative(r))) => {
                ValueRes::literal(control_flow::Effect {
                    stop_after: self.stop_after || other.stop_after,
                    dest: Some(control_flow::Target::Relative(
                        A::zero().wrapping_offset(*l).wrapping_offset(*r).diff(&A::zero()).unwrap_or_else(|| unsafe { std::hint::unreachable_unchecked() }) //.expect("can compute diff")
                    ))
                })
            }
            _ => {
                unsafe {
                    std::hint::unreachable_unchecked();
                    // panic!("bad add: {:?} + {:?}", self, other);
                }
            }
        }
    }
}

use arch::x86_64::analyses::data_flow::Location;

impl<Addr: Address + fmt::Debug + ToAddrDiff> DFG<control_flow::Effect<Addr>> for ControlFlowAnalysis<Addr> {
    type Location = Location;

    fn read_loc(&self, loc: Self::Location) -> control_flow::Effect<Addr> {
        if loc == Location::RIP {
            self.effect.clone()
        } else if let Location::Memory(_) = loc {
            control_flow::Effect::unknown()
        } else {
            control_flow::Effect::unknown()
        }
    }

    fn write_loc(&mut self, loc: Self::Location, value: control_flow::Effect<Addr>) {
        if loc == Location::RIP {
            self.effect = value;
        } else {
            // do nothing, it's a location we ignore for control flow analysis
        }
    }
}

impl <T> control_flow::Determinant<T, <x86_64 as Arch>::Address> for Instruction {
    fn control_flow(&self, _ctx: Option<&T>) -> control_flow::Effect<<x86_64 as Arch>::Address> {
        let mut instr_control_flow = ControlFlowAnalysis::new();
        semantic::evaluate(self, &mut instr_control_flow);
        let assume_calls_return = true;
        match self.opcode {
            Opcode::CALL |
            Opcode::SYSCALL => {
                if assume_calls_return {
                    return control_flow::Effect::cont();
                    // instr_control_flow.with_effect(control_flow::Effect::cont());
                }
            }
            _ => {}
        }
        instr_control_flow.effect
    }
}

#[test]
fn test_x86_determinant() {
    use yaxpeax_arch::Decoder;
    use analyses::control_flow::Determinant;
    let decoder = <x86_64 as Arch>::Decoder::default();
    // call 0x1234567
    let call = decoder.decode([0xe8, 0x78, 0x56, 0x34, 0x12].iter().cloned()).unwrap();
    assert_eq!(call.control_flow(Option::<&()>::None), control_flow::Effect::cont());
    // jmp 0x1234567
    let jmp = decoder.decode([0xe9, 0x78, 0x56, 0x34, 0x12].iter().cloned()).unwrap();
    assert_eq!(jmp.control_flow(Option::<&()>::None), control_flow::Effect::stop_and(
        control_flow::Target::Relative(AddressDiff::from_const(0x12345678))
    ));
    // ret
    let ret = decoder.decode([0xc3].iter().cloned()).unwrap();
    assert_eq!(ret.control_flow(Option::<&()>::None), control_flow::Effect::stop());
    // jl 0x14
    let jl = decoder.decode([0x7c, 0x14].iter().cloned()).unwrap();
    assert_eq!(jl.control_flow(Option::<&()>::None), control_flow::Effect::cont_and(
        control_flow::Target::Relative(AddressDiff::from_const(0x14))
    ));
}
