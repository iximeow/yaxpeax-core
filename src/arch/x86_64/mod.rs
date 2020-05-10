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
