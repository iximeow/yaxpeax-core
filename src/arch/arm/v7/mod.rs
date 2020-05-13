use yaxpeax_arch::{Arch, LengthedInstruction};
use yaxpeax_arm::armv7::{ARMv7, Instruction, Opcode, Operand, ConditionCode};

use arch::{Symbol, SymbolQuery};
use arch::Function;
use arch::FunctionQuery;
use arch::BaseUpdate;
use arch::CommentQuery;
use arch::FunctionImpl;
use arch::arm::v7::analyses::data_flow::DefaultCallingConvention;

use data::{Direction, ValueLocations};
use data::modifier::InstructionModifiers;

use std::cell::{Ref, RefCell};

use analyses::control_flow;
use analyses::static_single_assignment::SSA;
use analyses::xrefs;

use std::collections::HashMap;
use std::rc::Rc;

use petgraph::graphmap::GraphMap;

use num_traits::Zero;
use ContextRead;
use ContextWrite;

use tracing::{event, Level};

pub mod display;
pub mod analyses;
pub mod semantic;

// #[derive(Serialize, Deserialize)]
// pub struct Function { }

#[derive(Serialize, Deserialize)]
pub struct ARMv7Data {
    pub preferred_addr: <ARMv7 as Arch>::Address,
    pub contexts: MergedContextTable,
    pub cfg: control_flow::ControlFlowGraph<<ARMv7 as Arch>::Address>,
    pub ssa: HashMap<<ARMv7 as Arch>::Address, (
        control_flow::ControlFlowGraph<<ARMv7 as Arch>::Address>,
        SSA<ARMv7>
    )>,
}

pub struct DisplayCtx<'a> {
    functions: Ref<'a, HashMap<<ARMv7 as Arch>::Address, FunctionImpl<<ARMv7 as ValueLocations>::Location>>>,
    comments: &'a HashMap<<ARMv7 as Arch>::Address, String>,
    symbols: &'a HashMap<<ARMv7 as Arch>::Address, Symbol>,
}

impl FunctionQuery<<ARMv7 as Arch>::Address> for ARMv7Data {
    type Function = Function;
    fn function_at(&self, addr: <ARMv7 as Arch>::Address) -> Option<&Self::Function> {
        self.contexts.function_at(addr)
    }
    fn all_functions(&self) -> Vec<&Self::Function> {
        self.contexts.all_functions()
    }
}
impl CommentQuery<<ARMv7 as Arch>::Address> for ARMv7Data {
    fn comment_for(&self, addr: <ARMv7 as Arch>::Address) -> Option<&str> {
        self.contexts.comment_for(addr)
    }
}
impl SymbolQuery<<ARMv7 as Arch>::Address> for ARMv7Data {
    fn symbol_for(&self, addr: <ARMv7 as Arch>::Address) -> Option<&Symbol> {
        self.contexts.symbol_for(addr)
    }
    fn symbol_addr(&self, sym: &Symbol) -> Option<<ARMv7 as Arch>::Address> {
        self.contexts.symbol_addr(sym)
    }
}

impl<'a> FunctionQuery<<ARMv7 as Arch>::Address> for DisplayCtx<'a> {
    type Function = FunctionImpl<<ARMv7 as ValueLocations>::Location>;
    fn function_at(&self, addr: <ARMv7 as Arch>::Address) -> Option<&Self::Function> {
        self.functions.function_at(addr)
    }
    fn all_functions(&self) -> Vec<&Self::Function> {
        self.functions.all_functions()
    }
}

impl<'a> CommentQuery<<ARMv7 as Arch>::Address> for DisplayCtx<'a> {
    fn comment_for(&self, addr: <ARMv7 as Arch>::Address) -> Option<&str> {
        self.comments.get(&addr).map(String::as_ref)
    }
}

impl<'a> SymbolQuery<<ARMv7 as Arch>::Address> for DisplayCtx<'a> {
    fn symbol_for(&self, addr: <ARMv7 as Arch>::Address) -> Option<&Symbol> {
        self.symbols.get(&addr)
    }
    fn symbol_addr(&self, sym: &Symbol) -> Option<<ARMv7 as Arch>::Address> {
        for (k, v) in self.symbols.iter() {
            if v == sym {
                return Some(*k);
            }
        }

        None
    }
}

impl FunctionQuery<<ARMv7 as Arch>::Address> for MergedContextTable {
    type Function = Function;
    fn function_at(&self, _addr: <ARMv7 as Arch>::Address) -> Option<&Self::Function> {
        None
    }
    fn all_functions(&self) -> Vec<&Self::Function> {
        unimplemented!()
    }
}
impl CommentQuery<<ARMv7 as Arch>::Address> for MergedContextTable {
    fn comment_for(&self, addr: <ARMv7 as Arch>::Address) -> Option<&str> {
        self.comments.get(&addr).map(String::as_ref)
    }
}
impl SymbolQuery<<ARMv7 as Arch>::Address> for MergedContextTable {
    fn symbol_for(&self, addr: <ARMv7 as Arch>::Address) -> Option<&Symbol> {
        self.symbols.get(&addr)
    }
    fn symbol_addr(&self, sym: &Symbol) -> Option<<ARMv7 as Arch>::Address> {
        self.reverse_symbols.get(sym).map(|x| *x)
    }
}

impl Default for ARMv7Data {
    fn default() -> Self {
        ARMv7Data {
            preferred_addr: <ARMv7 as Arch>::Address::zero(),
            contexts: MergedContextTable::create_empty(),
            cfg: control_flow::ControlFlowGraph::new(),
            ssa: HashMap::new()
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct MergedContextTable {
    pub user_contexts: HashMap<<ARMv7 as Arch>::Address, Rc<PartialContext>>,
    pub computed_contexts: HashMap<<ARMv7 as Arch>::Address, Rc<ComputedContext>>,
    pub comments: HashMap<<ARMv7 as Arch>::Address, String>,
    #[serde(skip)]
    pub call_graph: GraphMap<<ARMv7 as Arch>::Address, (), petgraph::Directed>,
    #[serde(skip)]
    pub xrefs: xrefs::XRefCollection<<ARMv7 as Arch>::Address>,
    pub symbols: HashMap<<ARMv7 as Arch>::Address, Symbol>,
    #[serde(skip)]
    pub reverse_symbols: HashMap<Symbol, <ARMv7 as Arch>::Address>,
    pub functions: Rc<RefCell<HashMap<<ARMv7 as Arch>::Address, FunctionImpl<<ARMv7 as ValueLocations>::Location>>>>,
    pub function_data: HashMap<<ARMv7 as Arch>::Address, RefCell<InstructionModifiers<ARMv7>>>,
    pub function_hints: Vec<<ARMv7 as Arch>::Address>,
    pub default_abi: Option<DefaultCallingConvention>,
}

impl MergedContextTable {
    pub fn create_empty() -> MergedContextTable {
        MergedContextTable {
            user_contexts: HashMap::new(),
            computed_contexts: HashMap::new(),
            comments: HashMap::new(),
            call_graph: GraphMap::new(),
            xrefs: xrefs::XRefCollection::new(),
            symbols: HashMap::new(),
            reverse_symbols: HashMap::new(),
            functions: Rc::new(RefCell::new(HashMap::new())),
            function_data: HashMap::new(),
            function_hints: Vec::new(),
            default_abi: Some(DefaultCallingConvention::Standard),
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

pub type Update = BaseUpdate<ArmUpdate>;

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum ArmUpdate {
    AddXRef(xrefs::RefType, xrefs::RefAction, <ARMv7 as Arch>::Address),
    RemoveXRef(xrefs::RefType, xrefs::RefAction, <ARMv7 as Arch>::Address),
    FunctionHint
}

impl ContextRead<ARMv7, MergedContext> for MergedContextTable {
    fn at(&self, address: &<ARMv7 as Arch>::Address) -> MergedContext {
        MergedContext {
            user: self.user_contexts.get(address).map(|v| Rc::clone(v)),
            computed: self.computed_contexts.get(address).map(|v| Rc::clone(v))
        }
    }
}

impl ContextWrite<ARMv7, Update> for MergedContextTable {
    fn put(&mut self, address: <ARMv7 as Arch>::Address, update: Update) {
        match update {
            BaseUpdate::DefineSymbol(sym) => {
                self.symbols.insert(address, sym.clone());
                self.reverse_symbols.insert(sym, address);
            }
            BaseUpdate::AddCodeComment(comment) => {
                self.comments.insert(address, comment);
            }
            _ => { }
        }
    }
}

pub trait PartialInstructionContext {
    fn indicator_tag(&self) -> &'static str;
}

#[derive(Debug)]
pub struct MergedContext {
    pub computed: Option<Rc<ComputedContext>>,
    pub user: Option<Rc<PartialContext>>
}

impl PartialInstructionContext for MergedContext {
    fn indicator_tag(&self) -> &'static str {
        "+"
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ComputedContext {

}
impl PartialInstructionContext for ComputedContext {
    fn indicator_tag(&self) -> &'static str {
        "@"
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct PartialContext {
}
impl PartialInstructionContext for PartialContext {
    fn indicator_tag(&self) -> &'static str {
        "m"
    }
}
