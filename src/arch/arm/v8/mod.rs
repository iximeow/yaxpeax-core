use yaxpeax_arch::Arch;
use yaxpeax_arm::armv8::a64::ARMv8;

use arch::BaseUpdate;
use arch::{Symbol, SymbolQuery};
use arch::{Function, FunctionQuery};
use arch::CommentQuery;
use arch::FunctionImpl;
use arch::arm::v8::aarch64::analyses::data_flow::DefaultCallingConvention;
use analyses::static_single_assignment::SSA;
use analyses::xrefs;

use analyses::control_flow;

use data::ValueLocations;
use data::modifier::InstructionModifiers;

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::{Ref, RefCell};

use num_traits::Zero;
use ContextRead;
use ContextWrite;

pub mod display;
pub mod aarch64;

#[derive(Serialize, Deserialize)]
pub struct ARMv8Data {
    pub preferred_addr: <ARMv8 as Arch>::Address,
    pub contexts: MergedContextTable,
    pub cfg: control_flow::ControlFlowGraph<<ARMv8 as Arch>::Address>,
    pub ssa: HashMap<<ARMv8 as Arch>::Address, (
        control_flow::ControlFlowGraph<<ARMv8 as Arch>::Address>,
        SSA<ARMv8>
    )>,
}

pub struct DisplayCtx<'a> {
    functions: Ref<'a, HashMap<<ARMv8 as Arch>::Address, FunctionImpl<<ARMv8 as ValueLocations>::Location>>>,
    comments: &'a HashMap<<ARMv8 as Arch>::Address, String>,
    symbols: &'a HashMap<<ARMv8 as Arch>::Address, Symbol>,
}

impl FunctionQuery<<ARMv8 as Arch>::Address> for MergedContextTable {
    type Function = Function;
    fn function_at(&self, _addr: <ARMv8 as Arch>::Address) -> Option<&Self::Function> {
        unimplemented!("FunctionQuery::function_at for MergedContextTable");
    }
    fn all_functions(&self) -> Vec<&Self::Function> {
        unimplemented!("FunctionQuery::all_functions for MergedContextTable");
    }
}
impl CommentQuery<<ARMv8 as Arch>::Address> for MergedContextTable {
    fn comment_for(&self, addr: <ARMv8 as Arch>::Address) -> Option<&str> {
        self.comments.get(&addr).map(String::as_ref)
    }
}

impl SymbolQuery<<ARMv8 as Arch>::Address> for ARMv8Data {
    fn symbol_for(&self, _addr: <ARMv8 as Arch>::Address) -> Option<&Symbol> {
        None
    }
    fn symbol_addr(&self, _sym: &Symbol) -> Option<<ARMv8 as Arch>::Address> {
        None
    }
}

impl<'a> FunctionQuery<<ARMv8 as Arch>::Address> for DisplayCtx<'a> {
    type Function = FunctionImpl<<ARMv8 as ValueLocations>::Location>;
    fn function_at(&self, addr: <ARMv8 as Arch>::Address) -> Option<&Self::Function> {
        self.functions.function_at(addr)
    }
    fn all_functions(&self) -> Vec<&Self::Function> {
        self.functions.all_functions()
    }
}

impl<'a> CommentQuery<<ARMv8 as Arch>::Address> for DisplayCtx<'a> {
    fn comment_for(&self, addr: <ARMv8 as Arch>::Address) -> Option<&str> {
        self.comments.get(&addr).map(String::as_ref)
    }
}

impl<'a> SymbolQuery<<ARMv8 as Arch>::Address> for DisplayCtx<'a> {
    fn symbol_for(&self, addr: <ARMv8 as Arch>::Address) -> Option<&Symbol> {
        self.symbols.get(&addr)
    }
    fn symbol_addr(&self, sym: &Symbol) -> Option<<ARMv8 as Arch>::Address> {
        for (k, v) in self.symbols.iter() {
            if v == sym {
                return Some(*k);
            }
        }

        None
    }
}

impl SymbolQuery<<ARMv8 as Arch>::Address> for MergedContextTable {
    fn symbol_for(&self, addr: <ARMv8 as Arch>::Address) -> Option<&Symbol> {
        self.symbols.get(&addr)
    }
    fn symbol_addr(&self, sym: &Symbol) -> Option<<ARMv8 as Arch>::Address> {
        self.reverse_symbols.get(sym).map(|x| *x)
    }
}

impl Default for ARMv8Data {
    fn default() -> Self {
        ARMv8Data {
            preferred_addr: <ARMv8 as Arch>::Address::zero(),
            contexts: MergedContextTable::create_empty(),
            cfg: control_flow::ControlFlowGraph::new(),
            ssa: HashMap::new()
        }
    }
}

/*
#[allow(non_snake_case)]
impl <T> control_flow::Determinant<T, <ARMv8 as Arch>::Address> for Instruction
    where T: PartialInstructionContext {

    fn control_flow(&self, _ctx: Option<&T>) -> control_flow::Effect<<ARMv8 as Arch>::Address> {
        match self.opcode {
            Opcode::B |
            Opcode::BR |
            Opcode::BL |
            Opcode::BLR => {
                control_flow::Effect::stop_and(
                    control_flow::Target::Indeterminate
                )
            }
            Opcode::Bcc(_) => {
                control_flow::Effect::cont_and(
                    control_flow::Target::Indeterminate
                )
            },
            _ => {
                control_flow::Effect::cont()
            }
        }
    }
}
*/

#[derive(Serialize, Deserialize)]
pub struct MergedContextTable {
    pub user_contexts: HashMap<<ARMv8 as Arch>::Address, Rc<PartialContext>>,
    pub computed_contexts: HashMap<<ARMv8 as Arch>::Address, Rc<ComputedContext>>,
    pub comments: HashMap<<ARMv8 as Arch>::Address, String>,
    pub symbols: HashMap<<ARMv8 as Arch>::Address, Symbol>,
    #[serde(skip)]
    pub reverse_symbols: HashMap<Symbol, <ARMv8 as Arch>::Address>,
    pub functions: Rc<RefCell<HashMap<<ARMv8 as Arch>::Address, FunctionImpl<<ARMv8 as ValueLocations>::Location>>>>,
    pub function_data: HashMap<<ARMv8 as Arch>::Address, RefCell<InstructionModifiers<ARMv8>>>,
    pub function_hints: Vec<<ARMv8 as Arch>::Address>,
    pub default_abi: Option<DefaultCallingConvention>,
}

impl MergedContextTable {
    pub fn create_empty() -> MergedContextTable {
        MergedContextTable {
            user_contexts: HashMap::new(),
            computed_contexts: HashMap::new(),
            comments: HashMap::new(),
            symbols: HashMap::new(),
            reverse_symbols: HashMap::new(),
            functions: Rc::new(RefCell::new(HashMap::new())),
            function_data: HashMap::new(),
            function_hints: Vec::new(),
            default_abi: Some(DefaultCallingConvention::None), // TODO: a real calling convention defulat
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
    AddXRef(xrefs::RefType, xrefs::RefAction, <ARMv8 as Arch>::Address),
    RemoveXRef(xrefs::RefType, xrefs::RefAction, <ARMv8 as Arch>::Address),
    FunctionHint
}

impl ContextRead<ARMv8, MergedContext> for MergedContextTable {
    fn at(&self, address: &<ARMv8 as Arch>::Address) -> MergedContext {
        MergedContext {
            user: self.user_contexts.get(address).map(|v| Rc::clone(v)),
            computed: self.computed_contexts.get(address).map(|v| Rc::clone(v))
        }
    }
}

impl ContextWrite<ARMv8, Update> for MergedContextTable {
    fn put(&mut self, address: <ARMv8 as Arch>::Address, update: Update) {
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
