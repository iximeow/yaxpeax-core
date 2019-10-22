use yaxpeax_arch::Arch;
use yaxpeax_arm::armv8::a64::{ARMv8, Instruction, Opcode};

use arch::{Symbol, SymbolQuery};
use arch::{Function, FunctionQuery};

use analyses::control_flow;

use std::collections::HashMap;
use std::rc::Rc;

use num_traits::Zero;
use ContextRead;
use ContextWrite;

mod display;

#[derive(Serialize, Deserialize)]
pub struct ARMv8Data {
    pub preferred_addr: <ARMv8 as Arch>::Address,
    pub contexts: MergedContextTable,
    pub cfg: control_flow::ControlFlowGraph<<ARMv8 as Arch>::Address>,
    pub functions: HashMap<<ARMv8 as Arch>::Address, Function>
}

impl FunctionQuery<<ARMv8 as Arch>::Address> for ARMv8Data {
    type Function = Function;
    fn function_at(&self, addr: <ARMv8 as Arch>::Address) -> Option<&Self::Function> {
        self.functions.get(&addr)
    }
    fn all_functions(&self) -> Vec<&Self::Function> {
        self.functions.values().collect()
    }
}

impl FunctionQuery<<ARMv8 as Arch>::Address> for MergedContextTable {
    type Function = Function;
    fn function_at(&self, _addr: <ARMv8 as Arch>::Address) -> Option<&Self::Function> {
        None
    }
    fn all_functions(&self) -> Vec<&Self::Function> {
        unimplemented!()
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

impl Default for ARMv8Data {
    fn default() -> Self {
        ARMv8Data {
            preferred_addr: <ARMv8 as Arch>::Address::zero(),
            contexts: MergedContextTable::create_empty(),
            cfg: control_flow::ControlFlowGraph::new(),
            functions: HashMap::new()
        }
    }
}

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

#[derive(Serialize, Deserialize)]
pub struct MergedContextTable {
    pub user_contexts: HashMap<<ARMv8 as Arch>::Address, Rc<PartialContext>>,
    pub computed_contexts: HashMap<<ARMv8 as Arch>::Address, Rc<ComputedContext>>
}

impl MergedContextTable {
    pub fn create_empty() -> MergedContextTable {
        MergedContextTable {
            user_contexts: HashMap::new(),
            computed_contexts: HashMap::new()
        }
    }
}

impl ContextRead<ARMv8, MergedContext> for MergedContextTable {
    fn at(&self, address: &<ARMv8 as Arch>::Address) -> MergedContext {
        MergedContext {
            user: self.user_contexts.get(address).map(|v| Rc::clone(v)),
            computed: self.computed_contexts.get(address).map(|v| Rc::clone(v))
        }
    }
}

impl ContextWrite<ARMv8, /* Update */ ()> for MergedContextTable {
    fn put(&mut self, _address: <ARMv8 as Arch>::Address, _update: ()) {
        // do nothing
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
