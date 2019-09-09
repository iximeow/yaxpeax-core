use yaxpeax_arch::{Arch, LengthedInstruction};
use yaxpeax_arm::armv7::{ARMv7, Instruction, Opcode, Operands, ConditionCode};

use arch::{Symbol, SymbolQuery};
use arch::Function;
use arch::FunctionRepr;
use arch::FunctionQuery;
use arch::BaseUpdate;
use arch::CommentQuery;

use analyses::control_flow;

use std::collections::HashMap;
use std::rc::Rc;

use num_traits::Zero;
use ContextRead;
use ContextWrite;

mod display;

// #[derive(Serialize, Deserialize)]
// pub struct Function { }

#[derive(Serialize, Deserialize)]
pub struct ARMv7Data {
    pub preferred_addr: <ARMv7 as Arch>::Address,
    pub contexts: MergedContextTable,
    pub cfg: control_flow::ControlFlowGraph<<ARMv7 as Arch>::Address>,
    pub functions: HashMap<<ARMv7 as Arch>::Address, Function>
}

impl FunctionQuery<<ARMv7 as Arch>::Address> for ARMv7Data {
    type Function = Function;
    fn function_at(&self, addr: <ARMv7 as Arch>::Address) -> Option<&Function> {
        self.contexts.function_at(addr)
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

impl FunctionQuery<<ARMv7 as Arch>::Address> for MergedContextTable {
    type Function = Function;
    fn function_at(&self, _addr: <ARMv7 as Arch>::Address) -> Option<&Function> {
        None
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
            functions: HashMap::new()
        }
    }
}

#[allow(non_snake_case)]
impl <T> control_flow::Determinant<T, <ARMv7 as Arch>::Address> for Instruction {

    fn control_flow(&self, _ctx: Option<&T>) -> control_flow::Effect<<ARMv7 as Arch>::Address> {
        let conditional = self.condition == ConditionCode::AL;
        match self.opcode {
            Opcode::B |
            Opcode::BL |
            Opcode::BLX |
            Opcode::BXJ => {
                if conditional {
                    control_flow::Effect::stop_and(
                        control_flow::Target::Indeterminate
                    )
                } else {
                    control_flow::Effect::cont_and(
                        control_flow::Target::Indeterminate
                    )
                }
            },
            Opcode::AND |
            Opcode::EOR |
            Opcode::SUB |
            Opcode::RSB |
            Opcode::ADD |
            Opcode::ADC |
            Opcode::SBC |
            Opcode::RSC |
            Opcode::ORR |
            Opcode::MOV |
            Opcode::BIC |
            Opcode::MVN |
            Opcode::LSL |
            Opcode::LSR |
            Opcode::ASR |
            Opcode::RRX |
            Opcode::ROR |
            Opcode::ADR => {
                let stop = match self.operands {
                    Operands::RegImm(Rd, _) => {
                        Rd == 15
                    },
                    Operands::TwoOperand(Rd, _) => {
                        Rd == 15
                    },
                    Operands::ThreeOperand(Rd, _, _) => {
                        Rd == 15
                    },
                    Operands::ThreeOperandWithShift(Rd, _, _, _) => {
                        Rd == 15
                    },
                    _ => { unreachable!(); }
                };
                if conditional {
                    (if stop {
                        control_flow::Effect::stop_and
                    } else {
                        control_flow::Effect::cont_and
                    })(control_flow::Target::Relative(0))
                } else {
                    if stop {
                        control_flow::Effect::stop()
                    } else {
                        control_flow::Effect::cont()
                    }
                }
            },

            Opcode::LDREXH |
            Opcode::LDREXB |
            Opcode::LDREXD |
            Opcode::LDREX => {
                let stop = match self.operands {
                    Operands::ThreeOperand(_, Rd, _) => {
                        Rd == 15
                    },
                    // TODO: verify
                    Operands::ThreeOperandWithShift(_, Rd, _, _) => {
                        Rd == 15
                    },
                    _ => { unreachable!(); }
                };
                if conditional {
                    (if stop {
                        control_flow::Effect::stop_and
                    } else {
                        control_flow::Effect::cont_and
                    })(control_flow::Target::Relative(0))
                } else {
                    if stop {
                        control_flow::Effect::stop()
                    } else {
                        control_flow::Effect::cont()
                    }
                }
            },
            Opcode::LDM(_, _, wback, _) |
            Opcode::LDR(_, _, wback) |
            Opcode::LDRB(_, _, wback) => {
                let stop = match self.operands {
                    // TODO: verify
                    Operands::ThreeOperandWithShift(Rn, Rd, _, _) => {
                        Rd == 15 || (wback && (Rn == 15))
                    },
                    _ => { unreachable!(); }
                };
                if conditional {
                    (if stop {
                        control_flow::Effect::stop_and
                    } else {
                        control_flow::Effect::cont_and
                    })(control_flow::Target::Relative(0))
                } else {
                    if stop {
                        control_flow::Effect::stop()
                    } else {
                        control_flow::Effect::cont()
                    }
                }
            },
            Opcode::LDRT(_) |
            Opcode::LDRBT(_) => {
                let stop = match self.operands {
                    // TODO: verify
                    Operands::ThreeOperandWithShift(_Rn, Rd, _, _) => {
                        Rd == 15
                    },
                    _ => { unreachable!(); }
                };
                if conditional {
                    (if stop {
                        control_flow::Effect::stop_and
                    } else {
                        control_flow::Effect::cont_and
                    })(control_flow::Target::Relative(0))
                } else {
                    if stop {
                        control_flow::Effect::stop()
                    } else {
                        control_flow::Effect::cont()
                    }
                }
            },

            Opcode::SWP |
            Opcode::SWPB => {
                let stop = match self.operands {
                    Operands::ThreeOperand(Rn, _Rd, _Offset) => {
                        Rn == 15
                    },
                    _ => { unreachable!() }
                };
                if conditional {
                    (if stop {
                        control_flow::Effect::stop_and
                    } else {
                        control_flow::Effect::cont_and
                    })(control_flow::Target::Relative(0))
                } else {
                    if stop {
                        control_flow::Effect::stop()
                    } else {
                        control_flow::Effect::cont()
                    }
                }
            },

            Opcode::STREXH |
            Opcode::STREXB |
            Opcode::STREXD |
            Opcode::STREX => {
                let stop = match self.operands {
                    Operands::ThreeOperand(_, Rd, _) => {
                        Rd == 15
                    },
                    _ => { unreachable!(); }
                };

                if conditional {
                    (if stop {
                        control_flow::Effect::stop_and
                    } else {
                        control_flow::Effect::cont_and
                    })(control_flow::Target::Relative(0))
                } else {
                    if stop {
                        control_flow::Effect::stop()
                    } else {
                        control_flow::Effect::cont()
                    }
                }
            }
            Opcode::STM(_, _, wback, _) |
            Opcode::STR(_, _, wback) |
            Opcode::STRB(_, _, wback) => {
                let stop = match self.operands {
                    Operands::TwoRegImm(_, Rt, _) => {
                        Rt == 15 && wback
                    },
                    Operands::RegRegList(Rd, _) => {
                        Rd == 15 && wback
                    },
                    _ => { unreachable!(); }
                };

                if conditional {
                    (if stop {
                        control_flow::Effect::stop_and
                    } else {
                        control_flow::Effect::cont_and
                    })(control_flow::Target::Relative(0))
                } else {
                    if stop {
                        control_flow::Effect::stop()
                    } else {
                        control_flow::Effect::cont()
                    }
                }

            },

            Opcode::MUL |
            Opcode::MLA |
            Opcode::UMAAL |
            Opcode::MLS |
            Opcode::UMULL |
            Opcode::UMLAL |
            Opcode::SMULL |
            Opcode::SMLAL |
            _ => {
                if conditional {
                    control_flow::Effect::cont()
                } else {
                    control_flow::Effect::cont_and(
                        control_flow::Target::Relative(self.len())
                    )
                }
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct MergedContextTable {
    pub user_contexts: HashMap<<ARMv7 as Arch>::Address, Rc<PartialContext>>,
    pub computed_contexts: HashMap<<ARMv7 as Arch>::Address, Rc<ComputedContext>>,
    pub comments: HashMap<<ARMv7 as Arch>::Address, String>,
    pub symbols: HashMap<<ARMv7 as Arch>::Address, Symbol>,
    #[serde(skip)]
    pub reverse_symbols: HashMap<Symbol, <ARMv7 as Arch>::Address>,
}

impl MergedContextTable {
    pub fn create_empty() -> MergedContextTable {
        MergedContextTable {
            user_contexts: HashMap::new(),
            computed_contexts: HashMap::new(),
            comments: HashMap::new(),
            symbols: HashMap::new(),
            reverse_symbols: HashMap::new(),
        }
    }
}

impl ContextRead<ARMv7, MergedContext> for MergedContextTable {
    fn at(&self, address: &<ARMv7 as Arch>::Address) -> MergedContext {
        MergedContext {
            user: self.user_contexts.get(address).map(|v| Rc::clone(v)),
            computed: self.computed_contexts.get(address).map(|v| Rc::clone(v))
        }
    }
}

impl ContextWrite<ARMv7, BaseUpdate<()>> for MergedContextTable {
    fn put(&mut self, address: <ARMv7 as Arch>::Address, update: BaseUpdate<()>) {
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
