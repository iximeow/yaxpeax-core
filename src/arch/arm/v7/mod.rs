use yaxpeax_arch::{Arch, LengthedInstruction};
use yaxpeax_arm::armv7::{ARMv7, Instruction, Opcode, Operands, ConditionCode};

use analyses::control_flow;

use std::collections::HashMap;

impl <T> control_flow::Determinant<T, <ARMv7 as Arch>::Address> for Instruction
    where T: PartialInstructionContext {

    fn control_flow(&self, ctx: Option<&T>) -> control_flow::Effect<<ARMv7 as Arch>::Address> {
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
                    Operands::ThreeOperandWithShift(Rn, Rd, _, _) => {
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
                    Operands::ThreeOperand(Rn, Rd, Offset) => {
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

pub struct MergedContextTable {
    pub user_contexts: HashMap<<ARMv7 as Arch>::Address, PartialContext>,
    pub computed_contexts: HashMap<<ARMv7 as Arch>::Address, ComputedContext>
}

impl MergedContextTable {
    pub fn create_empty() -> MergedContextTable {
        MergedContextTable {
            user_contexts: HashMap::new(),
            computed_contexts: HashMap::new()
        }
    }
}

trait PartialInstructionContext {

}

#[derive(Debug)]
pub struct MergedContext<'a, 'b> {
    pub computed: Option<&'a ComputedContext>,
    pub user: Option<&'b PartialContext>
}

#[derive(Debug)]
pub struct ComputedContext {

}
impl PartialInstructionContext for ComputedContext {

}

#[derive(Debug)]
pub struct PartialContext {
}
impl PartialInstructionContext for PartialContext {

}
