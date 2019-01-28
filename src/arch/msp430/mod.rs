pub mod cpu;
// pub mod instruction;
pub mod display;
pub mod syntaxed_render;

use std::collections::HashMap;
use std::fs::OpenOptions;
use std::path::Path;
use serde_json;
use serde;
use serde::Deserialize;

use yaxpeax_arch::Arch;
use yaxpeax_msp430_mc::{Opcode, Operand, MSP430};
use ContextTable;

use analyses::control_flow;

impl <T> control_flow::Determinant<T, u16> for yaxpeax_msp430_mc::Instruction where T: PartialInstructionContext {
    fn control_flow(&self, ctx: Option<&T>) -> control_flow::Effect<u16> {
        match ctx.and_then(|x| x.control_flow()) {
            Some(effect) => { return effect.clone(); }
            _ => ()
        };
        match self.opcode {
            Opcode::JMP => {
                control_flow::Effect::stop_and(
                    control_flow::Target::Relative(
                        match self.operands[0] {
                            Operand::Offset(offs) => (offs as u16).wrapping_mul(2),
                            _ => { unreachable!() }
                        }
                    )
                )
            },
            Opcode::JL |
            Opcode::JGE |
            Opcode::JN |
            Opcode::JC |
            Opcode::JNC |
            Opcode::JEQ |
            Opcode::JNE => {
                control_flow::Effect::cont_and(
                    control_flow::Target::Relative(
                        match self.operands[0] {
                            Operand::Offset(offs) => (offs as u16).wrapping_mul(2),
                            _ => { unreachable!() }
                        }
                    )
                )
            },
            Opcode::MOV => {
                match self.operands[1] {
                    Operand::Register(0) => {
                        // well, this will modify PC.
                        match self.operands[0] {
                            Operand::Register(0) => {
                                control_flow::Effect::stop()
                            },
                            Operand::Register(_) => {
                                control_flow::Effect::stop_and(
                                    control_flow::Target::Indeterminate
                                )
                            },
                            Operand::Immediate(imm) => {
                                control_flow::Effect::stop_and(
                                    control_flow::Target::Absolute(imm)
                                )
                            },
                            _ => {
                                control_flow::Effect::stop_and(
                                    control_flow::Target::Indeterminate
                                )
                            }
                        }
                    },
                    _ => {
                        control_flow::Effect::cont()
                    }
                }
            },
            Opcode::RRA |
            Opcode::SXT |
            Opcode::BIT |
            Opcode::BIC |
            Opcode::BIS |
            Opcode::RRC |
            Opcode::SWPB => {
                match self.operands[0] {
                    Operand::Register(0) => {
                        control_flow::Effect::stop_and(
                            control_flow::Target::Indeterminate
                        )
                    },
                    _ => {
                        control_flow::Effect::cont()
                    }
                }
            }
            Opcode::ADD |
            Opcode::ADDC |
            Opcode::SUBC |
            Opcode::AND |
            Opcode::XOR |
            Opcode::SUB |
            Opcode::DADD => {
                match self.operands[1] {
                    Operand::Register(0) => {
                        control_flow::Effect::stop_and(
                            control_flow::Target::Indeterminate
                        )
                    },
                    _ => {
                        control_flow::Effect::cont()
                    }
                }
            }
            _ => {
                control_flow::Effect::cont()
            }
        }
    }
}

pub trait PartialInstructionContext {
    fn indicator_tag(&self) -> &'static str;
    fn address(&self) -> Option<u16>;
    fn comment(&self) -> Option<String>;
    fn control_flow(&self) -> Option<&control_flow::Effect<u16>>;
}

#[derive(Serialize, Deserialize, Debug)]
pub struct PartialContext {
    pub address: Option<u16>,
    pub comment: Option<String>,
    pub control_flow: Option<control_flow::Effect<u16>>
}

impl PartialInstructionContext for PartialContext {
    fn indicator_tag(&self) -> &'static str {
        "m"
    }
    fn address(&self) -> Option<u16> {
        self.address.map(|x| x.to_owned())
    }
    fn comment(&self) -> Option<String> {
        self.comment.as_ref().map(|x| x.to_owned())
    }
    fn control_flow(&self) -> Option<&control_flow::Effect<u16>> {
        self.control_flow.as_ref()
    }
}

pub struct ComputedContext {
    pub address: Option<u16>,
    pub comment: Option<String>
}

impl PartialInstructionContext for ComputedContext {
    fn indicator_tag(&self) -> &'static str {
        "~"
    }
    fn address(&self) -> Option<u16> {
        self.address.map(|x| x.to_owned())
    }
    fn comment(&self) -> Option<String> {
        self.comment.as_ref().map(|x| x.to_owned())
    }
    fn control_flow(&self) -> Option<&control_flow::Effect<u16>> {
        None
    }
}

pub struct MergedContext<'a, 'b> {
    pub user: Option<&'a PartialContext>,
    pub computed: Option<&'b ComputedContext>
}

pub struct MergedContextTable {
    pub user_contexts: HashMap<<MSP430 as Arch>::Address, PartialContext>,
    pub computed_contexts: HashMap<<MSP430 as Arch>::Address, ComputedContext>
}

impl <'it> ContextTable<MSP430, MergedContext<'it, 'it>, StateUpdate> for &'it MergedContextTable {
    fn at(&self, address: &<MSP430 as Arch>::Address) -> MergedContext<'it, 'it> {
        MergedContext {
            user: self.user_contexts.get(address),
            computed: self.computed_contexts.get(address)
        }
    }
    fn put(&mut self, address: <MSP430 as Arch>::Address, update: StateUpdate) {}
}

pub enum StateUpdate {
    FullContext(ComputedContext)
}

impl <'a, 'b> PartialInstructionContext for MergedContext<'a, 'b> {
    fn indicator_tag(&self) -> &'static str {
        "+"
    }
    fn address(&self) -> Option<u16> {
        self.user.and_then(|x| x.address()).or_else(|| { self.computed.and_then(|x| x.address()) })
    }
    fn comment(&self) -> Option<String> {
        self.user.and_then(|x| x.comment()).or_else(|| { self.computed.and_then(|x| x.comment()) })
    }
    fn control_flow(&self) -> Option<&control_flow::Effect<u16>> {
        self.user.and_then(|x| x.control_flow()).or_else(|| { self.computed.and_then(|x| x.control_flow()) })
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum Location {
    Register(u8), // one of r0-r15
    RegisterB(u8), // one of r0-r15
    Memory(u16), // memory at anywhere 0-0xffff. does not distinguish one/two byte memory
    MemoryAny,
    FlagZ,
    FlagN,
    FlagC,
    FlagV
}

const PC: Location = Location::Register(0);
const SP: Location = Location::Register(1);
const SR: Location = Location::Register(2);
const CG: Location = Location::Register(3);
const WriteAllFlags: [(Option<Location>, Direction); 4] = [
    (Some(Location::FlagZ), Direction::Write),
    (Some(Location::FlagN), Direction::Write),
    (Some(Location::FlagC), Direction::Write),
    (Some(Location::FlagV), Direction::Write)
];
const ReadAllFlags: [(Option<Location>, Direction); 4] = [
    (Some(Location::FlagZ), Direction::Read),
    (Some(Location::FlagN), Direction::Read),
    (Some(Location::FlagC), Direction::Read),
    (Some(Location::FlagV), Direction::Read)
];

use analyses::static_single_assignment::cytron::AliasInfo;
impl AliasInfo for Location {
    fn aliases_of(loc: &Self) -> Vec<Self> { vec![] }
    fn maximal_alias_of(loc: Self) -> Self {
        match loc {
            Location::FlagZ | Location::FlagN
                | Location::FlagC | Location::FlagV => {
                SR
            },
            Location::MemoryAny | Location::Memory(_) => {
                Location::MemoryAny
            },
            Location::Register(r) | Location::RegisterB(r) => {
                Location::Register(r)
            }
        }
    }
}

use analyses::static_single_assignment::cytron::{Direction, SSAValues};
impl SSAValues for MSP430 {
    type Location = Location;
    type Data = u8;

    fn decompose(instr: &Self::Instruction) -> Vec<(Option<Self::Location>, Direction)> {
        use arch::msp430::MergedContext;
        let ctx = MergedContext {
            computed: None,
            user: None
        };

        use yaxpeax_msp430_mc::Width;
        fn decompose_operand(op: Operand, width: Width, direction: Direction) -> Vec<(Option<Location>, Direction)> {
            match op {
                Operand::Immediate(_) | Operand::Const0 | Operand::Const1
                    | Operand::Const2 | Operand::Const4
                    | Operand::Const8 | Operand::ConstNeg1
                    | Operand::Nothing => {
                    vec![]
                },
                Operand::Register(x) => {
                    match width {
                        Width::W =>
                            vec![(Some(Location::Register(x)), direction)],
                        Width::B =>
                            vec![(Some(Location::RegisterB(x)), direction)],
                    }
                },
                Operand::Indexed(reg, offset) => {
                    vec![(Some(Location::Register(reg)), Direction::Read), (Some(Location::MemoryAny), direction)]
                },
                Operand::RegisterIndirect(reg) => {
                    vec![(Some(Location::Register(reg)), Direction::Read), (Some(Location::MemoryAny), direction)]
                },
                Operand::IndirectAutoinc(reg) => {
                    vec![
                        (Some(Location::Register(reg)), Direction::Read),
                        (Some(Location::Register(reg)), Direction::Write),
                        (Some(Location::MemoryAny), direction)
                    ]
                },
                Operand::Symbolic(offset) => {
                    vec![(Some(PC), Direction::Read), (Some(Location::MemoryAny), direction)]
                },
                Operand::Absolute(addr) => {
                    vec![(Some(Location::MemoryAny), direction)]
                },
                Operand::Offset(offset) => {
                    // Offset itself has no reads/writes
                    // it only is used in jmp which writes to PC
                    // but that's handled elsewhere.
                    vec![]
                }
            }
        }

        let mut result: Vec<(Option<Self::Location>, Direction)> = Vec::new();
        match instr.opcode {
            Opcode::Invalid(_) => {
                vec![]
            },
            Opcode::RRC => {
                let mut result = decompose_operand(instr.operands[0], instr.op_width, Direction::Read);
                result.append(&mut decompose_operand(instr.operands[0], instr.op_width, Direction::Write));
                result.push((Some(Location::FlagC), Direction::Read));
                result.push((Some(Location::FlagC), Direction::Write));
                result
            },
            Opcode::SWPB => {
                let mut result = decompose_operand(instr.operands[0], instr.op_width, Direction::Read);
                result.append(&mut decompose_operand(instr.operands[0], instr.op_width, Direction::Write));
                result
            },
            Opcode::RRA => {
                let mut result = decompose_operand(instr.operands[0], instr.op_width, Direction::Read);
                result.append(&mut decompose_operand(instr.operands[0], instr.op_width, Direction::Write));
                result.push((Some(Location::FlagC), Direction::Write));
                result
            },
            Opcode::SXT => {
                let mut result = decompose_operand(instr.operands[0], instr.op_width, Direction::Read);
                result.append(&mut decompose_operand(instr.operands[0], instr.op_width, Direction::Write));
                result.extend(WriteAllFlags.iter().cloned());
                result
            },
            Opcode::PUSH => {
                let mut result = decompose_operand(instr.operands[0], instr.op_width, Direction::Read);
                result.push((Some(SP), Direction::Read));
                result.push((Some(SP), Direction::Write));
                // this is, more precisely, the original value of sp that's written to...
                result.push((Some(Location::MemoryAny), Direction::Write));
                result
            },
            Opcode::CALL => {
                let mut result = decompose_operand(instr.operands[0], instr.op_width, Direction::Read);
                result.push((Some(PC), Direction::Write));
                result.push((Some(SP), Direction::Read));
                result.push((Some(SP), Direction::Write));
                // this is, more precisely, the original value of sp that's written to...
                result.push((Some(Location::MemoryAny), Direction::Write));
                result
            },
            Opcode::RETI => {
                let mut result = vec![];
                result.push((Some(PC), Direction::Write));
                result.push((Some(SP), Direction::Read));
                result.push((Some(SP), Direction::Write));
                result.extend(WriteAllFlags.iter().cloned());
                result
            },
            Opcode::JNE | Opcode::JEQ => {
                vec![
                    (Some(PC), Direction::Write),
                    (Some(Location::FlagZ), Direction::Read)
                ]
            },
            Opcode::JNC | Opcode::JC => {
                vec![
                    (Some(PC), Direction::Write),
                    (Some(Location::FlagC), Direction::Read)
                ]
            },
            Opcode::JN => {
                vec![
                    (Some(PC), Direction::Write),
                    (Some(Location::FlagN), Direction::Read)
                ]
            },
            Opcode::JGE | Opcode::JL => {
                vec![
                    (Some(PC), Direction::Write),
                    (Some(Location::FlagN), Direction::Read),
                    (Some(Location::FlagV), Direction::Read)
                ]
            },
            Opcode::JMP => {
                vec![
                    (Some(PC), Direction::Write)
                ]
            },
            // Figure out flags for all of these.
            Opcode::AND |
            Opcode::XOR |
            Opcode::ADD |
            Opcode::SUB |
            Opcode::DADD => {
                let mut result = decompose_operand(instr.operands[0], instr.op_width, Direction::Read);
                result.append(&mut decompose_operand(instr.operands[1], instr.op_width, Direction::Read));
                result.append(&mut decompose_operand(instr.operands[1], instr.op_width, Direction::Write));
                result.extend(WriteAllFlags.iter().cloned());
                result
            },
            Opcode::ADDC |
            Opcode::SUBC => {
                let mut result = decompose_operand(instr.operands[0], instr.op_width, Direction::Read);
                result.append(&mut decompose_operand(instr.operands[1], instr.op_width, Direction::Read));
                result.append(&mut decompose_operand(instr.operands[1], instr.op_width, Direction::Write));
                result.extend(ReadAllFlags.iter().cloned());
                result.extend(WriteAllFlags.iter().cloned());
                result
            }
            Opcode::MOV => {
                let mut result = decompose_operand(instr.operands[0], instr.op_width, Direction::Read);
                result.append(&mut decompose_operand(instr.operands[1], instr.op_width, Direction::Write));
                result
            },
            Opcode::BIT => {
                let mut result = decompose_operand(instr.operands[0], instr.op_width, Direction::Read);
                result.append(&mut decompose_operand(instr.operands[1], instr.op_width, Direction::Read));
                result.append(&mut decompose_operand(instr.operands[1], instr.op_width, Direction::Write));
                result.extend(WriteAllFlags.iter().cloned());
                result
            }
            Opcode::BIC |
            Opcode::BIS => {
                let mut result = decompose_operand(instr.operands[0], instr.op_width, Direction::Read);
                result.append(&mut decompose_operand(instr.operands[1], instr.op_width, Direction::Read));
                result.append(&mut decompose_operand(instr.operands[1], instr.op_width, Direction::Write));
                result
            },
            Opcode::CMP => {
                let mut result = decompose_operand(instr.operands[0], instr.op_width, Direction::Read);
                result.append(&mut decompose_operand(instr.operands[1], instr.op_width, Direction::Read));
                result.extend(WriteAllFlags.iter().cloned());
                result
            }
        }
    }
}
