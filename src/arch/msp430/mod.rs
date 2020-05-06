pub mod cpu;
// pub mod instruction;
pub mod display;
pub mod syntaxed_render;

use std::rc::Rc;
use std::collections::HashMap;
use serialize::Memoable;

use num_traits::Zero;

use yaxpeax_arch::{AddressDiff, Arch};
use yaxpeax_msp430::{Opcode, Operand, MSP430};
use ContextRead;
use ContextWrite;

use arch::{Function, Symbol, SymbolQuery};
use arch::FunctionQuery;
use arch::CommentQuery;

use analyses::control_flow;
use analyses::static_single_assignment::SSA;
use analyses::static_single_assignment::{DFGRef, HashedValue, Value};
use analyses::xrefs;

#[derive(Serialize, Deserialize)]
pub struct MSP430Data {
    pub preferred_addr: <MSP430 as Arch>::Address,
    pub contexts: MergedContextTable,
    pub cfg: control_flow::ControlFlowGraph<<MSP430 as Arch>::Address>,
    pub functions: HashMap<<MSP430 as Arch>::Address, Function>
}

impl CommentQuery<<MSP430 as Arch>::Address> for MergedContextTable {
    fn comment_for(&self, _addr: <MSP430 as Arch>::Address) -> Option<&str> {
        None
    }
}

impl FunctionQuery<<MSP430 as Arch>::Address> for MergedContextTable {
    type Function = Function;
    fn function_at(&self, _addr: <MSP430 as Arch>::Address) -> Option<&Self::Function> {
        None
    }
    fn all_functions(&self) -> Vec<&Self::Function> {
        unimplemented!()
    }
}

impl SymbolQuery<<MSP430 as Arch>::Address> for MSP430Data {
    fn symbol_for(&self, addr: <MSP430 as Arch>::Address) -> Option<&Symbol> {
        self.contexts.symbols.get(&addr)
    }
    fn symbol_addr(&self, sym: &Symbol) -> Option<<MSP430 as Arch>::Address> {
        for (k, v) in self.contexts.symbols.iter() {
            if v == sym {
                return Some(*k);
            }
        }

        None
    }
}

impl Default for MSP430Data {
    fn default() -> Self {
        MSP430Data {
            preferred_addr: <MSP430 as Arch>::Address::zero(),
            contexts: MergedContextTable::create_empty(),
            cfg: control_flow::ControlFlowGraph::new(),
            functions: HashMap::new()
        }
    }
}

impl <T> control_flow::Determinant<T, u16> for yaxpeax_msp430::Instruction {
    fn control_flow(&self, _ctx: Option<&T>) -> control_flow::Effect<u16> {
        match self.opcode {
            Opcode::JMP => {
                control_flow::Effect::stop_and(
                    control_flow::Target::Relative(
                        match self.operands[0] {
                            Operand::Offset(offs) => AddressDiff::from_const((offs as u16).wrapping_mul(2)),
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
                            Operand::Offset(offs) => AddressDiff::from_const((offs as u16).wrapping_mul(2)),
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
    fn comment(&self) -> Option<&str>;
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
    fn comment(&self) -> Option<&str> {
        self.comment.as_ref().map::<&str, _>(|x| &x)
    }
    fn control_flow(&self) -> Option<&control_flow::Effect<u16>> {
        self.control_flow.as_ref()
    }
}

#[derive(Serialize, Deserialize)]
pub struct ComputedContext {
    pub address: Option<u16>,
    pub comment: Option<String>
}

impl PartialInstructionContext for ComputedContext {
    fn indicator_tag(&self) -> &'static str {
        "~"
    }
    fn address(&self) -> Option<u16> {
        self.address.as_ref().map(|x| x.to_owned())
    }
    fn comment(&self) -> Option<&str> {
        self.comment.as_ref().map::<&str, _>(|x| &x)
    }
    fn control_flow(&self) -> Option<&control_flow::Effect<u16>> {
        None
    }
}

#[derive(Serialize)]
pub struct MergedContext {
    pub user: Option<Rc<PartialContext>>,
    pub computed: Option<Rc<ComputedContext>>
}

#[derive(Serialize, Deserialize)]
pub struct MergedContextTable {
    pub user_contexts: HashMap<<MSP430 as Arch>::Address, Rc<PartialContext>>,
    pub computed_contexts: HashMap<<MSP430 as Arch>::Address, Rc<ComputedContext>>,
    #[serde(skip)]
    pub xrefs: xrefs::XRefCollection<<MSP430 as Arch>::Address>,
    pub symbols: HashMap<<MSP430 as Arch>::Address, Symbol>,
    pub functions: HashMap<<MSP430 as Arch>::Address, Function>,
    pub function_hints: Vec<<MSP430 as Arch>::Address>,
    #[serde(skip)]
    pub ssa: HashMap<
        <MSP430 as Arch>::Address,
        (control_flow::ControlFlowGraph<<MSP430 as Arch>::Address>, SSA<MSP430>)
    >
}

impl MergedContextTable {
    pub fn create_empty() -> MergedContextTable {
        MergedContextTable {
            user_contexts: HashMap::new(),
            computed_contexts: HashMap::new(),
            xrefs: xrefs::XRefCollection::new(),
            functions: HashMap::new(),
            function_hints: Vec::new(),
            symbols: HashMap::new(),
            ssa: HashMap::new()
        }
    }
}

impl ContextWrite<MSP430, StateUpdate> for MergedContextTable {
    fn put(&mut self, address: <MSP430 as Arch>::Address, _update: StateUpdate) {
        self.user_contexts.remove(&address);
    }
}

impl ContextRead<MSP430, MergedContext> for MergedContextTable {
    fn at(&self, address: &<MSP430 as Arch>::Address) -> MergedContext {
        MergedContext {
            user: self.user_contexts.get(address).map(|v| Rc::clone(v)),
            computed: self.computed_contexts.get(address).map(|v| Rc::clone(v))
        }
    }
}

pub enum StateUpdate {
    FullContext(ComputedContext)
}

impl PartialInstructionContext for MergedContext {
    fn indicator_tag(&self) -> &'static str {
        "+"
    }
    fn address(&self) -> Option<u16> {
        self.user.as_ref().and_then(|x| x.address()).or_else(|| { self.computed.as_ref().and_then(|x| x.address()) })
    }
    fn comment(&self) -> Option<&str> {
        self.user.as_ref().and_then(|x| x.comment())
            .or_else(|| { self.computed.as_ref().and_then(|x| x.comment()) })
    }
    fn control_flow(&self) -> Option<&control_flow::Effect<u16>> {
        self.user.as_ref().and_then(|x| x.control_flow())
            .or_else(|| self.computed.as_ref().and_then(|x| x.control_flow()))
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Serialize, Deserialize)]
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
#[allow(dead_code)]
const CG: Location = Location::Register(3);
const WRITE_ALL_FLAGS: [(Option<Location>, Direction); 4] = [
    (Some(Location::FlagZ), Direction::Write),
    (Some(Location::FlagN), Direction::Write),
    (Some(Location::FlagC), Direction::Write),
    (Some(Location::FlagV), Direction::Write)
];
const READ_ALL_FLAGS: [(Option<Location>, Direction); 4] = [
    (Some(Location::FlagZ), Direction::Read),
    (Some(Location::FlagN), Direction::Read),
    (Some(Location::FlagC), Direction::Read),
    (Some(Location::FlagV), Direction::Read)
];

use data::AliasInfo;
impl AliasInfo for Location {
    fn aliases_of(&self) -> Vec<Self> { vec![] }
    fn maximal_alias_of(&self) -> Self {
        match self {
            Location::FlagZ | Location::FlagN
                | Location::FlagC | Location::FlagV => {
                SR
            },
            Location::MemoryAny | Location::Memory(_) => {
                Location::MemoryAny
            },
            Location::Register(r) | Location::RegisterB(r) => {
                Location::Register(*r)
            }
        }
    }
}

impl Memoable for HashedValue<DFGRef<MSP430>> {
    type Out = u32;

    fn memoize(&self, memos: &HashMap<Self, u32>) -> Self::Out {
        memos[self]
    }
    fn dememoize(idx: u32, memos: &[Self::Out], dememoized: &mut HashMap<u32, Self>) -> Self {
        use std::cell::RefCell;
        HashedValue { value: Rc::new(RefCell::new(Value {
            name: None,
            used: true,
            location: Location::MemoryAny,
            version: Some(0),
            data: None,
        })) }
    }
}

use analyses::static_single_assignment::SSAValues;
impl SSAValues for MSP430 {
    type Data = u8;
}

use data::ValueLocations;
use data::Direction;
impl ValueLocations for MSP430 {
    type Location = Location;

    fn decompose(instr: &Self::Instruction) -> Vec<(Option<Self::Location>, Direction)> {
        use yaxpeax_msp430::Width;
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
                Operand::Indexed(reg, _) => {
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
                Operand::Symbolic(_) => {
                    vec![(Some(PC), Direction::Read), (Some(Location::MemoryAny), direction)]
                },
                Operand::Absolute(_addr) => {
                    vec![(Some(Location::MemoryAny), direction)]
                },
                Operand::Offset(_offset) => {
                    // Offset itself has no reads/writes
                    // it only is used in jmp which writes to PC
                    // but that's handled elsewhere.
                    vec![]
                }
            }
        }

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
                result.extend(WRITE_ALL_FLAGS.iter().cloned());
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
                result.extend(WRITE_ALL_FLAGS.iter().cloned());
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
                result.extend(WRITE_ALL_FLAGS.iter().cloned());
                result
            },
            Opcode::ADDC |
            Opcode::SUBC => {
                let mut result = decompose_operand(instr.operands[0], instr.op_width, Direction::Read);
                result.append(&mut decompose_operand(instr.operands[1], instr.op_width, Direction::Read));
                result.append(&mut decompose_operand(instr.operands[1], instr.op_width, Direction::Write));
                result.extend(READ_ALL_FLAGS.iter().cloned());
                result.extend(WRITE_ALL_FLAGS.iter().cloned());
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
                result.extend(WRITE_ALL_FLAGS.iter().cloned());
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
                result.extend(WRITE_ALL_FLAGS.iter().cloned());
                result
            }
        }
    }
}
