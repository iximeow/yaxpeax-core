///
/// Note, some flags are inconsistently modeled.
/// "inconsistently" here means that some instructions may correctly model them,
/// while others are inaccurate, or entirely miss flags.
///
/// This is a best-effort approach to get something together, focusing
/// primarily on cf, pf, zf, of, and sf.
///
/// Some instructions, like `INTO`, could be correctly modeled with significantly
/// more effort, but since there is currently no mechanism to indicate if
/// this should be optimistic or pessimistic, I'm .. ignoring it :)

use arch::Symbol;
use arch::Function;
use analyses::static_single_assignment::cytron::{AliasInfo, Direction, SSAValues, Value};
use analyses::static_single_assignment::cytron::{Typed, TypeSpec, TypeAtlas};
use yaxpeax_x86::{RegSpec, RegisterBank, x86_64, Opcode, Operand};

use std::rc::Rc;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};

use std::collections::HashMap;
use analyses::static_single_assignment::cytron::HashedValue;
use serialize::Memoable;

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub enum Location {
    Register(RegSpec),
    Memory(u64, u8),
    MemoryAny,
    // not modeling eflags' system bits ... yet?
    CF, PF, AF, ZF, SF, TF, IF, DF, OF, IOPL
}

impl AliasInfo for Location {
    fn aliases_of(&self) -> Vec<Self> {
        match self {
            Location::CF |
            Location::PF |
            Location::AF |
            Location::ZF |
            Location::SF |
            Location::TF |
            Location::IF |
            Location::DF |
            Location::OF |
            Location::IOPL => {
                vec![Location::Register(RegSpec::rflags())]
            },
            Location::Memory(_, _) |
            Location::MemoryAny => vec![Location::MemoryAny],
            Location::Register(RegSpec { bank: RegisterBank::CR, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::DR, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::S, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::RIP, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::RFlags, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::ST, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::Z, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::Q, num: _ }) => {
                vec![self.clone()]
            },
            Location::Register(RegSpec { bank: RegisterBank::D, num }) => {
                vec![Location::Register(RegSpec { bank: RegisterBank::Q, num: *num })]
            }
            Location::Register(RegSpec { bank: RegisterBank::W, num }) => {
                vec![
                    Location::Register(RegSpec { bank: RegisterBank::Q, num: *num }),
                    Location::Register(RegSpec { bank: RegisterBank::D, num: *num })
                ]
            }
            Location::Register(RegSpec { bank: RegisterBank::B, num }) => {
                vec![
                    Location::Register(RegSpec { bank: RegisterBank::Q, num: *num }),
                    Location::Register(RegSpec { bank: RegisterBank::D, num: *num }),
                    Location::Register(RegSpec { bank: RegisterBank::W, num: *num })
                ]
            }
            Location::Register(RegSpec { bank: RegisterBank::rB, num }) => {
                vec![
                    Location::Register(RegSpec { bank: RegisterBank::Q, num: *num & 0x3 }),
                    Location::Register(RegSpec { bank: RegisterBank::D, num: *num & 0x3 }),
                    Location::Register(RegSpec { bank: RegisterBank::W, num: *num & 0x3 })
                ]
            }
            Location::Register(RegSpec { bank: RegisterBank::MM, num }) => {
                vec![Location::Register(RegSpec { bank: RegisterBank::ST, num: *num })]
            }
            Location::Register(RegSpec { bank: RegisterBank::Y, num }) => {
                vec![
                    Location::Register(RegSpec { bank: RegisterBank::Z, num: *num })
                ]
            }
            Location::Register(RegSpec { bank: RegisterBank::X, num }) => {
                vec![
                    Location::Register(RegSpec { bank: RegisterBank::Z, num: *num }),
                    Location::Register(RegSpec { bank: RegisterBank::Y, num: *num })
                ]
            }
            Location::Register(RegSpec { bank: RegisterBank::EIP, num: _ }) => {
                vec![Location::Register(RegSpec::RIP())]
            }
            Location::Register(RegSpec { bank: RegisterBank::EFlags, num: _ }) => {
                vec![Location::Register(RegSpec::rflags())]
            }
        }
    }
    fn maximal_alias_of(&self) -> Self {
        match self {
            Location::CF |
            Location::PF |
            Location::AF |
            Location::ZF |
            Location::SF |
            Location::TF |
            Location::IF |
            Location::DF |
            Location::OF |
            Location::IOPL => {
                Location::Register(RegSpec::rflags())
            },
            Location::Memory(_, _) |
            Location::MemoryAny => Location::MemoryAny,
            Location::Register(RegSpec { bank: RegisterBank::CR, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::DR, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::S, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::RIP, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::RFlags, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::ST, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::Z, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::Q, num: _ }) => {
                self.clone()
            },
            Location::Register(RegSpec { bank: RegisterBank::D, num }) |
            Location::Register(RegSpec { bank: RegisterBank::W, num }) |
            Location::Register(RegSpec { bank: RegisterBank::B, num }) => {
                Location::Register(RegSpec { bank: RegisterBank::Q, num: *num })
            }
            Location::Register(RegSpec { bank: RegisterBank::rB, num }) => {
                Location::Register(RegSpec { bank: RegisterBank::Q, num: *num & 0x3 })
            }
            Location::Register(RegSpec { bank: RegisterBank::MM, num }) => {
                Location::Register(RegSpec { bank: RegisterBank::ST, num: *num })
            }
            Location::Register(RegSpec { bank: RegisterBank::Y, num }) |
            Location::Register(RegSpec { bank: RegisterBank::X, num }) => {
                Location::Register(RegSpec { bank: RegisterBank::Z, num: *num })
            }
            Location::Register(RegSpec { bank: RegisterBank::EIP, num: _ }) => {
                Location::Register(RegSpec::RIP())
            }
            Location::Register(RegSpec { bank: RegisterBank::EFlags, num: _ }) => {
                Location::Register(RegSpec::rflags())
            }
        }

    }
}

// TODO: how does this interact with struct/type inference?
// SymbolicExpression::Deref(
//   SymbolicExpression::Add(rdi_input, 0x8)
// ) => HANDLE ?
#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize)]
pub enum SymbolicExpression {
    // Used to declare that a value has some type, but no additional information about it
    // For example, declaring the type of a global struct. This might be written as:
    // contexts.set_data(
    //   (Segment::GS, 0),
    //   SymbolicExpression::PointerTo(SymbolicExpression::Opaque(KPCR))
    // )
    Opaque(TypeSpec),
    // TODO: smaller offset? u64 is the worst case but probably necessary.. :(
    Add(Box<SymbolicExpression>, u64),
    Deref(Box<SymbolicExpression>),
    Symbol(Symbol),
    CopOut(String)
}

impl SymbolicExpression {
    pub fn offset(self, offset: u64) -> SymbolicExpression {
        match self {
            SymbolicExpression::Opaque(ty) => SymbolicExpression::Add(Box::new(
                SymbolicExpression::Opaque(ty)), offset
            ),
            SymbolicExpression::Add(ty, curr) => SymbolicExpression::Add(ty, curr.wrapping_add(offset)),
            x @ _ => SymbolicExpression::Add(Box::new(x), offset)
        }
    }
}

impl Typed for SymbolicExpression {
    fn type_of(&self, type_atlas: &TypeAtlas) -> TypeSpec {
        match self {
            SymbolicExpression::Opaque(spec) => spec.clone(),
            SymbolicExpression::Add(expr, offset) => {
                if let Some(field) = type_atlas.get_field(&expr.type_of(type_atlas), *offset as u32) {
                    field.type_of()
                } else {
                    TypeSpec::Unknown
                }
            }
            SymbolicExpression::Deref(expr) => {
                if let TypeSpec::PointerTo(ty) = expr.type_of(type_atlas) {
                    *ty.to_owned()
                } else {
                    TypeSpec::Unknown
                }
            }
            _ => TypeSpec::Unknown
        }
    }
}

// So, for example, `mov rcx_0, gs:[0x38]`
// => mov rcx_0, KPCR.field_7
// rcx_0.set_value(SymbolicExpression::Deref(
//   SymbolicExpression::Add(
// and then `mov rdx_1, [rcx_0 + 0x48]`
// => mov rdx_1, [KPCR.field_7 + 0x48]
// => mov rdx_1, KPCR.field_7.field_9

#[derive(Debug, Clone)]
pub enum Data {
    Concrete(u64, Option<TypeSpec>),
    Str(String),
    Expression(SymbolicExpression),
    Alias(Rc<RefCell<Value<x86_64>>>)
}

impl Typed for Data {
    fn type_of(&self, type_atlas: &TypeAtlas) -> TypeSpec {
        match self {
            Data::Concrete(_, ref t) => t.to_owned().unwrap_or(TypeSpec::Bottom),
            Data::Str(_) => TypeSpec::Bottom,
            Data::Expression(expr) => expr.type_of(type_atlas),
            Data::Alias(ptr) => ptr.borrow().data.to_owned().map(|d| d.type_of(type_atlas)).unwrap_or(TypeSpec::Unknown)
        }
    }
}

// TODO: update memo
#[derive(Debug, Serialize)]
pub enum DataMemo {
    Concrete(u64, Option<TypeSpec>),
    Str(String),
    Expression(SymbolicExpression),
    Alias(u32)
}

#[derive(Debug, Serialize)]
pub struct ValueMemo {
    pub location: (u64, Location),
    pub version: Option<u32>,
    pub data: Option<DataMemo>
}

impl Memoable for HashedValue<Rc<RefCell<Value<x86_64>>>> {
    type Out = ValueMemo;

    fn memoize(&self, memos: &HashMap<Self, u32>) -> Self::Out {
        let selfref: &Value<x86_64> = &*(&*self.value.borrow());
        let newdata = selfref.data.as_ref().map(|data| match data {
            Data::Concrete(v, ty) => DataMemo::Concrete(*v, ty.to_owned()),
            Data::Str(string) => DataMemo::Str(string.to_owned()),
            Data::Expression(expr) => DataMemo::Expression(expr.to_owned()),
            Data::Alias(ptr) => DataMemo::Alias(memos[&HashedValue { value: Rc::clone(ptr) }])
        });

        ValueMemo {
            location: selfref.location,
            version: selfref.version,
            data: newdata
        }
    }
}

impl Hash for Data {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Data::Concrete(ref value, _) => {
                state.write_u8(1);
                value.hash(state);
            }
            Data::Expression(ref expr) => {
                state.write_u8(2);
                expr.hash(state);
            }
            Data::Str(ref s) => {
                state.write_u8(4);
                s.hash(state);
            }
            Data::Alias(ref value) => {
                state.write_u8(3);
                value.borrow().hash(state);
            }
        }
    }
}

impl SSAValues for x86_64 {
    type Location = Location;
    type Data = Data;

    fn decompose(instr: &Self::Instruction) -> Vec<(Option<Self::Location>, Direction)> {
        fn decompose_write(op: &Operand) -> Vec<(Option<Location>, Direction)> {
            match op {
                Operand::ImmediateI8(_) |
                Operand::ImmediateU8(_) |
                Operand::ImmediateI16(_) |
                Operand::ImmediateU16(_) |
                Operand::ImmediateI32(_) |
                Operand::ImmediateU32(_) |
                Operand::ImmediateI64(_) |
                Operand::ImmediateU64(_) => {
                    // no instruction encodes this, this should be an error
                    unreachable!()
                }
                Operand::Register(spec) => {
                    vec![(Some(Location::Register(*spec)), Direction::Write)]
                },
                Operand::DisplacementU32(_) |
                Operand::DisplacementU64(_) => {
                    // TODO: lazy
                    vec![(Some(Location::MemoryAny), Direction::Write)]
                },
                Operand::RegDeref(spec) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Write)
                    ]
                },
                Operand::RegDisp(spec, _) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Write)
                    ]
                },
                Operand::RegScale(spec, _) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Write)
                    ]
                },
                Operand::RegIndexBase(base, index) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Write)
                    ]
                },
                Operand::RegIndexBaseDisp(base, index, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Write)
                    ]
                },
                Operand::RegScaleDisp(base, _, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Write)
                    ]
                },
                Operand::RegIndexBaseScale(base, index, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Write)
                    ]
                },
                Operand::RegIndexBaseScaleDisp(base, index, _, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Write)
                    ]
                },
                Operand::Many(_) => {
                    unreachable!()
                },
                Operand::Nothing => {
                    vec![]
                }
            }
        }
        fn decompose_read(op: &Operand) -> Vec<(Option<Location>, Direction)> {
            match op {
                Operand::ImmediateI8(_) |
                Operand::ImmediateU8(_) |
                Operand::ImmediateI16(_) |
                Operand::ImmediateU16(_) |
                Operand::ImmediateI32(_) |
                Operand::ImmediateU32(_) |
                Operand::ImmediateI64(_) |
                Operand::ImmediateU64(_) => {
                    vec![]
                }
                Operand::Register(spec) => {
                    vec![(Some(Location::Register(*spec)), Direction::Read)]
                },
                Operand::DisplacementU32(_) |
                Operand::DisplacementU64(_) => {
                    // TODO: lazy
                    vec![(Some(Location::MemoryAny), Direction::Read)]
                },
                Operand::RegDeref(spec) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Read)
                    ]
                },
                Operand::RegDisp(spec, _) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Read)
                    ]
                },
                Operand::RegScale(spec, _) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Read)
                    ]
                },
                Operand::RegIndexBase(base, index) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Read)
                    ]
                },
                Operand::RegIndexBaseDisp(base, index, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Read)
                    ]
                },
                Operand::RegScaleDisp(base, _, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Read)
                    ]
                },
                Operand::RegIndexBaseScale(base, index, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Read)
                    ]
                },
                Operand::RegIndexBaseScaleDisp(base, index, _, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Read)
                    ]
                },
                Operand::Many(_) => {
                    unreachable!()
                },
                Operand::Nothing => {
                    vec![]
                }
            }
        }
        fn decompose_readwrite(op: &Operand) -> Vec<(Option<Location>, Direction)> {
            match op {
                Operand::ImmediateI8(_) |
                Operand::ImmediateU8(_) |
                Operand::ImmediateI16(_) |
                Operand::ImmediateU16(_) |
                Operand::ImmediateI32(_) |
                Operand::ImmediateU32(_) |
                Operand::ImmediateI64(_) |
                Operand::ImmediateU64(_) => {
                    // no instruction encodes this, this should be an error
                    unreachable!()
                }
                Operand::Register(spec) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::Register(*spec)), Direction::Write)
                    ]
                },
                Operand::DisplacementU32(_) |
                Operand::DisplacementU64(_) => {
                    // TODO: lazy
                    vec![
                        (Some(Location::MemoryAny), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Write),
                    ]
                },
                Operand::RegDeref(spec) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Write)
                    ]
                },
                Operand::RegDisp(spec, _) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Write)
                    ]
                },
                Operand::RegScale(spec, _) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Write)
                    ]
                },
                Operand::RegIndexBase(base, index) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Write)
                    ]
                },
                Operand::RegIndexBaseDisp(base, index, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Write)
                    ]
                },
                Operand::RegScaleDisp(base, _, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Write)
                    ]
                },
                Operand::RegIndexBaseScale(base, index, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Write)
                    ]
                },
                Operand::RegIndexBaseScaleDisp(base, index, _, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Read),
                        (Some(Location::MemoryAny), Direction::Write)
                    ]
                },
                Operand::Many(_) => {
                    unreachable!()
                },
                Operand::Nothing => {
                    vec![]
                }
            }
        }
        match instr.opcode {
            Opcode::LEA |
            Opcode::MOVZX_b |
            Opcode::MOVZX_w |
            Opcode::MOVSX |
            Opcode::MOV => {
                let mut locs = decompose_read(&instr.operands[1]);
                locs.append(&mut decompose_write(&instr.operands[0]));
                locs
            }
            Opcode::XCHG => {
                let mut locs = decompose_readwrite(&instr.operands[1]);
                locs.append(&mut decompose_readwrite(&instr.operands[0]));
                locs
            }
            Opcode::STI |
            Opcode::CLI => {
                vec![
                    (Some(Location::IF), Direction::Write)
                ]
            }
            Opcode::STD |
            Opcode::CLD => {
                vec![
                    (Some(Location::DF), Direction::Write)
                ]
            }
            Opcode::STC |
            Opcode::CLC => {
                vec![
                    (Some(Location::CF), Direction::Write)
                ]
            }
            Opcode::SAR |
            Opcode::SAL |
            Opcode::SHR |
            Opcode::SHL => {
                // TODO: inspect imm8 and CL if context permits to decide
                // flags more granularly
                let mut locs = decompose_readwrite(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::CF), Direction::Write));
                locs.push((Some(Location::SF), Direction::Write));
                locs.push((Some(Location::ZF), Direction::Write));
                locs.push((Some(Location::PF), Direction::Write));
                locs.push((Some(Location::AF), Direction::Write));
                locs
            }
            Opcode::RCR |
            Opcode::RCL |
            Opcode::ROR |
            Opcode::ROL => {
                // TODO: inspect imm8 and CL if context permits to decide
                // flags more granularly
                let mut locs = decompose_readwrite(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::CF), Direction::Write));
                locs.push((Some(Location::OF), Direction::Write));
                locs
            }
            Opcode::ADC |
            Opcode::SBB => {
                // TODO: this is lazy and assumes writes of all flags
                // this may not be true
                let mut locs = decompose_read(&instr.operands[1]);
                locs.append(&mut decompose_readwrite(&instr.operands[0]));
                locs.push((Some(Location::CF), Direction::Read));
                locs.push((Some(Location::CF), Direction::Write));
                locs.push((Some(Location::OF), Direction::Write));
                locs.push((Some(Location::SF), Direction::Write));
                locs.push((Some(Location::ZF), Direction::Write));
                locs.push((Some(Location::PF), Direction::Write));
                locs.push((Some(Location::AF), Direction::Write));
                locs

            },
            Opcode::ADD |
            Opcode::SUB |
            Opcode::AND |
            Opcode::XOR |
            Opcode::OR => {
                // TODO: this is lazy and assumes writes of all flags
                // this may not be true
                let mut locs = decompose_read(&instr.operands[1]);
                locs.append(&mut decompose_readwrite(&instr.operands[0]));
                locs.push((Some(Location::CF), Direction::Write));
                locs.push((Some(Location::OF), Direction::Write));
                locs.push((Some(Location::SF), Direction::Write));
                locs.push((Some(Location::ZF), Direction::Write));
                locs.push((Some(Location::PF), Direction::Write));
                locs.push((Some(Location::AF), Direction::Write));
                locs
            }

            Opcode::IMUL => {
                // TODO: this.
                vec![]
            },
            Opcode::IDIV |
            Opcode::DIV => {
                // TODO: this is lazy and assumes writes of all flags
                // this may not be true
                // TODO: this may not read *dx (if this is idiv r/m 8)
                let mut locs = decompose_read(&instr.operands[0]);
                locs.push((Some(Location::Register(RegSpec::rax())), Direction::Read));
                locs.push((Some(Location::Register(RegSpec::rax())), Direction::Write));
                locs.push((Some(Location::Register(RegSpec::rdx())), Direction::Read));
                locs.push((Some(Location::CF), Direction::Write));
                locs.push((Some(Location::OF), Direction::Write));
                locs.push((Some(Location::SF), Direction::Write));
                locs.push((Some(Location::ZF), Direction::Write));
                locs.push((Some(Location::PF), Direction::Write));
                locs.push((Some(Location::AF), Direction::Write));
                locs

            }
            Opcode::MUL => {
                // TODO: this is lazy and assumes writes of all flags
                // this may not be true
                let mut locs = decompose_read(&instr.operands[0]);
                locs.push((Some(Location::Register(RegSpec::rax())), Direction::Read));
                locs.push((Some(Location::Register(RegSpec::rax())), Direction::Write));
                locs.push((Some(Location::Register(RegSpec::rdx())), Direction::Write));
                locs.push((Some(Location::CF), Direction::Write));
                locs.push((Some(Location::OF), Direction::Write));
                locs.push((Some(Location::SF), Direction::Write));
                locs.push((Some(Location::ZF), Direction::Write));
                locs.push((Some(Location::PF), Direction::Write));
                locs.push((Some(Location::AF), Direction::Write));
                locs
            }

            Opcode::PUSH => {
                let mut locs = decompose_read(&instr.operands[0]);
                locs.push((Some(Location::Register(RegSpec::rsp())), Direction::Read));
                locs.push((Some(Location::Register(RegSpec::rsp())), Direction::Write));
                locs.push((Some(Location::MemoryAny), Direction::Write));
                locs
            },
            Opcode::POP => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::Register(RegSpec::rsp())), Direction::Read));
                locs.push((Some(Location::Register(RegSpec::rsp())), Direction::Write));
                locs.push((Some(Location::MemoryAny), Direction::Read));
                locs
            },
            Opcode::INC |
            Opcode::DEC => {
                let mut locs = decompose_readwrite(&instr.operands[0]);
                locs.push((Some(Location::OF), Direction::Write));
                locs.push((Some(Location::SF), Direction::Write));
                locs.push((Some(Location::ZF), Direction::Write));
                locs.push((Some(Location::AF), Direction::Write));
                locs.push((Some(Location::PF), Direction::Write));
                locs
            }
            Opcode::ENTER => {
                vec![
                    (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                    (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                    (Some(Location::Register(RegSpec::rbp())), Direction::Read),
                    (Some(Location::Register(RegSpec::rbp())), Direction::Write),
                    (Some(Location::MemoryAny), Direction::Write)
                ]
            }
            Opcode::LEAVE => {
                vec![
                    (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                    (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                    (Some(Location::Register(RegSpec::rbp())), Direction::Write),
                    (Some(Location::MemoryAny), Direction::Read)
                ]
            }
            Opcode::POPF => {
                vec![
                    (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                    (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                    (Some(Location::MemoryAny), Direction::Read),
                    (Some(Location::Register(RegSpec::rflags())), Direction::Write)
                ]
            }
            Opcode::PUSHF => {
                vec![
                    (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                    (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                    (Some(Location::MemoryAny), Direction::Write),
                    (Some(Location::Register(RegSpec::rflags())), Direction::Read)
                ]
            }
            Opcode::CBW |
            Opcode::CDW => {
                vec![
                    (Some(Location::Register(RegSpec::rax())), Direction::Read),
                    (Some(Location::Register(RegSpec::rax())), Direction::Write)
                ]
            }
            Opcode::LAHF => {
                vec![
                    (Some(Location::Register(RegSpec::ax())), Direction::Write),
                    (Some(Location::Register(RegSpec::rflags())), Direction::Read)
                ]
            }
            Opcode::SAHF => {
                vec![
                    (Some(Location::Register(RegSpec::ax())), Direction::Read),
                    (Some(Location::Register(RegSpec::rflags())), Direction::Write)
                ]
            }
            Opcode::TEST |
            Opcode::CMP => {
                /* not strictly correct for either */
                let mut locs = decompose_read(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::Register(RegSpec::rflags())), Direction::Write));
                locs
            }
            Opcode::NEG => {
                let mut locs = decompose_readwrite(&instr.operands[0]);
                locs.push((Some(Location::CF), Direction::Write));
                locs
            }
            Opcode::NOT => {
                decompose_readwrite(&instr.operands[0])
            }
            Opcode::CMPXCHG => {
                let mut locs = match &instr.operands[1] {
                    Operand::Register(RegSpec { bank: RegisterBank::Q, num }) => {
                        vec![
                            (Some(Location::Register(RegSpec { bank: RegisterBank::Q, num: *num })), Direction::Read),
                            (Some(Location::Register(RegSpec::rax())), Direction::Read)
                        ]
                    },
                    Operand::Register(RegSpec { bank: RegisterBank::D, num }) => {
                        vec![
                            (Some(Location::Register(RegSpec { bank: RegisterBank::D, num: *num })), Direction::Read),
                            (Some(Location::Register(RegSpec::eax())), Direction::Read)
                        ]
                    },
                    Operand::Register(RegSpec { bank: RegisterBank::W, num }) => {
                        vec![
                            (Some(Location::Register(RegSpec { bank: RegisterBank::W, num: *num })), Direction::Read),
                            (Some(Location::Register(RegSpec::ax())), Direction::Read)
                        ]
                    },
                    Operand::Register(RegSpec { bank: RegisterBank::B, num }) => {
                        vec![
                            (Some(Location::Register(RegSpec { bank: RegisterBank::B, num: *num })), Direction::Read),
                            (Some(Location::Register(RegSpec::al())), Direction::Read)
                        ]
                    },
                    Operand::Register(RegSpec { bank: RegisterBank::rB, num }) => {
                        vec![
                            (Some(Location::Register(RegSpec { bank: RegisterBank::B, num: *num })), Direction::Read),
                            (Some(Location::Register(RegSpec::al())), Direction::Read)
                        ]
                    },
                    _ => { unreachable!() }
                };
                locs.push((Some(Location::ZF), Direction::Write));
                locs.append(&mut decompose_readwrite(&instr.operands[0]));
                locs
            },
            Opcode::CALLF | // TODO: this is wrong
            Opcode::CALL => {
                let mut locs = decompose_read(&instr.operands[0]);
                locs.push((Some(Location::Register(RegSpec::rsp())), Direction::Read));
                locs.push((Some(Location::Register(RegSpec::rsp())), Direction::Write));
                locs.push((Some(Location::MemoryAny), Direction::Write));
                locs
            }
            Opcode::JMP => {
                decompose_read(&instr.operands[0])
            },
            Opcode::JMPF => { // TODO: this is wrong.
                vec![]
            },
            Opcode::IRET | // TODO: this is wrong
            Opcode::RETF | // TODO: this is wrong
            Opcode::RETURN => {
                vec![
                    (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                    (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                    (Some(Location::MemoryAny), Direction::Read)
                ]
            }
            Opcode::LFENCE |
            Opcode::MFENCE |
            Opcode::SFENCE |
            Opcode::NOP |
            Opcode::WAIT => { vec![] }
            Opcode::CLFLUSH |
            Opcode::FXSAVE |
            Opcode::FXRSTOR |
            Opcode::XSAVE |
            Opcode::XSTOR |
            Opcode::XSAVEOPT |
            Opcode::STMXCSR |
            Opcode::SGDT |
            Opcode::SIDT => {
                decompose_write(&instr.operands[0])
            }
            Opcode::LDMXCSR |
            Opcode::LGDT |
            Opcode::LIDT => {
                decompose_read(&instr.operands[0])
            }
            // TODO: this is wrong
            Opcode::SMSW => {
                decompose_write(&instr.operands[0])
            }
            // TODO: this is wrong
            Opcode::LMSW => {
                decompose_read(&instr.operands[0])
            }
            Opcode::SWAPGS => {
                vec![
                    (Some(Location::Register(RegSpec::gs())), Direction::Read),
                    (Some(Location::Register(RegSpec::gs())), Direction::Write)
                ]
            }
            Opcode::RDTSCP => {
                vec![
                    (Some(Location::Register(RegSpec::rax())), Direction::Write),
                    (Some(Location::Register(RegSpec::rcx())), Direction::Write),
                    (Some(Location::Register(RegSpec::rdx())), Direction::Write)
                ]
            }
            Opcode::WRMSR => {
                vec![
                    (Some(Location::Register(RegSpec::rax())), Direction::Read),
                    (Some(Location::Register(RegSpec::rdx())), Direction::Read)
                ]
            }
            Opcode::RDMSR => {
                vec![
                    (Some(Location::Register(RegSpec::rax())), Direction::Write),
                    (Some(Location::Register(RegSpec::rdx())), Direction::Write),
                    (Some(Location::Register(RegSpec::rcx())), Direction::Read)
                ]
            }
            Opcode::RDTSC => {
                vec![
                    (Some(Location::Register(RegSpec::rax())), Direction::Write),
                    (Some(Location::Register(RegSpec::rdx())), Direction::Write)
                ]
            }
            Opcode::RDPMC => {
                vec![
                    (Some(Location::Register(RegSpec::rax())), Direction::Write),
                    (Some(Location::Register(RegSpec::rdx())), Direction::Write),
                    (Some(Location::Register(RegSpec::rcx())), Direction::Read)
                ]
            }
            Opcode::VERR |
            Opcode::VERW => {
                let mut locs = decompose_read(&instr.operands[0]);
                locs.push((Some(Location::ZF), Direction::Write));
                locs
            }
            Opcode::SLDT |
            Opcode::STR |
            Opcode::INVLPG => {
                decompose_write(&instr.operands[0])
            }
            Opcode::LLDT |
            Opcode::LTR => {
                decompose_read(&instr.operands[0])
            }
            // these are immediate-only or have no operands
            Opcode::JMPE |
            Opcode::UD2 |
            Opcode::INT |
            Opcode::INTO |
            Opcode::HLT |
            Opcode::Invalid => {
                vec![]
            },
            Opcode::JO => {
                vec![(Some(Location::OF), Direction::Read)]
            }
            Opcode::JNO => {
                vec![(Some(Location::OF), Direction::Read)]
            }
            Opcode::JB => {
                vec![(Some(Location::CF), Direction::Read)]
            }
            Opcode::JNB => {
                vec![(Some(Location::CF), Direction::Read)]
            }
            Opcode::JZ => {
                vec![(Some(Location::ZF), Direction::Read)]
            }
            Opcode::JNZ => {
                vec![(Some(Location::ZF), Direction::Read)]
            }
            Opcode::JA => {
                vec![
                    (Some(Location::CF), Direction::Read),
                    (Some(Location::ZF), Direction::Read)
                ]
            }
            Opcode::JNA => {
                vec![
                    (Some(Location::CF), Direction::Read),
                    (Some(Location::ZF), Direction::Read)
                ]
            }
            Opcode::JS => {
                vec![(Some(Location::SF), Direction::Read)]
            }
            Opcode::JNS => {
                vec![(Some(Location::SF), Direction::Read)]
            }
            Opcode::JP => {
                vec![(Some(Location::PF), Direction::Read)]
            }
            Opcode::JNP => {
                vec![(Some(Location::PF), Direction::Read)]
            }
            Opcode::JL => {
                vec![
                    (Some(Location::SF), Direction::Read),
                    (Some(Location::OF), Direction::Read)
                ]
            }
            Opcode::JGE => {
                vec![
                    (Some(Location::SF), Direction::Read),
                    (Some(Location::OF), Direction::Read)
                ]
            }
            Opcode::JLE => {
                vec![
                    (Some(Location::ZF), Direction::Read),
                    (Some(Location::SF), Direction::Read),
                    (Some(Location::OF), Direction::Read)
                ]
            }
            Opcode::JG => {
                vec![
                    (Some(Location::ZF), Direction::Read),
                    (Some(Location::SF), Direction::Read),
                    (Some(Location::OF), Direction::Read)
                ]
            }

            Opcode::CMOVA => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::CF), Direction::Read));
                locs.push((Some(Location::ZF), Direction::Read));
                locs
            }
            Opcode::CMOVB => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::CF), Direction::Read));
                locs
            }
            Opcode::CMOVG => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::ZF), Direction::Read));
                locs.push((Some(Location::SF), Direction::Read));
                locs.push((Some(Location::OF), Direction::Read));
                locs
            }
            Opcode::CMOVGE => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::SF), Direction::Read));
                locs.push((Some(Location::OF), Direction::Read));
                locs
            }
            Opcode::CMOVL => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::SF), Direction::Read));
                locs.push((Some(Location::OF), Direction::Read));
                locs
            }
            Opcode::CMOVLE => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::ZF), Direction::Read));
                locs.push((Some(Location::SF), Direction::Read));
                locs.push((Some(Location::OF), Direction::Read));
                locs
            }
            Opcode::CMOVNA => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::CF), Direction::Read));
                locs.push((Some(Location::ZF), Direction::Read));
                locs
            }
            Opcode::CMOVNB => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::CF), Direction::Read));
                locs
            }
            Opcode::CMOVNO => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::OF), Direction::Read));
                locs
            }
            Opcode::CMOVNP => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::PF), Direction::Read));
                locs
            }
            Opcode::CMOVNS => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::SF), Direction::Read));
                locs
            }
            Opcode::CMOVNZ => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::ZF), Direction::Read));
                locs
            }
            Opcode::CMOVO => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::OF), Direction::Read));
                locs
            }
            Opcode::CMOVP => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::PF), Direction::Read));
                locs
            }
            Opcode::CMOVS => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::SF), Direction::Read));
                locs
            }
            Opcode::CMOVZ => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::ZF), Direction::Read));
                locs
            }
            Opcode::SETO => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::OF), Direction::Read));
                locs
            }
            Opcode::SETNO => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::OF), Direction::Read));
                locs
            }
            Opcode::SETB => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::CF), Direction::Read));
                locs
            }
            Opcode::SETAE => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::CF), Direction::Read));
                locs
            }
            Opcode::SETZ => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::ZF), Direction::Read));
                locs
            }
            Opcode::SETNZ => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::ZF), Direction::Read));
                locs
            }
            Opcode::SETBE => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::CF), Direction::Read));
                locs.push((Some(Location::ZF), Direction::Read));
                locs
            }
            Opcode::SETA => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::CF), Direction::Read));
                locs.push((Some(Location::ZF), Direction::Read));
                locs
            }
            Opcode::SETS => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::SF), Direction::Read));
                locs
            }
            Opcode::SETNS => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::SF), Direction::Read));
                locs
            }
            Opcode::SETP => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::PF), Direction::Read));
                locs
            }
            Opcode::SETNP => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::PF), Direction::Read));
                locs
            }
            Opcode::SETL => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::SF), Direction::Read));
                locs.push((Some(Location::OF), Direction::Read));
                locs
            }
            Opcode::SETGE => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::SF), Direction::Read));
                locs.push((Some(Location::OF), Direction::Read));
                locs
            }
            Opcode::SETLE => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::SF), Direction::Read));
                locs.push((Some(Location::OF), Direction::Read));
                locs.push((Some(Location::ZF), Direction::Read));
                locs
            }
            Opcode::SETG => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::SF), Direction::Read));
                locs.push((Some(Location::OF), Direction::Read));
                locs.push((Some(Location::ZF), Direction::Read));
                locs
            }
            Opcode::LSL => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::ZF), Direction::Write));
                locs
            }
            Opcode::LAR => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::ZF), Direction::Read));
                locs
            }
            Opcode::CPUID |
            Opcode::WBINVD |
            Opcode::INVD |
            Opcode::SYSRET |
            Opcode::CLTS |
            Opcode::SYSCALL |
            Opcode::CMPS |
            Opcode::SCAS |
            Opcode::MOVS |
            Opcode::LODS |
            Opcode::STOS |
            Opcode::INS |
            Opcode::OUTS => {
                /* TODO: these. */
                vec![]
            }
        }
    }
}
