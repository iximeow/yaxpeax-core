#![allow(dead_code)]
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
use analyses::static_single_assignment::{DFGRef, SSAValues, Value};
use data::types::{Typed, TypeSpec, TypeAtlas};
use yaxpeax_x86::{ConditionCode, RegSpec, RegisterBank, x86_64, Opcode, Operand};

use std::rc::Rc;
use std::fmt;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};

use std::collections::HashMap;
use analyses::static_single_assignment::HashedValue;
use serialize::Memoable;

use data::{Direction, Disambiguator, ValueLocations};

pub const FLAGS: [Location; 10] = [
    Location::CF,
    Location::PF,
    Location::AF,
    Location::ZF,
    Location::SF,
    Location::TF,
    Location::IF,
    Location::DF,
    Location::OF,
    Location::IOPL
];

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct MemoryRegion(u16);

const ANY: MemoryRegion = MemoryRegion(0);
const STACK: MemoryRegion = MemoryRegion(1);
const PROGRAM: MemoryRegion = MemoryRegion(2);
const STATIC: MemoryRegion = MemoryRegion(3);

impl fmt::Display for MemoryRegion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            0 => write!(f, "any"),
            1 => write!(f, "stack"),
            2 => write!(f, "program"),
            other => write!(f, "MemoryRegion({})", other),
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub enum Location {
    Register(RegSpec),
    Memory(MemoryRegion),
    MemoryLocation(MemoryRegion, u16, i32),
    // not modeling eflags' system bits ... yet?
    CF, PF, AF, ZF, SF, TF, IF, DF, OF, IOPL
}

use data::AliasInfo;
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
            Location::Register(RegSpec { bank: RegisterBank::CR, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::DR, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::S, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::RIP, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::RFlags, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::ST, num: _ }) |
            Location::Register(RegSpec { bank: RegisterBank::Z, num: _ }) => {
                vec![]
            }
            Location::Register(RegSpec { bank: RegisterBank::Q, num }) => {
                vec![
                    Location::Register(RegSpec { bank: RegisterBank::D, num: *num }),
                    Location::Register(RegSpec { bank: RegisterBank::W, num: *num }),
                    Location::Register(RegSpec { bank: RegisterBank::B, num: *num })
                ]
            },
            Location::Register(RegSpec { bank: RegisterBank::D, num }) => {
                vec![
                    Location::Register(RegSpec { bank: RegisterBank::Q, num: *num }),
                    Location::Register(RegSpec { bank: RegisterBank::W, num: *num }),
                    Location::Register(RegSpec { bank: RegisterBank::B, num: *num })
                ]
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
            Location::Memory(_) => {
                vec![]
            }
            // TODO:
            // this is not precise! it may be the case that for some `i`, `j`, `k`, and `l`,
            // MemoryLocation(R, i, j) and MemoryLocation(R, k, l) overlap!
            // this would be the case when a location is written by one size/offset and written
            // by some other pair, like `dword [rsp + 4]` vs `word [rsp + 6]`, where +6 and +7
            // overlap. it is (incorrectly!) assumed this will not happen in code we're
            // considering. in the future, this will be likely mitigated by pessimistic layout
            // choices for memory locations we know overlap.
            Location::MemoryLocation(region, _, _) => {
                vec![Location::Memory(*region)]
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
            Location::Memory(r) => Location::Memory(*r),
            Location::MemoryLocation(r, _, _) => Location::Memory(*r),
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

#[test]
fn test_expr_fields() {
    use data::types::KPCR;
    let type_atlas = &TypeAtlas::new();
    let expr = SymbolicExpression::Opaque(TypeSpec::PointerTo(Box::new(TypeSpec::PointerTo(Box::new(TypeSpec::LayoutId(KPCR))))));
    println!("expr: {}", expr.show(type_atlas));
    println!("  ty: {:?}", expr.type_of(type_atlas));
    let access = SymbolicExpression::Deref(Box::new(expr.clone()));
    println!("access: {}", access.show(type_atlas));
    println!("  ty: {:?}", access.show(type_atlas));
    let field_ptr = access.offset(0x8);
    println!("field_ptr: {}", field_ptr.show(type_atlas));
    println!("field_ptr: {:?}", field_ptr);
    println!("  ty: {:?}", field_ptr.type_of(type_atlas));
    let field = SymbolicExpression::Deref(Box::new(field_ptr));
    println!("field: {}", field.show(type_atlas));
    println!("  ty: {:?}", field.type_of(type_atlas));
    let field_ptr_2 = field.offset(0x4);
    println!("field_ptr_2: {}", field_ptr_2.show(type_atlas));
    println!("  ty: {:?}", field_ptr_2.type_of(type_atlas));
    let field_2 = SymbolicExpression::Deref(Box::new(field_ptr_2));
    println!("field_2: {}", field_2.show(type_atlas));
    println!("  ty: {:?}", field_2.type_of(type_atlas));
    assert!(false);
}

impl SymbolicExpression {
    pub fn show(&self, type_atlas: &TypeAtlas) -> String {
        // inner expression first, if applicable..
        match self {
            SymbolicExpression::Opaque(spec) => {
                format!("<{}>", type_atlas.name_of(spec))
            },
            SymbolicExpression::Add(expr, offset) => {
                // inner part first:
                let inner_ty = expr.type_of(type_atlas);
                let inner_expr = expr.show(type_atlas);
                let (_this_ty, this_name) = if let Some(this_field) = type_atlas.get_field(&inner_ty, *offset as u32) {
                    (this_field.ty.as_ref(), this_field.name.as_ref())
                } else {
                    (None, None)
                };

                if let Some(field_name) = this_name {
                    format!("{}.{}", inner_expr, field_name)
                } else {
                    format!("({} + {:#x})", inner_expr, offset)
                }
            }
            SymbolicExpression::Deref(expr) => {
                let _inner_ty = expr.type_of(type_atlas);
                let inner_expr = expr.show(type_atlas);

                format!("*{}", inner_expr)
            },
            _ => { panic!("aaaadsfasfa"); }
        }
    }

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
                    TypeSpec::PointerTo(Box::new(field.type_of()))
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

#[derive(Debug, Clone, Hash, PartialEq)]
pub enum ValueRange {
    Between(Data, Data),
    Precisely(Data),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Data {
    Concrete(u64, Option<TypeSpec>),
    Str(String),
    Expression(SymbolicExpression),
    Alias(Rc<RefCell<Value<x86_64>>>),
    // This is a disjunction of "ranges" that may be a range of a single element
    // eg this is a set of ranges/specific values
    //
    // It is a logical error for this to be ValueSet(vec![Precisely(_)]).
    ValueSet(Vec<ValueRange>),
    // This value has contradictory bounds, or is otherwise unknowable. This must be distinct
    // from "not yet inspected" to avoid situations where analysis starts with an unknown, infers
    // some value, disproves the value, resets to unknown, and loops.
    // Not yet added because analyses are not applied more than once, so we know on first check
    // that the value has not been considered yet.
    // Indeterminate,
}

impl Data {
    pub fn add(left: &Data, right: &Data) -> Option<Data> {
        match (left, right) {
            (Data::ValueSet(values), Data::Concrete(right, _)) => {
                let mut new_values: Vec<ValueRange> = Vec::new();
                for value in values {
                    match value {
                        ValueRange::Precisely(Data::Concrete(v, _)) => {
                            new_values.push(ValueRange::Precisely(Data::Concrete(v.wrapping_add(*right), None)));
                        },
                        ValueRange::Between(Data::Concrete(low, _), Data::Concrete(high, _)) => {
                            if let (Some(new_low), Some(new_high)) = (low.checked_add(*right), high.checked_add(*right)) {
                                new_values.push(ValueRange::Between(Data::Concrete(new_low, None), Data::Concrete(new_high, None)));
                            } else {
                                // TODO: handle overflow
                                return None;
                            }
                        }
                        _ => {
                            println!("{:?} + {:?}", left, right);
                            panic!("aaa");
                        }
                    }
                }
                Some(Data::ValueSet(new_values))
            },
            _ => {
                use arch::x86_64::display::DataDisplay;
                println!("Adding {} and {}", DataDisplay { data: left, colors: None }, DataDisplay { data: right, colors: None });
                return None;
                // panic!("add!");
            }
        }
    }

    pub fn mul(left: &Data, right: &Data) -> Option<Data> {
        // panic!("mul!");
        match (left, right) {
            (Data::ValueSet(values), Data::Concrete(m, _)) => {
                let mut out_values: Vec<ValueRange> = Vec::new();
                for value in values {
                    match value {
                        ValueRange::Between(Data::Concrete(low, _), Data::Concrete(high, _)) => {
                            // TODO: check that low < high
                            for i in *low..=*high {
                                out_values.push(
                                    ValueRange::Precisely(
                                        Data::Concrete(i.wrapping_mul(*m), None)
                                    )
                                );
                            }
                        },
                        ValueRange::Between(_, _) => {
                            // TODO
                            // nothing
                        }
                        ValueRange::Precisely(Data::Concrete(v, _)) => {
                            out_values.push(
                                ValueRange::Precisely(
                                    Data::Concrete(v.wrapping_mul(*m), None)
                                )
                            );
                        }
                        ValueRange::Precisely(_) => {
                            // TODO
                            // nothing
                        }
                    }
                }
                if out_values.len() == 0 {
                    return None;
                } else if out_values.len() == 1 {
                    match &out_values[0] {
                        ValueRange::Precisely(v) => {
                            return Some(v.clone());
                        },
                        _ => { }
                    };
                }
                return Some(Data::ValueSet(out_values));
            },
            _ => { None }
        }
    }
}

impl Typed for Data {
    fn type_of(&self, type_atlas: &TypeAtlas) -> TypeSpec {
        match self {
            Data::Concrete(_, ref t) => t.to_owned().unwrap_or(TypeSpec::Bottom),
            Data::Str(_) => TypeSpec::Bottom,
            Data::Expression(expr) => expr.type_of(type_atlas),
            Data::Alias(ptr) => ptr.borrow().data.to_owned().map(|d| d.type_of(type_atlas)).unwrap_or(TypeSpec::Unknown),
            // TODO: technically not valid - we could see if there's a shared type among
            // all the elements of the value set.
            Data::ValueSet(_) => TypeSpec::Bottom,
        }
    }
}

// TODO: update memo
#[derive(Debug, Serialize)]
pub enum DataMemo {
    Concrete(u64, Option<TypeSpec>),
    Str(String),
    Expression(SymbolicExpression),
    Alias(u32),
    ValueSet(Vec<ValueRangeMemo>)
}

#[derive(Debug, Serialize)]
pub enum ValueRangeMemo {
    Between(DataMemo, DataMemo),
    Precisely(DataMemo),
}

#[derive(Debug, Serialize)]
pub struct ValueMemo {
    pub location: Location,
    pub version: Option<u32>,
    pub data: Option<DataMemo>
}

impl Memoable for HashedValue<Rc<RefCell<Value<x86_64>>>> {
    type Out = ValueMemo;

    fn memoize(&self, memos: &HashMap<Self, u32>) -> Self::Out {
        fn memoize_data(data: &Data, memos: &HashMap<HashedValue<DFGRef<x86_64>>, u32>) -> DataMemo {
            match data {
                Data::Concrete(v, ty) => DataMemo::Concrete(*v, ty.to_owned()),
                Data::Str(string) => DataMemo::Str(string.to_owned()),
                Data::Expression(expr) => DataMemo::Expression(expr.to_owned()),
                Data::Alias(ptr) => DataMemo::Alias(memos[&HashedValue { value: Rc::clone(ptr) }]),
                Data::ValueSet(values) => {
                    let mut memoized_values: Vec<ValueRangeMemo> = vec![];
                    for value in values {
                        let memoized_value = match value {
                            ValueRange::Between(start, end) => {
                                ValueRangeMemo::Between(memoize_data(start, memos), memoize_data(end, memos))
                            }
                            ValueRange::Precisely(v) => {
                                ValueRangeMemo::Precisely(memoize_data(v, memos))
                            }
                        };
                        memoized_values.push(memoized_value)
                    }
                    DataMemo::ValueSet(memoized_values)
                }
            }
        }

        let selfref: &Value<x86_64> = &*(&*self.value.borrow());
        let newdata = selfref.data.as_ref().map(|data| memoize_data(data, memos));

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
            Data::ValueSet(ref values) => {
                state.write_u8(4);
                for v in values {
                    v.hash(state);
                }
            }
        }
    }
}

impl SSAValues for x86_64 {
    type Data = Data;
}

fn cond_to_flags(cond: ConditionCode) -> &'static [(Option<Location>, Direction)] {
    match cond {
        ConditionCode::O => {
            &[(Some(Location::OF), Direction::Read)]
        }
        ConditionCode::NO => {
            &[(Some(Location::OF), Direction::Read)]
        }
        ConditionCode::B => {
            &[(Some(Location::CF), Direction::Read)]
        }
        ConditionCode::NB => {
            &[(Some(Location::CF), Direction::Read)]
        }
        ConditionCode::Z => {
            &[(Some(Location::ZF), Direction::Read)]
        }
        ConditionCode::NZ => {
            &[(Some(Location::ZF), Direction::Read)]
        }
        ConditionCode::A => {
            &[
                (Some(Location::CF), Direction::Read),
                (Some(Location::ZF), Direction::Read)
            ]
        }
        ConditionCode::NA => {
            &[
                (Some(Location::CF), Direction::Read),
                (Some(Location::ZF), Direction::Read)
            ]
        }
        ConditionCode::S => {
            &[(Some(Location::SF), Direction::Read)]
        }
        ConditionCode::NS => {
            &[(Some(Location::SF), Direction::Read)]
        }
        ConditionCode::P => {
            &[(Some(Location::PF), Direction::Read)]
        }
        ConditionCode::NP => {
            &[(Some(Location::PF), Direction::Read)]
        }
        ConditionCode::L => {
            &[
                (Some(Location::SF), Direction::Read),
                (Some(Location::OF), Direction::Read)
            ]
        }
        ConditionCode::GE => {
            &[
                (Some(Location::SF), Direction::Read),
                (Some(Location::OF), Direction::Read)
            ]
        }
        ConditionCode::LE => {
            &[
                (Some(Location::ZF), Direction::Read),
                (Some(Location::SF), Direction::Read),
                (Some(Location::OF), Direction::Read)
            ]
        }
        ConditionCode::G => {
            &[
                (Some(Location::ZF), Direction::Read),
                (Some(Location::SF), Direction::Read),
                (Some(Location::OF), Direction::Read)
            ]
        }
    }
}

#[derive(PartialEq, Eq, Copy, Clone)]
enum Use {
    Read, Write, ReadWrite
}

impl Use {
    pub fn first_use(&self) -> Direction {
        match self {
            Use::Read | Use::ReadWrite => {
                Direction::Read
            },
            Use::Write => {
                Direction::Write
            }
        }
    }
}

#[derive(Default)]
pub struct NoDisambiguation { }
impl Disambiguator<Location, (u8, u8)> for NoDisambiguation {
    fn disambiguate(&mut self, _spec: (u8, u8)) -> Option<Location> {
        None
    }
}

pub struct ContextualDisambiguation<'a> {
    dfg: &'a crate::analyses::static_single_assignment::SSA<x86_64>,
}

impl <'a> Disambiguator<Location, (u8, u8)> for ContextualDisambiguation<'a> {
    fn disambiguate(&mut self, _spec: (u8, u8)) -> Option<Location> {
        // figure out if the location is sp-relative or rip-relative
        // .. or, has a relocation?
        None
    }
}

pub struct LocationIter<'a, 'b, D: Disambiguator<Location, (u8, u8)> + ?Sized> {
    inst: &'a yaxpeax_x86::Instruction,
    op_count: u8,
    op_idx: u8,
    loc_count: u8,
    loc_idx: u8,
    curr_op: Option<&'a yaxpeax_x86::Operand>,
    curr_use: Option<Use>,
    disambiguator: &'b mut D,
}

fn operands_in(op: yaxpeax_x86::Opcode) -> u8 {
    match op {
        Opcode::SQRTSD |
        Opcode::SQRTSS |
        Opcode::MOVDDUP |
        Opcode::MOVSLDUP |
        Opcode::MOVSD |
        Opcode::MOVSS |
        Opcode::CVTSI2SS |
        Opcode::CVTTSS2SI |
        Opcode::CVTSS2SI |
        Opcode::CVTSS2SD |
        Opcode::CVTSI2SD |
        Opcode::CVTTSD2SI |
        Opcode::CVTSD2SI |
        Opcode::CVTSD2SS |
        Opcode::LDDQU |
        Opcode::LEA |
        Opcode::MOVSX_b |
        Opcode::MOVSX_w |
        Opcode::MOVZX_b |
        Opcode::MOVZX_w |
        Opcode::MOVSX |
        Opcode::MOVSXD |
        Opcode::MOV => {
            3
        }
        Opcode::XADD |
        Opcode::XCHG => {
            3
        }
        Opcode::STI |
        Opcode::CLI |
        Opcode::STD |
        Opcode::CLD |
        Opcode::STC |
        Opcode::CLC => {
            1
        }
        Opcode::BT |
        Opcode::BTS |
        Opcode::BTR |
        Opcode::BTC |
        Opcode::BSR |
        Opcode::BSF |
        Opcode::SAR |
        Opcode::SAL |
        Opcode::SHR |
        Opcode::SHL |
        Opcode::RCR |
        Opcode::RCL |
        Opcode::ROR |
        Opcode::ROL |
        Opcode::ADC |
        Opcode::SBB |
        Opcode::ADD |
        Opcode::SUB |
        Opcode::AND |
        Opcode::XOR |
        Opcode::OR => {
            3
        }

        Opcode::IMUL => {
            // TODO: this.
            //if let Operand::Nothing = instr.operands[1] {
                2
            //} else {
            //    3
            //}
        },
        Opcode::IDIV |
        Opcode::DIV => {
            // TODO: this is lazy and assumes writes of all flags
            // this may not be true
            // TODO: this may not read *dx (if this is idiv r/m 8)
            //if let Operand::Nothing = instr.operands[1] {
                2
            //} else {
            //    3
            //}
        }
        Opcode::MUL => {
            2
        }

        Opcode::PUSH |
        Opcode::POP => {
            2
        },
        Opcode::INC |
        Opcode::DEC => {
            2
        }
        Opcode::ENTER |
        Opcode::LEAVE |
        Opcode::POPF |
        Opcode::PUSHF |
        Opcode::CBW |
        Opcode::CDW |
        Opcode::LAHF |
        Opcode::SAHF => {
            1
        }
        Opcode::TEST |
        Opcode::CMP => {
            3
        }
        Opcode::NEG |
        Opcode::NOT => {
            2
        }
        Opcode::CMPXCHG => {
            3
        },
        Opcode::CALLF | // TODO: this is wrong
        Opcode::CALL => {
            2
        }
        Opcode::JMP => {
            2
        },
        Opcode::JMPF => { // TODO: this is wrong.
            0
        },
        Opcode::IRET | // TODO: this is wrong
        Opcode::RETF | // TODO: this is wrong
        Opcode::RETURN => {
            1
        }
        Opcode::LFENCE |
        Opcode::MFENCE |
        Opcode::SFENCE |
        Opcode::NOP |
        Opcode::WAIT => { 0 }
        Opcode::CLFLUSH |
        Opcode::FXSAVE |
        Opcode::FXRSTOR |
        Opcode::XSAVE |
        Opcode::XSTOR |
        Opcode::XSAVEOPT |
        Opcode::STMXCSR |
        Opcode::SGDT |
        Opcode::SIDT => {
            2
        }
        Opcode::LDMXCSR |
        Opcode::LGDT |
        Opcode::LIDT => {
            2
        }
        // TODO: this is wrong
        Opcode::SMSW => {
            2
        }
        // TODO: this is wrong
        Opcode::LMSW => {
            2
        }
        Opcode::SWAPGS |
        Opcode::RDTSCP |
        Opcode::WRMSR |
        Opcode::RDMSR |
        Opcode::RDTSC |
        Opcode::RDPMC => {
            1
        }
        Opcode::VERR |
        Opcode::VERW => {
            2
        }
        Opcode::SLDT |
        Opcode::STR |
        Opcode::INVLPG => {
            2
        }
        Opcode::LLDT |
        Opcode::LTR => {
            2
        }
        // these are immediate-only or have no operands
        Opcode::JMPE |
        Opcode::UD2 |
        Opcode::INT |
        Opcode::INTO |
        Opcode::HLT |
        Opcode::Invalid => {
            0
        },
        Opcode::Jcc(_cond) => {
            2
        }

        Opcode::MOVcc(_cond) => {
            3
        }
        Opcode::SETcc(_cond) => {
            3
        }
        Opcode::LSL => {
            3
        }
        Opcode::LAR => {
            3
        }
        Opcode::ADDSD |
        Opcode::SUBSD |
        Opcode::MULSD |
        Opcode::DIVSD |
        Opcode::MINSD |
        Opcode::MAXSD |
        Opcode::ADDSS |
        Opcode::SUBSS |
        Opcode::MULSS |
        Opcode::DIVSS |
        Opcode::MINSS |
        Opcode::MAXSS |
        Opcode::HADDPS |
        Opcode::HSUBPS |
        Opcode::ADDSUBPS => {
            3
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
            0
            /* TODO: these. */
            // vec![]
        }

    }
}

fn locations_in(op: &yaxpeax_x86::Operand, usage: Use) -> u8 {
    (if usage == Use::ReadWrite { 1 } else { 0 }) + match op {
        Operand::Register(_spec) => {
            // reg
            1
        },
        Operand::DisplacementU32(_) |
        Operand::DisplacementU64(_) => {
            // mem
            1
        },
        Operand::RegDeref(_spec) |
        Operand::RegDisp(_spec, _) |
        Operand::RegScale(_spec, _) |
        Operand::RegScaleDisp(_spec, _, _) => {
            // reg, mem
            2
        },
        Operand::RegIndexBase(_base, _index) |
        Operand::RegIndexBaseDisp(_base, _index, _) |
        Operand::RegIndexBaseScale(_base, _index, _) |
        Operand::RegIndexBaseScaleDisp(_base, _index, _, _) => {
            // reg, reg, mem
            3
        },
        Operand::Many(_) => {
            unreachable!()
        },
        _ => {
            0
        }
    }
}

impl <'a, 'b, D: Disambiguator<Location, (u8, u8)> + ?Sized> LocationIter<'a, 'b, D> {
    pub fn new(inst: &'a yaxpeax_x86::Instruction, disambiguator: &'b mut D) -> Self {
        LocationIter {
            inst,
            op_count: operands_in(inst.opcode),
            op_idx: 0,
            loc_count: implicit_locs(inst.opcode),
            loc_idx: 0,
            curr_op: None,
            curr_use: None,
            disambiguator,
        }
    }
    fn curr_loc(&self) -> (u8, u8) {
        (self.op_idx, self.loc_idx - 1)
    }
}

fn use_of(op: yaxpeax_x86::Opcode, idx: u8) -> Use {
    match op {
        Opcode::SQRTSD |
        Opcode::SQRTSS |
        Opcode::MOVDDUP |
        Opcode::MOVSLDUP |
        Opcode::MOVSD |
        Opcode::MOVSS |
        Opcode::CVTSI2SS |
        Opcode::CVTTSS2SI |
        Opcode::CVTSS2SI |
        Opcode::CVTSS2SD |
        Opcode::CVTSI2SD |
        Opcode::CVTTSD2SI |
        Opcode::CVTSD2SI |
        Opcode::CVTSD2SS |
        Opcode::LDDQU |
        Opcode::LEA |
        Opcode::MOVSX_b |
        Opcode::MOVSX_w |
        Opcode::MOVZX_b |
        Opcode::MOVZX_w |
        Opcode::MOVSX |
        Opcode::MOVSXD |
        Opcode::MOV => {
            [Use::Write, Use::Read][idx as usize]
        }
        Opcode::XADD |
        Opcode::XCHG => {
            [Use::ReadWrite, Use::ReadWrite][idx as usize]
        }
        Opcode::STI |
        Opcode::CLI |
        Opcode::STD |
        Opcode::CLD |
        Opcode::STC |
        Opcode::CLC => {
            Use::Read
        }
        Opcode::BT |
        Opcode::BTS |
        Opcode::BTR |
        Opcode::BTC |
        Opcode::BSR |
        Opcode::BSF => {
            Use::Read
        }
        Opcode::SAR |
        Opcode::SAL |
        Opcode::SHR |
        Opcode::SHL |
        Opcode::RCR |
        Opcode::RCL |
        Opcode::ROR |
        Opcode::ROL |
        Opcode::ADC |
        Opcode::SBB |
        Opcode::ADD |
        Opcode::SUB |
        Opcode::AND |
        Opcode::XOR |
        Opcode::OR => {
            [Use::ReadWrite, Use::Read][idx as usize]
        }
        Opcode::IMUL => {
            // TODO: this.
            Use::Read
        },
        Opcode::IDIV |
        Opcode::DIV => {
            // TODO: this is lazy and assumes writes of all flags
            // this may not be true
            // TODO: this may not read *dx (if this is idiv r/m 8)
            Use::Read
        }
        Opcode::MUL => {
            Use::Read
        }

        Opcode::PUSH => {
            Use::Read
        },
        Opcode::POP => {
            Use::Write
        },
        Opcode::INC |
        Opcode::DEC => {
            Use::ReadWrite
        }
        Opcode::ENTER |
        Opcode::LEAVE |
        Opcode::POPF |
        Opcode::PUSHF |
        Opcode::CBW |
        Opcode::CDW |
        Opcode::LAHF |
        Opcode::SAHF => {
            Use::Read
        }
        Opcode::TEST |
        Opcode::CMP => {
            Use::Read
        }
        Opcode::NEG => {
            Use::ReadWrite
        }
        Opcode::NOT => {
            Use::ReadWrite
        }
        Opcode::CMPXCHG => {
            [Use::ReadWrite, Use::Read][idx as usize]
        },
        Opcode::CALLF | // TODO: this is wrong
        Opcode::CALL => {
            Use::Read
        }
        Opcode::JMP => {
            Use::Read
        },
        Opcode::JMPF => { // TODO: this is wrong.
            Use::Read
        },
        Opcode::IRET | // TODO: this is wrong
        Opcode::RETF | // TODO: this is wrong
        Opcode::RETURN => {
            Use::Read
        }
        Opcode::LFENCE |
        Opcode::MFENCE |
        Opcode::SFENCE |
        Opcode::NOP |
        Opcode::WAIT => { Use::Read }
        Opcode::CLFLUSH |
        Opcode::FXSAVE |
        Opcode::FXRSTOR |
        Opcode::XSAVE |
        Opcode::XSTOR |
        Opcode::XSAVEOPT |
        Opcode::STMXCSR |
        Opcode::SGDT |
        Opcode::SIDT => {
            Use::Write
        }
        Opcode::LDMXCSR |
        Opcode::LGDT |
        Opcode::LIDT => {
            Use::Read
        }
        // TODO: this is wrong
        Opcode::SMSW => {
            Use::Write
        }
        // TODO: this is wrong
        Opcode::LMSW => {
            Use::Read
        }
        Opcode::SWAPGS |
        Opcode::RDTSCP |
        Opcode::WRMSR |
        Opcode::RDMSR |
        Opcode::RDTSC |
        Opcode::RDPMC => {
            Use::Read
        }
        Opcode::VERR |
        Opcode::VERW => {
            Use::Read
        }
        Opcode::SLDT |
        Opcode::STR |
        Opcode::INVLPG => {
            Use::Write
        }
        Opcode::LLDT |
        Opcode::LTR => {
            Use::Read
        }
        // these are immediate-only or have no operands
        Opcode::JMPE |
        Opcode::UD2 |
        Opcode::INT |
        Opcode::INTO |
        Opcode::HLT |
        Opcode::Invalid => {
            Use::Read
        },
        Opcode::Jcc(_cond) => {
            Use::Read
        }

        Opcode::MOVcc(_cond) => {
            [Use::Write, Use::Read][idx as usize]
        }
        Opcode::SETcc(_cond) => {
            Use::Write
        }
        Opcode::LSL => {
            [Use::Write, Use::Read][idx as usize]
        }
        Opcode::LAR => {
            [Use::Write, Use::Read][idx as usize]
        }
        Opcode::ADDSD |
        Opcode::SUBSD |
        Opcode::MULSD |
        Opcode::DIVSD |
        Opcode::MINSD |
        Opcode::MAXSD |
        Opcode::ADDSS |
        Opcode::SUBSS |
        Opcode::MULSS |
        Opcode::DIVSS |
        Opcode::MINSS |
        Opcode::MAXSS |
        Opcode::HADDPS |
        Opcode::HSUBPS |
        Opcode::ADDSUBPS => {
            [Use::ReadWrite, Use::Read][idx as usize]
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
            Use::Read
        }
    }
}

fn implicit_loc(op: yaxpeax_x86::Opcode, i: u8) -> (Option<Location>, Direction) {
    match op {
        Opcode::SQRTSD |
        Opcode::SQRTSS |
        Opcode::MOVDDUP |
        Opcode::MOVSLDUP |
        Opcode::MOVSD |
        Opcode::MOVSS |
        Opcode::CVTSI2SS |
        Opcode::CVTTSS2SI |
        Opcode::CVTSS2SI |
        Opcode::CVTSS2SD |
        Opcode::CVTSI2SD |
        Opcode::CVTTSD2SI |
        Opcode::CVTSD2SI |
        Opcode::CVTSD2SS |
        Opcode::LDDQU |
        Opcode::LEA |
        Opcode::MOVSX_b |
        Opcode::MOVSX_w |
        Opcode::MOVZX_b |
        Opcode::MOVZX_w |
        Opcode::MOVSX |
        Opcode::MOVSXD |
        Opcode::MOV |
        Opcode::XADD |
        Opcode::XCHG => {
            unreachable!();
        }
        Opcode::STI |
        Opcode::CLI => {
            (Some(Location::IF), Direction::Write)
        }
        Opcode::STD |
        Opcode::CLD => {
            (Some(Location::DF), Direction::Write)
        }
        Opcode::STC |
        Opcode::CLC => {
            (Some(Location::CF), Direction::Write)
        }
        Opcode::BT |
        Opcode::BTS |
        Opcode::BTR |
        Opcode::BTC |
        Opcode::BSR |
        Opcode::BSF => {
            (Some(Location::ZF), Direction::Write)
        }
        Opcode::SAR |
        Opcode::SAL |
        Opcode::SHR |
        Opcode::SHL => {
            // TODO: inspect imm8 and CL if context permits to decide
            // flags more granularly
            [
                (Some(Location::CF), Direction::Write),
                (Some(Location::SF), Direction::Write),
                (Some(Location::ZF), Direction::Write),
                (Some(Location::PF), Direction::Write),
                (Some(Location::AF), Direction::Write),
            ][i as usize]
        }
        Opcode::RCR |
        Opcode::RCL |
        Opcode::ROR |
        Opcode::ROL => {
            // TODO: inspect imm8 and CL if context permits to decide
            // flags more granularly
            [
                (Some(Location::CF), Direction::Write),
                (Some(Location::OF), Direction::Write),
            ][i as usize]
        }
        Opcode::ADC |
        Opcode::SBB => {
            // TODO: this is lazy and assumes writes of all flags
            // this may not be true
            [
                (Some(Location::CF), Direction::Read),
                (Some(Location::CF), Direction::Write),
                (Some(Location::OF), Direction::Write),
                (Some(Location::SF), Direction::Write),
                (Some(Location::ZF), Direction::Write),
                (Some(Location::PF), Direction::Write),
                (Some(Location::AF), Direction::Write),
            ][i as usize]
        },
        Opcode::ADD |
        Opcode::SUB |
        Opcode::AND |
        Opcode::XOR |
        Opcode::OR => {
            // TODO: this is lazy and assumes writes of all flags
            // this may not be true
            [
                (Some(Location::CF), Direction::Write),
                (Some(Location::OF), Direction::Write),
                (Some(Location::SF), Direction::Write),
                (Some(Location::ZF), Direction::Write),
                (Some(Location::PF), Direction::Write),
                (Some(Location::AF), Direction::Write),
            ][i as usize]
        }

        Opcode::IMUL => {
            // TODO: this.
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Read),
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Write),
                (Some(Location::CF), Direction::Write),
                (Some(Location::OF), Direction::Write),
                (Some(Location::SF), Direction::Write),
                (Some(Location::ZF), Direction::Write),
                (Some(Location::PF), Direction::Write),
                (Some(Location::AF), Direction::Write),
            ][i as usize]
        },
        Opcode::IDIV |
        Opcode::DIV => {
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Read),
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Read),
                (Some(Location::CF), Direction::Write),
                (Some(Location::OF), Direction::Write),
                (Some(Location::SF), Direction::Write),
                (Some(Location::ZF), Direction::Write),
                (Some(Location::PF), Direction::Write),
                (Some(Location::AF), Direction::Write),
            ][i as usize]

        }
        Opcode::MUL => {
            // TODO: this is lazy and assumes writes of all flags
            // this may not be true
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Read),
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Write),
                (Some(Location::CF), Direction::Write),
                (Some(Location::OF), Direction::Write),
                (Some(Location::SF), Direction::Write),
                (Some(Location::ZF), Direction::Write),
                (Some(Location::PF), Direction::Write),
                (Some(Location::AF), Direction::Write),
            ][i as usize]
        }

        Opcode::PUSH => {
            [
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Memory(STACK)), Direction::Write),
            ][i as usize]
        },
        Opcode::POP => {
            [
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Memory(STACK)), Direction::Read),
            ][i as usize]
        },
        Opcode::INC |
        Opcode::DEC => {
            [
                (Some(Location::OF), Direction::Write),
                (Some(Location::SF), Direction::Write),
                (Some(Location::ZF), Direction::Write),
                (Some(Location::AF), Direction::Write),
                (Some(Location::PF), Direction::Write),
            ][i as usize]
        }
        Opcode::ENTER => {
            [
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Register(RegSpec::rbp())), Direction::Read),
                (Some(Location::Register(RegSpec::rbp())), Direction::Write),
                (Some(Location::Memory(STACK)), Direction::Write)
            ][i as usize]
        }
        Opcode::LEAVE => {
            [
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Register(RegSpec::rbp())), Direction::Write),
                (Some(Location::Memory(STACK)), Direction::Read)
            ][i as usize]
        }
        Opcode::POPF => {
            [
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Memory(STACK)), Direction::Read),
                (Some(Location::Register(RegSpec::rflags())), Direction::Write)
            ][i as usize]
        }
        Opcode::PUSHF => {
            [
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Memory(STACK)), Direction::Write),
                (Some(Location::Register(RegSpec::rflags())), Direction::Read)
            ][i as usize]
        }
        Opcode::CBW |
        Opcode::CDW => {
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Read),
                (Some(Location::Register(RegSpec::rax())), Direction::Write)
            ][i as usize]
        }
        Opcode::LAHF => {
            [
                (Some(Location::Register(RegSpec::ax())), Direction::Write),
                (Some(Location::Register(RegSpec::rflags())), Direction::Read)
            ][i as usize]
        }
        Opcode::SAHF => {
            [
                (Some(Location::Register(RegSpec::ax())), Direction::Read),
                (Some(Location::Register(RegSpec::rflags())), Direction::Write)
            ][i as usize]
        }
        Opcode::TEST |
        Opcode::CMP => {
            /* not strictly correct for either */
            (Some(Location::Register(RegSpec::rflags())), Direction::Write)
        }
        Opcode::NEG => {
            (Some(Location::CF), Direction::Write)
        }
        Opcode::NOT => {
            panic!();
        }
        Opcode::CMPXCHG => {
            (Some(Location::ZF), Direction::Write)
        },
        Opcode::CALLF | // TODO: this is wrong
        Opcode::CALL => {
            [
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Memory(ANY)), Direction::Write),
            ][i as usize]
        }
        Opcode::JMP => {
            panic!();
        },
        Opcode::JMPF => { // TODO: this is wrong.
            panic!();
        },
        Opcode::IRET | // TODO: this is wrong
        Opcode::RETF | // TODO: this is wrong
        Opcode::RETURN => {
            [
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Memory(ANY)), Direction::Read)
            ][i as usize]
        }
        Opcode::LFENCE |
        Opcode::MFENCE |
        Opcode::SFENCE |
        Opcode::NOP |
        Opcode::WAIT => { panic!() }
        Opcode::CLFLUSH |
        Opcode::FXSAVE |
        Opcode::FXRSTOR |
        Opcode::XSAVE |
        Opcode::XSTOR |
        Opcode::XSAVEOPT |
        Opcode::STMXCSR |
        Opcode::SGDT |
        Opcode::SIDT => {
            panic!()
        }
        Opcode::LDMXCSR |
        Opcode::LGDT |
        Opcode::LIDT => {
            panic!()
        }
        // TODO: this is wrong
        Opcode::SMSW => {
            panic!()
        }
        // TODO: this is wrong
        Opcode::LMSW => {
            panic!()
        }
        Opcode::SWAPGS => {
            [
                (Some(Location::Register(RegSpec::gs())), Direction::Read),
                (Some(Location::Register(RegSpec::gs())), Direction::Write)
            ][i as usize]
        }
        Opcode::RDTSCP => {
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rcx())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Write)
            ][i as usize]
        }
        Opcode::WRMSR => {
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Read),
                (Some(Location::Register(RegSpec::rdx())), Direction::Read)
            ][i as usize]
        }
        Opcode::RDMSR => {
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Write),
                (Some(Location::Register(RegSpec::rcx())), Direction::Read)
            ][i as usize]
        }
        Opcode::RDTSC => {
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Write)
            ][i as usize]
        }
        Opcode::RDPMC => {
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Write),
                (Some(Location::Register(RegSpec::rcx())), Direction::Read)
            ][i as usize]
        }
        Opcode::VERR |
        Opcode::VERW => {
            (Some(Location::ZF), Direction::Write)
        }
        Opcode::SLDT |
        Opcode::STR |
        Opcode::INVLPG => {
            panic!()
        }
        Opcode::LLDT |
        Opcode::LTR => {
            panic!()
        }
        // these are immediate-only or have no operands
        Opcode::JMPE |
        Opcode::UD2 |
        Opcode::INT |
        Opcode::INTO |
        Opcode::HLT |
        Opcode::Invalid => {
            panic!()
        },
        Opcode::Jcc(cond) => {
            cond_to_flags(cond)[i as usize]
        }

        Opcode::MOVcc(cond) => {
            cond_to_flags(cond)[i as usize]
        }
        Opcode::SETcc(cond) => {
            cond_to_flags(cond)[i as usize]
        }
        Opcode::LSL => {
            (Some(Location::ZF), Direction::Write)
        }
        Opcode::LAR => {
            (Some(Location::ZF), Direction::Read)
        }
        Opcode::ADDSD |
        Opcode::SUBSD |
        Opcode::MULSD |
        Opcode::DIVSD |
        Opcode::MINSD |
        Opcode::MAXSD |
        Opcode::ADDSS |
        Opcode::SUBSS |
        Opcode::MULSS |
        Opcode::DIVSS |
        Opcode::MINSS |
        Opcode::MAXSS |
        Opcode::HADDPS |
        Opcode::HSUBPS |
        Opcode::ADDSUBPS => {
            panic!()
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
            panic!()
        }

    }
}

fn implicit_locs(op: yaxpeax_x86::Opcode) -> u8 {
    match op {
        Opcode::SQRTSD |
        Opcode::SQRTSS |
        Opcode::MOVDDUP |
        Opcode::MOVSLDUP |
        Opcode::MOVSD |
        Opcode::MOVSS |
        Opcode::CVTSI2SS |
        Opcode::CVTTSS2SI |
        Opcode::CVTSS2SI |
        Opcode::CVTSS2SD |
        Opcode::CVTSI2SD |
        Opcode::CVTTSD2SI |
        Opcode::CVTSD2SI |
        Opcode::CVTSD2SS |
        Opcode::LDDQU |
        Opcode::LEA |
        Opcode::MOVSX_b |
        Opcode::MOVSX_w |
        Opcode::MOVZX_b |
        Opcode::MOVZX_w |
        Opcode::MOVSX |
        Opcode::MOVSXD |
        Opcode::MOV => {
            0
        }
        Opcode::XADD |
        Opcode::XCHG => {
            0
        }
        Opcode::STI |
        Opcode::CLI |
        Opcode::STD |
        Opcode::CLD |
        Opcode::STC |
        Opcode::CLC |
        Opcode::BT |
        Opcode::BTS |
        Opcode::BTR |
        Opcode::BTC |
        Opcode::BSR |
        Opcode::BSF => {
            1
        }
        Opcode::SAR |
        Opcode::SAL |
        Opcode::SHR |
        Opcode::SHL => {
            5
        }
        Opcode::RCR |
        Opcode::RCL |
        Opcode::ROR |
        Opcode::ROL => {
            2
        }
        Opcode::ADC |
        Opcode::SBB => {
            7
        },
        Opcode::ADD |
        Opcode::SUB |
        Opcode::AND |
        Opcode::XOR |
        Opcode::OR => {
            6
        }

        Opcode::IMUL => {
            9
        },
        Opcode::IDIV |
        Opcode::DIV => {
            9
        }
        Opcode::MUL => {
            9
        }
        Opcode::PUSH => {
            3
        },
        Opcode::POP => {
            3
        },
        Opcode::INC |
        Opcode::DEC => {
            5
        }
        Opcode::ENTER => {
            5
        }
        Opcode::LEAVE => {
            4
        }
        Opcode::POPF => {
            4
        }
        Opcode::PUSHF => {
            4
        }
        Opcode::CBW |
        Opcode::CDW => {
            2
        }
        Opcode::LAHF => {
            2
        }
        Opcode::SAHF => {
            2
        }
        Opcode::TEST |
        Opcode::CMP => {
            1
        }
        Opcode::NEG => {
            1
        }
        Opcode::NOT => {
            0
        }
        Opcode::CMPXCHG => { // TODO: this is wrong
            1
        },
        Opcode::CALLF | // TODO: this is wrong
        Opcode::CALL => {
            3
        }
        Opcode::JMP => {
            0
        },
        Opcode::JMPF => { // TODO: this is wrong.
            0
        },
        Opcode::IRET | // TODO: this is wrong
        Opcode::RETF | // TODO: this is wrong
        Opcode::RETURN => {
            3
        }
        Opcode::LFENCE |
        Opcode::MFENCE |
        Opcode::SFENCE |
        Opcode::NOP |
        Opcode::WAIT => { 0 }
        Opcode::CLFLUSH |
        Opcode::FXSAVE |
        Opcode::FXRSTOR |
        Opcode::XSAVE |
        Opcode::XSTOR |
        Opcode::XSAVEOPT |
        Opcode::STMXCSR |
        Opcode::SGDT |
        Opcode::SIDT => {
            0
        }
        Opcode::LDMXCSR |
        Opcode::LGDT |
        Opcode::LIDT => {
            0
        }
        // TODO: this is wrong
        Opcode::SMSW => {
            0
        }
        // TODO: this is wrong
        Opcode::LMSW => {
            0
        }
        Opcode::SWAPGS => {
            2
        }
        Opcode::RDTSCP => {
            3
        }
        Opcode::WRMSR => {
            2
        }
        Opcode::RDMSR => {
            3
        }
        Opcode::RDTSC => {
            2
        }
        Opcode::RDPMC => {
            3
        }
        Opcode::VERR |
        Opcode::VERW => {
            1
        }
        Opcode::SLDT |
        Opcode::STR |
        Opcode::INVLPG |
        Opcode::LLDT |
        Opcode::LTR => {
            0
        }
        // these are immediate-only or have no operands
        Opcode::JMPE |
        Opcode::UD2 |
        Opcode::INT |
        Opcode::INTO |
        Opcode::HLT |
        Opcode::Invalid => {
            0
        },
        Opcode::Jcc(cond) => {
            cond_to_flags(cond).len() as u8
        }

        Opcode::MOVcc(cond) => {
            cond_to_flags(cond).len() as u8
        }
        Opcode::SETcc(cond) => {
            cond_to_flags(cond).len() as u8
        }
        Opcode::LSL |
        Opcode::LAR => {
            1
        }
        Opcode::ADDSD |
        Opcode::SUBSD |
        Opcode::MULSD |
        Opcode::DIVSD |
        Opcode::MINSD |
        Opcode::MAXSD |
        Opcode::ADDSS |
        Opcode::SUBSS |
        Opcode::MULSS |
        Opcode::DIVSS |
        Opcode::MINSS |
        Opcode::MAXSS |
        Opcode::HADDPS |
        Opcode::HSUBPS |
        Opcode::ADDSUBPS => {
            0
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
            0
        }

    }
}
#[test]
fn test_xor_locations() {
    use yaxpeax_arch::Decodable;
    let inst = yaxpeax_x86::Instruction::decode([0x33u8, 0xc1].iter().map(|x| *x)).unwrap();
    use data::LocIterator;
    let locs: Vec<(Option<Location>, Direction)> = inst.iter_locs(&mut NoDisambiguation::default()).collect();
    panic!("{:?}", locs);
}

impl <'a, 'b, D: Disambiguator<Location, (u8, u8)>> Iterator for LocationIter<'a, 'b, D> {
    type Item = (Option<Location>, Direction);
    fn next(&mut self) -> Option<Self::Item> {
        fn next_loc<'a, 'b, D: Disambiguator<Location, (u8, u8)>>(iter: &mut LocationIter<'a, 'b, D>) -> Option<(Option<Location>, Direction)> {
            while iter.loc_count == iter.loc_idx {
                // advance op
                iter.op_idx += 1;

                if iter.op_idx >= iter.op_count {
                    // but we're at the last op, so we're actually done...
                    return None;
                }
    //            println!("opc: {}", iter.inst.opcode);

                let op = &iter.inst.operands[iter.op_idx as usize - 1];
                let op_use = use_of(iter.inst.opcode, iter.op_idx - 1);
                iter.loc_count = locations_in(op, op_use);
                iter.loc_idx = 0;
                iter.curr_op = Some(op);
                iter.curr_use = Some(op_use);
            }

            if iter.op_idx == 0 {
                iter.loc_idx += 1;
                return Some(implicit_loc(iter.inst.opcode, iter.loc_idx - 1));
            }

            if let Some(op) = &iter.curr_op {
                iter.loc_idx += 1;
                match op {
                    Operand::Register(spec) => {
                        if iter.loc_idx == 1 {
                            Some((Some(Location::Register(*spec)), iter.curr_use.unwrap().first_use()))
                        } else {
                            Some((Some(Location::Register(*spec)), Direction::Write))
                        }
                    },
                    Operand::DisplacementU32(_) |
                    Operand::DisplacementU64(_) => {
                        if iter.loc_idx == 1 {
                            Some((Some(Location::Memory(ANY)), iter.curr_use.unwrap().first_use()))
                        } else {
                            Some((Some(Location::Memory(ANY)), Direction::Write))
                        }
                    },
                    Operand::RegDeref(spec) |
                    Operand::RegDisp(spec, _) |
                    Operand::RegScale(spec, _) |
                    Operand::RegScaleDisp(spec, _, _) => {
                        match iter.loc_idx {
                            1 => {
                                Some((Some(Location::Register(*spec)), Direction::Read))
                            },
                            2 => {
                                Some((Some(Location::Memory(ANY)), iter.curr_use.unwrap().first_use()))
                            },
                            3 => {
                                Some((Some(Location::Memory(ANY)), Direction::Write))
                            }
                            _ => {
                                unreachable!();
                            }
                        }
                    },
                    Operand::RegIndexBase(base, index) |
                    Operand::RegIndexBaseDisp(base, index, _) |
                    Operand::RegIndexBaseScale(base, index, _) |
                    Operand::RegIndexBaseScaleDisp(base, index, _, _) => {
                        match iter.loc_idx {
                            1 => {
                                Some((Some(Location::Register(*base)), Direction::Read))
                            },
                            2 => {
                                Some((Some(Location::Register(*index)), Direction::Read))
                            },
                            3 => {
                                Some((Some(Location::Memory(ANY)), iter.curr_use.unwrap().first_use()))
                            },
                            4 => {
                                Some((Some(Location::Memory(ANY)), Direction::Write))
                            }
                            _ => {
                                unreachable!()
                            }
                        }
                    },
                    Operand::Many(_) => {
                        unreachable!()
                    },
                    _ => {
                        unreachable!()
                    }
                }
            } else {
                unreachable!()
            }
        }

        next_loc(self).map(|loc| {
            let loc_spec = (self.op_idx, self.loc_idx - 1);
            self.disambiguator.disambiguate(loc_spec).map(|new_loc| (Some(new_loc), loc.1)).unwrap_or(loc)
        })
    }
}

impl <'a, 'b, D: 'b + Disambiguator<Location, (u8, u8)>> crate::data::LocIterator<'b, Location, D> for &'a yaxpeax_x86::Instruction {
    type Item = (Option<Location>, Direction);
    type LocSpec = (u8, u8);
    type Iter = LocationIter<'a, 'b, D>;
    fn iter_locs(self, disam: &'b mut D) -> Self::Iter {
        LocationIter::new(self, disam)
    }
}

impl ValueLocations for x86_64 {
    type Location = Location;

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
                    vec![(Some(Location::Memory(ANY)), Direction::Write)]
                },
                Operand::RegDeref(spec) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Write)
                    ]
                },
                Operand::RegDisp(spec, _) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Write)
                    ]
                },
                Operand::RegScale(spec, _) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Write)
                    ]
                },
                Operand::RegIndexBase(base, index) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Write)
                    ]
                },
                Operand::RegIndexBaseDisp(base, index, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Write)
                    ]
                },
                Operand::RegScaleDisp(base, _, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Write)
                    ]
                },
                Operand::RegIndexBaseScale(base, index, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Write)
                    ]
                },
                Operand::RegIndexBaseScaleDisp(base, index, _, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Write)
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
                    vec![(Some(Location::Memory(ANY)), Direction::Read)]
                },
                Operand::RegDeref(spec) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Read)
                    ]
                },
                Operand::RegDisp(spec, _) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Read)
                    ]
                },
                Operand::RegScale(spec, _) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Read)
                    ]
                },
                Operand::RegIndexBase(base, index) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Read)
                    ]
                },
                Operand::RegIndexBaseDisp(base, index, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Read)
                    ]
                },
                Operand::RegScaleDisp(base, _, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Read)
                    ]
                },
                Operand::RegIndexBaseScale(base, index, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Read)
                    ]
                },
                Operand::RegIndexBaseScaleDisp(base, index, _, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Read)
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
                        (Some(Location::Memory(ANY)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Write),
                    ]
                },
                Operand::RegDeref(spec) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Write)
                    ]
                },
                Operand::RegDisp(spec, _) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Write)
                    ]
                },
                Operand::RegScale(spec, _) => {
                    vec![
                        (Some(Location::Register(*spec)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Write)
                    ]
                },
                Operand::RegIndexBase(base, index) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Write)
                    ]
                },
                Operand::RegIndexBaseDisp(base, index, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Write)
                    ]
                },
                Operand::RegScaleDisp(base, _, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Write)
                    ]
                },
                Operand::RegIndexBaseScale(base, index, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Write)
                    ]
                },
                Operand::RegIndexBaseScaleDisp(base, index, _, _) => {
                    vec![
                        (Some(Location::Register(*base)), Direction::Read),
                        (Some(Location::Register(*index)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Read),
                        (Some(Location::Memory(ANY)), Direction::Write)
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
            Opcode::SQRTSD |
            Opcode::SQRTSS |
            Opcode::MOVDDUP |
            Opcode::MOVSLDUP |
            Opcode::MOVSD |
            Opcode::MOVSS |
            Opcode::CVTSI2SS |
            Opcode::CVTTSS2SI |
            Opcode::CVTSS2SI |
            Opcode::CVTSS2SD |
            Opcode::CVTSI2SD |
            Opcode::CVTTSD2SI |
            Opcode::CVTSD2SI |
            Opcode::CVTSD2SS |
            Opcode::LDDQU |
            Opcode::LEA |
            Opcode::MOVSX_b |
            Opcode::MOVSX_w |
            Opcode::MOVZX_b |
            Opcode::MOVZX_w |
            Opcode::MOVSX |
            Opcode::MOVSXD |
            Opcode::MOV => {
                let mut locs = decompose_read(&instr.operands[1]);
                locs.append(&mut decompose_write(&instr.operands[0]));
                locs
            }
            Opcode::XADD |
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
            Opcode::BT |
            Opcode::BTS |
            Opcode::BTR |
            Opcode::BTC |
            Opcode::BSR |
            Opcode::BSF => {
                let mut locs = decompose_read(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.push((Some(Location::ZF), Direction::Write));
                locs
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
                let mut locs = if let Operand::Nothing = instr.operands[1] {
                    decompose_read(&instr.operands[0])
                } else {
                    let mut ls = decompose_readwrite(&instr.operands[0]);
                    ls.append(&mut decompose_read(&instr.operands[1]));
                    ls
                };
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
            },
            Opcode::IDIV |
            Opcode::DIV => {
                // TODO: this is lazy and assumes writes of all flags
                // this may not be true
                // TODO: this may not read *dx (if this is idiv r/m 8)
                let mut locs = if let Operand::Nothing = instr.operands[1] {
                    decompose_read(&instr.operands[0])
                } else {
                    let mut ls = decompose_readwrite(&instr.operands[0]);
                    ls.append(&mut decompose_read(&instr.operands[1]));
                    ls
                };
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
                locs.push((Some(Location::Memory(ANY)), Direction::Write));
                locs
            },
            Opcode::POP => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.push((Some(Location::Register(RegSpec::rsp())), Direction::Read));
                locs.push((Some(Location::Register(RegSpec::rsp())), Direction::Write));
                locs.push((Some(Location::Memory(ANY)), Direction::Read));
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
                    (Some(Location::Memory(ANY)), Direction::Write)
                ]
            }
            Opcode::LEAVE => {
                vec![
                    (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                    (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                    (Some(Location::Register(RegSpec::rbp())), Direction::Write),
                    (Some(Location::Memory(ANY)), Direction::Read)
                ]
            }
            Opcode::POPF => {
                vec![
                    (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                    (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                    (Some(Location::Memory(ANY)), Direction::Read),
                    (Some(Location::Register(RegSpec::rflags())), Direction::Write)
                ]
            }
            Opcode::PUSHF => {
                vec![
                    (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                    (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                    (Some(Location::Memory(ANY)), Direction::Write),
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
                locs.push((Some(Location::Memory(ANY)), Direction::Write));
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
                    (Some(Location::Memory(ANY)), Direction::Read)
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
            Opcode::Jcc(cond) => {
                cond_to_flags(cond).to_vec()
            }

            Opcode::MOVcc(cond) => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
                locs.extend_from_slice(cond_to_flags(cond));
                locs
            }
            Opcode::SETcc(cond) => {
                let mut locs = decompose_write(&instr.operands[0]);
                locs.extend_from_slice(cond_to_flags(cond));
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
            Opcode::ADDSD |
            Opcode::SUBSD |
            Opcode::MULSD |
            Opcode::DIVSD |
            Opcode::MINSD |
            Opcode::MAXSD |
            Opcode::ADDSS |
            Opcode::SUBSS |
            Opcode::MULSS |
            Opcode::DIVSS |
            Opcode::MINSS |
            Opcode::MAXSS |
            Opcode::HADDPS |
            Opcode::HSUBPS |
            Opcode::ADDSUBPS => {
                let mut locs = decompose_readwrite(&instr.operands[0]);
                locs.append(&mut decompose_read(&instr.operands[1]));
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
