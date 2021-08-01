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
use arch::{AbiDefaults, FunctionAbiReference};
use analyses::static_single_assignment::{DFGRef, SSAValues};
use data::types::{Typed, TypeSpec, TypeAtlas};
use yaxpeax_x86::long_mode::{register_class, ConditionCode, RegSpec};
use yaxpeax_x86::x86_64;

use std::rc::Rc;
use std::fmt;
// use std::cell::RefCell;
use std::hash::{Hash, Hasher};

// use std::collections::HashMap;
use analyses::static_single_assignment::HashedValue;
// use serialize::Memoable;
use serde::{Serialize, Deserialize};
use serde::de::{self, Deserializer, Visitor, Unexpected};
use serde::ser::{Serializer};

use tracing::error;

use data::{Direction, Disambiguator, ValueLocations};
use data::LocationAliasDescriptions;
use arch::x86_64::display::DataDisplay;
use yaxpeax_arch::ColorSettings;

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
pub struct MemoryRegion(pub u16);

pub const ANY: MemoryRegion = MemoryRegion(0);
pub const STACK: MemoryRegion = MemoryRegion(1);
pub const PROGRAM: MemoryRegion = MemoryRegion(2);
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

#[derive(Clone, Hash, Eq, PartialEq)]
pub enum Location {
    Register(RegSpec),
    Memory(MemoryRegion),
    MemoryLocation(MemoryRegion, u8, Option<(Data, Data)>),
    // not modeling eflags' system bits ... yet?
    CF, PF, AF, ZF, SF, TF, IF, DF, OF, IOPL,
    // necessary to have a location to write that is provably not an Operand variant.
    // no x86 instruction explicitly writes to RIP in a way that could be ambiguous with other
    // operands, so this allows x86 semantics to specialize nicely for control flow.
    //
    // well. would. unfortunately, match codegen does not play well. so this doesn't specialize
    // nearly as nicely as i'd hoped, and might have to get rethunk.
    RIP,
}

impl Location {
    pub fn rip() -> Self { Location::RIP }

    pub fn rax() -> Self { Location::Register(RegSpec::rax()) }
    pub fn rcx() -> Self { Location::Register(RegSpec::rcx()) }
    pub fn rdx() -> Self { Location::Register(RegSpec::rdx()) }
//    pub fn rbx() -> Self { Location::Register(RegSpec::rbx()) }
    pub fn rsp() -> Self { Location::Register(RegSpec::rsp()) }
    pub fn rbp() -> Self { Location::Register(RegSpec::rbp()) }
    pub fn rsi() -> Self { Location::Register(RegSpec::rsi()) }
    pub fn rdi() -> Self { Location::Register(RegSpec::rdi()) }

    pub fn eax() -> Self { Location::Register(RegSpec::eax()) }
    pub fn ecx() -> Self { Location::Register(RegSpec::ecx()) }
    pub fn edx() -> Self { Location::Register(RegSpec::edx()) }
    pub fn ebx() -> Self { Location::Register(RegSpec::ebx()) }
}

use analyses::static_single_assignment::{DFGRebase, SSA};
use analyses::static_single_assignment::DefSource;
impl DFGRebase<yaxpeax_x86::x86_64> for Data {
    fn rebase_references(&self, old_dfg: &SSA<yaxpeax_x86::x86_64>, new_dfg: &SSA<yaxpeax_x86::x86_64>) -> Self {
        match self {
            Data::Expression(expr) => {
                Data::Expression(expr.rebase_references(old_dfg, new_dfg))
            },
            Data::Alias(dfg_ref) => {
                if !new_dfg.defs.contains_key(&HashedValue { value: Rc::clone(dfg_ref) }) {
                    let (_old_ref_addr, old_ref_source) = old_dfg.get_def_site(Rc::clone(dfg_ref));
                    // "External" might mean f.ex `rsp_input`.
                    match old_ref_source {
                        DefSource::External => {
                            Data::Alias(Rc::clone(new_dfg.external_defs.get(&dfg_ref.borrow().location).expect("external def has matching new def")))
                        }
                        DefSource::Instruction => {
//                            self.to_owned()
                            panic!("instruction def source");
                        }
                        other => {
                            panic!("unhandled def source: {:?}", other);
                        }
                    }
                } else {
                    Data::Alias(Rc::clone(dfg_ref))
                }
            },
            Data::ValueSet(value_ranges) => Data::ValueSet(value_ranges.to_owned()),
            other => other.to_owned(),
        }
    }
}

impl DFGRebase<yaxpeax_x86::x86_64> for Location {
    fn rebase_references(&self, old_dfg: &SSA<yaxpeax_x86::x86_64>, new_dfg: &SSA<yaxpeax_x86::x86_64>) -> Self {
        match self {
            Location::MemoryLocation(region, size, Some((base, addend))) => {
                let new_base = base.rebase_references(old_dfg, new_dfg);
                let new_addend = addend.rebase_references(old_dfg, new_dfg);
                // it doesn't actually matter if `new_base` and `new_addend` are the same as the
                // pre-rebase forms. if they are, either `new_*` are correct here, or they're
                // different and we want `new_` ones. we would check here if `rebase_references`
                // returned an indicator of "this is now a different location", but that's left to
                // callers for now.
                Location::MemoryLocation(*region, *size, Some((new_base, new_addend)))
            }
            other => other.to_owned()
        }
    }
}

#[derive(Default)]
struct LocationVisitor {}

impl<'de> Visitor<'de> for LocationVisitor {
    type Value = Location;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("enum Location")
    }

    fn visit_str<E: de::Error>(self, s: &str) -> Result<Self::Value, E> {
        // ok. Location is serialized as a letter for the variant and variable data afterward.
        let mut parts = s.split(":");
        let start = parts.next().ok_or_else(|| {
            E::invalid_length(0, &"serialized location should have at least one character")
        })?;

        fn check_end<'a, E: de::Error>(read: usize, mut parts: impl Iterator<Item=&'a str>) -> Result<(), E> {
            if parts.next().is_some() {
                Err(E::invalid_length(read, &"expected end of input"))
            } else {
                Ok(())
            }
        }

        match start {
            "R" => {
                let regstr = parts.next().ok_or(
                    E::invalid_length(1, &"expected regspec in serialized location")
                )?;
                // !!!
                let regspec: RegSpec = serde_json::from_str(regstr).unwrap();
                check_end(2, parts)?;
                Ok(Location::Register(regspec))
            },
            "m" => {
                let memstr = parts.next().ok_or(
                    E::invalid_length(1, &"expected memory region in serialized location")
                )?;
                // !!!
                let memory: u16 = serde_json::from_str(memstr).unwrap();
                check_end(2, parts)?;
                Ok(Location::Memory(MemoryRegion(memory)))
            },
            "M" => {
                panic!("can't serialize or deserialize Location::MemoryLocation yet");
                /*
                let memstr = parts.next().ok_or(
                    E::invalid_length(1, &"expected memory region in serialized location")
                )?;
                // !!!
                let memory: u16 = serde_json::from_str(memstr).unwrap();
                let szstr = parts.next().ok_or(
                    E::invalid_length(2, &"expected memory size in serialized location")
                )?;
                // !!!
                let memory_size: u16 = serde_json::from_str(szstr).unwrap();
                // TODO: !!!
                let _addrstr = parts.next().ok_or(
                    E::invalid_length(3, &"expected memory address in serialized location")
                )?;
                // !!!
                let memory_addr: i32 = serde_json::from_str(szstr).unwrap();
                check_end(4, parts)?;
                Ok(Location::MemoryLocation(MemoryRegion(memory), memory_size, memory_addr))
                */
            },
            "c" => { check_end(1, parts)?; Ok(Location::CF) }
            "p" => { check_end(1, parts)?; Ok(Location::PF) }
            "a" => { check_end(1, parts)?; Ok(Location::AF) }
            "z" => { check_end(1, parts)?; Ok(Location::ZF) }
            "s" => { check_end(1, parts)?; Ok(Location::SF) }
            "t" => { check_end(1, parts)?; Ok(Location::TF) }
            "i" => { check_end(1, parts)?; Ok(Location::IF) }
            "d" => { check_end(1, parts)?; Ok(Location::DF) }
            "o" => { check_end(1, parts)?; Ok(Location::OF) }
            "l" => { check_end(1, parts)?; Ok(Location::IOPL) }
            u => { Err(E::invalid_value(Unexpected::Str(u), &"invalid location enum discriminant")) }
        }
    }
}

impl Serialize for Location {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut serialized_loc = String::new();
        match self {
            Location::Register(spec) => {
                serialized_loc.push_str("R:");
                // !!!
                serialized_loc.push_str(&serde_json::to_string(&spec).unwrap());
            }
            Location::Memory(region) => {
                serialized_loc.push_str("m:");
                // !!!
                serialized_loc.push_str(&serde_json::to_string(&region.0).unwrap());
            }
            Location::MemoryLocation(_region, _size, _access_info) => {
                panic!("can't serialize or deserialize Location::MemoryLocation yet");
                /*
                serialized_loc.push_str("M:");

                // !!!
                serialized_loc.push_str(&serde_json::to_string(&region.0).unwrap());
                serialized_loc.push_str(":");
                // !!!
                serialized_loc.push_str(&serde_json::to_string(size).unwrap());
                serialized_loc.push_str(":");
                // !!!
                serialized_loc.push_str(&serde_json::to_string(offset).unwrap());
                */
            },
            Location::CF => { serialized_loc.push_str("c"); }
            Location::PF => { serialized_loc.push_str("p"); }
            Location::AF => { serialized_loc.push_str("a"); }
            Location::ZF => { serialized_loc.push_str("z"); }
            Location::SF => { serialized_loc.push_str("s"); }
            Location::TF => { serialized_loc.push_str("t"); }
            Location::IF => { serialized_loc.push_str("i"); }
            Location::DF => { serialized_loc.push_str("d"); }
            Location::OF => { serialized_loc.push_str("o"); }
            Location::IOPL => { serialized_loc.push_str("l"); }
            Location::RIP => {
                serialized_loc.push_str("$");
            }
        }
        serializer.serialize_str(&serialized_loc)
    }
}

impl<'de> Deserialize<'de> for Location {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(
            LocationVisitor::default()
        )
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Location::Register(reg) => write!(f, "{}", reg),
            Location::Memory(region) => write!(f, "(mem:{})", region),
            Location::MemoryLocation(mem, size, access_info) => {
                if let Some((base, addend)) = access_info {
                    write!(f, "(mem:{} + {} + {}):{}", mem, base, addend, size)
                } else {
                    write!(f, "(mem:{}):{}", mem, size)
                }
            },
            Location::CF => write!(f, "cf"),
            Location::PF => write!(f, "pf"),
            Location::AF => write!(f, "af"),
            Location::ZF => write!(f, "zf"),
            Location::SF => write!(f, "sf"),
            Location::TF => write!(f, "tf"),
            Location::IF => write!(f, "if"),
            Location::DF => write!(f, "df"),
            Location::OF => write!(f, "of"),
            Location::IOPL => write!(f, "iopl"),
            Location::RIP => write!(f, "rip"),
        }
    }
}

impl fmt::Debug for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
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
            Location::RIP => { vec![] },
            Location::Register(reg) => {
                match reg.class() {
                    register_class::CR |
                    register_class::DR |
                    register_class::S |
                    register_class::RIP |
                    register_class::RFLAGS |
                    register_class::ST |
                    register_class::K |
                    register_class::Z => {
                        vec![]
                    }
                    register_class::Q => {
                        vec![
                            Location::Register(RegSpec::d(reg.num())),
                            Location::Register(RegSpec::w(reg.num())),
                            Location::Register(RegSpec::rb(reg.num())),
                        ]
                    }
                    register_class::D => {
                        vec![
                            Location::Register(RegSpec::q(reg.num())),
                            Location::Register(RegSpec::w(reg.num())),
                            Location::Register(RegSpec::rb(reg.num())),
                        ]
                    }
                    register_class::W => {
                        vec![
                            Location::Register(RegSpec::q(reg.num())),
                            Location::Register(RegSpec::d(reg.num())),
                            Location::Register(RegSpec::rb(reg.num())),
                        ]
                    }
                    register_class::B => {
                        vec![
                            Location::Register(RegSpec::q(reg.num() & 3)),
                            Location::Register(RegSpec::d(reg.num() & 3)),
                            Location::Register(RegSpec::w(reg.num() & 3)),
                        ]
                    }
                    register_class::RB => {
                        vec![
                            Location::Register(RegSpec::q(reg.num())),
                            Location::Register(RegSpec::d(reg.num())),
                            Location::Register(RegSpec::w(reg.num())),
                        ]
                    }
                    register_class::MM => {
                        vec![
                            Location::Register(RegSpec::st(reg.num())),
                        ]
                    }
                    register_class::Y => {
                        vec![
                            Location::Register(RegSpec::zmm(reg.num())),
                        ]
                    }
                    register_class::X => {
                        vec![
                            Location::Register(RegSpec::zmm(reg.num())),
                            Location::Register(RegSpec::ymm(reg.num())),
                        ]
                    }
                    register_class::EIP => {
                        vec![
                            Location::Register(RegSpec::rip())
                        ]
                    },
                    register_class::EFLAGS => {
                        vec![
                            Location::Register(RegSpec::rflags())
                        ]
                    },
                }
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
            Location::RIP => {
                self.clone()
            }
            Location::Register(reg) => {
                match reg.class() {
                    register_class::CR |
                    register_class::DR |
                    register_class::S |
                    register_class::RIP |
                    register_class::RFLAGS |
                    register_class::ST |
                    register_class::K |
                    register_class::Z |
                    register_class::Q => {
                        self.clone()
                    }
                    register_class::D |
                    register_class::W |
                    register_class::RB => {
                        Location::Register(RegSpec::q(reg.num()))
                    }
                    register_class::B => {
                        Location::Register(RegSpec::q(reg.num() & 3))
                    }
                    register_class::MM => {
                        Location::Register(RegSpec::st(reg.num()))
                    }
                    register_class::X |
                    register_class::Y => {
                        Location::Register(RegSpec::zmm(reg.num()))
                    }
                    register_class::EIP => {
                        Location::Register(RegSpec::rip())
                    }
                    register_class::EFLAGS => {
                        Location::Register(RegSpec::rflags())
                    }
                }
            }
        }
    }
}

// TODO: how does this interact with struct/type inference?
// SymbolicExpression::Deref(
//   SymbolicExpression::Add(rdi_input, 0x8)
// ) => HANDLE ?
#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
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
    CopOut(String),

    // `MemoryAccessBaseInference` can be impl'd for `Data`, where a `SymbolicExpression` is where
    // all the good stuff comes in
    /*
    Const(u64),
    Value(Data),
    Add(Box<SymbolicExpression>, Box<SymbolicExpression>),
    Sub(Box<SymbolicExpression>, Box<SymbolicExpression>),
    Mul(Box<SymbolicExpression>, Box<SymbolicExpression>),
    Div(Box<SymbolicExpression>, Box<SymbolicExpression>),
    */
}

#[ignore]
#[test]
fn test_expr_fields() {
    use data::types::KPCR;
    let type_atlas = &TypeAtlas::new();
    let expr = SymbolicExpression::Opaque(TypeSpec::PointerTo(Rc::new(TypeSpec::PointerTo(Rc::new(TypeSpec::LayoutId(KPCR))))));
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
                    TypeSpec::PointerTo(Rc::new(field.type_of()))
                } else {
                    TypeSpec::Unknown
                }
            }
            SymbolicExpression::Deref(expr) => {
                if let TypeSpec::PointerTo(_ty) = expr.type_of(type_atlas) {
                    todo!("type_of");
                    // *ty.to_owned()
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ValueRange {
    Between(Data, Data),
    Precisely(Data),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Data {
    Concrete(u64, Option<TypeSpec>),
    Str(String),
    Expression(Rc<crate::analyses::Item<crate::analyses::ValueOrImmediate<x86_64>>>),
    Alias(DFGRef<x86_64>),
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

impl crate::analyses::memory_layout::Underlying for Data {
    type Arch = x86_64;

    fn underlying(&self) -> Option<DFGRef<Self::Arch>> {
        match self {
            Data::Alias(alias) => {
                let mut underlying = Rc::clone(alias);
                loop {
                    let next = if let Some(Data::Alias(inner)) = &underlying.borrow().data {
                        Rc::clone(inner)
                    } else {
                        break;
                    };
                    underlying = next;
                }
                Some(underlying)
            }
            _ => None,
        }
    }

    fn expression(&self) -> Option<Rc<crate::analyses::Item<crate::analyses::ValueOrImmediate<x86_64>>>> {
        if let Data::Expression(expr) = self {
            Some(Rc::clone(expr))
        } else {
            None
        }
    }
}

impl fmt::Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.display(None))
    }
}

impl Data {
    pub fn display<'a, 'b>(&'a self, colors: Option<&'b ColorSettings>) -> DataDisplay<'a, 'b> {
        DataDisplay {
            data: self,
            colors,
        }
    }

    pub fn underlying(&self) -> Option<Data> {
        let mut curr = self.to_owned();
        while let Data::Alias(alias) = &curr {
            let alias = Rc::clone(alias);
            let referent = alias.borrow().data.clone();
            match referent {
                Some(aliased) => {
                    curr = aliased;
                }
                None => {
                    break;
                }
            }
        }

        Some(curr)
    }

    pub fn add(left: &Data, right: &Data) -> Option<Data> {
        match (left, right) {
            (Data::Concrete(left, _), Data::Concrete(right, _)) => {
                // TODO: preserve types
                Some(Data::Concrete(left.wrapping_add(*right), None))
            }
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
            Data::Expression(expr) => expr.ty.as_ref().map(|x| x.clone()).unwrap_or(TypeSpec::Bottom),
            Data::Alias(ptr) => ptr.borrow().data.to_owned().map(|d| d.type_of(type_atlas)).unwrap_or(TypeSpec::Unknown),
            // TODO: technically not valid - we could see if there's a shared type among
            // all the elements of the value set.
            Data::ValueSet(_) => TypeSpec::Bottom,
        }
    }
}

/*
// TODO: update memo
#[derive(Debug, Serialize, Deserialize)]
pub enum DataMemo {
    Concrete(u64, Option<TypeSpec>),
    Str(String),
    Expression(SymbolicExpression),
    Alias(u32),
    ValueSet(Vec<ValueRangeMemo>)
}

#[derive(Debug, Serialize, Deserialize)]
pub enum ValueRangeMemo {
    Between(DataMemo, DataMemo),
    Precisely(DataMemo),
}

#[derive(Debug, Serialize, Deserialize)]
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
    fn dememoize(idx: u32, memos: &[Self::Out], dememoized: &mut HashMap<u32, Self>) -> Self {
        fn dememoize_data(data: &DataMemo, memos: &[ValueMemo], dememoized: &mut HashMap<u32, HashedValue<Rc<RefCell<Value<x86_64>>>>>) -> Data {
            match data {
                DataMemo::Concrete(v, ty) => Data::Concrete(*v, ty.to_owned()),
                DataMemo::Str(string) => Data::Str(string.to_owned()),
                DataMemo::Expression(expr) => Data::Expression(expr.to_owned()),
                DataMemo::Alias(idx) => {
                    Data::Alias(<HashedValue<Rc<RefCell<Value<x86_64>>>> as Memoable>::dememoize(*idx, memos, dememoized).value)
                }
                DataMemo::ValueSet(values) => {
                    let mut memoized_values: Vec<ValueRange> = vec![];
                    for value in values {
                        let memoized_value = match value {
                            ValueRangeMemo::Between(start, end) => {
                                ValueRange::Between(dememoize_data(start, memos, dememoized), dememoize_data(end, memos, dememoized))
                            }
                            ValueRangeMemo::Precisely(v) => {
                                ValueRange::Precisely(dememoize_data(v, memos, dememoized))
                            }
                        };
                        memoized_values.push(memoized_value)
                    }
                    Data::ValueSet(memoized_values)
                }
            }
        }

        let memo = &memos[idx as usize];

        use std::collections::hash_map::Entry;
        match dememoized.entry(idx) {
            Entry::Occupied(v) => {
                // something already caused us to dememoize this value. it's part of a
                // cycle or something.
                HashedValue { value: Rc::clone(&v.get().value) }
            }
            Entry::Vacant(e) => {
                let loc = memo.location;
                let version = memo.version;

                let dememo = Rc::new(RefCell::new(Value {
                    name: None,
                    used: true,
                    location: loc,
                    version: version,
                    data: None
                }));

                e.insert(HashedValue { value: Rc::clone(&dememo) });

                if let Some(data) = &memo.data {
                    (&mut *dememo.borrow_mut()).data = Some(dememoize_data(data, memos, dememoized));
                }

                HashedValue { value: dememo }
            }
        }
    }
}
*/

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

pub(crate) fn cond_to_flags(cond: ConditionCode) -> &'static [(Option<Location>, Direction)] {
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
        ConditionCode::AE => {
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
        ConditionCode::BE => {
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

#[derive(Default)]
pub struct NoDisambiguation { }
impl<T> Disambiguator<x86_64, T> for NoDisambiguation {
    fn disambiguate(&self, _instr: &<x86_64 as crate::Arch>::Instruction, _loc: (Option<Location>, Direction), _spec: T) -> Option<Location> {
        None
    }
}

impl LocationAliasDescriptions<x86_64> for NoDisambiguation {
    fn may_alias(&self, _left: &Location, _right: &Location) -> bool {
        false
    }

    fn aliases_for(&self, loc: &Location) -> Vec<Location> {
        loc.aliases_of()
    }
}

pub struct ContextualDisambiguation<'dfg, 'mem> {
    pub dfg: &'dfg crate::analyses::static_single_assignment::SSA<x86_64>,
    pub memory_layout: Option<&'mem crate::analyses::memory_layout::MemoryLayout<'dfg, x86_64>>,
}

impl <'dfg, 'mem> LocationAliasDescriptions<x86_64> for ContextualDisambiguation<'dfg, 'mem> {
    fn may_alias(&self, left: &Location, right: &Location) -> bool {
        match (left, right) {
            (Location::MemoryLocation(_, _l_size, Some((l_base, l_addend))), Location::MemoryLocation(_, _r_size, Some((r_base, r_addend)))) => {
                if let Some(memory_layout) = self.memory_layout {
                    let segments = memory_layout.segments.borrow();

                    use analyses::Expression;
                    let l_segment = if let Data::Expression(base) = l_base {
                        if let Expression::Value(v) = &base.value {
                            segments.get(&v)
                        } else {
                            error!("l_base is a Data::Expression, but the expression is non-Value: {:?}", base.value);
                            return true;
                        }
                    } else {
                        error!("l_base is not a Data::Expression, but is assumed to always be an Expression");
                        return true;
                    };
                    let r_segment = if let Data::Expression(base) = r_base {
                        if let Expression::Value(v) = &base.value {
                            segments.get(&v)
                        } else {
                            error!("r_base is a Data::Expression, but the expression is non-Value: {:?}", base.value);
                            return true;
                        }
                    } else {
                        error!("r_base is not a Data::Expression, but is assumed to always be an Expression");
                        return true;
                    };
                    match (l_segment, r_segment) {
                        (Some(l_segment), Some(r_segment)) => {
                            // if one or both accesses are unknown, they may alias
                            /*
                            let _l_access = if let Some(access) = l_segment.borrow().get(l_addend) {
                                access
                            } else {
                                return true;
                            };
                            let _r_access = if let Some(access) = r_segment.borrow().get(r_addend) {
                                access
                            } else {
                                return true;
                            };
                            */

                            if Rc::ptr_eq(l_segment, r_segment) {
                                // the two segments are the same, then these only overlap if the
                                // two accesses are known to be the same
                                //
                                // TODO: not entirely accurate. if l_access and r_access overlap
                                // but are not identical, this incorrectly reports no alias. at
                                // some point the following line should be uncomment-able:
                                // ```
                                // l_access.is_overlapping(r_access)
                                // ```
                                //
                                // TODO: segments are the same, which means these would look up
                                // the same MemoryRegion, but there's no Eq on MemoryRegion yet
                                // and we can just use these directly. eventually,
                                // `.is_overlapping` will handle cases where non-equal regions
                                // do overlap a bit.
                                l_addend == r_addend
                            } else {
                                // if the segments are known, but not known to be the same,
                                // regardless of addends they may overlap - one may be a subset of
                                // another, so different addends could select the same location.
                                //
                                // TODO: `noalias` assumption to optimistically conclude that some
                                // regions still would not overlap? some operations are assumed to
                                // return new non-aliasing memory (malloc, new, mmap,
                                // NtAllocateVirtualMemory, and others..). this still needs to be
                                // controllable because in circumstances like malloc corruption -
                                // which yaxpeax may be used to reason about - these assumptions
                                // certainly could be violated..
                                true
                            }
                        }
                        _ => {
                            true
                        }
                    }
                } else {
                    // we have no memory layout information; pessimistically assume that any memory
                    // accesses can alias.
                    //
                    // TODO: if both memory operands are constants we can still determine they
                    // don't overlap. that circumstance is ... probably rare.
                    true
                }
            }
            (Location::MemoryLocation(_, _, _), Location::Memory(_)) |
            (Location::Memory(_), Location::MemoryLocation(_, _, _)) |
            (Location::Memory(_), Location::Memory(_)) => {
                true
            }
            (l, r) => {
                l.aliases_of().contains(r) || r.aliases_of().contains(l)
            }
        }
    }

    // given this `memory_layout` and `dfg`, what locations (including memory partitions) may
    // overlap with `loc`?
    fn aliases_for(&self, loc: &Location) -> Vec<Location> {
        // TODO: (incorrectly) assert that there are no different aliasing relationships
        loc.aliases_of()
    }
}

impl <'dfg, 'mem> Disambiguator<x86_64, (<x86_64 as crate::Arch>::Address, u8, u8)> for ContextualDisambiguation<'dfg, 'mem> {
    fn disambiguate(&self, instr: &<x86_64 as crate::Arch>::Instruction, loc: (Option<Location>, Direction), spec: (<x86_64 as crate::Arch>::Address, u8, u8)) -> Option<Location> {
        // figure out if the location is sp-relative or rip-relative
        // .. or, has a relocation?
//        println!("so you'd like me to disambiguate {}, {:?}", instr, spec);
        use analyses::DFG;
        use analyses::IndirectQuery;
        use arch::x86_64::semantic::DFGAccessExt;
        if let Some(memory) = self.memory_layout {
            if let Some(Location::Memory(ANY)) = loc.0 {
                use yaxpeax_x86::long_mode::{Opcode, Operand};

                let q = memory.query_at(spec.0);
                let access = if spec.1 == 0 {
                    match instr.opcode() {
                        Opcode::PUSH => {
                            q.effective_address(&Operand::RegDeref(RegSpec::rsp()))
                        }
                        Opcode::POP => {
                            q.effective_address(&Operand::RegDeref(RegSpec::rsp()))
                        }
                        _ => {
                            tracing::error!("FIXME: bailing out for implied memory operand");
                            // HACK: effective_address is not appropriate when *the operand to disambiguate
                            // is implied*. this is the case for movs, cmps, push, and pop's memory
                            // operands...
                            return None;
                        }
                    }
                } else {
                    q.effective_address(&instr.operand(spec.1 - 1))
                };
                use analyses::memory_layout::MemoryAccessBaseInference;
                use analyses::Expression;
                if let Some((base, addend)) = MemoryAccessBaseInference::infer_base_and_addend(&access) {
                    if base.value != Expression::Unknown && addend.value != Expression::Unknown {
                        use analyses::IntoValueIndex;
                        println!("base {}, addend {}, instr {}", base, addend, instr);
                        let indirect = self.memory_layout.expect("mem layout").indirect_loc(spec.0, loc.0.unwrap());
                        //println!("  {:?}", indirect);
                        match loc.1 {
                            Direction::Read => {
                                if indirect.try_get_load(access.qword()).is_some() {
                                    // TODO: 8 is the access width. should be inferred from the operand.
                                    // ... one day :)
                                    println!("disambiguated {}, {:?} to {}+{}", instr, spec, base, addend);
                                    return Some(Location::MemoryLocation(ANY, 8, Some((Data::Expression(base), Data::Expression(addend)))));
                                }
                            }
                            Direction::Write => {
                                if indirect.try_get_store(access.qword()).is_some() {
                                    // TODO: 8 is the access width. should be inferred from the operand.
                                    // ... one day :)
                                    println!("disambiguated {}, {:?} to {}+{}", instr, spec, base, addend);
                                    return Some(Location::MemoryLocation(ANY, 8, Some((Data::Expression(base), Data::Expression(addend)))));
                                }
                            }
                        }
                    }
                }
            }
        }
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DefaultCallingConvention {
    None,
    SystemV,
    Microsoft,
}

impl Default for DefaultCallingConvention {
    fn default() -> Self {
        DefaultCallingConvention::None
    }
}

impl FunctionAbiReference<Location> for DefaultCallingConvention {
    fn argument_at(&mut self, i: usize) -> Option<Location> {
        match self {
            DefaultCallingConvention::None => None,
            DefaultCallingConvention::SystemV => {
                [
                    Location::Register(RegSpec::rdi()),
                    Location::Register(RegSpec::rsi()),
                    Location::Register(RegSpec::rdx()),
                    Location::Register(RegSpec::rcx()),
                    Location::Register(RegSpec::r8()),
                    Location::Register(RegSpec::r9())
                ].get(i).cloned()
            },
            DefaultCallingConvention::Microsoft => {
                [
                    Location::Register(RegSpec::rcx()),
                    Location::Register(RegSpec::rdx()),
                    Location::Register(RegSpec::r8()),
                    Location::Register(RegSpec::r9())
                ].get(i).cloned()
            }
        }
    }
    fn return_at(&mut self, i: usize) -> Option<Location> {
        if i == 0 {
            Some(Location::Register(RegSpec::rax()))
        } else {
            None
        }
    }
    fn clobber_at(&mut self, _: usize) -> Option<Location> {
        None
    }
    fn return_address(&mut self) -> Option<Location> {
        Some(Location::Register(RegSpec::rip()))
    }
}

impl AbiDefaults for Location {
    type AbiDefault = DefaultCallingConvention;
}

impl ValueLocations for x86_64 {
    type Location = Location;

    fn decompose(instr: &Self::Instruction) -> Vec<(Option<Self::Location>, Direction)> {
        crate::arch::x86_64::semantic::specialized::data_flow::decompose_locations(instr)
    }
}
