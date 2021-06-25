use arch::{FunctionImpl, FunctionQuery};
use arch::{AbiDefaults, FunctionAbiReference};
use analyses::static_single_assignment::{DFGRef, SSAValues, Value};

use std::rc::Rc;
use std::fmt;
use std::cell::RefCell;

use std::collections::HashMap;
use analyses::static_single_assignment::HashedValue;
use serialize::Memoable;
use data::{Direction, Disambiguator, ValueLocations};
use data::types::{TypeAtlas, TypeSpec, Typed};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde::de::{self, Visitor, Unexpected};
use yaxpeax_arm::armv7::{ARMv7, Operand};
use analyses::data_flow::Use;
use std::hash::{Hasher, Hash};

#[derive(Clone, Copy, PartialEq, Hash, Eq)]
pub enum Location {
    Register(u8),
    Memory,
    NF, ZF, CF, VF,
}

impl Location {
    pub fn pc() -> Self {
        Location::Register(15)
    }

    pub fn lr() -> Self {
        Location::Register(14)
    }

    pub fn sp() -> Self {
        Location::Register(13)
    }

    pub fn ip() -> Self {
        Location::Register(12)
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
                let reg: u8 = serde_json::from_str(regstr).unwrap();
                check_end(2, parts)?;
                Ok(Location::Register(reg))
            },
            "M" => {
                check_end(1, parts)?;
                Ok(Location::Memory)
            },
            "n" => { check_end(1, parts)?; Ok(Location::NF) }
            "z" => { check_end(1, parts)?; Ok(Location::ZF) }
            "c" => { check_end(1, parts)?; Ok(Location::CF) }
            "v" => { check_end(1, parts)?; Ok(Location::VF) }
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
            Location::Memory => {
                serialized_loc.push_str("M");
            }
            Location::NF => { serialized_loc.push_str("n"); }
            Location::ZF => { serialized_loc.push_str("z"); }
            Location::CF => { serialized_loc.push_str("c"); }
            Location::VF => { serialized_loc.push_str("v"); }
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
            Location::Register(reg) => write!(f, "r{}", reg),
            Location::Memory => write!(f, "mem"),
            Location::NF => write!(f, "nf"),
            Location::ZF => write!(f, "zf"),
            Location::CF => write!(f, "cf"),
            Location::VF => write!(f, "vf"),
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
        // ARMv7 aliasing rules are very simple!
        //
        // TODO: precise `Memory` model, as well as APSR?
        vec![self.clone()]
    }
    fn maximal_alias_of(&self) -> Self {
        self.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Data {
    Concrete(u32),
    Alias(Rc<RefCell<Value<ARMv7>>>),
}

impl Hash for Data {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Data::Concrete(ref value) => {
                state.write_u8(1);
                value.hash(state);
            }
            Data::Alias(ref value) => {
                state.write_u8(2);
                value.borrow().hash(state);
            }
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum DataMemo {
    Concrete(u32),
    Alias(u32),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ValueMemo {
    pub location: Location,
    pub version: Option<u32>,
    pub data: Option<DataMemo>
}

impl Typed for Data {
    fn type_of(&self, _type_atlas: &TypeAtlas) -> TypeSpec {
        TypeSpec::Bottom
    }
}

impl Memoable for HashedValue<Rc<RefCell<Value<ARMv7>>>> {
    type Out = ValueMemo;

    fn memoize(&self, memos: &HashMap<Self, u32>) -> Self::Out {
        fn memoize_data(data: &Data, memos: &HashMap<HashedValue<DFGRef<ARMv7>>, u32>) -> DataMemo {
            match data {
                Data::Concrete(v) => DataMemo::Concrete(*v),
                Data::Alias(ptr) => DataMemo::Alias(memos[&HashedValue { value: Rc::clone(ptr) }])
            }
        }

        let selfref: &Value<ARMv7> = &*(&*self.value.borrow());
        let newdata = selfref.data.as_ref().map(|data| memoize_data(data, memos));

        ValueMemo {
            location: selfref.location,
            version: selfref.version,
            data: newdata
        }
    }

    fn dememoize(_idx: u32, _memos: &[Self::Out], _dememoized: &mut HashMap<u32, Self>) -> Self {
        unimplemented!("data_flow::Memoable::dememoize");
    }
}

impl SSAValues for ARMv7 {
    type Data = Data;
}

#[derive(Default)]
pub struct NoDisambiguation {}
impl Disambiguator<Location, (u8, u8)> for NoDisambiguation {
    fn disambiguate(&mut self, _spec: (u8, u8)) -> Option<Location> {
        None
    }
}

pub struct LocationIter<'a, 'b, 'c, D: Disambiguator<Location, (u8, u8)> + ?Sized, F: FunctionQuery<<yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Address> + ?Sized> {
    // TODO:
    _addr: <yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Address,
    inst: &'a yaxpeax_arm::armv7::Instruction,
    op_count: u8,
    op_idx: u8,
    loc_count: u8,
    loc_idx: u8,
    curr_op: Option<yaxpeax_arm::armv7::Operand>,
    curr_use: Option<Use>,
    disambiguator: &'b mut D,
    // TODO:
    _fn_query: &'c F,
}

impl <'a, 'b, 'c, D: Disambiguator<Location, (u8, u8)> + ?Sized, F: FunctionQuery<<yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Address>> LocationIter<'a, 'b, 'c, D, F> {
    pub fn new(_addr: <yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Address, inst: &'a yaxpeax_arm::armv7::Instruction, disambiguator: &'b mut D, _fn_query: &'c F) -> Self {
        LocationIter {
            _addr,
            inst,
            op_count: operands_in(inst),
            op_idx: 0,
            loc_count: implicit_locs(inst.opcode),
            loc_idx: 0,
            curr_op: None,
            curr_use: None,
            disambiguator,
            _fn_query,
        }
    }
    // TODO:
    #[allow(dead_code)]
    fn curr_loc(&self) -> (u8, u8) {
        (self.op_idx, self.loc_idx - 1)
    }
}

fn loc_by_id(_idx: u8, _usage: Use, _op: &Operand) -> Option<(Option<Location>, Direction)> {
    unimplemented!("data flow analysis for armv7");
}
fn implicit_loc(_op: yaxpeax_arm::armv7::Opcode, _: u8) -> (Option<Location>, Direction) {
    (None, Direction::Read)
}
fn implicit_locs(_op: yaxpeax_arm::armv7::Opcode) -> u8 {
    0
}

fn locations_in(_op: &yaxpeax_arm::armv7::Operand, _usage: Use) -> u8 {
    unimplemented!("data flow analysis for armv7");
}

fn operands_in(_inst: &yaxpeax_arm::armv7::Instruction) -> u8 {
    unimplemented!("data flow analysis for armv7");
}

impl <'a, 'b, 'c, D: Disambiguator<Location, (u8, u8)>, F: FunctionQuery<<yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Address>> Iterator for LocationIter<'a, 'b, 'c, D, F> {
    type Item = (Option<Location>, Direction);
    fn next(&mut self) -> Option<Self::Item> {
        fn next_loc<'a, 'b, 'c, D: Disambiguator<Location, (u8, u8)>, F: FunctionQuery<<yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Address>>(iter: &mut LocationIter<'a, 'b, 'c, D, F>) -> Option<(Option<Location>, Direction)> {
            while iter.loc_count == iter.loc_idx {
                // advance op
                iter.op_idx += 1;

                if iter.op_idx >= iter.op_count {
                    // but we're at the last op, so we're actually done...
                    return None;
                }
    //            println!("opc: {}", iter.inst.opcode);

                let op = &iter.inst.operands[0];
                let op_use = Use::Read; //use_of(iter.inst, iter.op_idx - 1);
                iter.loc_count = locations_in(&op, op_use);
                iter.loc_idx = 0;
                iter.curr_op = Some(op.clone());
                iter.curr_use = Some(op_use);
            }

            if iter.op_idx == 0 {
                iter.loc_idx += 1;
                return Some(implicit_loc(iter.inst.opcode, iter.loc_idx - 1));
            }

            if let Some(op) = &iter.curr_op {
                iter.loc_idx += 1;
                loc_by_id(iter.loc_idx - 1, iter.curr_use.unwrap(), op)
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
impl <'a, 'b, 'c, D: 'b + Disambiguator<Location, (u8, u8)>, F: 'c + FunctionQuery<<yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Address, Function=FunctionImpl<Location>>> crate::data::LocIterator<'b, 'c, <yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Address, Location, D, F> for &'a yaxpeax_arm::armv7::Instruction {
    type Item = (Option<Location>, Direction);
    type LocSpec = (u8, u8);
    type Iter = LocationIter<'a, 'b, 'c, D, F>;
    fn iter_locs(self, addr: <yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Address, disam: &'b mut D, functions: &'c F) -> Self::Iter {
        LocationIter::new(addr, self, disam, functions)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DefaultCallingConvention {
    None,
    Standard,
}

impl FunctionAbiReference<Location> for DefaultCallingConvention {
    fn argument_at(&mut self, i: usize) -> Option<Location> {
        match self {
            DefaultCallingConvention::None => None,
            DefaultCallingConvention::Standard => {
                [
                    Location::Register(0),
                    Location::Register(1),
                    Location::Register(2),
                    Location::Register(3)
                ].get(i).cloned().or(Some(Location::Memory))
            }
        }
    }
    fn return_at(&mut self, i: usize) -> Option<Location> {
        if i == 0 {
            Some(Location::Register(0))
        } else {
            None
        }
    }
    fn clobber_at(&mut self, _: usize) -> Option<Location> {
        None
    }
    fn return_address(&mut self) -> Option<Location> {
        Some(Location::Register(15))
    }
}

impl Default for DefaultCallingConvention {
    fn default() -> Self {
        DefaultCallingConvention::None
    }
}

impl AbiDefaults for Location {
    type AbiDefault = DefaultCallingConvention;
}

impl ValueLocations for ARMv7 {
    type Location = Location;

    fn decompose(_instr: &Self::Instruction) -> Vec<(Option<Self::Location>, Direction)> {
        unimplemented!("data flow analysis for armv7");
    }
}
