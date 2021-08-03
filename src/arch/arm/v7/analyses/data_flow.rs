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

use crate::ColorSettings;
impl<'data, 'colors> crate::analyses::static_single_assignment::DataDisplay<'data, 'colors> for Data {
    type Displayer = &'static str;
    fn display(&'data self, detailed: bool, colors: Option<&'colors ColorSettings>) -> &'static str {
        unimplemented!()
    }
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
impl Disambiguator<yaxpeax_arm::armv7::ARMv7, (u8, u8)> for NoDisambiguation {
    fn disambiguate(&self, _instr: &<yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Instruction, _loc: (Option<Location>, Direction), _spec: (u8, u8)) -> Option<Location> {
        None
    }
}

impl crate::data::LocationAliasDescriptions<yaxpeax_arm::armv7::ARMv7> for NoDisambiguation {
    fn may_alias(&self, _left: &Location, _right: &Location) -> bool {
        false
    }

    fn aliases_for(&self, loc: &Location) -> Vec<Location> {
        loc.aliases_of()
    }
}

pub struct LocationIter<'a, 'b, 'c, D: Disambiguator<yaxpeax_arm::armv7::ARMv7, (u8, u8)> + ?Sized, F: FunctionQuery<<yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Address> + ?Sized> {
    // TODO:
    _addr: <yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Address,
    inst: &'a yaxpeax_arm::armv7::Instruction,
    op_count: u8,
    op_idx: u8,
    loc_count: u8,
    loc_idx: u8,
    curr_op: Option<yaxpeax_arm::armv7::Operand>,
    curr_use: Option<Use>,
    disambiguator: &'b D,
    // TODO:
    _fn_query: &'c F,
}

impl <'a, 'b, 'c, D: Disambiguator<yaxpeax_arm::armv7::ARMv7, (u8, u8)> + ?Sized, F: FunctionQuery<<yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Address>> LocationIter<'a, 'b, 'c, D, F> {
    pub fn new(_addr: <yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Address, inst: &'a yaxpeax_arm::armv7::Instruction, disambiguator: &'b D, _fn_query: &'c F) -> Self {
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
    panic!("bad");
    /*
    match op {
        Operands::RegisterList(list) => {
            let mut item = 0u8;
            let mut i = 0u8;
            while i < 16 {
                if (*list & (1 << i)) != 0 {
                    if item == idx {
                        return Some((Some(Location::Register(item)), usage.first_use()));
                    } else {
                        item += 1;
                    }
                }
                i += 1;
            }
            None
        }
        Operands::OneOperand(reg) => {
            if idx == 0 {
                Some((Some(Location::Register(*reg)), usage.first_use()))
            } else {
                None
            }
        }
        Operands::TwoOperand(rd, rs) => {
            if idx == 0 {
                Some((Some(Location::Register(*rd)), usage.first_use()))
            } else if idx == 1 {
                Some((Some(Location::Register(*rs)), usage.first_use()))
            } else {
                None
            }
        }
        Operands::RegImm(reg, imm) => {
            if idx == 0 {
                Some((Some(Location::Register(*reg)), usage.first_use()))
            } else {
                None
            }
        }
        Operands::RegRegList(reg, list) => {
            if idx == 0 {
                return Some((Some(Location::Register(*reg)), usage.first_use()));
            }
            let mut item = 1u8;
            let mut i = 0u8;
            while i < 16 {
                if (*list & (1 << i)) != 0 {
                    if item == idx {
                        return Some((Some(Location::Register(item)), usage.first_use()));
                    } else {
                        item += 1;
                    }
                }
                i += 1;
            }
            None
        }
        Operands::TwoRegImm(_rs, _rd, _imm) => {
            None
        }
        Operands::BranchOffset(_offs) => {
            None
        }
        _ => None
    }
    */
}
fn implicit_loc(_op: yaxpeax_arm::armv7::Opcode, _: u8) -> (Option<Location>, Direction) {
    (None, Direction::Read)
}
fn implicit_locs(_op: yaxpeax_arm::armv7::Opcode) -> u8 {
    0
}

fn locations_in(_op: &yaxpeax_arm::armv7::Operand, _usage: Use) -> u8 {
    panic!("bad bad");
    /*
    match op {
        Operands::RegisterList(list) => {
            list.count_ones() as u8
        }
        Operands::OneOperand(_) => {
            1
        }
        Operands::TwoOperand(_, _) => {
            2
        }
        Operands::RegImm(_, _) => {
            1
        }
        Operands::RegRegList(_, list) => {
            1 + (list.count_ones() as u8)
        }
        Operands::TwoRegImm(_, _, _) => {
            2
        }
        Operands::ThreeOperand(_, _, _) => {
            3
        }
        Operands::ThreeOperandImm(_, _, _) => {
            2
        }
        Operands::ThreeOperandWithShift(_, _, _, _) => {
            3
        }
        Operands::MulThreeRegs(_, _, _) => {
            3
        }
        Operands::MulFourRegs(_, _, _, _) => {
            4
        }
        Operands::BranchOffset(_) => {
            0
        }
    }
    */

}

fn operands_in(_inst: &yaxpeax_arm::armv7::Instruction) -> u8 {
    panic!("bad bad bad");
    /*
    match inst.operands {
        Operands::RegisterList(list) => {
            list.count_ones() as u8
        }
        Operands::OneOperand(_) => {
            1
        }
        Operands::TwoOperand(_, _) => {
            2
        }
        Operands::RegImm(_, _) => {
            1
        }
        Operands::RegRegList(_, list) => {
            1 + (list.count_ones() as u8)
        }
        Operands::TwoRegImm(_, _, _) => {
            2
        }
        Operands::ThreeOperand(_, _, _) => {
            3
        }
        Operands::ThreeOperandImm(_, _, _) => {
            2
        }
        Operands::ThreeOperandWithShift(_, _, _, _) => {
            3
        }
        Operands::MulThreeRegs(_, _, _) => {
            3
        }
        Operands::MulFourRegs(_, _, _, _) => {
            4
        }
        Operands::BranchOffset(_) => {
            0
        }
    }
    */
}

impl <'a, 'b, 'c, D: Disambiguator<yaxpeax_arm::armv7::ARMv7, (u8, u8)>, F: FunctionQuery<<yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Address>> Iterator for LocationIter<'a, 'b, 'c, D, F> {
    type Item = (Option<Location>, Direction);
    fn next(&mut self) -> Option<Self::Item> {
        fn next_loc<'a, 'b, 'c, D: Disambiguator<yaxpeax_arm::armv7::ARMv7, (u8, u8)>, F: FunctionQuery<<yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Address>>(iter: &mut LocationIter<'a, 'b, 'c, D, F>) -> Option<(Option<Location>, Direction)> {
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
            self.disambiguator.disambiguate(self.inst, loc, loc_spec).map(|new_loc| (Some(new_loc), loc.1)).unwrap_or(loc)
        })
    }
}
impl <'a, 'b, 'c, D: 'b + Disambiguator<yaxpeax_arm::armv7::ARMv7, (u8, u8)>, F: 'c + FunctionQuery<<yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Address, Function=FunctionImpl<Location>>> crate::data::LocIterator<'b, 'c, yaxpeax_arm::armv7::ARMv7, Location, D, F> for &'a yaxpeax_arm::armv7::Instruction {
    type Item = (Option<Location>, Direction);
    type LocSpec = (u8, u8);
    type Iter = LocationIter<'a, 'b, 'c, D, F>;
    fn iter_locs(self, addr: <yaxpeax_arm::armv7::ARMv7 as yaxpeax_arch::Arch>::Address, disam: &'b D, functions: &'c F) -> Self::Iter {
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
        panic!("value locations");
        /*
        fn decompose_write(op: &Operands) -> Vec<(Option<Location>, Direction)> {
            match op {
                Operands::RegisterList(regs) => {
                    let mut writes = Vec::new();
                    for i in 0..16 {
                        if regs & (1 << i) != 0 {
                            writes.push(((Some(Location::Register(i))), Direction::Write));
                        }
                    }
                    writes
                }
                Operands::OneOperand(reg) => {
                    vec![(Some(Location::Register(*reg)), Direction::Write)]
                }
                Operands::TwoOperand(_source, dest) => {
                    vec![(Some(Location::Register(*dest)), Direction::Write)]
                }
                Operands::RegImm(reg, _imm) => {
                    vec![(Some(Location::Register(*reg)), Direction::Write)]
                }
                Operands::TwoRegImm(dest, _source, _imm) => {
                    vec![(Some(Location::Register(*dest)), Direction::Write)]
                }
                // TODO...
                o => {
                    event!(Level::ERROR, opcode = ?o, component = "instruction description", "missing operand decomposition for write {:?}", o);
                    Vec::new()
                }
            }
        }
        fn decompose_read(op: &Operands) -> Vec<(Option<Location>, Direction)> {
            match op {
                Operands::RegisterList(regs) => {
                    let mut writes = Vec::new();
                    for i in 0..16 {
                        if regs & (1 << i) != 0 {
                            writes.push(((Some(Location::Register(i))), Direction::Read));
                        }
                    }
                    writes
                }
                Operands::OneOperand(reg) => {
                    vec![(Some(Location::Register(*reg)), Direction::Read)]
                }
                Operands::TwoOperand(source, _dest) => {
                    vec![(Some(Location::Register(*source)), Direction::Read)]
                }
                Operands::RegImm(reg, _imm) => {
                    vec![(Some(Location::Register(*reg)), Direction::Read)]
                }
                Operands::TwoRegImm(_dest, source, _imm) => {
                    vec![(Some(Location::Register(*source)), Direction::Read)]
                }
                // TODO...
                o => {
                    event!(Level::ERROR, opcode = ?o, component = "instruction description", "missing operand decomposition for write {:?}", o);
                    Vec::new()
                }
            }
        }
        let mut parts = match instr.opcode {
            Opcode::Incomplete(_) |
            Opcode::Invalid => {
                // TODO: error handling?
                Vec::new()
            }
    /*
     * These two don't really have direct encodings, but are for the specific instances
     * where the semantics of the original instruction are the same as push (specifically
     * ldm/stm/mov that write to the stack and increment/decrement appropriately
     */
            Opcode::POP => {
                let mut parts = decompose_write(&instr.operands);
                parts.push((Some(Location::Register(13)), Direction::Read));
                parts.push((Some(Location::Register(13)), Direction::Write));
                parts
            }
            Opcode::PUSH => {
                let mut parts = decompose_read(&instr.operands);
                parts.push((Some(Location::Register(13)), Direction::Read));
                parts.push((Some(Location::Register(13)), Direction::Write));
                parts
            }

            Opcode::B => {
                decompose_read(&instr.operands)
            }
            Opcode::BL => {
                let mut parts = decompose_read(&instr.operands);
                parts.push((Some(Location::Register(14)), Direction::Write));
                parts
            }
            Opcode::BLX => {
                // TODO: note thumb state change?
                let mut parts = decompose_read(&instr.operands);
                parts.push((Some(Location::Register(14)), Direction::Write));
                parts
            }
            Opcode::BX => {
                decompose_read(&instr.operands)
            }
            Opcode::BXJ => {
                panic!("jazelle instructions not supported");
            }
            Opcode::AND |
            Opcode::EOR |
            Opcode::SUB |
            Opcode::RSB |
            Opcode::ADD |
            Opcode::ADC |
            Opcode::SBC |
            Opcode::RSC |
            Opcode::CMN |
            Opcode::ORR |
            Opcode::BIC |
            Opcode::MVN |
            Opcode::LSL |
            Opcode::LSR |
            Opcode::ASR |
            Opcode::RRX |
            Opcode::ROR |
            Opcode::ADR => {
                let mut parts = decompose_read(&instr.operands);
                parts.append(&mut decompose_write(&instr.operands));
                parts
            }
            Opcode::MOV => {
                // TODO: MOV imm may set C!
                let mut parts = decompose_write(&instr.operands);
                parts
            }

            Opcode::LDR(_, _, _) |
            Opcode::LDRB(_, _, _) => {
                let mut parts = decompose_write(&instr.operands);
                parts
            }
            Opcode::STR(_, _, _) |
            Opcode::STRB(_, _, _) => {
                let mut parts = decompose_read(&instr.operands);
                parts
            }

            Opcode::TEQ |
            Opcode::CMP |
            Opcode::TST => {
                let mut parts = decompose_read(&instr.operands);
                parts.push((Some(Location::NF), Direction::Write));
                parts.push((Some(Location::ZF), Direction::Write));
                parts.push((Some(Location::CF), Direction::Write));
                parts.push((Some(Location::VF), Direction::Write));
                parts
            }
            o => {
                event!(Level::ERROR, opcode = ?o, component = "instruction description", "missing operand decomposition");
                vec![]
            }
        };

        parts.push((Some(Location::Register(15)), Direction::Read));
        parts.push((Some(Location::Register(15)), Direction::Write));
        parts

        */
    }
}
