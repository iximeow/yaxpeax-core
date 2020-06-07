use yaxpeax_arm::armv8::a64::ARMv8;
use analyses::static_single_assignment::DFGRef;
use analyses::static_single_assignment::SSAValues;
use analyses::static_single_assignment::HashedValue;
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::hash::{Hash, Hasher};
use arch::FunctionAbiReference;
use arch::AbiDefaults;
use analyses::static_single_assignment::Value;
use data::ValueLocations;
use data::Direction;
use data::types::{Typed, TypeAtlas, TypeSpec};
use serialize::Memoable;
use analyses::data_flow::Use;
use yaxpeax_arm::armv8::a64::Operand;
use data::Disambiguator;
use arch::FunctionQuery;
use arch::FunctionImpl;

pub struct LocationIter<'a, 'b, 'c, D: Disambiguator<Location, (u8, u8)> + ?Sized, F: FunctionQuery<<ARMv8 as yaxpeax_arch::Arch>::Address> + ?Sized> {
    _addr: <ARMv8 as yaxpeax_arch::Arch>::Address,
    inst: &'a yaxpeax_arm::armv8::a64::Instruction,
    op_count: u8,
    op_idx: u8,
    loc_count: u8,
    loc_idx: u8,
    curr_op: Option<yaxpeax_arm::armv8::a64::Operand>,
    curr_use: Option<Use>,
    disambiguator: &'b mut D,
    _fn_query: &'c F,
}

#[allow(dead_code)]
impl <'a, 'b, 'c, D: Disambiguator<Location, (u8, u8)> + ?Sized, F: FunctionQuery<<ARMv8 as yaxpeax_arch::Arch>::Address>> LocationIter<'a, 'b, 'c, D, F> {
    pub fn new(_addr: <ARMv8 as yaxpeax_arch::Arch>::Address, inst: &'a yaxpeax_arm::armv8::a64::Instruction, disambiguator: &'b mut D, _fn_query: &'c F) -> Self {
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
fn implicit_loc(_op: yaxpeax_arm::armv8::a64::Opcode, _i: u8) -> (Option<Location>, Direction) {
    (None, Direction::Read)
}
fn implicit_locs(_op: yaxpeax_arm::armv8::a64::Opcode) -> u8 {
    0
}

fn locations_in(_op: &yaxpeax_arm::armv8::a64::Operand, _usage: Use) -> u8 {
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

fn operands_in(_inst: &yaxpeax_arm::armv8::a64::Instruction) -> u8 {
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

impl <'a, 'b, 'c, D: Disambiguator<Location, (u8, u8)>, F: FunctionQuery<<ARMv8 as yaxpeax_arch::Arch>::Address>> Iterator for LocationIter<'a, 'b, 'c, D, F> {
    type Item = (Option<Location>, Direction);
    fn next(&mut self) -> Option<Self::Item> {
        fn next_loc<'a, 'b, 'c, D: Disambiguator<Location, (u8, u8)>, F: FunctionQuery<<ARMv8 as yaxpeax_arch::Arch>::Address>>(iter: &mut LocationIter<'a, 'b, 'c, D, F>) -> Option<(Option<Location>, Direction)> {
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
impl <'a, 'b, 'c, D: 'b + Disambiguator<Location, (u8, u8)>, F: 'c + FunctionQuery<<ARMv8 as yaxpeax_arch::Arch>::Address, Function=FunctionImpl<Location>>> crate::data::LocIterator<'b, 'c, <ARMv8 as yaxpeax_arch::Arch>::Address, Location, D, F> for &'a yaxpeax_arm::armv8::a64::Instruction {
    type Item = (Option<Location>, Direction);
    type LocSpec = (u8, u8);
    type Iter = LocationIter<'a, 'b, 'c, D, F>;
    fn iter_locs(self, addr: <ARMv8 as yaxpeax_arch::Arch>::Address, disam: &'b mut D, functions: &'c F) -> Self::Iter {
        LocationIter::new(addr, self, disam, functions)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum DefaultCallingConvention {
    None,
}

impl FunctionAbiReference<Location> for DefaultCallingConvention {
    fn argument_at(&mut self, _: usize) -> Option<Location> {
        match self {
            DefaultCallingConvention::None => None,
        }
    }
    fn return_at(&mut self, _: usize) -> Option<Location> {
        None
    }
    fn clobber_at(&mut self, _: usize) -> Option<Location> {
        None
    }
    fn return_address(&mut self) -> Option<Location> {
        None
    }
}

#[derive(Clone, Copy, PartialEq, Hash, Eq, Debug, Serialize, Deserialize)]
pub enum Location {
    Register(u8),
    Memory,
    PC,
    SP,
    NF, ZF, CF, VF,
}

impl Default for DefaultCallingConvention {
    fn default() -> Self {
        DefaultCallingConvention::None
    }
}

impl AbiDefaults for Location {
    type AbiDefault = DefaultCallingConvention;
}

impl SSAValues for ARMv8 {
    type Data = Data;
}

#[derive(Default)]
pub struct NoDisambiguation {}
impl Disambiguator<Location, (u8, u8)> for NoDisambiguation {
    fn disambiguate(&mut self, _spec: (u8, u8)) -> Option<Location> {
        None
    }
}

use data::AliasInfo;
impl AliasInfo for Location {
    fn aliases_of(&self) -> Vec<Self> {
        // TODO: this
        vec![self.clone()]
    }
    fn maximal_alias_of(&self) -> Self {
        self.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Data {
    Concrete(u64),
    Alias(Rc<RefCell<Value<ARMv8>>>),
}

impl Hash for Data {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Data::Concrete(value) => {
                state.write_u8(1);
                value.hash(state);
            }
            Data::Alias(value) => {
                state.write_u8(2);
                value.borrow().hash(state);
            }
        }
    }
}

impl ValueLocations for ARMv8 {
    type Location = Location;

    fn decompose(_instr: &Self::Instruction) -> Vec<(Option<Self::Location>, Direction)> {
        vec![]
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum DataMemo {
    Concrete(u64),
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

impl Memoable for HashedValue<Rc<RefCell<Value<ARMv8>>>> {
    type Out = ValueMemo;

    fn memoize(&self, memos: &HashMap<Self, u32>) -> Self::Out {
        fn memoize_data(data: &Data, memos: &HashMap<HashedValue<DFGRef<ARMv8>>, u32>) -> DataMemo {
            match data {
                Data::Concrete(v) => DataMemo::Concrete(*v),
                Data::Alias(ptr) => DataMemo::Alias(memos[&HashedValue { value: Rc::clone(ptr) }])
            }
        }

        let selfref: &Value<ARMv8> = &*(&*self.value.borrow());
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
