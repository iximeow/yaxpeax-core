use yaxpeax_arch::Arch;
use std::collections::HashMap;
use analyses::DFG;
use data::ValueLocations;
use analyses::static_single_assignment::{DFGRef, SSA, SSAValues};
use std::cell::RefCell;
use std::rc::Rc;
use std::fmt;

/// some kind of a description of memory usage in a dfg
#[derive(Debug)]
pub struct MemoryRegion<A: Arch + ValueLocations + SSAValues> where A::Data: Eq + fmt::Display {
    /// accesses is a collection of memory accesses, read or write, to delineate this region. it is
    /// both the offset into this region where the access occurs, and the size of the region.
    ///
    /// perhaps one day this will map to values such that a unified data flow graph can exist
    /// describing aliasable (memory) and non-aliasable (register) regions.
    ///
    /// clearly, registers in some machines may alias; the complicating distinction is that memory
    /// has unbounded aliasing and indirection, where registers in most architectures are not
    /// indirectly accessed and have a well-defined set of aliasing rules (f.ex x86's
    /// rax->ecx->cx->{cl,ch})
//    accesses: Vec<usize>,
    // revised: offset -> size -> read/write
    // this is to support cases where an offset is accessed by two different sizes
    // as might be the case for unions or truncation.
    // TODO: these could be keyed by A::Address
    pub accesses: HashMap<Rc<Item<ValueOrImmediate<A>>>, HashMap<u64, UsePattern>>,
    // TODO: how to avoid recording memcpy-like accesses that disregard internal structure? does
    // that get resolved here, or by MemoryLayout? something else entirely? hm...
}

impl<A: Arch + ValueLocations + SSAValues> Clone for MemoryRegion<A> where A::Data: Eq + fmt::Display {
    fn clone(&self) -> Self {
        Self {
            accesses: self.accesses.clone(),
        }
    }
}

// #[repr(transparent)]
#[derive(Copy, Clone)]
pub struct UsePattern(pub u8);

impl fmt::Debug for UsePattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (self.is_read(), self.is_write()) {
            (false, false) => f.write_str("(no access)"),
            (false, true) => f.write_str("{ write }"),
            (true, false) => f.write_str("{ read }"),
            (true, true) => f.write_str("{ read, write }"),
        }
    }
}

#[allow(dead_code)]
impl UsePattern {
    fn none() -> Self {
        UsePattern(0)
    }
    fn read() -> Self {
        Self::none().with_read()
    }
    fn write() -> Self {
        Self::none().with_write()
    }
    fn merge(&self, other: &UsePattern) -> Self {
        UsePattern(self.0 | other.0)
    }
    fn with_read(mut self) -> Self {
        self.mark_read();
        self
    }
    fn with_write(mut self) -> Self {
        self.mark_write();
        self
    }
    fn mark_read(&mut self) {
        self.0 |= 1;
    }
    fn mark_write(&mut self) {
        self.0 |= 2;
    }
    pub fn is_read(&self) -> bool {
        (self.0 & 1) != 0
    }
    pub fn is_write(&self) -> bool {
        (self.0 & 2) != 0
    }
}

impl<A: Arch + ValueLocations + SSAValues> MemoryRegion<A> where A::Data: Eq + fmt::Display {
    fn new() -> Self {
        Self {
            accesses: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct IndirectLayout<A: Arch + ValueLocations + SSAValues> where A::Data: Eq + fmt::Display {
    /// a mapping to distinct regions in this indirect area from the base values inferred for their
    /// accesses.
    ///
    /// the expectation^Whope is that analyses can canonicalize effective addresses so the
    /// least-bound value can be used as a base, and bound addends can restrict the aliasing set so
    /// a write to some value does not necessarily cause all referents through `A::Location` to be
    /// clobbered.
    regions_uses: Option<Rc<RefCell<HashMap<ValueOrImmediate<A>, MemoryRegion<A>>>>>,
    regions_defs: Option<Rc<RefCell<HashMap<ValueOrImmediate<A>, MemoryRegion<A>>>>>,
    ssa_use: Option<DFGRef<A>>,
    ssa_def: Option<DFGRef<A>>,
}

pub struct MemoryLayout<'ssa, A: Arch + ValueLocations + SSAValues> where A::Data: Eq + fmt::Display {
    pub ssa: &'ssa SSA<A>,
    /// map SSA values at some `A::Location` to their referent layout.
    /// key is likely versions of an architecture's Location::Memory.
    pub segments: RefCell<HashMap<ValueOrImmediate<A>, Rc<RefCell<HashMap<ValueOrImmediate<A>, MemoryRegion<A>>>>>>,
}

/*
#[derive(Copy, Clone)]
pub enum LocationAccess<A: Arch> {
    // access, and its size
    Access(A::Address),
}
*/

/// in most cases, `Base` and `Addend` should both be `Self`.
pub trait MemoryAccessBaseInference {
    /// type of the base address for some. as a common example, the value of a machine's
    /// stack pointer will often be of this type.
    type Base;
    /// type of the offsets for some access. this is defined with respect to `Self::Base`;
    /// it's possible to simply say `Self::Addend` is always zero, but memory layout inference will
    /// be better behaved if `Self::Addend` can be selected so that a common `Self::Base` is
    /// reported. as a common example, offsets for stack-local storage probably should be
    /// represented by `Addend`. additionally, value sets in registers would make for good
    /// `Self::Addend` candidates.
    type Addend;

    /// given some `Value`, `self`, try to interpret `self` as a base plus addend-style memory access. the base should be a constant or a fully-unbounded variable, where `addend` may be a constant, bounded variable, or value set.
    fn infer_base_and_addend(&self) -> Option<(Self::Base, Self::Addend)>;
}

/// allow `Self` to have simple aliases for other values of the same type.
/// for example, an x86_64 `Data` can be `Data::Alias(DFGRef<x86_64>)`. for all purposes other
/// than display reasoning, these values should be operated on as if they were the underlying
/// aliased location.
pub trait Underlying {
    type Arch: yaxpeax_arch::Arch + SSAValues;

    fn underlying(&self) -> Option<DFGRef<Self::Arch>>;

    fn expression(&self) -> Option<Rc<Item<ValueOrImmediate<Self::Arch>>>> where <<Self as Underlying>::Arch as SSAValues>::Data: Eq + fmt::Display;
}

impl<A: SSAValues> MemoryAccessBaseInference for Rc<Item<ValueOrImmediate<A>>> where A::Data: Underlying<Arch=A> + Eq + fmt::Display {
    type Base = Self;
    type Addend = Self;

    fn infer_base_and_addend(&self) -> Option<(Self::Base, Self::Addend)> {
//        println!("impl infer_base_and_addend: {:?}", self);
        let res = match &self.value.dealiased() {
            Expression::Unknown => None,
            Expression::Value(ValueOrImmediate::Value(v)) => {
                if let Some(expr) = v.borrow().data.as_ref().and_then(|x| x.expression()) {
                    expr.infer_base_and_addend()
                } else {
                    Some((Item::value(ValueOrImmediate::Value(Rc::clone(v))), Item::untyped(Expression::Value(ValueOrImmediate::Immediate(0)))))
                }
            }
            Expression::Value(ValueOrImmediate::Immediate(_i)) => None,
            Expression::Add { left, right } => {
                if let Expression::Value(ValueOrImmediate::Immediate(i)) = right.value {
                    if let Some((inner_base, inner_addend)) = left.infer_base_and_addend() {
                        if let Expression::Value(ValueOrImmediate::Immediate(j)) = inner_addend.value {
                            Some((inner_base.dealiased(), Item::untyped(Expression::Value(ValueOrImmediate::Immediate(i.wrapping_add(j))))))
                        } else {
                            Some((left.dealiased(), right.dealiased()))
                        }
                    } else {
                        Some((left.dealiased(), right.dealiased()))
                    }
                } else {
                    None
                }
            }
            Expression::Sub { left, right } => {
                if let Expression::Value(ValueOrImmediate::Immediate(i)) = right.value {
                    if let Some((inner_base, inner_addend)) = left.infer_base_and_addend() {
                        if let Expression::Value(ValueOrImmediate::Immediate(j)) = inner_addend.value {
                            Some((inner_base.dealiased(), Item::untyped(Expression::Value(ValueOrImmediate::Immediate(i.wrapping_sub(j))))))
                        } else {
                            Some((left.dealiased(), right.dealiased()))
                        }
                    } else {
                        Some((left.dealiased(), right.dealiased()))
                    }
                } else {
                    None
                }
            }
            _ => None,
        };
//        println!("  inferred base and addend: {:?}", res);
        res
    }
}

/*
use yaxpeax_arch::AddressDiff;
use analyses::Value;
impl<A: Arch + SSAValues> Value for LocationAccess<A> {
    fn unknown() -> Self {
        panic!("LocationAccess::unknown");
    }

    fn from_set(_xs: &[Self]) -> Self { Self::unknown() }

    fn from_const(c: u64) -> Self {
        panic!("LocationAccess::from_const");
    }

    fn to_const(&self) -> Option<u64> {
        None
    }
}

impl From<AddressDiff<u64>> for LocationAccess<amd64> {
    fn from(diff: AddressDiff<u64>) -> Self {
        LocationAccess::Const { diff: <amd64 as Arch>::Address::zero().wrapping_offset(diff).to_linear() as u64 }
    }
}
*/

use analyses::{IndirectQuery, ValueIndex};

impl<A: Arch + ValueLocations + SSAValues> IndirectQuery<Rc<Item<ValueOrImmediate<A>>>> for IndirectLayout<A> where A::Data: Underlying<Arch=A> + Eq + fmt::Display {
    fn try_get_load(&self, index: ValueIndex<Rc<Item<ValueOrImmediate<A>>>>) -> Option<Rc<Item<ValueOrImmediate<A>>>> {
        if let Some((base, addend)) = index.base.infer_base_and_addend() {
            // base must be a Value otherwise it's some complex composite, OR unknown, and not
            // eligible for a base of a memory region
            if let Expression::Value(v) = &base.as_ref().value {
                let regions = self.regions_uses.as_ref().expect("indirect load has a corresponding ssa use").borrow();
                let v: ValueOrImmediate<A> = v.clone();
                return regions.get(&v)
                    .and_then(|region| region.accesses.get(&addend))
                    .and_then(|offset| offset.get(&(index.size as u64)))
                    .and_then(|pat| if pat.is_read() {
                        Some(Item::untyped(Expression::Unknown))
                    } else {
                        None
                    });
            }
        }
        None
    }
    fn try_get_store(&self, index: ValueIndex<Rc<Item<ValueOrImmediate<A>>>>) -> Option<()> {
        if let Some((base, addend)) = index.base.infer_base_and_addend() {
            // base must be a Value otherwise it's some complex composite, OR unknown, and not
            // eligible for a base of a memory region
            if let Expression::Value(v) = &base.as_ref().value {
                let regions = self.regions_defs.as_ref().expect("indirect store has a corresponding ssa def").borrow();
                let v: ValueOrImmediate<A> = v.clone();
                return regions.get(&v)
                    .and_then(|region| region.accesses.get(&addend))
                    .and_then(|offset| offset.get(&(index.size as u64)))
                    .and_then(|pat| if pat.is_write() {
                        Some(())
                    } else {
                        None
                    });
            }
        }
        None
    }
    fn load(&self, index: ValueIndex<Rc<Item<ValueOrImmediate<A>>>>) -> Rc<Item<ValueOrImmediate<A>>> {
        if let Some((base, addend)) = index.base.infer_base_and_addend() {
            // base must be a Value otherwise it's some complex composite, OR unknown, and not
            // eligible for a base of a memory region
            if let Expression::Value(v) = &base.as_ref().value {
                let mut regions = self.regions_uses.as_ref().expect("indirect load has a corresponding ssa use").borrow_mut();
                let v: ValueOrImmediate<A> = v.clone();
                let region = regions.entry(v).or_insert_with(MemoryRegion::new);
                // TODO: figure out when addends would read other offsets
                let offset = region.accesses.entry(addend).or_insert_with(HashMap::new);
                let usage = offset.entry(index.size as u64).or_insert_with(UsePattern::none);
                usage.mark_read();
            } else {
                tracing::error!("base of an access ({:?}) is not a Value, but is {:?}", index.base, base);
            }
        } else {
            tracing::error!("unable to infer base/addend of {:?}", index.base);
        }
        Item::untyped(Expression::Unknown)
    }
    fn store(&self, index: ValueIndex<Rc<Item<ValueOrImmediate<A>>>>, _value: &Rc<Item<ValueOrImmediate<A>>>) {
//        eprintln!("store: {:?}", index);
//        eprintln!("old: {:?}", self.regions_uses);
//        eprintln!("new: {:?}", self.regions_defs);
        // copy old stores to this memory
        if let (Some(uses), Some(defs)) = (self.regions_uses.as_ref(), self.regions_defs.as_ref()) {
            let mut defs_mut = defs.borrow_mut();
            for (base, region) in &*uses.borrow() {
                if defs_mut.contains_key(base) {
                    // merge
                    let new_defs = &mut defs_mut.get_mut(base).unwrap().accesses;
                    for (old_offset, accesses) in &region.accesses {
                        if new_defs.contains_key(old_offset.as_ref()) {
                            let new_accesses = new_defs.get_mut(old_offset.as_ref()).unwrap();
                            // again, merge...
                            for (sz, pattern) in accesses {
                                if new_accesses.contains_key(&sz) {
                                    // merge, somehow
                                    let new_pattern = &new_accesses[&sz];
                                    let new_pattern = new_pattern.merge(pattern);
                                    new_accesses.insert(*sz, new_pattern);
                                } else {
                                    new_accesses.insert(*sz, pattern.clone());
                                }
                            }
                        } else {
                            new_defs.insert(old_offset.clone(), accesses.clone());
                        }
                    }
                } else {
                    defs_mut.insert(base.clone(), region.to_owned());
                }
            }
        }

        // TODO: use (store?) `value`, which should get memory analysis in a place where it could
        // try tracking spills/reloads.
        if let Some((base, addend)) = index.base.infer_base_and_addend() {
            // base must be a Value otherwise it's some complex composite, OR unknown, and not
            // eligible for a base of a memory region
            if let Expression::Value(v) = &base.as_ref().value {
                let mut regions = self.regions_defs.as_ref().expect("indirect store has a corresponding ssa def").borrow_mut();
                let v: ValueOrImmediate<A> = v.clone();
                let region = regions.entry(v).or_insert_with(MemoryRegion::new);
                // TODO: figure out when addends would write over other offsets
                let offset = region.accesses.entry(addend).or_insert_with(HashMap::new);
                let usage = offset.entry(index.size as u64).or_insert_with(UsePattern::none);
                usage.mark_write();
            } else {
                tracing::error!("base of an access ({:?}) is not a Value, but is {:?}", index.base, base);
            }
        } else {
            tracing::error!("unable to infer base/addend of {:?}", index.base);
        }
    }
}

impl<'ssa, A: Arch + ValueLocations + SSAValues> MemoryLayout<'ssa, A> where A::Data: Underlying<Arch=A> + Eq + fmt::Display {
    fn get_segment(&self, indirection_value: DFGRef<A>) -> Rc<RefCell<HashMap<ValueOrImmediate<A>, MemoryRegion<A>>>> {
        let mut underlying = Rc::clone(&indirection_value);
        if let Some(inner) = indirection_value.borrow().data.as_ref().and_then(|x| x.underlying()) {
            underlying = inner;
        }

        let segment_base = ValueOrImmediate::Value(underlying);
        Rc::clone(self.segments.borrow_mut().entry(segment_base)
            .or_insert_with(|| Rc::new(RefCell::new(HashMap::new()))))
    }

    pub fn render(&self) {
        let segments = self.segments.borrow();
        let mut regions: Vec<&ValueOrImmediate<A>> = segments.keys().collect();
        regions.sort_by(|l, r| {
            l.to_string().cmp(&r.to_string())
        });
        for region in regions {
            let segment = segments.get(region).unwrap();
            println!("at region {}:", region);
            for (base, segment) in segment.borrow().iter() {
                println!("          --- address <{}> ---", base.name());
                let mut exprs: Vec<Rc<Item<ValueOrImmediate<A>>>> = segment.accesses.keys().cloned().collect();
                use std::cmp::Ordering;
                exprs.sort_by(|l, r| {
                    let lv = &l.value;
                    let rv = &r.value;
                    if lv == rv {
                        Ordering::Equal
                    } else {
                        match (lv, rv) {
                            (Expression::Unknown, _) => {
                                Ordering::Greater
                            }
                            (_, Expression::Unknown) => {
                                Ordering::Less
                            },
                            (Expression::Value(lv), Expression::Value(rv)) => {
                                use analyses::ValueOrImmediate::*;
                                match (lv, rv) {
                                    (Immediate(l), Immediate(r)) => {
                                        l.cmp(r)
                                    }
                                    (Immediate(_), _) => {
                                        Ordering::Less
                                    }
                                    (_, Immediate(_)) => {
                                        Ordering::Greater
                                    }
                                    _ => {
                                        Ordering::Equal
                                    }
                                }
                            }
                            (Expression::Value(_), _) => {
                                Ordering::Less
                            }
                            (_, Expression::Value(_)) => {
                                Ordering::Greater
                            }
                            _ => {
                                Ordering::Equal
                            }
                        }
                    }
                });
                for expr in exprs {
                    let accesses = segment.accesses.get(&expr).unwrap();
                    assert!(accesses.len() > 0);

                    let mut base_str = expr.to_string();
                    while base_str.len() < 6 {
                        base_str = format!(" {}", base_str);
                    }

                    let mut access_sizes: Vec<&u64> = accesses.keys().collect();
                    access_sizes.sort();
                    for size in access_sizes.iter() {
                        let access = accesses.get(size).unwrap();
                        let access_str = format!("{}{}-",
                            if access.is_read() { "r" } else { "-" },
                            if access.is_write() { "w" } else { "-" },
                        );
                        println!("  {}  | {:#06x}:         {}     |", base_str, size, access_str);
                    }
                }
                println!("          ---------------------------");
            }
        }
    }
}

use yaxpeax_x86::long_mode::{Arch as amd64};
use analyses::Expression;
use analyses::Item;
use analyses::ValueOrImmediate;
impl<'ssa> DFG<Rc<Item<ValueOrImmediate<amd64>>>, amd64, <amd64 as Arch>::Address> for MemoryLayout<'ssa, amd64> {
    type Indirect = IndirectLayout<amd64>;

    fn indirect_loc(&self, when: <amd64 as Arch>::Address, loc: <amd64 as ValueLocations>::Location) -> IndirectLayout<amd64> {
        let ssa_def = self.ssa.try_get_def(when, loc.clone());
        let ssa_use = self.ssa.try_get_use(when, loc.clone());
        if ssa_def.is_none() {
            println!("no ssa def for {} at {}", loc, when);
        }
        if ssa_use.is_none() {
            println!("no ssa use for {} at {}", loc, when);
        }
        let regions_defs = ssa_def.as_ref().map(|value| {
            self.get_segment(Rc::clone(value))
        });
        let regions_uses = ssa_use.as_ref().map(|value| {
            self.get_segment(Rc::clone(value))
        });
        IndirectLayout {
            regions_defs,
            regions_uses,
            ssa_def,
            ssa_use,
        }
    }
    fn read_loc(&self, when: <amd64 as Arch>::Address, loc: <amd64 as ValueLocations>::Location) -> Rc<Item<ValueOrImmediate<amd64>>> {
        // TODO: HACK: ignore weird rip semantics for now
        if loc != crate::arch::x86_64::analyses::data_flow::Location::RIP {
            // TODO: return a value?
            let dfg_ref = self.ssa.get_use(when, loc).as_rc();
            return Item::untyped(Expression::value(ValueOrImmediate::Value(dfg_ref)));
        }
        // LocationAccess::Unknown
        Item::untyped(Expression::Unknown)
    }
    fn write_loc(&mut self, when: <amd64 as Arch>::Address, loc: <amd64 as ValueLocations>::Location, value: Rc<Item<ValueOrImmediate<amd64>>>) {
        // TODO: HACK: ignore weird rip semantics for now
        if loc != crate::arch::x86_64::analyses::data_flow::Location::RIP {
            let dest = self.ssa.get_def(when, loc);
            if dest.value.borrow().data.is_none() {
                dest.update(crate::arch::x86_64::analyses::data_flow::Data::Expression(value));
            }
        }
    }
}
/*
impl<'ssa, A: Arch + ValueLocations + SSAValues> DFG<LocationAccess, A, A::Address> for MemoryLayout<'ssa, A> {
    fn read_loc(&self, when: A::Address, loc: A::Location) -> LocationAccess {
        if let Some(when) = self.per_address_layout.get(&when) {
            if let Some(accesses) = when.get(&loc) {
                return LocationAccess::Memory { size: 0, offset: 0 };
            }
        }

        LocationAccess::Unknown
    }
    fn write_loc(&mut self, when: A::Address, loc: A::Location, value: LocationAccess) {
        if let crate::arch::x86_64::analyses::data_flow::Location::Memory(region) = loc {
            let accesses = self.per_address_layout
                .entry(when)
                .or_insert_with(|| HashMap::new())
                .entry(loc)
                .or_insert_with(|| Vec::new());
            // TODO: sizes should be regions starting at 0 of the kinds of accesses that have been
            // seen..
            accesses.push((Use::Write, region));
        }
    }
}
*/
