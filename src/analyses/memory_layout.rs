use yaxpeax_arch::{Arch, AddressBase};
use std::collections::HashMap;
use std::convert::TryInto;
use analyses::DFG;
use analyses::ValueRes;
use data::ValueLocations;
use analyses::static_single_assignment::{DFGRef, SSA, SSAValues};
use std::cell::RefCell;
use std::rc::Rc;
use num_traits::Zero;
use std::fmt;

use analyses::static_single_assignment::HashedValue;

/// some kind of a description of memory usage in a dfg
#[derive(Debug)]
pub struct MemoryRegion<A: Arch + ValueLocations + SSAValues> {
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
    pub accesses: HashMap<u64, HashMap<u64, UsePattern<A>>>,
    // TODO: how to avoid recording memcpy-like accesses that disregard internal structure? does
    // that get resolved here, or by MemoryLayout? something else entirely? hm...
}

impl<A: Arch + ValueLocations + SSAValues> Clone for MemoryRegion<A> {
    fn clone(&self) -> Self {
        Self {
            accesses: self.accesses.clone(),
        }
    }
}

// #[repr(transparent)]
pub struct UsePattern<A: Arch + ValueLocations + SSAValues>(pub u8, pub Option<LocationAccess<A>>);

impl<A: Arch + ValueLocations + SSAValues> Clone for UsePattern<A> {
    fn clone(&self) -> Self {
        let UsePattern(v, data) = self;
        UsePattern(
            *v,
            data.as_ref().map(|access| access.to_owned())
        )
    }
}

impl<A: Arch + ValueLocations + SSAValues> fmt::Debug for UsePattern<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (self.is_read(), self.is_write()) {
            (false, false) => f.write_str("(no access)"),
            (false, true) => write!(f, "{{ write (value? {}) }}", self.1.is_some()),
            (true, false) => f.write_str("{ read }"),
            (true, true) => write!(f, "{{ read, write ({}) }}", self.1.is_some()),
        }
    }
}

#[allow(dead_code)]
impl<A: Arch + ValueLocations + SSAValues> UsePattern<A> {
    fn none() -> Self {
        UsePattern(0, None)
    }
    fn read() -> Self {
        Self::none().with_read()
    }
    fn write(value: LocationAccess<A>) -> Self {
        Self::none().with_write(value)
    }
    fn merge(&self, other: &UsePattern<A>) -> Self {
        // TODO: merging values should ... not work like this.
        let merged = match (self.1.as_ref(), other.1.as_ref()) {
            (Some(_ours), Some(_theirs)) => {
                None
            },
            (Some(ours), None) => {
                Some(ours.clone())
            }
            (None, Some(theirs)) => {
                Some(theirs.clone())
            }
            (None, None) => None
        };
        UsePattern(self.0 | other.0, merged)
    }
    fn value(&self) -> Option<&LocationAccess<A>> {
        self.1.as_ref()
    }
    fn with_read(mut self) -> Self {
        self.mark_read();
        self
    }
    fn with_write(mut self, value: LocationAccess<A>) -> Self {
        self.mark_write(value);
        self
    }
    fn mark_read(&mut self) {
        self.0 |= 1;
    }
    fn mark_write(&mut self, value: LocationAccess<A>) {
        self.0 |= 2;
        self.1 = Some(value);
    }
    pub fn is_read(&self) -> bool {
        (self.0 & 1) != 0
    }
    pub fn is_write(&self) -> bool {
        (self.0 & 2) != 0
    }
}

impl<A: Arch + ValueLocations + SSAValues> MemoryRegion<A> {
    fn new() -> Self {
        Self {
            accesses: HashMap::new(),
        }
    }
}

pub struct IndirectLayout<A: Arch + ValueLocations + SSAValues> {
    /// a mapping to distinct regions in this indirect area from the base values inferred for their
    /// accesses.
    ///
    /// the expectation^Whope is that analyses can canonicalize effective addresses so the
    /// least-bound value can be used as a base, and bound addends can restrict the aliasing set so
    /// a write to some value does not necessarily cause all referents through `A::Location` to be
    /// clobbered.
    regions_uses: Option<Rc<RefCell<HashMap<HashedValue<DFGRef<A>>, MemoryRegion<A>>>>>,
    regions_defs: Option<Rc<RefCell<HashMap<HashedValue<DFGRef<A>>, MemoryRegion<A>>>>>,
    ssa_use: Option<DFGRef<A>>,
    ssa_def: Option<DFGRef<A>>,
}

pub struct MemoryLayout<'ssa, A: Arch + ValueLocations + SSAValues> {
    pub ssa: &'ssa SSA<A>,
    /// map SSA values at some `A::Location` to their referent layout.
    /// key is likely versions of an architecture's Location::Memory.
    pub segments: HashMap<HashedValue<DFGRef<A>>, Rc<RefCell<HashMap<HashedValue<DFGRef<A>>, MemoryRegion<A>>>>>,
}

// TODO: how are locations like [rsp + 8] discussed? x86 location precision is only "memory".
// perhaps arch locations should be `Memory(EffectiveAddress)` where `EffectiveAddress` is an x86
// addressing mode, and `read_loc` looks up values for the associated registers (if any) in an
// ancillary register dfg?
pub enum LocationAccess<A: Arch + ValueLocations + SSAValues> {
    Unknown,
    Symbolic {
        base: DFGRef<A>,
        offset: LocationOffset<A>,
    },
    Const { diff: u64 },
}

impl<A: Arch + ValueLocations + SSAValues> Clone for LocationAccess<A> {
    fn clone(&self) -> Self {
        match self {
            LocationAccess::Unknown => LocationAccess::Unknown,
            LocationAccess::Symbolic { base, offset } => LocationAccess::Symbolic {
                base: Rc::clone(base),
                offset: offset.clone(),
            },
            LocationAccess::Const { diff } => LocationAccess::Const { diff: *diff },
        }
    }
}

pub enum LocationOffset<A: Arch + ValueLocations + SSAValues> {
    Concrete(u64),
    Symbolic(DFGRef<A>),
}

impl<A: Arch + ValueLocations + SSAValues> Clone for LocationOffset<A> {
    fn clone(&self) -> Self {
        match self {
            LocationOffset::Concrete(v) => LocationOffset::Concrete(*v),
            LocationOffset::Symbolic(r) => LocationOffset::Symbolic(Rc::clone(r)),
        }
    }
}

use yaxpeax_arch::AddressDiff;
use analyses::Value;
impl<A: Arch + ValueLocations + SSAValues> Value for LocationAccess<A> {
    type Indirect = IndirectLayout<A>;

    fn unknown() -> Self {
        LocationAccess::Unknown
    }

    fn from_set(_xs: &[Self]) -> Self { Self::unknown() }

    fn from_const(c: u64) -> Self {
        LocationAccess::Const { diff: c }
    }

    fn to_const(&self) -> Option<u64> {
        match self {
            LocationAccess::Const { diff } => {
                // TODO: if "zero" and "diff" are incomparable (eg code/data offsets?) this is not
                // a sensical operation. when `Address` develops the ability to comprehensively
                // describe distinct address spaces, this will have to change.
                Some(*diff)
            }
            _ => { None }
        }
    }

    fn add(&self, other: &Self) -> ValueRes<Self> {
        let value = match (self, other) {
            (LocationAccess::Unknown, _) |
            (_, LocationAccess::Unknown) |
            (LocationAccess::Symbolic { offset: LocationOffset::Symbolic(_), .. }, LocationAccess::Const { .. }) |
            (LocationAccess::Const { .. }, LocationAccess::Symbolic { offset: LocationOffset::Symbolic(_), .. }) |
            (LocationAccess::Symbolic { .. }, LocationAccess::Symbolic { .. }) => {
                Self::unknown()
            },
            (LocationAccess::Symbolic { base, offset: LocationOffset::Concrete(offs) }, LocationAccess::Const { diff }) |
            (LocationAccess::Const { diff }, LocationAccess::Symbolic { base, offset: LocationOffset::Concrete(offs) }) => {
                LocationAccess::Symbolic {
                    base: Rc::clone(base),
                    offset: LocationOffset::Concrete(*offs + *diff),
                }
            }
            (LocationAccess::Const { diff: left }, LocationAccess::Const { diff: right }) => {
                LocationAccess::Const { diff: *left + *right }
            }
        };
        ValueRes {
            value,
            carry: Self::unknown()
        }
    }
}

impl From<AddressDiff<u64>> for LocationAccess<amd64> {
    fn from(diff: AddressDiff<u64>) -> Self {
        LocationAccess::Const { diff: <amd64 as Arch>::Address::zero().wrapping_offset(diff).to_linear() as u64 }
    }
}

use analyses::{IndirectQuery, ValueIndex};

impl<A: Arch + ValueLocations + SSAValues> IndirectQuery<LocationAccess<A>> for IndirectLayout<A> {
    fn load(&self, index: ValueIndex<LocationAccess<A>>) -> LocationAccess<A> {
        match index.base {
            LocationAccess::Unknown => {
                // load from an unknown location, there's really not much to do. this comes from
                // some kind of operation that results in a total inability to track symbolic
                // values - could be multiple levels of pointer chasing, arbitrary bitwise
                // arithmetic, multiplies/divides, or even just the value being a result of
                // unimplemented semantics.
                LocationAccess::Unknown
            }
            LocationAccess::Const { diff } => {
                // access to a const address with a const size? well. no good way to model this at
                // the moment, though it happens often enough...
                tracing::error!("const bases are not yet supported in memory analysis. saw one anyway: {:?}", diff);
                LocationAccess::Unknown
            }
            LocationAccess::Symbolic { base: dfg_ref, offset } => {
                // load with some symbolic base address, possibly an offset.
                //
                // if there is no inferred structure at the given base, create one, then record a
                // load at `offset`.
                let offset = match offset {
                    LocationOffset::Concrete(diff) => {
                        *diff
                    }
                    LocationOffset::Symbolic(offset) => {
                        tracing::error!("symbolic offsets are not yet supported in memory analysis, but saw symbolic offst {:?}", offset);
                        return LocationAccess::Unknown;
                    }
                };
                let mut regions = self.regions_uses.as_ref().expect("indirect load has a corresponding ssa use").borrow_mut();
                let region = regions.entry(HashedValue { value: Rc::clone(dfg_ref) }).or_insert_with(MemoryRegion::new);
                let offset = region.accesses.entry(offset).or_insert_with(HashMap::new);
                let usage = offset.entry(index.size as u64).or_insert_with(UsePattern::none);
                usage.mark_read();
                LocationAccess::Unknown
            }
        }
    }
    fn store(&self, index: ValueIndex<LocationAccess<A>>, value: &LocationAccess<A>) {
//        eprintln!("store: {:?}", index);
//        eprintln!("old: {:?}", self.regions_uses);
//        eprintln!("new: {:?}", self.regions_defs);
        // copy old stores to this memory
        if let (Some(uses), Some(defs)) = (self.regions_uses.as_ref(), self.regions_defs.as_ref()) {
            let mut defs_mut = defs.borrow_mut();
            for (base, region) in &*uses.borrow() {
                if defs_mut.contains_key(base) {
                    // merge
                    let mut new_defs = &mut defs_mut.get_mut(base).unwrap().accesses;
                    for (old_offset, accesses) in &region.accesses {
                        if new_defs.contains_key(&old_offset) {
                            let mut new_accesses = new_defs.get_mut(&old_offset).unwrap();
                            // again, merge...
                            for (sz, pattern) in accesses {
                                if new_accesses.contains_key(&sz) {
                                    // merge, somehow
                                    let new_pattern = &new_accesses[&sz];
                                    new_accesses.insert(*sz, new_pattern.merge(pattern));
                                } else {
                                    new_accesses.insert(*sz, pattern.clone());
                                }
                            }
                        } else {
                            new_defs.insert(old_offset.clone(), accesses.clone());
                        }
                    }
                } else {
                    defs_mut.insert(HashedValue { value: Rc::clone(&base.value) }, region.to_owned());
                }
            }
        }

        // TODO: use (store?) `value`, which should get memory analysis in a place where it could
        // try tracking spills/reloads.
        match index.base {
            LocationAccess::Unknown => {
                // store to an unknown location, there's really not much to do. this comes from
                // some kind of operation that results in a total inability to track symbolic
                // values - could be multiple levels of pointer chasing, arbitrary bitwise
                // arithmetic, multiplies/divides, or even just the value being a result of
                // unimplemented semantics.
            }
            LocationAccess::Const { diff } => {
                // access to a const address with a const size? well. no good way to model this at
                // the moment, though it happens often enough...
                tracing::error!("const bases are not yet supported in memory analysis. saw one anyway: {:?}", diff);
            }
            LocationAccess::Symbolic { base: dfg_ref, offset } => {
                // store with some symbolic base address, possibly an offset.
                //
                // if there is no inferred structure at the given base, create one, then record a
                // store at `offset`.
                let offset = match offset {
                    LocationOffset::Concrete(diff) => {
                        *diff
                    }
                    LocationOffset::Symbolic(offset) => {
                        tracing::error!("symbolic offsets are not yet supported in memory analysis, but saw symbolic offst {:?}", offset);
                        return;
                    }
                };
                let mut regions = self.regions_defs.as_ref().expect("indirect store has a corresponding ssa def").borrow_mut();
                let region = regions.entry(HashedValue { value: Rc::clone(dfg_ref) }).or_insert_with(MemoryRegion::new);
                let offset = region.accesses.entry(offset).or_insert_with(HashMap::new);
                let usage = offset.entry(index.size as u64).or_insert_with(UsePattern::none);
                usage.mark_write(value.to_owned());
            }
        }
    }
}

impl<'ssa, A: Arch + ValueLocations + SSAValues> MemoryLayout<'ssa, A> {
    fn get_segment(&mut self, indirection_value: DFGRef<A>) -> Rc<RefCell<HashMap<HashedValue<DFGRef<A>>, MemoryRegion<A>>>> {
        Rc::clone(self.segments.entry(HashedValue { value: indirection_value })
            .or_insert_with(|| Rc::new(RefCell::new(HashMap::new()))))
    }
}

use yaxpeax_x86::long_mode::{Arch as amd64};
impl<'ssa> DFG<LocationAccess<amd64>, amd64, <amd64 as Arch>::Address> for MemoryLayout<'ssa, amd64> {
    fn indirect_loc(&mut self, when: <amd64 as Arch>::Address, loc: <amd64 as ValueLocations>::Location) -> IndirectLayout<amd64> {
        let ssa_def = self.ssa.try_get_def(when, loc);
        let ssa_use = self.ssa.try_get_use(when, loc);
        let regions_defs = ssa_def.as_ref().map(|value| self.get_segment(Rc::clone(value)));
        let regions_uses = ssa_use.as_ref().map(|value| self.get_segment(Rc::clone(value)));
        IndirectLayout {
            regions_defs,
            regions_uses,
            ssa_def,
            ssa_use,
        }
    }
    fn read_loc(&self, when: <amd64 as Arch>::Address, loc: <amd64 as ValueLocations>::Location) -> LocationAccess<amd64> {
        // TODO: HACK: ignore weird rip semantics for now
        if loc != crate::arch::x86_64::analyses::data_flow::Location::RIP {
            // TODO: return a value?
            let dfg_ref = self.ssa.get_use(when, loc).as_rc();
            return LocationAccess::Symbolic {
                base: dfg_ref,
                offset: LocationOffset::Concrete(0)
            };
        }
        LocationAccess::Unknown
    }
    fn write_loc(&mut self, when: <amd64 as Arch>::Address, loc: <amd64 as ValueLocations>::Location, _value: LocationAccess<amd64>) {
        // TODO: HACK: ignore weird rip semantics for now
        if loc != crate::arch::x86_64::analyses::data_flow::Location::RIP {
            // TODO: use value?
            let _dfg_ref = self.ssa.get_def(when, loc).as_rc();
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
