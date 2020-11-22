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
pub struct MemoryRegion {
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
    accesses: HashMap<u64, HashMap<u64, UsePattern>>,
    // TODO: how to avoid recording memcpy-like accesses that disregard internal structure? does
    // that get resolved here, or by MemoryLayout? something else entirely? hm...
}

#[repr(transparent)]
struct UsePattern(u8);

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
        UsePattern(0).with_read()
    }
    fn write() -> Self {
        UsePattern(0).with_write()
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
    fn is_read(&self) -> bool {
        (self.0 & 1) != 0
    }
    fn is_write(&self) -> bool {
        (self.0 & 2) != 0
    }
}

impl MemoryRegion {
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
    regions_uses: Option<Rc<RefCell<HashMap<HashedValue<DFGRef<A>>, MemoryRegion>>>>,
    regions_defs: Option<Rc<RefCell<HashMap<HashedValue<DFGRef<A>>, MemoryRegion>>>>,
}

pub struct MemoryLayout<'ssa, A: Arch + ValueLocations + SSAValues> {
    pub ssa: &'ssa SSA<A>,
    /// map SSA values at some `A::Location` to their referent layout.
    /// key is likely versions of an architecture's Location::Memory.
    pub segments: HashMap<HashedValue<DFGRef<A>>, Rc<RefCell<HashMap<HashedValue<DFGRef<A>>, MemoryRegion>>>>,
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

pub enum LocationOffset<A: Arch + ValueLocations + SSAValues> {
    Concrete(u64),
    Symbolic(DFGRef<A>),
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
    fn store(&self, index: ValueIndex<LocationAccess<A>>, _value: &LocationAccess<A>) {
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
                usage.mark_write();
            }
        }
    }
}

impl<'ssa, A: Arch + ValueLocations + SSAValues> MemoryLayout<'ssa, A> {
    fn get_segment(&mut self, indirection_value: DFGRef<A>) -> Rc<RefCell<HashMap<HashedValue<DFGRef<A>>, MemoryRegion>>> {
        Rc::clone(self.segments.entry(HashedValue { value: indirection_value })
            .or_insert_with(|| Rc::new(RefCell::new(HashMap::new()))))
    }
}

use yaxpeax_x86::long_mode::{Arch as amd64};
impl<'ssa> DFG<LocationAccess<amd64>, amd64, <amd64 as Arch>::Address> for MemoryLayout<'ssa, amd64> {
    fn indirect_loc(&mut self, when: <amd64 as Arch>::Address, loc: <amd64 as ValueLocations>::Location) -> IndirectLayout<amd64> {
        let ssa_def = self.ssa.try_get_def(when, loc);
        let ssa_use = self.ssa.try_get_use(when, loc);
        IndirectLayout {
            regions_defs: ssa_def.map(|value| self.get_segment(value)),
            regions_uses: ssa_use.map(|value| self.get_segment(value)),
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
