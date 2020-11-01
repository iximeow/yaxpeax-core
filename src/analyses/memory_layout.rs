use analyses::data_flow::Use;
use yaxpeax_arch::Arch;
use std::collections::HashMap;
use analyses::DFG;
use data::ValueLocations;
use analyses::static_single_assignment::{DFGRef, SSA, SSAValues};
use std::cell::RefCell;

/// some kind of a description of memory usage in a dfg
struct MemoryRegion {
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
    accesses: Vec<usize>,
}

pub struct MemoryLayout<'ssa, A: Arch + ValueLocations + SSAValues> {
    pub ssa: &'ssa SSA<A>,
    pub per_address_layout: RefCell<HashMap<A::Address, HashMap<A::Location, Vec<(Use, usize)>>>>,
    pub per_value_accesses: RefCell<HashMap<DFGRef<A>, Vec<(usize, usize)>>>,
}

// TODO: how are locations like [rsp + 8] discussed? x86 location precision is only "memory".
// perhaps arch locations should be `Memory(EffectiveAddress)` where `EffectiveAddress` is an x86
// addressing mode, and `read_loc` looks up values for the associated registers (if any) in an
// ancillary register dfg?
pub enum LocationAccess {
    Unknown,
    Register { size: usize },
    Memory { size: usize, offset: usize },
}

use yaxpeax_arch::AddressDiff;
use analyses::Value;
impl Value for LocationAccess {
    fn unknown() -> Self {
        LocationAccess::Unknown
    }

    fn from_set(_xs: &[Self]) -> Self { Self::unknown() }

    fn to_const(&self) -> Option<u64> { None }
}

impl From<AddressDiff<u64>> for LocationAccess {
    fn from(_diff: AddressDiff<u64>) -> Self {
        LocationAccess::unknown()
    }
}

use analyses::IndirectDFG;
use analyses::{Indirect, ValueIndex};

impl<'x, 'y> Indirect<LocationAccess> for MemoryRegion2<'x, 'y> {
    fn load(&self, _: ValueIndex<LocationAccess>) -> LocationAccess {
        LocationAccess::unknown()
    }
    fn store(&self, _: ValueIndex<LocationAccess>, _: &LocationAccess) {}
}
struct MemoryRegion2<'cursor, 'ssa> {
    dfg: &'cursor mut MemoryLayout<'ssa, amd64>,
    when: <amd64 as Arch>::Address,
    loc: <amd64 as ValueLocations>::Location,
}

impl<'ssa, 'cursor> IndirectDFG<LocationAccess, amd64, <amd64 as Arch>::Address> for &'cursor mut MemoryLayout<'ssa, amd64> {
    type Indirect = MemoryRegion2<'cursor, 'ssa>;
    fn indirect_loc(&mut self, when: <amd64 as Arch>::Address, loc: <amd64 as ValueLocations>::Location) -> Self::Indirect {
        MemoryRegion2 {
            dfg: self,
            when,
            loc
        }
    }
}

use yaxpeax_x86::long_mode::{Arch as amd64};
impl<'ssa> DFG<LocationAccess, amd64, <amd64 as Arch>::Address> for MemoryLayout<'ssa, amd64> {
    fn read_loc(&self, when: <amd64 as Arch>::Address, loc: <amd64 as ValueLocations>::Location) -> LocationAccess {
        if let crate::arch::x86_64::analyses::data_flow::Location::Memory(region) = loc {
            let mut layout = self.per_address_layout.borrow_mut();
            let accesses = layout
                .entry(when)
                .or_insert_with(|| HashMap::new())
                .entry(loc)
                .or_insert_with(|| Vec::new());
            accesses.push((Use::Read, region.0 as usize));

            let usage = self.ssa.get_use(when, loc).as_rc();

            LocationAccess::Memory { size: 0, offset: 0 }
        } else {
            LocationAccess::Unknown
        }
    }
    fn write_loc(&mut self, when: <amd64 as Arch>::Address, loc: <amd64 as ValueLocations>::Location, value: LocationAccess) {
        if let crate::arch::x86_64::analyses::data_flow::Location::Memory(region) = loc {
            let mut layout = self.per_address_layout.borrow_mut();
            let accesses = layout
                .entry(when)
                .or_insert_with(|| HashMap::new())
                .entry(loc)
                .or_insert_with(|| Vec::new());
            // TODO: sizes should be regions starting at 0 of the kinds of accesses that have been
            // seen..
            accesses.push((Use::Write, region.0 as usize));
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
