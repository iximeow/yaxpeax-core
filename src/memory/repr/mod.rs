use yaxpeax_arch::Address;
use memory::{MemoryRange, MemoryRepr, Named};
use std::ops::Range;

pub mod cursor;
pub use self::cursor::{ReadCursor, UnboundedCursor};
pub mod flat;
pub use self::flat::FlatMemoryRepr;
pub mod process;
pub use self::process::ProcessMemoryRepr;
pub mod remote;
pub use self::remote::RemoteMemoryRepr;
pub mod adapter;
pub use self::adapter::MemoryReprAdapter;
pub use crate::memory::reader::FileRepr;
use memory::repr::process::ModuleInfo;

impl <A: Address> MemoryRepr<A> for Vec<u8> {
    fn read(&self, addr: A) -> Option<u8> {
        let linear_addr = addr.to_linear();
        if linear_addr < self.len() {
            Some(self[linear_addr])
        } else {
            None
        }
    }
    fn to_flat(self) -> Option<FlatMemoryRepr> {
        Some(FlatMemoryRepr::of(self))
    }
    fn module_info(&self) -> Option<&ModuleInfo> { None }
    fn module_for(&self, _addr: A) -> Option<&dyn MemoryRepr<A>> {
        Some(self)
    }
    fn size(&self) -> Option<u64> {
        Some(self.len() as u64)
    }
}

impl Named for Vec<u8> {
    fn name(&self) -> &str {
        "anonymous_vec"
    }
}

/*
impl <A: Address> MemoryRepr<A> for [u8] {
    fn read(&self, addr: A) -> Option<u8> {
        let linear_addr = addr.to_linear();
        if linear_addr < self.len() {
            Some(self[linear_addr])
        } else {
            None
        }
    }
    fn to_flat(self) -> Option<FlatMemoryRepr> {
        Some(FlatMemoryRepr::of(self.to_vec()))
    }
}
*/

impl <A: Address> MemoryRange<A> for Vec<u8> {
    fn range<'a>(&'a self, range: Range<A>) -> Option<ReadCursor<'a, A, Self>> {
        if range.start.to_linear() < self.len() && range.end.to_linear() < self.len() && range.start < range.end {
            Some(ReadCursor::from(self, range))
        } else {
            None
        }
    }
    fn range_from<'a>(&'a self, start: A) -> Option<UnboundedCursor<'a, A, Self>> {
        if start.to_linear() < self.len() {
            Some(UnboundedCursor::from(self, start))
        } else {
            None
        }
    }
}

/*
impl <A: Address> MemoryRange<A> for [u8] {
    fn range<'a>(&'a self, range: Range<A>) -> Option<ReadCursor<'a, A, Self>> {
        if range.start.to_linear() < self.len() && range.end.to_linear() < self.len() && range.start < range.end {
            Some(ReadCursor::from(self, range))
        } else {
            None
        }
    }
    fn range_from<'a>(&'a self, start: A) -> Option<UnboundedCursor<'a, A, Self>> {
        if start.to_linear() < self.len() {
            Some(UnboundedCursor::from(self, start))
        } else {
            None
        }
    }
}
*/

