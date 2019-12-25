use std::ops::Range;

use yaxpeax_arch::Address;

pub mod repr;
pub mod reader;

use memory::repr::flat::FlatMemoryRepr;
use memory::repr::cursor::{ReadCursor, UnboundedCursor};
use memory::repr::process::ModuleInfo;

#[derive(Debug)]
pub enum LayoutError {
    AddressConflict,
    Unsupported
}

pub trait MemoryRange<A: Address> where Self: MemoryRepr<A> {
    fn range<'a>(&'a self, range: Range<A>) -> Option<ReadCursor<'a, A, Self>>;
    fn range_from<'a>(&'a self, start: A) -> Option<UnboundedCursor<'a, A, Self>>;
}

pub trait MemoryRepr<A: Address>: Named {
    fn module_info(&self) -> Option<&ModuleInfo>;
    fn read(&self, addr: A) -> Option<u8>;
    fn to_flat(self) -> Option<FlatMemoryRepr>;
    fn module_for(&self, addr: A) -> Option<&dyn MemoryRepr<A>>;
    fn size(&self) -> Option<u64>;
    fn start(&self) -> Option<u64> { None }
    fn end(&self) -> Option<u64> { self.start().and_then(|x| self.size().map(|y| x + y)) }
}

pub trait Named {
    fn name(&self) -> &str;
}

pub trait PatchyMemoryRepr<A: Address> {
    fn add(&mut self, data: Vec<u8>, addr: A) -> Result<(), LayoutError>;
}
