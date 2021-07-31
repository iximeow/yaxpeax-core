use std::ops::Range;

pub mod repr;
pub mod reader;

pub use memory::repr::MemoryReprReader;
use memory::repr::flat::FlatMemoryRepr;
use memory::repr::cursor::ReadCursor;
use memory::repr::process::ModuleInfo;

use yaxpeax_arch::Arch;

#[derive(Debug)]
pub enum LayoutError {
    AddressConflict,
    Unsupported
}

pub trait MemoryRange<A: Arch + ?Sized> where Self: MemoryRepr<A> {
    /// a cursor to read data contained in `range`. this willfully misinterprets `std::ops::Range`
    /// to be inclusive on both ends, rather than `[inclusive, exclusive)` as the docs say. this
    /// is, for the time being, necessary because yaxpeax consistently uses ranges that are
    /// inclusive on both ends. yaxpeax must represent `[inclusive, exclusive)` ranges most clearly
    /// because this significantly simplifies expressing a basic block that ends at the end of its
    /// architecture's address space.
    fn range<'a>(&'a self, range: Range<A::Address>) -> Option<ReadCursor<'a, A, Self>>;
    fn range_from<'a>(&'a self, start: A::Address) -> Option<ReadCursor<'a, A, Self>>;
}

pub trait MemoryRepr<A: Arch + ?Sized>: Named {
    fn module_info(&self) -> Option<&ModuleInfo>;
    fn read(&self, addr: A::Address) -> Option<u8>;
    fn as_flat(&self) -> Option<FlatMemoryRepr>;
    fn module_for(&self, addr: A::Address) -> Option<&dyn MemoryRepr<A>>;
    fn size(&self) -> Option<u64>;
    fn start(&self) -> Option<u64> { None }
    fn end(&self) -> Option<u64> { self.start().and_then(|x| self.size().map(|y| x + y)) }
}

pub trait Named {
    fn name(&self) -> &str;
}

pub trait PatchyMemoryRepr<A: Arch> {
    fn add(&mut self, data: Vec<u8>, addr: A::Address) -> Result<(), LayoutError>;
}
