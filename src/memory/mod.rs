use std::io::Read;
use std::fs::File;
use std::collections::HashMap;
use std::ops::{Range, RangeFrom};
use std::fmt::Debug;

use debug::Peek;

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

pub trait MemoryRepr<A: Address> {
    fn module_info(&self) -> Option<&ModuleInfo>;
    fn read(&self, addr: A) -> Option<u8>;
    fn to_flat(self) -> Option<FlatMemoryRepr>;
}

pub trait PatchyMemoryRepr<A: Address> {
    fn add(&mut self, data: Vec<u8>, addr: A) -> Result<(), LayoutError>;
}
