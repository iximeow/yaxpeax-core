use yaxpeax_arch::Address;
use memory::{MemoryRange, MemoryRepr};
use memory::repr::{FlatMemoryRepr, ReadCursor, UnboundedCursor};
use memory::repr::process::ModuleInfo;
use std::ops::Range;

pub trait MemoryAdapter<'a, A: Address, B: Address, M: MemoryRepr<A> + ?Sized> {
    fn to_address_space(&'a self) -> MemoryReprAdapter<'a, A, B, M>;
}

#[derive(Debug)]
pub struct MemoryReprAdapter<'a, A: Address, B, M: MemoryRepr<A> + ?Sized> {
    repr: &'a M,
    f: fn(B) -> A
}

fn f_u16_to_usize(u: u16) -> usize { u as usize }

impl <'a, 'b, M: MemoryRepr<usize> + ?Sized> MemoryAdapter<'a, usize, u16, M> for M {
    fn to_address_space(&'a self) -> MemoryReprAdapter<'a, usize, u16, M> {
        MemoryReprAdapter {
            repr: self,
            f: f_u16_to_usize
        }
    }
}

impl <'a, 'b, M: MemoryRepr<usize>> MemoryRepr<u16> for MemoryReprAdapter<'a, usize, u16, M> {
    fn read(&self, addr: u16) -> Option<u8> {
        self.repr.read((self.f)(addr))
    }
    fn to_flat(self) -> Option<FlatMemoryRepr> { None }
    fn module_info(&self) -> Option<&ModuleInfo> { self.repr.module_info() }
    fn module_for(&self, addr: u16) -> Option<&MemoryRepr<u16>> {
        None
        // TODO: figure out if it's possible to write this?
        // this really messes up stuff...
    }
    fn name(&self) -> &str { self.repr.name() }
    fn size(&self) -> Option<u64> { self.repr.size() }
}

fn f_u32_to_usize(u: u32) -> usize { u as usize }

impl <'a, M: MemoryRepr<usize> + ?Sized> MemoryAdapter<'a, usize, u32, M> for M {
    fn to_address_space(&'a self) -> MemoryReprAdapter<'a, usize, u32, M> {
        MemoryReprAdapter {
            repr: self,
            f: f_u32_to_usize
        }
    }
}

impl <'a, M: MemoryRepr<usize>> MemoryRepr<u32> for MemoryReprAdapter<'a, usize, u32, M> {
    fn read(&self, addr: u32) -> Option<u8> {
        self.repr.read((self.f)(addr))
    }
    fn to_flat(self) -> Option<FlatMemoryRepr> { None }
    fn module_info(&self) -> Option<&ModuleInfo> { self.repr.module_info() }
    fn module_for(&self, addr: u32) -> Option<&MemoryRepr<u32>> {
        None
        // TODO: figure out if it's possible to write this?
        // self.repr.module_for((self.f)(addr))
    }
    fn name(&self) -> &str { self.repr.name() }
    fn size(&self) -> Option<u64> { self.repr.size() }
}

fn f_u64_to_usize(u: u64) -> usize { u as usize }

impl <'a, M: MemoryRepr<usize> + ?Sized> MemoryAdapter<'a, usize, u64, M> for M {
    fn to_address_space(&'a self) -> MemoryReprAdapter<'a, usize, u64, M> {
        MemoryReprAdapter {
            repr: self,
            f: f_u64_to_usize
        }
    }
}

impl <'a, M: MemoryRepr<usize>> MemoryRepr<u64> for MemoryReprAdapter<'a, usize, u64, M> {
    fn read(&self, addr: u64) -> Option<u8> {
        self.repr.read((self.f)(addr))
    }
    fn to_flat(self) -> Option<FlatMemoryRepr> { None }
    fn module_info(&self) -> Option<&ModuleInfo> { self.repr.module_info() }
    fn module_for(&self, addr: u64) -> Option<&MemoryRepr<u64>> {
        None
        // TODO: figure out if it's possible to write this?
        // self.repr.module_for((self.f)(addr))
    }
    fn name(&self) -> &str { self.repr.name() }
    fn size(&self) -> Option<u64> { self.repr.size() }
}

impl <'a, A: Address, B: Address, M: MemoryRepr<A> + MemoryRange<A>> MemoryRange<B> for MemoryReprAdapter<'a, A, B, M> where Self: MemoryRepr<B> {
    // TODO: I am incredibly lazy, plz ignore
    fn range<'c>(&'c self, range: Range<B>) -> Option<ReadCursor<'c, B, Self>> {
        let valid_range = self.repr.range((self.f)(range.start)..(self.f)(range.end));
        if valid_range.is_some() {
            Some(ReadCursor::from(self, range))
        } else {
            None
        }
    }
    fn range_from<'c>(&'c self, start: B) -> Option<UnboundedCursor<'c, B, Self>> {
        let valid_range = self.repr.range_from((self.f)(start));
        if valid_range.is_some() {
            Some(UnboundedCursor::from(self, start))
        } else {
            None
        }
    }
}
