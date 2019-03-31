use yaxpeax_arch::Address;
use memory::repr::flat::FlatMemoryRepr;
use memory::repr::process::ModuleInfo;
use memory::{MemoryRange, MemoryRepr};

use std::ops::Range;

#[derive(Debug)]
pub struct ReadCursor<'a, A: Address, T: MemoryRepr<A> + ?Sized> {
    pub data: &'a T,
    pub start: A,
    pub end: A
    // TODO: do these need to be pub? used in From<memory::ReadCursor> for
    // InstructionIteratorSpanned
}

impl <'a, A: Address, T: MemoryRepr<A> + ?Sized> ReadCursor<'a, A, T> {
    pub fn from(data: &'a T, range: Range<A>) -> ReadCursor<'a, A, T> {
        ReadCursor {
            data: data,
            start: range.start,
            end: range.end
        }
    }
}

impl <'a, A: Address, T: MemoryRepr<A>> MemoryRepr<A> for ReadCursor<'a, A, T> {
    fn read(&self, addr: A) -> Option<u8> {
        if self.start + addr < self.end {
            self.data.read(self.start + addr)
        } else {
            None
        }
    }
    fn to_flat(self) -> Option<FlatMemoryRepr> {
        Some(FlatMemoryRepr::of(self.collect()))
    }
    fn module_info(&self) -> Option<&ModuleInfo> { self.data.module_info() }
}

#[derive(Debug)]
pub struct UnboundedCursor<'a, A: Address, T: MemoryRepr<A> + ?Sized> {
    data: &'a T,
    addr: A
}

impl <'a, A: Address, T: MemoryRepr<A> + ?Sized> UnboundedCursor<'a, A, T> {
    pub fn from(data: &'a T, addr: A) -> UnboundedCursor<'a, A, T> {
        UnboundedCursor {
            data: data,
            addr: addr
        }
    }
}

impl <'a, A: Address, T: MemoryRepr<A>> Iterator for UnboundedCursor<'a, A, T> {
    type Item = u8;
    fn next(&mut self) -> Option<u8> {
        let res = self.data.read(self.addr);
        if res.is_some() {
            self.addr += A::one();
        }
        res
    }
}

impl <'a, A: Address, T: MemoryRepr<A>> MemoryRepr<A> for UnboundedCursor<'a, A, T> {
    fn read(&self, addr: A) -> Option<u8> {
        self.data.read(self.addr + addr)
    }
    fn to_flat(self) -> Option<FlatMemoryRepr> {
        Some(FlatMemoryRepr::of(self.collect()))
    }
    fn module_info(&self) -> Option<&ModuleInfo> { self.data.module_info() }
}

impl <'a, A: Address, T: MemoryRepr<A>> Iterator for ReadCursor<'a, A, T> {
    type Item = u8;
    fn next(&mut self) -> Option<u8> {
        if self.start < self.end {
            let res = self.data.read(self.start);
            if res.is_some() {
                self.start += A::one();
            }
            res
        } else {
            None
        }
    }
}
