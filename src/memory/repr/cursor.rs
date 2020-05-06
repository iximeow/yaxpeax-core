use yaxpeax_arch::{Address, AddressDiff};
use memory::repr::flat::FlatMemoryRepr;
use memory::repr::process::ModuleInfo;
use memory::MemoryRepr;
use memory::Named;

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
    fn module_for(&self, addr: A) -> Option<&dyn MemoryRepr<A>> {
        self.data.module_for(self.start + addr)
    }
    fn size(&self) -> Option<u64> {
        // TODO: should be able to get a `ptrdiff`-style diff type for addresses
        Some((self.end - self.start).to_linear() as u64)
    }
}

impl <'a, A: Address, T: MemoryRepr<A>> Named for ReadCursor<'a, A, T> {
    fn name(&self) -> &str {
        self.data.name()
    }
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
            self.addr += AddressDiff::one();
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
    fn module_for(&self, addr: A) -> Option<&dyn MemoryRepr<A>> {
        self.data.module_for(self.addr + addr)
    }
    fn size(&self) -> Option<u64> {
        None
    }
}

impl <'a, A: Address, T: MemoryRepr<A>> Named for UnboundedCursor<'a, A, T> {
    fn name(&self) -> &str {
        self.data.name()
    }
}

impl <'a, A: Address, T: MemoryRepr<A>> Iterator for ReadCursor<'a, A, T> {
    type Item = u8;
    fn next(&mut self) -> Option<u8> {
        if self.start < self.end {
            let res = self.data.read(self.start);
            if res.is_some() {
                self.start += AddressDiff::one();
            }
            res
        } else {
            None
        }
    }
}
