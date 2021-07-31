use yaxpeax_arch::{Arch, AddressDiff};
use memory::repr::MemoryReprReader;
use memory::repr::flat::FlatMemoryRepr;
use memory::repr::process::ModuleInfo;
use memory::MemoryRepr;
use memory::Named;
use num_traits::Zero;

use std::ops::Range;

#[derive(Clone, Debug)]
pub struct ReadCursor<'a, A: Arch + ?Sized, T: MemoryRepr<A> + ?Sized> {
    pub data: &'a T,
    pub start: A::Address,
    pub end: Option<A::Address>
}

impl <'a, A: Arch, T: MemoryRepr<A> + ?Sized> ReadCursor<'a, A, T> {
    pub fn from(data: &'a T, start: A::Address, end: Option<A::Address>) -> ReadCursor<'a, A, T> {
        ReadCursor {
            data: data,
            start: start,
            end: end
        }
    }

    pub fn to_reader<'data>(&'data self) -> MemoryReprReader<'data, A, Self> {
        MemoryReprReader {
            data: self,
            position: A::Address::zero(),
            mark: A::Address::zero(),
            start: A::Address::zero(),
        }
    }
}

impl <'a, A: Arch, T: MemoryRepr<A> + ?Sized> MemoryRepr<A> for ReadCursor<'a, A, T> {
    fn read(&self, addr: A::Address) -> Option<u8> {
        if let Some(end) = self.end {
            if self.start + addr < end {
                self.data.read(self.start + addr)
            } else {
                None
            }
        } else {
            self.data.read(self.start + addr)
        }
    }
    fn as_flat(&self) -> Option<FlatMemoryRepr> {
        // TODO: why can't i just `self.clone()` here? this is only giving a &ReadCusror? but
        // ReadCursor impls Clone...
        let ReadCursor { data, start, end } = self;
        let mut cursor: Self = ReadCursor { data, start: *start, end: *end };
        let data: Vec<u8> = Iterator::collect(cursor);
        Some(FlatMemoryRepr::of(data))
    }
    fn module_info(&self) -> Option<&ModuleInfo> { self.data.module_info() }
    fn module_for(&self, addr: A::Address) -> Option<&dyn MemoryRepr<A>> {
        self.data.module_for(self.start + addr)
    }
    fn size(&self) -> Option<u64> {
        // TODO: should be able to get a `ptrdiff`-style diff type for addresses
        // Some(self.end.diff(self.start).unwrap().to_const() as u64)
        unimplemented!("ReadCursor::size() (need to be able to convert AddressBase::Diff to a u64...")
    }
}

impl <'a, A: Arch, T: MemoryRepr<A> + ?Sized> Named for ReadCursor<'a, A, T> {
    fn name(&self) -> &str {
        self.data.name()
    }
}

impl <'a, A: Arch, T: MemoryRepr<A> + ?Sized> Iterator for ReadCursor<'a, A, T> {
    type Item = u8;
    fn next(&mut self) -> Option<u8> {
        if let Some(end) = self.end {
            if self.start < end {
                let res = self.data.read(self.start);
                if res.is_some() {
                    self.start += AddressDiff::one();
                }
                res
            } else {
                None
            }
        } else {
            let res = self.data.read(self.start);
            if res.is_some() {
                self.start += AddressDiff::one();
            }
            res
        }
    }
}
