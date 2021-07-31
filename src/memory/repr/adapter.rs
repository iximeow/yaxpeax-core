use yaxpeax_arch::{Arch, Address};
use memory::{MemoryRange, MemoryRepr, Named};
use memory::repr::{FlatMemoryRepr, ReadCursor};
use memory::repr::process::ModuleInfo;
use std::ops::Range;

pub trait MemoryAdapter<
    'a,
    AAddr: Address,
    BAddr: Address,
    A: Arch<Address = AAddr>,
    B: Arch<Address = BAddr, Word = A::Word>,
    M: MemoryRepr<A> + ?Sized
> {
    fn to_address_space(&'a self) -> MemoryReprAdapter<'a, AAddr, BAddr, A, B, M>;
}

#[derive(Debug)]
pub struct MemoryReprAdapter<
    'a,
    AAddr: Address,
    BAddr: Address,
    A: Arch<Address = AAddr>,
    B: Arch<Address = BAddr, Word = A::Word>,
    M: MemoryRepr<A> + ?Sized
> {
    repr: &'a M,
    f: fn(B::Address) -> A::Address
}

fn f_u16_to_usize(u: u16) -> usize { u as usize }

impl <
    'a,
    A: Arch<Address = usize>,
    B: Arch<Word = A::Word, Address = u16>,
    M: MemoryRepr<A> + ?Sized
> MemoryAdapter<'a, usize, u16, A, B, M> for M {
    fn to_address_space(&'a self) -> MemoryReprAdapter<'a, usize, u16, A, B, M> {
        MemoryReprAdapter {
            repr: self,
            f: f_u16_to_usize
        }
    }
}

impl <
    'a,
    A: Arch<Address = usize>,
    B: Arch<Word = A::Word, Address = u16>,
    M: MemoryRepr<A>
> MemoryRepr<B> for MemoryReprAdapter<'a, usize, u16, A, B, M> {
    fn read(&self, addr: u16) -> Option<u8> {
        self.repr.read((self.f)(addr))
    }
    fn as_flat(&self) -> Option<FlatMemoryRepr> { None }
    fn module_info(&self) -> Option<&ModuleInfo> { self.repr.module_info() }
    fn module_for(&self, _addr: u16) -> Option<&dyn MemoryRepr<B>> {
        None
        // TODO: figure out if it's possible to write this?
        // this really messes up stuff...
    }
    fn size(&self) -> Option<u64> { self.repr.size() }
}

impl <
    'a,
    A: Arch<Address = usize>,
    B: Arch<Word = A::Word, Address = u16>,
    M: MemoryRepr<A>
> Named for MemoryReprAdapter<'a, usize, u16, A, B, M> {
    fn name(&self) -> &str { self.repr.name() }
}

fn f_u32_to_usize(u: u32) -> usize { u as usize }

impl <
    'a,
    A: Arch<Address = usize>,
    B: Arch<Word = A::Word, Address = u32>,
    M: MemoryRepr<A>
> MemoryAdapter<'a, usize, u32, A, B, M> for M {
    fn to_address_space(&'a self) -> MemoryReprAdapter<'a, usize, u32, A, B, M> {
        MemoryReprAdapter {
            repr: self,
            f: f_u32_to_usize
        }
    }
}

impl <
    'a,
    A: Arch<Address = usize>,
    B: Arch<Word = A::Word, Address = u32>,
    M: MemoryRepr<A>
> MemoryRepr<B> for MemoryReprAdapter<'a, usize, u32, A, B, M> {
    fn read(&self, addr: u32) -> Option<u8> {
        self.repr.read((self.f)(addr))
    }
    fn as_flat(&self) -> Option<FlatMemoryRepr> { None }
    fn module_info(&self) -> Option<&ModuleInfo> { self.repr.module_info() }
    fn module_for(&self, _addr: u32) -> Option<&dyn MemoryRepr<B>> {
        None
        // TODO: figure out if it's possible to write this?
        // self.repr.module_for((self.f)(addr))
    }
    fn size(&self) -> Option<u64> { self.repr.size() }
}

impl <
    'a,
    A: Arch<Address = usize>,
    B: Arch<Word = A::Word, Address = u32>,
    M: MemoryRepr<A>
> Named for MemoryReprAdapter<'a, usize, u32, A, B, M> {
    fn name(&self) -> &str { self.repr.name() }
}

fn f_u64_to_usize(u: u64) -> usize { u as usize }

impl <
    'a,
    A: Arch<Address = usize>,
    B: Arch<Word = A::Word, Address = u64>,
    M: MemoryRepr<A>
> MemoryAdapter<'a, usize, u64, A, B, M> for M {
    fn to_address_space(&'a self) -> MemoryReprAdapter<'a, usize, u64, A, B, M> {
        MemoryReprAdapter {
            repr: self,
            f: f_u64_to_usize
        }
    }
}

impl <
    'a,
    A: Arch<Address = usize>,
    B: Arch<Word = A::Word, Address = u64>,
    M: MemoryRepr<A>
> MemoryRepr<B> for MemoryReprAdapter<'a, usize, u64, A, B, M> {
    fn read(&self, addr: u64) -> Option<u8> {
        self.repr.read((self.f)(addr))
    }
    fn as_flat(&self) -> Option<FlatMemoryRepr> { None }
    fn module_info(&self) -> Option<&ModuleInfo> { self.repr.module_info() }
    fn module_for(&self, _addr: u64) -> Option<&dyn MemoryRepr<B>> {
        None
        // TODO: figure out if it's possible to write this?
        // self.repr.module_for((self.f)(addr))
    }
    fn size(&self) -> Option<u64> { self.repr.size() }
}

impl <
    'a,
    A: Arch<Address = usize>,
    B: Arch<Word = A::Word, Address = u64>,
    M: MemoryRepr<A>
> Named for MemoryReprAdapter<'a, usize, u64, A, B, M> {
    fn name(&self) -> &str { self.repr.name() }
}

impl <
    'a,
    A: Arch,
    B: Arch<Word = A::Word>,
    M: MemoryRepr<A> + MemoryRange<A>
> MemoryRange<B> for MemoryReprAdapter<'a, A::Address, B::Address, A, B, M> where Self: MemoryRepr<B> {
    // TODO: I am incredibly lazy, plz ignore
    fn range<'c>(&'c self, range: Range<B::Address>) -> Option<ReadCursor<'c, B, Self>> {
        let valid_range = self.repr.range((self.f)(range.start)..(self.f)(range.end));
        if valid_range.is_some() {
            Some(ReadCursor::from(self, range.start, Some(range.end)))
        } else {
            None
        }
    }
    fn range_from<'c>(&'c self, start: B::Address) -> Option<ReadCursor<'c, B, Self>> {
        let valid_range = self.repr.range_from((self.f)(start));
        if valid_range.is_some() {
            Some(ReadCursor::from(self, start, None))
        } else {
            None
        }
    }
}
