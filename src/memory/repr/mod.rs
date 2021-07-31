use yaxpeax_arch::{Arch, AddressBase, AddressDiff, ReadError, U16le};
use memory::{MemoryRange, MemoryRepr, Named};
use std::ops::Range;
use num_traits::Zero;

pub mod cursor;
pub use self::cursor::{ReadCursor};
pub mod slice;
pub use self::slice::{ProgramSlice, SourceInfo};
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

impl <A: Arch> MemoryRepr<A> for Vec<u8> {
    fn read(&self, addr: A::Address) -> Option<u8> {
        let linear_addr = addr.to_linear();
        if linear_addr < self.len() {
            Some(self[linear_addr])
        } else {
            None
        }
    }
    fn as_flat(&self) -> Option<FlatMemoryRepr> {
        Some(FlatMemoryRepr::of(self.clone()))
    }
    fn module_info(&self) -> Option<&ModuleInfo> { None }
    fn module_for(&self, _addr: A::Address) -> Option<&dyn MemoryRepr<A>> {
        Some(self)
    }
    fn size(&self) -> Option<u64> {
        Some(self.len() as u64)
    }
    fn start(&self) -> Option<u64> {
        Some(0)
    }
}

impl Named for [u8] {
    fn name(&self) -> &str {
        "anonymous_slice"
    }
}

impl Named for Vec<u8> {
    fn name(&self) -> &str {
        "anonymous_vec"
    }
}

impl <A: Arch> MemoryRepr<A> for [u8] {
    fn read(&self, addr: A::Address) -> Option<u8> {
        let linear_addr = addr.to_linear();
        if linear_addr < self.len() {
            Some(self[linear_addr])
        } else {
            None
        }
    }
    fn as_flat(&self) -> Option<FlatMemoryRepr> {
        Some(FlatMemoryRepr::of(self.to_vec()))
    }
    fn module_info(&self) -> Option<&ModuleInfo> {
        None
    }
    fn module_for(&self, addr: A::Address) -> Option<&dyn MemoryRepr<A>> {
        None
    }
    fn size(&self) -> Option<u64> {
        Some(self.len() as u64)
    }
}

impl <A: Arch> MemoryRange<A> for Vec<u8> {
    fn range<'a>(&'a self, range: Range<A::Address>) -> Option<ReadCursor<'a, A, Self>> {
        if range.start.to_linear() < self.len() && range.end.to_linear() <= self.len() && range.start < range.end {
            Some(ReadCursor::from(self, range.start, Some(range.end)))
        } else {
            None
        }
    }
    fn range_from<'a>(&'a self, start: A::Address) -> Option<ReadCursor<'a, A, Self>> {
        if start.to_linear() < self.len() {
            Some(ReadCursor::from(self, start, None))
        } else {
            None
        }
    }
}

impl <A: Arch> MemoryRange<A> for [u8] {
    fn range<'a>(&'a self, range: Range<A::Address>) -> Option<ReadCursor<'a, A, Self>> {
        if range.start.to_linear() < self.len() && range.end.to_linear() <= self.len() && range.start < range.end {
            Some(ReadCursor::from(self, range.start, Some(range.end)))
        } else {
            None
        }
    }
    fn range_from<'a>(&'a self, start: A::Address) -> Option<ReadCursor<'a, A, Self>> {
        if start.to_linear() < self.len() {
            Some(ReadCursor::from(self, start, None))
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct MemoryReprReader<'data, A: Arch, T: MemoryRepr<A> + ?Sized> {
    data: &'data T,
    position: A::Address,
    mark: A::Address,
    start: A::Address,
}

impl<'data, A: Arch, M: MemoryRepr<A>> yaxpeax_arch::Reader<A::Address, u8> for MemoryReprReader<'data, A, M> {
    fn next(&mut self) -> Result<u8, yaxpeax_arch::ReadError> {
        self.data.read(self.position).map(|b| {
            self.position = self.position.wrapping_offset(AddressDiff::one());
            b
        }).ok_or(yaxpeax_arch::ReadError::ExhaustedInput)
    }
    fn next_n(&mut self, buf: &mut [u8]) -> Result<(), yaxpeax_arch::ReadError> {
        for i in 0..buf.len() {
            match self.data.read(self.position) {
                Some(b) => {
                    buf[i] = b;
                    self.position += AddressDiff::one();
                }
                None => {
                    return Err(yaxpeax_arch::ReadError::ExhaustedInput);
                }
            }
        }
        Ok(())
    }
    fn mark(&mut self) {
        self.mark = self.position;
    }
    fn offset(&mut self) -> A::Address {
        A::Address::zero().wrapping_offset(self.position.diff(&self.mark).unwrap())
    }
    fn total_offset(&mut self) -> A::Address {
        A::Address::zero().wrapping_offset(self.position.diff(&self.start).unwrap())
    }
}

impl<'data, A: Arch, M: MemoryRepr<A>> yaxpeax_arch::Reader<A::Address, U16le> for MemoryReprReader<'data, A, M> {
    fn next(&mut self) -> Result<U16le, yaxpeax_arch::ReadError> {
        let high_addr = self.position.wrapping_offset(AddressDiff::one());

        let low = self.data.read(self.position).ok_or(ReadError::ExhaustedInput)? as u16;
        let high = self.data.read(self.position).ok_or(ReadError::ExhaustedInput)? as u16;

        self.position = high_addr.wrapping_offset(AddressDiff::one());

        Ok(U16le(low | (high << 8)))
    }
    fn next_n(&mut self, buf: &mut [U16le]) -> Result<(), yaxpeax_arch::ReadError> {
        for i in 0..buf.len() {
            buf[i] = self.next()?;
        }
        Ok(())
    }
    fn mark(&mut self) {
        self.mark = self.position;
    }
    fn offset(&mut self) -> A::Address {
        A::Address::zero().wrapping_offset(self.position.diff(&self.mark).unwrap())
    }
    fn total_offset(&mut self) -> A::Address {
        A::Address::zero().wrapping_offset(self.position.diff(&self.start).unwrap())
    }
}
