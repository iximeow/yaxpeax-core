use yaxpeax_arch::{Arch, AddressBase};
use memory::repr::ReadCursor;
use memory::{LayoutError, MemoryRange, MemoryRepr, Named, PatchyMemoryRepr};
use memory::repr::process::ModuleInfo;
use std::ops::Range;

#[derive(Clone, Debug)]
pub struct FlatMemoryRepr {
    data: Vec<u8>,
    pub name: String
}

impl FlatMemoryRepr {
    pub fn empty(name: String) -> FlatMemoryRepr {
        FlatMemoryRepr {
            data: vec![],
            name: name
        }
    }
    pub fn of(data: Vec<u8>) -> FlatMemoryRepr {
        let mut mem = FlatMemoryRepr::empty("anon_flat_repr".to_string());
        mem.add(data, 0 as usize).unwrap();
        mem
    }
    pub fn len(&self) -> usize {
        self.data.len()
    }
    pub fn add(&mut self, data: Vec<u8>, offset: usize) -> Result<(), LayoutError> {
        let mut cursor = 0;

        if data.len() + offset > self.data.len() {
            // If we're going to need more space, reserve it all together
            let needed = (data.len() + offset) - self.data.len();
            self.data.reserve(needed);
        }

        if offset < self.data.len() {
            // we can copy over up to `self.data.len() - offset` bytes
            while (cursor + offset) < self.data.len() && cursor < data.len() {
                self.data[cursor + offset] = self.data[cursor];
                cursor += 1;
            }
        }
        while cursor < data.len() {
            self.data.push(data[cursor]);
            cursor += 1;
        }

        Ok(())
    }
    pub(crate) fn data(&self) -> &[u8] {
        &self.data[..]
    }
}

impl <A: Arch> MemoryRepr<A> for FlatMemoryRepr {
    fn read(&self, addr: A::Address) -> Option<u8> {
        if addr.to_linear() < self.data.len() {
            Some(self.data[addr.to_linear()])
        } else {
            None
        }
    }
    fn as_flat(&self) -> Option<FlatMemoryRepr> {
        Some(self.clone())
    }
    fn module_info(&self) -> Option<&ModuleInfo> { None }
    fn module_for(&self, _addr: A::Address) -> Option<&dyn MemoryRepr<A>> {
        Some(self)
    }
    fn size(&self) -> Option<u64> {
        Some(self.data.len() as u64)
    }
}

impl Named for FlatMemoryRepr {
    fn name(&self) -> &str {
        &self.name
    }
}

impl <A: Arch> PatchyMemoryRepr<A> for FlatMemoryRepr {
    fn add(&mut self, data: Vec<u8>, addr: A::Address) -> Result<(), LayoutError> {
        FlatMemoryRepr::add(self, data, addr.to_linear())
    }
}

/*
 * TODO: pick up here.
 */
impl <A: Arch> MemoryRange<A> for FlatMemoryRepr {
    fn range<'a>(&'a self, range: Range<A::Address>) -> Option<ReadCursor<'a, A, Self>> {
        if range.start.to_linear() < self.data.len() && range.end.to_linear() < self.data.len() && range.start < range.end {
            Some(ReadCursor::from(self, range.start, Some(range.end)))
        } else {
            None
        }
    }
    fn range_from<'a>(&'a self, start: A::Address) -> Option<ReadCursor<'a, A, Self>> {
        if start.to_linear() < self.data.len() {
            Some(ReadCursor::from(self, start, None))
        } else {
            None
        }
    }
}

