use yaxpeax_arch::Address;
use std::cell::RefCell;
use memory::repr::FlatMemoryRepr;
use memory::{MemoryRange, MemoryRepr, Named};
use debug::Peek;
use std::ops::Range;
use memory::repr::process::ModuleInfo;
use memory::repr::cursor::{UnboundedCursor, ReadCursor};

pub struct RemoteMemoryRepr<T: Peek> {
    pub cache: RefCell<Vec<(usize, Vec<u8>)>>,
    pub last_addr: RefCell<usize>,
    pub dirty_bit: bool,
    pub target: T
}

impl <A: Address, T: Peek> MemoryRange<A> for RemoteMemoryRepr<T> {
    fn range<'a>(&'a self, range: Range<A>) -> Option<ReadCursor<'a, A, Self>> {
        Some(ReadCursor::from(self, range))
    }
    fn range_from<'a>(&'a self, start: A) -> Option<UnboundedCursor<'a, A, Self>> {
        Some(UnboundedCursor::from(self, start))
    }
}

impl <A: Address, T: Peek> MemoryRepr<A> for RemoteMemoryRepr<T> {
    fn read(&self, addr: A) -> Option<u8> {
        let mut cache = self.cache.borrow_mut();
        if *self.last_addr.borrow() + 1 != addr.to_linear() {
            // not linearly scanning anymore, drop the cache
            // TODO: this is a hack!! because gdb can't report when remote memory changed, so the
            // cache becomes stale.
            *cache = Vec::new();
        }
        let addr = addr.to_linear();
        let line = cache.iter().find(|(start, data)| addr >= *start && addr < *start + data.len());
//        tracing::event!(tracing::Level::INFO, "cache line result for address {}: {:?}", addr.to_linear(), line.map(|(s, _)| s));
        eprintln!("cache line result for address {:#x}: {:?}", addr.to_linear(), line.map(|(s, _)| s));
        if line.is_none() { // || self.dirty_bit {
            let mut new_data = vec![];
            if self.target.read_bytes(addr & !255, 256, &mut new_data).is_some() {
//                eprintln!("got new line starting at {:#x}: {:#x?}", addr & !255, new_data);
                cache.push((addr & !255, new_data));
            }
        }

        cache.iter().find(|(start, data)| addr >= *start && addr < *start + data.len()).map(|(start, data)| {
            *self.last_addr.borrow_mut() = addr;
            data[addr - start]
        })
    }
    fn to_flat(self) -> Option<FlatMemoryRepr> {
        None
    }
    // TODO: read a ModuleInfo out of the remote process
    fn module_info(&self) -> Option<&ModuleInfo> { None }
    fn module_for(&self, _addr: A) -> Option<&dyn MemoryRepr<A>> {
        /*
        for module in self.modules.iter() {
            if module.contains(addr) {
                Some(&module)
            } else {
                None
            }
        }
        */
        None
    }
    fn size(&self) -> Option<u64> {
        Some(std::u64::MAX)
    }
}

impl <T: Peek> Named for RemoteMemoryRepr<T> {
    fn name(&self) -> &str {
//  TODO: proper trait over debug target stuff
//        self.target.name
        "remote target"
    }
}
