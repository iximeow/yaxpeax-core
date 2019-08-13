use yaxpeax_arch::Address;
use memory::repr::FlatMemoryRepr;
use memory::{MemoryRange, MemoryRepr, Named};
use debug::Peek;
use debug::gdb_remote::GDBRemote;
use std::fmt::Display;
use std::ops::Range;
use memory::repr::process::ModuleInfo;
use memory::repr::cursor::{UnboundedCursor, ReadCursor};

pub struct RemoteMemoryRepr<T: Peek> {
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
        self.target.read(addr)
    }
    fn to_flat(self) -> Option<FlatMemoryRepr> {
        None
    }
    // TODO: read a ModuleInfo out of the remote process
    fn module_info(&self) -> Option<&ModuleInfo> { None }
    fn module_for(&self, _addr: A) -> Option<&MemoryRepr<A>> {
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
        None
    }
}

impl <T: Peek> Named for RemoteMemoryRepr<T> {
    fn name(&self) -> &str {
//  TODO: proper trait over debug target stuff
//        self.target.name
        "remote target"
    }
}
