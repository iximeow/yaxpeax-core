use yaxpeax_arch::Address;
use memory::repr::FlatMemoryRepr;
use memory::MemoryRepr;
use debug::Peek;
use std::fmt::Debug;
use memory::repr::process::ModuleInfo;

#[derive(Debug)]
pub struct RemoteMemoryRepr<T: Peek + Debug> {
    target: T,
}

impl <A: Address, T: Peek + Debug> MemoryRepr<A> for RemoteMemoryRepr<T> {
    fn read(&self, addr: A) -> Option<u8> {
        self.target.read(addr)
    }
    fn to_flat(self) -> Option<FlatMemoryRepr> {
        None
    }
    // TODO: read a ModuleInfo out of the remote process
    fn module_info(&self) -> Option<&ModuleInfo> { None }
}

