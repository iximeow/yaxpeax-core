use arch;

use tracing::{event, Level};

use std::collections::BTreeMap;
use std::ops::Range;
use std::ops::Bound::Included;

use yaxpeax_arch::{Address, AddressDisplay};
use memory::repr::{FlatMemoryRepr, ReadCursor, UnboundedCursor};
use memory::{LayoutError, ModuleInfo, MemoryRange, MemoryRepr, Named, PatchyMemoryRepr};
use arch::ISA;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SourceInfo {
    filename: String,
    sha256: String,
}

impl SourceInfo {
    pub fn new(filename: String, sha256: String) -> Self {
        SourceInfo {
            filename,
            sha256,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProgramSlice {
    name: String,
    source_info: Option<SourceInfo>,
    entrypoint: Option<u64>,
    chunks: BTreeMap<u64, Vec<u8>>,
}

impl ProgramSlice {
    pub fn add_chunk(&mut self, addr: u64, data: Vec<u8>) {
        // TODO: assert that no data overlaps with `addr`, or that if data does overlap, it
        // matches `vec`
        self.chunks.insert(addr, data);
    }

    pub fn empty(name: String) -> ProgramSlice {
        ProgramSlice {
            name,
            source_info: None,
            entrypoint: None,
            chunks: BTreeMap::new(),
        }
    }
    pub fn with_source_info(&mut self, source_info: Option<SourceInfo>) -> &mut Self {
        self.source_info = source_info;
        self
    }
    pub fn with_entrypoint(&mut self, entrypoint: Option<u64>) -> &mut Self {
        self.entrypoint = entrypoint;
        self
    }
    pub fn of(data: Vec<u8>) -> FlatMemoryRepr {
        let mut mem = FlatMemoryRepr::empty("anon_flat_repr".to_string());
        mem.add(data, 0 as u32).unwrap();
        mem
    }
    pub fn len(&self) -> usize {
        if self.chunks.len() > 0 {
            let end = *self.chunks.keys().last().unwrap() + self.chunks.values().last().unwrap().len() as u64;
            (end - self.chunks.keys().next().unwrap()) as usize
        } else {
            0
        }
    }
}

impl <A: Address> MemoryRepr<A> for ProgramSlice {
    fn read(&self, addr: A) -> Option<u8> {
        let (start, data) = if let Some((start, data)) = self.chunks.range((Included(&0), Included(&(addr.to_linear() as u64)))).rev().next() {
            (start, data)
        } else {
            return None;
        };

        // if we find a chunk, either it contains this address, or it's the last chunk before it.
        let offset = addr.to_linear() as u64 - *start;
        let offset = if offset <= std::usize::MAX as u64 {
            offset as usize
        } else {
            // this would be a larger-than-vec offset? this is an internal error, really
            return None;
        };
        if offset < data.len() {
            Some(data[offset])
        } else {
            None
        }
    }
    fn to_flat(self) -> Option<FlatMemoryRepr> {
        None
    }
    fn module_info(&self) -> Option<&ModuleInfo> { None }
    fn module_for(&self, _addr: A) -> Option<&dyn MemoryRepr<A>> {
        None
    }
    fn size(&self) -> Option<u64> {
        if self.chunks.len() > 0 {
            let end = *self.chunks.keys().last().unwrap() + self.chunks.values().last().unwrap().len() as u64;
            Some(end - self.chunks.keys().next().unwrap())
        } else {
            None
        }
    }
}

impl Named for ProgramSlice {
    fn name(&self) -> &str {
        &self.name
    }
}
