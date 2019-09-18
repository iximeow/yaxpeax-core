pub mod modifier;
pub mod types;

use yaxpeax_arch::Arch;

use serde::Serialize;
use std::hash::Hash;
use std::fmt::Debug;

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone, Serialize, Deserialize)]
pub enum Direction {
    Read,
    Write
}

pub trait AliasInfo where Self: Sized {
    fn aliases_of(&self) -> Vec<Self>;
    fn maximal_alias_of(&self) -> Self;
}

pub trait ValueLocations: Arch {
    type Location: Debug + Hash + Eq + Serialize + Copy + Clone + AliasInfo;

    fn decompose(op: &Self::Instruction) -> Vec<(Option<Self::Location>, Direction)>;
}

pub trait LocIterator<Location> {
    type Item;
    type Iter: Iterator<Item = Self::Item>;
    fn iter_locs(self) -> Self::Iter;
}
