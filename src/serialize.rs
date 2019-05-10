use std::cell::Cell;
use serde::{Serialize, Serializer};
use serde::ser::SerializeStruct;

use petgraph::graphmap::{GraphMap, NodeTrait};

use std::hash::Hash;
use std::collections::HashMap;

pub struct GraphSerializer<'a, A> {
    graph: &'a GraphMap<A, (), petgraph::Directed>
}

impl <'a, A: NodeTrait + Hash + Serialize> GraphSerializer<'a, A> {
    pub fn from(graph: &'a GraphMap<A, (), petgraph::Directed>) -> GraphSerializer<'a, A> {
        GraphSerializer { graph }
    }
}

impl <'a, A: NodeTrait + Hash + Serialize> Serialize for GraphSerializer<'a, A> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut struc = serializer.serialize_struct("GraphMap<A>", 2)?;
        let edgevec: Vec<(A, A)> = self.graph.all_edges().map(|(a, b, _)| (a.to_owned(), b.to_owned())).collect();
        struc.serialize_field("edges", &edgevec)?;
        let nodevec: Vec<A> = self.graph.nodes().map(|n| n.to_owned()).collect();
        struc.serialize_field("nodes", &nodevec)?;
        struc.end()
    }
}

pub trait Memoable: Sized {
    type Out: Sized + Serialize;

    fn memoize(&self, memos: &HashMap<Self, u32>) -> Self::Out;
}

pub struct Memos<T: Hash + PartialEq + Eq> {
    pub node_ids: HashMap<T, u32>,
}

impl <T: Hash + PartialEq + Eq> Memos<T> {
    pub fn new() -> Memos<T> {
        Memos {
            node_ids: HashMap::new(),
        }
    }

    pub fn id_of(&mut self, t: T) -> u32 {
        let next = self.node_ids.len();
        *self.node_ids.entry(t).or_insert_with(|| {
            next as u32 // TODO: error on overflow
        })
    }
}

pub struct MemoizingSerializer<'a, 'b, T: ?Sized, M: Hash + PartialEq + Eq> {
    pub memos: Cell<&'a mut Memos<M>>,
    pub inner: &'b T,
}

impl <'a, 'b, T: ?Sized, M: Hash + PartialEq + Eq> MemoizingSerializer<'a, 'b, T, M> {
    pub fn new(memos: &'a mut Memos<M>, inner: &'b T) -> Self {
        MemoizingSerializer { memos: Cell::new(memos), inner }
    }

    pub fn memos(&self) -> &'a mut Memos<M> {
        unsafe {
            *self.memos.as_ptr()
        }
    }

    pub fn id_of(&self, memo: M) -> u32 {
        unsafe {
            (*self.memos.as_ptr()).id_of(memo)
        }
    }
}

