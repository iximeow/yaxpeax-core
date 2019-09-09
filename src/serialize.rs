use serde::de;
use std::fmt;
use serde::de::{MapAccess, SeqAccess, Deserialize, Deserializer, Visitor};
use std::cell::Cell;
use serde::{Serialize, Serializer};
use serde::ser::SerializeStruct;

use yaxpeax_arch::Address;
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

pub struct GraphDeserializer<A> {
    graph: GraphMap<A, (), petgraph::Directed>
}

impl <A> GraphDeserializer<A> {
    pub fn into_inner(self) -> GraphMap<A, (), petgraph::Directed> {
        self.graph
    }
}

#[derive(Deserialize)]
#[serde(field_identifier, rename_all = "lowercase")]
enum Field { Edges, Nodes }

struct GraphVisitor<A> {
    _marker: std::marker::PhantomData<A>
}

impl <A: Hash + Ord + Copy + Clone> GraphVisitor<A> {
    pub fn new() -> Self {
        GraphVisitor {
            _marker: std::marker::PhantomData
        }
    }
}

impl <'de, A: Address + Hash> Visitor<'de> for GraphVisitor<A> {
    type Value = GraphDeserializer<A>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("struct GraphMap<A, (), Directed>")
    }

    fn visit_seq<V>(self, mut seq: V) -> Result<Self::Value, V::Error>
    where
        V: SeqAccess<'de>
    {
        let mut graph = GraphMap::new();
        let edges: Vec<(A, A)> = seq.next_element()?
            .ok_or_else(|| de::Error::invalid_length(0, &self))?;
        for (start, end) in edges.into_iter() {
            graph.add_edge(start, end, ());
        }
        let nodes: Vec<A> = seq.next_element()?
            .ok_or_else(|| de::Error::invalid_length(1, &self))?;
        for node in nodes.into_iter() {
            graph.add_node(node);
        }

        Ok(GraphDeserializer { graph })
    }

    fn visit_map<V>(self, mut map: V) -> Result<Self::Value, V::Error>
    where
        V: MapAccess<'de>,
    {
        let mut graph = GraphMap::new();
        let mut edges = None;
        let mut nodes = None;
        while let Some(key) = map.next_key()? {
            match key {
                Field::Edges => {
                    if edges.is_some() {
                        return Err(de::Error::duplicate_field("edges"));
                    }
                    edges = Some(map.next_value()?);
                },
                Field::Nodes => {
                    if nodes.is_some() {
                        return Err(de::Error::duplicate_field("nodes"));
                    }
                    nodes = Some(map.next_value()?);
                }
            }
        }
        let edges: Vec<(A, A)> = edges.ok_or_else(|| de::Error::missing_field("edges"))?;
        for (start, end) in edges.into_iter() {
            graph.add_edge(start, end, ());
        }
        let nodes: Vec<A> = nodes.ok_or_else(|| de::Error::missing_field("nodes"))?;
        for node in nodes.into_iter() {
            graph.add_node(node);
        }

        Ok(GraphDeserializer { graph })
    }
}

impl<'de, A: Address + Hash> Deserialize<'de> for GraphDeserializer<A> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        const FIELDS: &'static [&'static str] = &["edges", "nodes"];
        deserializer.deserialize_struct(
            "GraphMap<A, (), Directed>",
            FIELDS,
            GraphVisitor::new()
        )
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

