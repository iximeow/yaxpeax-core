use analyses::control_flow::ControlFlowGraph;

use yaxpeax_arch::Address;
use std::hash::Hash;
use std::fmt;

use serde::de::{self, Deserialize, Deserializer, Visitor, SeqAccess, MapAccess};

#[derive(Deserialize)]
#[serde(field_identifier, rename_all = "lowercase")]
enum Field { Entrypoint, Blocks, Graph }

struct CFGVisitor<A> { _marker: std::marker::PhantomData<A> }

impl<'de, A: Address + Hash> Visitor<'de> for CFGVisitor<A> {
    type Value = ControlFlowGraph<A>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("struct ControlFlowGraph<A>")
    }

    fn visit_seq<V>(self, mut seq: V) -> Result<ControlFlowGraph<A>, V::Error>
    where
        V: SeqAccess<'de>,
    {
        let entrypoint = seq.next_element()?
            .ok_or_else(|| de::Error::invalid_length(0, &self))?;
        let blocks = seq.next_element()?
            .ok_or_else(|| de::Error::invalid_length(1, &self))?;
        let graph: crate::serialize::GraphDeserializer<A> = seq.next_element()?
            .ok_or_else(|| de::Error::invalid_length(2, &self))?;
        Ok(ControlFlowGraph {
            entrypoint,
            blocks,
            graph: graph.into_inner(),
        })
    }

    fn visit_map<V>(self, mut map: V) -> Result<ControlFlowGraph<A>, V::Error>
    where
        V: MapAccess<'de>,
    {
        let mut entrypoint = None;
        let mut blocks = None;
        let mut graph = None;
        while let Some(key) = map.next_key()? {
            match key {
                Field::Entrypoint => {
                    if entrypoint.is_some() {
                        return Err(de::Error::duplicate_field("entrypoint"));
                    }
                    entrypoint = Some(map.next_value()?);
                },
                Field::Blocks => {
                    if blocks.is_some() {
                        return Err(de::Error::duplicate_field("blocks"));
                    }
                    blocks = Some(map.next_value()?);
                },
                Field::Graph => {
                    if graph.is_some() {
                        return Err(de::Error::duplicate_field("graph"));
                    }
                    graph = Some(map.next_value()?);
                }
            }
        }
        let entrypoint = entrypoint.ok_or_else(|| de::Error::missing_field("entrypoint"))?;
        let blocks = blocks.ok_or_else(|| de::Error::missing_field("blocks"))?;
        let graph: crate::serialize::GraphDeserializer<A> = graph.ok_or_else(|| de::Error::missing_field("graph"))?;
        Ok(ControlFlowGraph {
            entrypoint,
            blocks,
            graph: graph.into_inner(),
        })
    }
}

impl<'de, A: Address + Hash> Deserialize<'de> for ControlFlowGraph<A> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {

        const FIELDS: &'static [&'static str] = &["entrypoint", "blocks", "graph"];
        let visitor: CFGVisitor<A> = CFGVisitor { _marker: std::marker::PhantomData };
        deserializer.deserialize_struct("CFG<A>", FIELDS, visitor)
    }
}
