use analyses::control_flow::ControlFlowGraph;

use std::fmt;

use serde::de::{self, Deserialize, Deserializer, Visitor, SeqAccess, MapAccess};

impl<'de, A: Address + Hash + Deserialize> Deserialize<'de> for ControlFlowGraph<A> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "lowercase")]
        enum Field { Entrypoint, Blocks, Graph };

        struct GraphVisitor;

        impl<'de> Visitor<'de> for GraphVisitor {
            type Value = Duration;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Duration")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<ControlFlowGraph<A>, V::Error>
            where
                V: SeqAccess<'de>,
            {
                let entrypoint = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                let blocks = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(1, &self))?;
                let graph = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(2, &self))?;
                Ok(ControlFlowGraph {
                    entrypoint,
                    blocks,
                    graph,
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
                let graph = graph.ok_or_else(|| de::Error::missing_field("graph"))?;
                Ok(ControlFlowGraph {
                    entrypoint,
                    blocks,
                    graph,
                })
            }
        }

        const FIELDS: &'static [&'static str] = &["entrypoint", "blocks", "graph"];
        deserializer.deserialize_struct("CFG<A>", FIELDS, GraphVisitor)
    }
}
