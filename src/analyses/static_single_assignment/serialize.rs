use std::collections::HashMap;
use std::rc::Rc;
use serde::{Serialize, Serializer};
use serde::ser::{SerializeMap, SerializeStruct, SerializeSeq};
use serialize::{Memoable, Memos, MemoizingSerializer};
use analyses::static_single_assignment::{SSA, SSAValues, HashedValue, DFGRef, RWMap, PhiLocations};
use yaxpeax_arch::Arch;

impl <'a, 'b, A: Arch + SSAValues> Serialize for MemoizingSerializer<'a, 'b, HashMap<A::Address, PhiLocations<A>>, HashedValue<DFGRef<A>>> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut phi_map = serializer.serialize_map(Some(self.inner.len()))?;
        for (addr, phis) in self.inner.iter() {
            phi_map.serialize_entry(addr, &MemoizingSerializer::new(unsafe { *self.memos.as_ptr() }, phis))?;
        }
        phi_map.end()
    }
}

impl <'a, 'b, A: Arch + SSAValues> Serialize for MemoizingSerializer<'a, 'b, PhiLocations<A>, HashedValue<DFGRef<A>>> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut location_phis_map = serializer.serialize_map(Some(self.inner.len()))?;
        for (loc, phispec) in self.inner.iter() {
            let new_phiargs: Vec<u32> = phispec.ins.iter().map(|v| {
                self.id_of(HashedValue { value: Rc::clone(v) })
            }).collect();
            let newvalue = (self.id_of(HashedValue { value: Rc::clone(&phispec.out) }), new_phiargs);
            location_phis_map.serialize_entry(&format!("{:?}", loc), &newvalue)?;
        }
        location_phis_map.end()
    }
}

impl <'a, 'b, A: Arch + SSAValues> Serialize for MemoizingSerializer<'a, 'b, HashMap<A::Address, RWMap<A>>, HashedValue<DFGRef<A>>> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut version_maps = serializer.serialize_map(Some(self.inner.len()))?;
        for (k, m) in self.inner.iter() {
            version_maps.serialize_entry(k, &MemoizingSerializer::new(unsafe { *self.memos.as_ptr() }, m))?;
        }
        version_maps.end()
    }
}

impl <'a, 'b, A: Arch + SSAValues> Serialize for MemoizingSerializer<'a, 'b, RWMap<A>, HashedValue<DFGRef<A>>> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut version_map = serializer.serialize_map(Some(self.inner.len()))?;
        for ((loc, dir), v) in self.inner.iter() {
            let s = format!("loc={:?}:dir={:?}", loc, dir);
            version_map.serialize_entry(&s, &self.id_of(HashedValue { value: Rc::clone(v) })).unwrap();
        };
        version_map.end()
    }
}

impl <A: Arch + SSAValues> Serialize for Memos<HashedValue<DFGRef<A>>> where HashedValue<DFGRef<A>>: Memoable {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut seq = serializer.serialize_seq(Some(self.node_ids.len()))?;
        for i in 0..self.node_ids.len() {
            for (k, v) in self.node_ids.iter() {
                if (i as u32) == *v {
                    seq.serialize_element(&k.memoize(&self.node_ids))?;
                }
            }
        }
        seq.end()
    }
}

impl <A: Arch + SSAValues + 'static> Serialize for SSA<A> where HashedValue<DFGRef<A>>: Memoable {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut memoizer: Memos<HashedValue<DFGRef<A>>> = Memos::new();

        let mut ssa_serializer = serializer.serialize_struct("SSA", 3)?;

        {
            let values = MemoizingSerializer::new(&mut memoizer, &self.instruction_values);
            ssa_serializer.serialize_field("values", &values)?;
        }

        {
            let phis = MemoizingSerializer::new(&mut memoizer, &self.phi);
            ssa_serializer.serialize_field("phis", &phis)?;
        }

        ssa_serializer.serialize_field("values", &memoizer)?;

        ssa_serializer.end()

    }
}
