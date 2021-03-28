use std::collections::HashMap;
use std::fmt;
use data::Direction;
use std::rc::Rc;
use std::cell::RefCell;
use analyses::static_single_assignment::data::{HashedValue, Value, PhiOp, SSA, SSAValues};
use serialize::Memoable;
use yaxpeax_arch::Arch;
use serde::de::{self, Deserialize, Deserializer, Visitor, SeqAccess};

struct DFGVisitor<A> { _marker: std::marker::PhantomData<A> }

impl<'de, A: Arch + SSAValues + 'static> Visitor<'de> for DFGVisitor<A>
where HashedValue<Rc<RefCell<Value<A>>>>: Memoable {
    type Value = SSA<A>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("struct SSA<A>")
    }

    fn visit_seq<V>(self, mut seq: V) -> Result<SSA<A>, V::Error>
    where
        V: SeqAccess<'de>,
    {
        type Memoed<A> = <HashedValue<Rc<RefCell<Value<A>>>> as Memoable>::Out;

        // indices are numbers to rebuild memos from
        let instruction_values: HashMap<A::Address, HashMap<(A::Location, Direction), u32>> = seq.next_element()?
            .ok_or_else(|| de::Error::invalid_length(0, &self))?;

        // TODO: this should actually involve a PhiOp<A>: Memoable bound and involve that, but
        // PhiOp currently is not actually Memoable and is ad-hoc serialized.
        //
        // so for the time being, match the type best-effort and hope it doesn't explode.
        let phis: HashMap<A::Address, HashMap<A::Location, (Vec<u32>, u32)>> = seq.next_element()?
            .ok_or_else(|| de::Error::invalid_length(1, &self))?;

        let memos: Vec<Memoed<A>> = seq.next_element()?
            .ok_or_else(|| de::Error::invalid_length(2, &self))?;

        // ok! we've read all the data out, now to turn it into something useful.
        //
        // first, rebuild memos into their original data
        let mut values: HashMap<u32, HashedValue<Rc<RefCell<Value<A>>>>> = HashMap::new();

        // TODO: why is it correct to ignore v?
        for (i, _v) in memos.iter().enumerate() {
            if values.contains_key(&(i as u32)) {
                continue;
            }
            let to_insert = <HashedValue<Rc<RefCell<Value<A>>>> as Memoable>::dememoize(i as u32, &memos, &mut values);

            // it is technically possible (in the case of reference cycles) that we just inserted
            // to_insert already, but a correct implementation would then also maintain the case
            // that to_insert is an Rc to the same RefCell as the existing entry for to_insert's
            // index, i. so correct implementations will never be made wrong by just inserting
            // to_insert again.

            values.insert(i as u32, to_insert);
        }

        // TODO:
        // type ValueEntry<A> = HashedValue<Rc<RefCell<Value<A>>>>;

        // we have all the values! on to the easy part of rebuilding maps.
        let mut dememoized_phis: HashMap<A::Address, HashMap<A::Location, PhiOp<A>>> = HashMap::new();
        let mut dememoized_values: HashMap<A::Address, HashMap<(A::Location, Direction), Rc<RefCell<Value<A>>>>> = HashMap::new();

        for (addr, vmap) in phis.iter() {
            let mut dememoized_valuemap = HashMap::new();
            for (loc, (phi_ins, phi_out)) in vmap.iter() {
                let dememoized_phi_ins: Vec<Rc<RefCell<Value<A>>>> = phi_ins.iter().map(|idx| values[idx].clone().value).collect();
                let dememoized_phi_out: Rc<RefCell<Value<A>>> = values[phi_out].clone().value;
                dememoized_valuemap.insert(loc.to_owned(), PhiOp { out: dememoized_phi_out, ins: dememoized_phi_ins });
            }
            dememoized_phis.insert(*addr, dememoized_valuemap);
        }

        for (addr, locmap) in instruction_values.iter() {
            let mut value_locmap = HashMap::new();
            for (locdir, idx) in locmap.iter() {
                value_locmap.insert(locdir.clone(), values[idx].clone().value);
            }
            dememoized_values.insert(*addr, value_locmap);
        }

        Ok(SSA {
            instruction_values: dememoized_values,
            modifier_values: HashMap::new(),
            control_dependent_values: HashMap::new(),
            defs: HashMap::new(),
            phi: dememoized_phis,
            indirect_values: HashMap::new(), // TODO: serialize and deserialize
        })
    }
}

impl<'de, A: Arch + SSAValues + 'static> Deserialize<'de> for SSA<A>
where
    HashedValue<Rc<RefCell<Value<A>>>>: Memoable,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        const FIELDS: &'static [&'static str] = &["instruction_values", "phis", "memos"];
        let visitor: DFGVisitor<A> = DFGVisitor { _marker: std::marker::PhantomData };
        deserializer.deserialize_struct("SSA<A>", FIELDS, visitor)
    }
}
