use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use std::cmp::Eq;
use petgraph::graphmap::GraphMap;
use petgraph;
use petgraph::visit::Bfs;

use std::cell::RefCell;
use std::rc::Rc;
use std::cell::Ref;
use std::cell::Cell;

use std::fmt::Debug;

use yaxpeax_arch::{Arch, LengthedInstruction};
use analyses::control_flow::{BasicBlock, ControlFlowGraph};
use memory::MemoryRange;

use serde::{Deserialize, Serialize, Serializer};
use serde::ser::{SerializeMap, SerializeStruct, SerializeSeq};
use std::borrow::Borrow;

#[derive(Debug)]
pub struct SSA<A: Arch + SSAValues> where A::Location: Hash + Eq, A::Address: Hash + Eq {
    // TODO: Fairly sure these Rc<RefCell<...>> could all just be raw pointers
    // these aren't individually freed so Rc shouldn't be necessary?
    pub values: HashMap<A::Address, HashMap<(A::Location, Direction), Rc<RefCell<Value<A>>>>>,
    pub phi: HashMap<A::Address, HashMap<A::Location, (Rc<RefCell<Value<A>>>, Vec<Rc<RefCell<Value<A>>>>)>>
}

impl <'a, 'b, A: Arch + SSAValues> Serialize for MemoizingSerializer<'a, 'b, HashMap<A::Address, HashMap<A::Location, (Rc<RefCell<Value<A>>>, Vec<Rc<RefCell<Value<A>>>>)>>, HashedValue<Rc<RefCell<Value<A>>>>> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut phi_map = serializer.serialize_map(Some(self.inner.len()))?;
        for (addr, phis) in self.inner.iter() {
            phi_map.serialize_entry(addr, &MemoizingSerializer::new(unsafe { *self.memos.as_ptr() }, phis))?;
        }
        phi_map.end()
    }
}

impl <'a, 'b, A: Arch + SSAValues> Serialize for MemoizingSerializer<'a, 'b, HashMap<A::Location, (Rc<RefCell<Value<A>>>, Vec<Rc<RefCell<Value<A>>>>)>, HashedValue<Rc<RefCell<Value<A>>>>> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut location_phis_map = serializer.serialize_map(Some(self.inner.len()))?;
        for (loc, phispec) in self.inner.iter() {
            let new_phiargs: Vec<u32> = phispec.1.iter().map(|v| {
                self.id_of(HashedValue { value: Rc::clone(v) })
            }).collect();
            let newvalue = (self.id_of(HashedValue { value: Rc::clone(&phispec.0) }), new_phiargs);
            location_phis_map.serialize_entry(loc, &newvalue)?;
        }
        location_phis_map.end()
    }
}

impl <'a, 'b, A: Arch + SSAValues> Serialize for MemoizingSerializer<'a, 'b, HashMap<A::Address, HashMap<(A::Location, Direction), Rc<RefCell<Value<A>>>>>, HashedValue<Rc<RefCell<Value<A>>>>> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut version_maps = serializer.serialize_map(Some(self.inner.len()))?;
        for (k, m) in self.inner.iter() {
            version_maps.serialize_entry(k, &MemoizingSerializer::new(unsafe { *self.memos.as_ptr() }, m))?;
        }
        version_maps.end()
    }
}

impl <'a, 'b, A: Arch + SSAValues> Serialize for MemoizingSerializer<'a, 'b, HashMap<(A::Location, Direction), Rc<RefCell<Value<A>>>>, HashedValue<Rc<RefCell<Value<A>>>>> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut version_map = serializer.serialize_map(Some(self.inner.len()))?;
        self.inner.iter().map(|(k, v)| {
            version_map.serialize_entry(k, &self.id_of(HashedValue { value: Rc::clone(v) })).unwrap();
        });
        version_map.end()
    }
}

pub trait Memoable: Sized {
    type Out: Sized + Serialize;

    fn memoize(&self, memos: &HashMap<Self, u32>) -> Self::Out;
}

struct Memos<T: Hash + PartialEq + Eq> {
    node_ids: HashMap<T, u32>,
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

impl <A: Arch + SSAValues> Serialize for Memos<HashedValue<Rc<RefCell<Value<A>>>>> where HashedValue<Rc<RefCell<Value<A>>>>: Memoable {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut seq = serializer.serialize_seq(Some(self.node_ids.len()))?;
        for i in 0..self.node_ids.len() {
            for (k, v) in self.node_ids.iter() {
                if (i as u32) == *v {
//                    let v: Ref<Value<A>> = (&*k.value).borrow();
//                    seq.serialize_element((&*v).memoize(self.node_ids))?;
                    seq.serialize_element(&k.memoize(&self.node_ids))?;
                }
            }
        }
        seq.end()
    }
}

/*
trait ToMemo {
    type Out: Debug + Hash + PartialEq + Eq;

    fn memoize(&self, ctx: MemoizingSerialize<Self>) -> Self::Out;
}
*/

struct MemoizingSerializer<'a, 'b, T: ?Sized, M: Hash + PartialEq + Eq> {
    memos: Cell<&'a mut Memos<M>>,
    inner: &'b T,
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

impl <A: Arch + SSAValues + 'static> Serialize for SSA<A> where HashedValue<Rc<RefCell<Value<A>>>>: Memoable {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut memoizer: Memos<HashedValue<Rc<RefCell<Value<A>>>>> = Memos::new();

        let mut ssa_serializer = serializer.serialize_struct("SSA", 3)?;

        {
            let values = MemoizingSerializer::new(&mut memoizer, &self.values);
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

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone, Serialize, Deserialize)]
pub enum Direction {
    Read,
    Write
}

#[derive(Debug)]
pub struct Value<A: SSAValues> {
    // TODO: this should be removable...
    // mapped pretty directly to a location both with values and phi
    location: A::Location,
    version: u32,
    data: Option<A::Data>
}

impl <A: SSAValues> Hash for Value<A> where A::Location: Hash, A::Data: Hash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.location.hash(state);
        self.version.hash(state);
        self.data.hash(state);
    }
}

#[derive(Debug)]
pub struct HashedValue<A> {
    pub value: A
}

use std::hash::Hasher;
impl <A: SSAValues> Hash for HashedValue<Rc<RefCell<Value<A>>>> where Value<A>: Hash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let v: &RefCell<Value<A>> = &*self.value;
        let v2 = v.borrow();
        (v.borrow()).hash(state);
    }
}

impl <A: SSAValues> Eq for HashedValue<Rc<RefCell<Value<A>>>> { }

impl <A: SSAValues> PartialEq for HashedValue<Rc<RefCell<Value<A>>>> {
    fn eq(&self, other: &HashedValue<Rc<RefCell<Value<A>>>>) -> bool {
        Rc::ptr_eq(&self.value, &other.value)
    }
}

impl <A: SSAValues> PartialEq for Value<A> {
    fn eq(&self, rhs: &Value<A>) -> bool {
        self as *const Value<A> == rhs as *const Value<A>
    }
}
impl <A: SSAValues> Eq for Value<A> {}

impl <A> Value<A> where A: SSAValues {
    pub fn version(&self) -> u32 {
        self.version
    }
}

impl <A: SSAValues> Value<A> {
    fn new(location: A::Location, version: u32) -> Value<A> {
        Value {
            location: location,
            version: version,
            data: None
        }
    }
}

pub trait NoAliasing { }

impl <T> AliasInfo for T where T: NoAliasing + Clone + Copy {
    fn aliases_of(&self) -> Vec<Self> { vec![] }
    fn maximal_alias_of(&self) -> Self { self.clone() }
}

pub trait AliasInfo where Self: Sized {
    fn aliases_of(&self) -> Vec<Self>;
    fn maximal_alias_of(&self) -> Self;
}

pub trait SSAValues where Self: Arch {
    type Location: Debug + AliasInfo + Hash + Eq + Serialize;
    type Data: Debug + Hash;

    fn decompose(op: &Self::Instruction) -> Vec<(Option<Self::Location>, Direction)>;
}

impl <A: SSAValues> SSA<A> where A::Address: Hash + Eq, A::Location: Hash + Eq {
    fn written_value(&self, addr: A::Address, loc: A::Location) -> Option<Rc<RefCell<Value<A>>>> {
        let addr_values = self.values.get(&addr);
        let value: Option<&Rc<RefCell<Value<A>>>> = addr_values.and_then(|addr_values| addr_values.get(&(loc, Direction::Write)));
        value.map(|x| Rc::clone(x)) //map(|x| *x.borrow()).as_ref()
    }
    //fn written_value_mut(&self, addr: A::Address, loc: A::Location) -> Option<&mut Value<A>> {
    //    self.values[&addr].get_mut(&(loc, Direction::Write)).map(|x| *x.borrow_mut()).as_mut()
    //}
    fn read_value(&self, addr: A::Address, loc: A::Location) -> Option<Rc<RefCell<Value<A>>>> {
    //    self.values[&addr].get(&(loc, Direction::Read)).map(|x| *x.borrow()).as_ref()
    //}
    //fn read_value_mut(&mut self, addr: A::Address, loc: A::Location) -> Option<Rc<RefCell<Value<A>>>> {
        let addr_values = self.values.get(&addr);
        let value: Option<&Rc<RefCell<Value<A>>>> = addr_values.and_then(|addr_values| addr_values.get(&(loc, Direction::Read)));
        value.map(|x| Rc::clone(x))
        // value.map(|x| &mut *x.borrow_mut())
    }
}

#[test]
fn test_immediate_dominators_construction() {
    let start = 83u16;
    let map = GraphMap::<u16, (), petgraph::Directed>::from_edges(&[
        (start, 89), (89, 90),
        (89, 91), (90, 89),
        (91, 93), (91, 94)
    ]);
    let idom = petgraph::algo::dominators::simple_fast(&map, start);
    let frontier = compute_dominance_frontiers_from_idom(&map, start, &idom);

    // not distinguishing between something not reachable from the start and something with no
    // frontier
    let mut map = HashMap::new();
    map.insert(89, vec![89]);
    map.insert(90, vec![89]);
    assert!(frontier == map);

    let start = 1u16;
    let map = GraphMap::<u16, (), petgraph::Directed>::from_edges(&[
        (start, 94), (94, 83),
        (83, 88), (88, 90),
        (88, 91), (90, 88),
        (91, 93), (91, 94)
    ]);
    let idom = petgraph::algo::dominators::simple_fast(&map, start);
    let frontier = compute_dominance_frontiers_from_idom(&map, start, &idom);

    // not distinguishing between something not reachable from the start and something with no
    // frontier
    let mut map = HashMap::new();
    // this should be a HashMap<A, HashSet<A>>, not vec.
    map.insert(83, vec![94]);
    map.insert(88, vec![94, 88]);
    map.insert(90, vec![88]);
    map.insert(91, vec![94]);
    map.insert(94, vec![94]);
    assert!(frontier == map);
}

pub fn compute_dominance_frontiers_from_idom<A>(graph: &GraphMap<A, (), petgraph::Directed>, start: A, idom: &petgraph::algo::dominators::Dominators<A>) -> HashMap<A, Vec<A>> where A: Eq + Hash + Copy + Ord + Debug {
    // first thing we do is compute dominance frontiers..
    let mut dominance_frontiers: HashMap<A, Vec<A>> = HashMap::new();

    let mut bfs = Bfs::new(&graph, start);
    while let Some(u) = bfs.next(&graph) {
        let u_idom = match idom.immediate_dominator(u) {
            Some(value) => value,
            None => continue
        };

        /*
        // Not entirely sure how true this one is
        // and even if it's not the case that u_idom <idom> u implies u_idom being present,
        // the loop down on 96 might be adjustable to fit...
        let u_idom = match idom.immediate_dominator(u) {
            Some(value) => value,
            None => continue
        };*/

        // this COULD be rewritten to avoid collecting, since we're just iterating...
        let preds: Vec<A> = graph.neighbors_directed(u, petgraph::Direction::Incoming).collect();
        if preds.len() >= 2 {
            for pred in preds {
                let mut v = pred;
                match idom.immediate_dominator(v) {
                    Some(value) => (),
                    None => continue // this predecessor is not reachable from start
                };
                while v != u_idom {
                    dominance_frontiers.entry(v).or_insert_with(|| Vec::new()).push(u);
                    v = idom.immediate_dominator(v)
                        .expect("V was reachable, so its immeidate dominator must also be");
                }
            }
        }
    }

    dominance_frontiers
}

pub fn generate_ssa<A: Arch + SSAValues, M: MemoryRange<A::Address>>(
    data: &M,
    entry: A::Address,
    basic_blocks: &ControlFlowGraph<A::Address>,
    cfg: &GraphMap<A::Address, (), petgraph::Directed>
) -> SSA<A> where A::Address: Copy + Ord + Hash + Eq, A::Location: Copy + Hash + Eq {
    let idom = petgraph::algo::dominators::simple_fast(&cfg, entry);

    let dominance_frontiers = compute_dominance_frontiers_from_idom(cfg, entry, &idom);

    // extract out dominance frontiers .... one day...

    let mut all_locations: HashSet<A::Location> = HashSet::new();
    let mut assignments: HashMap<A::Location, HashSet<A::Address>> = HashMap::new();

    use arch::InstructionSpan;

    let mut has_already: HashMap<A::Address, u32> = HashMap::new();
    let mut work: HashMap<A::Address, u32> = HashMap::new();

    let mut bfs = Bfs::new(&cfg, entry);
    while let Some(k) = bfs.next(&cfg) {
        let block = basic_blocks.get_block(k);
        has_already.insert(k, 0);
        work.insert(k, 0);
        let mut iter = data.instructions_spanning::<A::Instruction>(block.start, block.end);
        while let Some((address, instr)) = iter.next() {
            for (maybeloc, direction) in A::decompose(&instr).into_iter() {
                match (maybeloc, direction) {
                    (Some(loc), Direction::Write) => {
                        let widening = loc.maximal_alias_of();
                        all_locations.insert(widening);
                        assignments.entry(widening).or_insert_with(|| HashSet::new()).insert(address);
                    }
                    (None, Direction::Write) => {
                        // TODO: this is a write to something, we don't know what
                        // this should set a bit to indicate potential all-clobber or something
                    }
                    (Some(loc), Direction::Read) => {
                        let widening = loc.maximal_alias_of();
                        all_locations.insert(widening);
                    }
                    (None, Direction::Read) => {
                        // TODO: this is a read from something, but we don't know what
                        // not sure if there's anything we can do with this, really
                    }
                }
            }
        }
    }

    // TODO: some nice abstraction to look up by (Address, Location) but also
    // find all Location for an Address
    let mut values: HashMap<A::Address, HashMap<(A::Location, Direction), Rc<RefCell<Value<A>>>>> = HashMap::new();
    let mut phi: HashMap<A::Address, HashMap<A::Location, (Rc<RefCell<Value<A>>>, Vec<Rc<RefCell<Value<A>>>>)>> = HashMap::new();

    let mut iter_count = 0;

    #[allow(non_snake_case)]
    let mut W: VecDeque<A::Address> = VecDeque::new();


//    for each variable in vars {
    for loc in all_locations.iter() {
        iter_count += 1;
//        for each X in A(variable) {
        if ! assignments.contains_key(loc) {
            continue;
        }
        #[allow(non_snake_case)]
        for X in assignments[loc].iter() {
            work.insert(*X, iter_count);
            W.push_back(*X);
        }

        while let Some(x) = W.pop_front() {
            if let Some(frontier) = dominance_frontiers.get(&x) {
                #[allow(non_snake_case)]
                for Y in frontier {
                    if has_already[Y] < iter_count {
                        // versioned at 0xffffffff to indicate a specialness to them.
                        // These should never be present after search().
                        phi.entry(*Y).or_insert_with(|| HashMap::new())
                            .insert(*loc, (Rc::new(RefCell::new(Value::new(*loc, 0xffffffff))), vec![]));
                        // TODO: phi nodes are assignments too! this is definitely a bug.
                        has_already.insert(*Y, iter_count);
                        if work[Y] < iter_count {
                            work.insert(*Y, iter_count);
                            if !W.contains(Y) {
                                W.push_back(*Y);
                            }
                        }
                    }
                }
            }
        }
    }

    /* phi nodes placed, now number.. */

    /* and for memory
     * track writes to `unknown` w.r.t the current basic block, SSA that forward
     * as a global opaque value. then track if it is set in the current basic block wherein it
     * invalidates reads as a fallback. then ssa resolution goes specific memory first,
     * with fallback to global unknown. global unknown satisfies a write to any specific address.
     * SSA memory::unknown first so we know when looking at precise memory variables if
     * memory should be forgotten (eg marked written w/ no data propagation possible)
     */

    #[allow(non_snake_case)]
    fn search<A: Arch + SSAValues, M: MemoryRange<A::Address>>(
        data: &M,
        block: &BasicBlock<A::Address>,
        values: &mut HashMap<A::Address, HashMap<(A::Location, Direction), Rc<RefCell<Value<A>>>>>,
        phi: &mut HashMap<A::Address, HashMap<A::Location, (Rc<RefCell<Value<A>>>, Vec<Rc<RefCell<Value<A>>>>)>>,
        basic_blocks: &ControlFlowGraph<A::Address>,
        cfg: &GraphMap<A::Address, (), petgraph::Directed>,
        dominance_frontiers: &HashMap<A::Address, Vec<A::Address>>,
        idom: &petgraph::algo::dominators::Dominators<A::Address>,
        C: &mut HashMap<A::Location, u32>,
        S: &mut HashMap<A::Location, Vec<Rc<RefCell<Value<A>>>>>
    ) where <A as SSAValues>::Location: Copy + Hash + Eq, <A as Arch>::Address: Copy + Ord + Hash + Eq, <A as Arch>::Instruction: Debug + LengthedInstruction<Unit=<A as Arch>::Address>, <A as SSAValues>::Location: AliasInfo {
        let mut assignments: Vec<A::Location> = Vec::new();
        // for each statement in block {
        // also check phis at start of the block...
        if let Some(phis) = phi.get(&block.start) {
            for (loc, data) in phis {
                // these are very clear reads vs assignments:
                let widening = loc.maximal_alias_of();
                let i = C[&widening];
                let mut phi_dest = Rc::clone(&phi[&block.start][loc].0);
                phi_dest.replace(Value::new(*loc, i));
                S.get_mut(&widening).expect("S should have entries for all locations.").push(Rc::clone(&phi_dest));
                C.entry(widening).and_modify(|x| *x += 1);
                // for very assignment-heavy blocks maybe there's a good way to summarize multiple of the same location being assigned
                // eg is it faster to store thsi and pop it back or is it faster to just
                // decode again..?
                assignments.push(widening); // ???
            }
        }
        let mut iter = data.instructions_spanning::<A::Instruction>(block.start, block.end);
        while let Some((address, instr)) = iter.next() {
            for (maybeloc, direction) in A::decompose(&instr).into_iter() {
                match (maybeloc, direction) {
                    (Some(loc), Direction::Read) => {
                        let widening = loc.maximal_alias_of();
                        let at_address = values.entry(address).or_insert_with(||
                            HashMap::new()
                        );
                        at_address.insert((loc, Direction::Read), Rc::clone(&S[&widening][S[&widening].len() - 1]));
                    },
                    (None, Direction::Read) => {
                        // it's a read of something, but we don't know what, 
                    },
                    (Some(loc), Direction::Write) => {
                        let widening = loc.maximal_alias_of();
                        let i = C[&widening];
                        let new_value = Rc::new(RefCell::new(Value::new(loc, i)));
                        let at_address = values.entry(address).or_insert_with(||
                            HashMap::new()
                        );
                        at_address.insert((loc, Direction::Write), Rc::clone(&new_value));
                        S.get_mut(&widening).expect("S should have entries for all locations.").push(new_value);
                        C.entry(widening).and_modify(|x| *x += 1);
                        // for very assignment-heavy blocks maybe there's a good way to summarize multiple of the same location being assigned
                        // eg is it faster to store thsi and pop it back or is it faster to just
                        // decode again..?
                        assignments.push(widening); // ???
                    },
                    (None, Direction::Write) => {
                        // a write to somewhere, we don't know where, this should def ...
                        // everything
                    }
                }
            }
        }

        // succ == traditional successors-in-cfg
        for Y in cfg.neighbors(block.start) {
//            j = whichpred(Y, block);
//            for each phi in Y {
            if let Some(block_phis) = phi.get_mut(&Y) {
//                for loc in phi.get(Y)
                for (loc, (dest, args)) in block_phis.iter_mut() {
                    let widening = loc.maximal_alias_of();
//                    phi.operands[j] = .. /* value for S[V] */
//                    // not quite perfect, but good enough
                    args.push(S[&widening][S[&widening].len() - 1].clone());
                }
            }
        }

        // children == nodes immediately dominated by block
//        for each Y in children(block) {
        let mut bfs = Bfs::new(&cfg, block.start);
        while let Some(u) = bfs.next(&cfg) {
            if let Some(u_idom) = idom.immediate_dominator(u) {
                if u_idom == block.start && u != block.start {
                search(
                    data,
                    basic_blocks.get_block(u),
                    values,
                    phi,
                    basic_blocks,
                    cfg,
                    dominance_frontiers,
                    idom,
                    C,
                    S
                );
                }
            }
        }

        for assignment in assignments.into_iter() {
            S.get_mut(&assignment).expect("S should have entries for all locations, right now.").pop();
        }
    }

    #[allow(non_snake_case)]
    let mut C: HashMap<A::Location, u32> = HashMap::new();
    #[allow(non_snake_case)]
    let mut S: HashMap<A::Location, Vec<Rc<RefCell<Value<A>>>>> = HashMap::new();

    // all_locations should be the widest aliases ONLY
//    for each variable in vars {
    for loc in all_locations {
        C.insert(loc, 0);
        S.insert(loc, vec![Rc::new(RefCell::new(Value::new(loc, 0)))]);
    }

    search(
        data,
        basic_blocks.get_block(entry),
        &mut values,
        &mut phi,
        basic_blocks,
        cfg,
        &dominance_frontiers,
        &idom,
        &mut C,
        &mut S
    );

    SSA { values: values, phi: phi }
}
