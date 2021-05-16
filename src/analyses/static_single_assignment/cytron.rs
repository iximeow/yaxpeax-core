use yaxpeax_arch::Arch;
use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use std::cmp::Eq;
use petgraph::graphmap::GraphMap;
use petgraph;
use petgraph::visit::Bfs;

use std::cell::RefCell;
use std::rc::Rc;

use std::fmt::Debug;

use analyses::control_flow::{BasicBlock, ControlFlowGraph};
use memory::MemoryRange;

use analyses::static_single_assignment::{HashedValue, DefSource, DFGRef, Value, SSA, SSAValues, PhiLocations};
use analyses::static_single_assignment::data::PhiOp;
use analyses::static_single_assignment::data::DFGRebase;
use data::{AliasInfo, Direction, Disambiguator, LocIterator};
use data::LocationAliasDescriptions;
use data::ValueLocations;
use data::modifier::ModifierCollection;
use data::modifier;

use arch::{AbiDefaults, FunctionImpl, FunctionQuery, InstructionSpan};

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
                    Some(_value) => (),
                    None => continue // this predecessor is not reachable from start
                };
                while v != u_idom {
                    dominance_frontiers.entry(v).or_insert_with(|| Vec::new()).push(u);
                    v = idom.immediate_dominator(v)
                        .expect("V was reachable, so its immediate dominator must also be");
                }
            }
        }
    }

    dominance_frontiers
}

struct UseDefTracker<'lad, A: crate::data::ValueLocations, LAD: LocationAliasDescriptions<A>> {
    loc_descs: Option<&'lad LAD>,
    pub all_locations: HashSet<A::Location>,
    pub assignments: HashMap<A::Location, HashSet<A::Address>>,
    pub aliases: HashMap<A::Location, HashSet<A::Location>>,
}

impl <'lad, A: crate::data::ValueLocations, LAD: LocationAliasDescriptions<A>> UseDefTracker<'lad, A, LAD> {
    pub fn new(loc_descs: Option<&'lad LAD>) -> Self {
        UseDefTracker {
            loc_descs,
            all_locations: HashSet::new(),
            assignments: HashMap::new(),
            aliases: HashMap::new(),
        }
    }

    fn track_use(&mut self, loc: A::Location) {
        self.all_locations.insert(loc.clone());
        if let Some(loc_descs) = self.loc_descs {
            // NOTE: `loc_descs.aliases_for(loc)` should be a superset of `loc.aliases_of()`
            // TODO: verify this with asserts.
            for alias in loc_descs.aliases_for(&loc) {
                self.track_alias(alias, loc.clone());
            }
        } else {
            for alias in loc.aliases_of() {
                self.track_alias(alias, loc.clone());
            }
        }
    }

    fn track_def(&mut self, loc: A::Location, addr: A::Address) {
        self.assignments.entry(loc.clone()).or_insert_with(|| HashSet::new()).insert(addr);
        self.track_use(loc);
    }

    fn track_alias(&mut self, alias: A::Location, loc: A::Location) {
        self.all_locations.insert(alias.clone());
        self.aliases.entry(alias).or_insert_with(|| HashSet::new())
            .insert(loc);
    }

    pub fn track(&mut self, addr: A::Address, loc: Option<A::Location>, dir: Direction) {
        match dir {
            Direction::Read => {
                if let Some(loc) = loc {
                    self.track_use(loc);
                } else {
                    // TODO: this is a read from something, but we don't know what
                    // not sure if there's anything we can do with this, really
                }
            },
            Direction::Write => {
                if let Some(loc) = loc {
                    // TODO: this us the cause of some inaccuracies f.ex on x86:
                    // cl and ch both have a widest alias of ecx (or rcx) so a write to ch may
                    // clobber a prior write of cl.
                    self.track_def(loc, addr);
                } else {
                    // TODO: this is a write to something, we don't know what
                    // this should set a bit to indicate potential all-clobber or something
                }
            }
        }
    }
}

#[allow(non_snake_case)]
struct ValueAllocator<A: Arch + SSAValues> {
    C: HashMap<A::Location, u32>,
    S: HashMap<A::Location, Vec<DFGRef<A>>>,
}

impl<A: Arch + SSAValues> ValueAllocator<A> {
    fn from_use_tracker<'lad, LAD: LocationAliasDescriptions<A>>(tracker: UseDefTracker<'lad, A, LAD>) -> Self {
        let mut allocator = ValueAllocator {
            C: HashMap::new(),
            S: HashMap::new(),
        };

        for loc in tracker.all_locations {
            allocator.C.insert(loc.clone(), 0);
            allocator.S.insert(loc.clone(), vec![Rc::new(RefCell::new(Value::new(loc.clone(), None)))]);
        }

        allocator
    }

    fn track_external(&mut self, loc: A::Location, value: Rc<RefCell<Value<A>>>) {
        self.S.get_mut(&loc).expect("S has entries for locations").push(value);
    }

    fn new_value(&mut self, loc: A::Location) -> Value<A> {
        use std::collections::hash_map::Entry;
        if let Entry::Occupied(mut e) = self.C.entry(loc.clone()) {
            let i = *e.get();
            *e.get_mut() += 1;
            Value::new(loc, Some(i))
        } else {
            unreachable!();
        }
    }

    pub fn free(&mut self, loc: A::Location) {
        self.S.get_mut(&loc).expect("S has entries for locations").pop();
    }

    pub fn alloc_value(&mut self, loc: A::Location) -> Rc<RefCell<Value<A>>> {
        let new_ref = Rc::new(RefCell::new(self.new_value(loc.clone())));
        self.S.get_mut(&loc).expect("S should have entries for all locations.").push(Rc::clone(&new_ref));
        new_ref
    }
    pub fn alloc_value_with<F: FnMut(Value<A>) -> Rc<RefCell<Value<A>>>>(&mut self, loc: A::Location, mut f: F) -> Rc<RefCell<Value<A>>> {
        let new_ref = f(self.new_value(loc.clone()));
        self.S.get_mut(&loc).expect("S should have entries for all locations.").push(Rc::clone(&new_ref));
        new_ref
    }

    pub fn current(&self, loc: &A::Location) -> &Rc<RefCell<Value<A>>> {
        let stack = &self.S[loc];
        &stack[stack.len() - 1]
    }

    pub fn previous(&self, loc: &A::Location) -> &Rc<RefCell<Value<A>>> {
        let stack = &self.S[loc];
        &stack[stack.len() - 2]
    }

    pub fn allocate_region<I: Iterator<Item=(Option<A::Location>, Direction)>, F: Fn(&mut SSA<A>, (A::Location, Direction), DFGRef<A>)>(&mut self, ssa: &mut SSA<A>, assignments: &mut Vec<A::Location>, items: I, insert_entry: &F, def_source: (A::Address, DefSource<A::Address>)) {
        let mut writelog: HashSet<A::Location> = HashSet::new();
        for (maybeloc, direction) in items {
            if let Some(loc) = maybeloc {
                // TODO: use a `LocationAliasDescriptions` here in place of `loc.aliases_of()`
                for loc in std::iter::once(loc.clone()).chain(loc.aliases_of().into_iter()) {
                    match direction {
                        Direction::Read => {
                            let value = if writelog.contains(&loc) {
                                self.previous(&loc)
                            } else {
                                self.current(&loc)
                            };
                            value.borrow_mut().used = true;
                            insert_entry(ssa, (loc, Direction::Read), Rc::clone(value));
                        },
                        Direction::Write => {
                            // treat writes also as reads of the location they write. this is to
                            // support linking defs with prior values the def overwrites.
                            let value = if writelog.contains(&loc) {
                                self.previous(&loc)
                            } else {
                                self.current(&loc)
                            };
                            value.borrow_mut().used = true;
                            insert_entry(ssa, (loc.clone(), Direction::Read), Rc::clone(value));

                            // original write logic.
                            if writelog.contains(&loc) {
                                continue;
                            } else {
                                writelog.insert(loc.clone());
                            }
                            let new_value = self.alloc_value(loc.clone());
                            ssa.defs.insert(HashedValue {
                                value: Rc::clone(&new_value)
                            }, def_source);
                            insert_entry(ssa, (loc.clone(), Direction::Write), new_value);
                            // for very assignment-heavy blocks maybe there's a good way to summarize multiple of the same location being assigned
                            // eg is it faster to store this and pop it back or is it faster to just
                            // decode again..?
                            assignments.push(loc); // ???
                        },
                    }
                }
            } else {
                match direction {
                    Direction::Read => {
                        // it's a read of something, but we don't know what.
                    },
                    Direction::Write => {
                        // a write to somewhere, we don't know where, this should def ...
                        // everything
                    }
                }
            }
        }
    }
}

/*
 * need a variant of `generate_ssa` for "generate refinement of old dfg, then translate expressions
 * referencing old dfg values"
 *
 * this is because expressions describing memory regions can (and should!) reference ssa values as
 * part of their expressions. so once a new dfg is generated from refinements drived from the old
 * dfg, those refinements need to instead be phrased in terms of values in the new dfg that will be
 * returned.
 *
 * in general, the values and data in a dfg should be self-referential (otherwise dfg updates and
 * refinements could cause broken and spooky action!)
 */
pub fn generate_refined_ssa<
    'functions,
    'disambiguator,
    'dfg,
    'location_disambiguator,
    A: SSAValues,
    M: MemoryRange<A::Address>,
    U: ModifierCollection<A>,
    LocSpec,
    Disam: Disambiguator<A, LocSpec> + LocationAliasDescriptions<A>,
    F: FunctionQuery<A::Address, Function=FunctionImpl<A::Location>>,
>(
    data: &M,
    entry: A::Address,
    basic_blocks: &ControlFlowGraph<A::Address>,
    cfg: &GraphMap<A::Address, (), petgraph::Directed>,
    old_dfg: &'dfg SSA<A>,
    value_modifiers: &U,
    disambiguator: &'disambiguator Disam,
    functions: &'functions F,
) -> SSA<A> where
    A::Location: 'static + AbiDefaults,
    for<'a> &'a <A as Arch>::Instruction: LocIterator<'disambiguator, 'functions, A, A::Location, Disam, F, Item=(Option<A::Location>, Direction), LocSpec=LocSpec>,
    <A as ValueLocations>::Location: DFGRebase<A>,
{
    let mut new_dfg = generate_ssa(data, entry, basic_blocks, cfg, value_modifiers, disambiguator, functions);

    // iterate `new_dfg` values and replace any `DFGRef` in `old_dfg` with corresponding `DFGRef` in `new_dfg`.
    println!("{:?}", &new_dfg.instruction_values);
    let mut loc_updates = HashMap::new();
    for (addr, v) in &new_dfg.instruction_values {
        println!("{:?}", addr);
        for ((loc, dir), dfg_ref) in v {
            println!("location for {:?}, {:?}", loc, dir);
            if let Some(old_value) = old_dfg.get_value(*addr, loc.to_owned(), *dir) {
                if dfg_ref.borrow().location != old_value.borrow().location || dfg_ref.borrow().version != old_value.borrow().version {
                    println!("{:?} -> {:?}", old_value, dfg_ref);
                } else {
                    println!("{:?} == {:?}", old_value, dfg_ref);
                }
            } else {
                // okay this is some kinda location that didn't exist before. maybe this is a new
                // memory access expression. iterate the fields and try to update those to
                // values in this dfg...?
                let new_loc = loc.rebase_references(old_dfg, &new_dfg);
                if &new_loc != loc {
                    loc_updates.insert(loc.clone(), new_loc);
                }
            }
        }
        // every value has a location, 
    }
    println!("{} locations to update", loc_updates.len());
    for (k, v) in loc_updates.iter() {
        println!("  - {:?} => {:?}", k, v);
    }

    for (_addr, values) in new_dfg.instruction_values.iter_mut() {
        let mut new_values = values.clone();
        for ((loc, dir), value) in values.iter() {
            if loc_updates.contains_key(loc) {
                new_values.remove(&(loc.to_owned(), *dir));
                new_values.insert((loc_updates[loc].to_owned(), *dir), Rc::clone(value));
                value.borrow_mut().location = loc_updates[loc].to_owned();
            }
        }
        *values = new_values;
    }

    if true {
        println!("instruction values {:?}", &new_dfg.instruction_values);
        println!("modifier values {:?}", &new_dfg.modifier_values);
        println!("defs {:?}", &new_dfg.defs);
//        println!("phi {:?}", &new_dfg.phi);
        /*
        for v in new_dfg.values() {
            assert!(A::Data::test_integrity(v, &new_dfg))
        }
        */
    }

    new_dfg
}

pub fn generate_ssa<
    'functions,
    'disambiguator,
    A: SSAValues,
    M: MemoryRange<A::Address>,
    U: ModifierCollection<A>,
    LocSpec,
    Disam: Disambiguator<A, LocSpec> + LocationAliasDescriptions<A>,
    F: FunctionQuery<A::Address, Function=FunctionImpl<A::Location>>,
>(
    data: &M,
    entry: A::Address,
    basic_blocks: &ControlFlowGraph<A::Address>,
    cfg: &GraphMap<A::Address, (), petgraph::Directed>,
    value_modifiers: &U,
    disambiguator: &'disambiguator Disam,
    functions: &'functions F,
) -> SSA<A> where
    A::Location: 'static + AbiDefaults,
    for<'a> &'a <A as Arch>::Instruction: LocIterator<'disambiguator, 'functions, A, A::Location, Disam, F, Item=(Option<A::Location>, Direction), LocSpec=LocSpec>,
{
    let idom = petgraph::algo::dominators::simple_fast(&cfg, entry);

    let dominance_frontiers = compute_dominance_frontiers_from_idom(cfg, entry, &idom);

    // extract out dominance frontiers .... one day...

    let mut use_tracker = UseDefTracker::<A, _>::new(Some(disambiguator));

    let mut has_already: HashMap<A::Address, u32> = HashMap::new();
    let mut work: HashMap<A::Address, u32> = HashMap::new();

    let mut bfs = Bfs::new(&cfg, entry);
    while let Some(k) = bfs.next(&cfg) {
        let block = basic_blocks.get_block(k);
        has_already.insert(k, 0);
        work.insert(k, 0);
        let mut iter = data.instructions_spanning(A::Decoder::default(), block.start, block.end);
        while let Some((address, instr)) = iter.next() {
            for (maybeloc, direction) in value_modifiers.before(address).iter().cloned().chain(instr.iter_locs(address, disambiguator, functions)).chain(value_modifiers.after(address).iter().cloned()) {
                use_tracker.track(block.start, maybeloc, direction);
            }
        }

        // now, it's possible that an assignment only occurs in the between-block limbo states
        // added f.ex by control-dependent constants, like `x` in constructs like
        //
        // if (x == 10) {
        //   // <---- HERE
        // } else {
        //   // <---- AND HERE
        // }
        //
        // but otherwise are unused in this basic block. so, check the inter-block modifiers
        //
        // this is imprecise! any write on any out-edge is treated as if it were a write on all
        // out-edges. for initial use, this may be correct, but there may be cases where this
        // intruoduces unnecessary phi nodes.
        for next in cfg.neighbors(block.start) {
            for (maybeloc, dir) in value_modifiers.between(block.start, next) {
                use_tracker.track(block.start, maybeloc, dir)
            }
        }
    }

    // TODO: some nice abstraction to look up by (Address, Location) but also
    // find all Location for an Address
    let mut ssa = SSA {
        instruction_values: HashMap::new(),
        modifier_values: HashMap::new(),
        control_dependent_values: HashMap::new(),
        defs: HashMap::new(),
        phi: HashMap::new(),
        indirect_values: HashMap::new(),
        external_defs: HashMap::new(),
    };

    let mut iter_count = 0;

    #[allow(non_snake_case)]
    let mut W: VecDeque<A::Address> = VecDeque::new();


//    for each variable in vars {
    for loc in use_tracker.all_locations.iter() {
        iter_count += 1;
//        fo_r each X in A(variable) {
        if ! use_tracker.assignments.contains_key(loc) {
            continue;
        }
        #[allow(non_snake_case)]
        for X in use_tracker.assignments[loc].iter() {
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
                        let new_value = Rc::new(RefCell::new(Value::new(loc.clone(), Some(0xffffffff))));
                        ssa.phi.entry(*Y).or_insert_with(|| HashMap::new())
                            .insert(loc.clone(), PhiOp { out: new_value, ins: vec![] });
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
    fn search<
        'functions,
        'disambiguator,
        A: Arch + SSAValues,
        M: MemoryRange<A::Address>,
        U: ModifierCollection<A>,
        LocSpec,
        Disam: Disambiguator<A, LocSpec>,
        F: FunctionQuery<A::Address, Function=FunctionImpl<A::Location>>,
    >(
        data: &M,
        block: &BasicBlock<A::Address>,
        ssa: &mut SSA<A>,
        basic_blocks: &ControlFlowGraph<A::Address>,
        cfg: &GraphMap<A::Address, (), petgraph::Directed>,
        idom: &petgraph::algo::dominators::Dominators<A::Address>,
        value_allocator: &mut ValueAllocator<A>,
        value_modifiers: &U,
        disambiguator: &'disambiguator Disam,
        functions: &'functions F,
    ) where
        A::Location: 'static + AbiDefaults,
        for<'a> &'a <A as Arch>::Instruction: LocIterator<'disambiguator, 'functions, A, A::Location, Disam, F, Item=(Option<A::Location>, Direction), LocSpec=LocSpec>
    {
        let mut assignments: Vec<A::Location> = Vec::new();
        // for each statement in block {
        // also check phis at start of the block...
        if let Some(phis) = ssa.phi.get(&block.start) {
            for (loc, _data) in phis {
                // these are very clear reads vs assignments:
                let phi_dest = value_allocator.alloc_value_with(loc.clone(), |value| {
                    let phi_dest = Rc::clone(&ssa.phi[&block.start][loc].out);
                    phi_dest.replace(value);
                    phi_dest
                });
                ssa.defs.insert(HashedValue {
                    value: Rc::clone(&phi_dest)
                }, (block.start, DefSource::Phi));
                // for very assignment-heavy blocks maybe there's a good way to summarize multiple of the same location being assigned
                // eg is it faster to store this and pop it back or is it faster to just
                // decode again..?
                assignments.push(loc.clone()); // ???
            }
        }

        let mut iter = data.instructions_spanning(A::Decoder::default(), block.start, block.end);
        while let Some((address, instr)) = iter.next() {
            let before_fn = |ssa: &mut SSA<A>, pos, value| {
                ssa.modifier_values.entry((address, modifier::Precedence::Before)).or_insert_with(|| HashMap::new()).insert(pos, value);
            };
            value_allocator.allocate_region(ssa, &mut assignments, value_modifiers.before(address).into_iter(), &before_fn, (address, DefSource::Modifier(modifier::Precedence::Before)));
            let between_fn = |ssa: &mut SSA<A>, pos, value| {
                ssa.instruction_values.entry(address).or_insert_with(|| HashMap::new()).insert(pos, value);
            };
            value_allocator.allocate_region(ssa, &mut assignments, instr.iter_locs(address, disambiguator, functions), &between_fn, (address, DefSource::Instruction));
            let after_fn = |ssa: &mut SSA<A>, pos, value| {
                ssa.modifier_values.entry((address, modifier::Precedence::After)).or_insert_with(|| HashMap::new()).insert(pos, value);
            };
            value_allocator.allocate_region(ssa, &mut assignments, value_modifiers.after(address).into_iter(), &after_fn, (address, DefSource::Modifier(modifier::Precedence::After)));
        }

        /*
         * Now compute the modifier-affected locations we need to shim in when we look at
         * neighbors..
         *
         * there will be at most one modifier to a location, so at most one assignment per
         * out-edge to a neighbor. this is handy because we can track all this stuff as a map of
         * next_block -> (map<location, value>)
         *
         * this is merged with the typical phi argument placement loop because both iterate
         * neighbors of the current basic block
         */

        // succ == traditional successors-in-cfg
        for Y in cfg.neighbors(block.start) {
            let introduced_bounds_fn = |ssa: &mut SSA<A>, pos, value| {
                let introduced_bounds = ssa.control_dependent_values.entry(block.start).or_insert_with(|| HashMap::new());
                introduced_bounds.entry(Y).or_insert_with(|| HashMap::new()).insert(pos, value);
            };
            value_allocator.allocate_region(ssa, &mut assignments, value_modifiers.between(block.start, Y).into_iter(), &introduced_bounds_fn, (block.start, DefSource::Between(Y)));

//            j = whichpred(Y, block);
//            for each phi in Y {
            if let Some(block_phis) = ssa.phi.get_mut(&Y) {
//                for loc in phi.get(Y)
                for (loc, phi_op) in block_phis.iter_mut() {
//                    phi.operands[j] = .. /* value for S[V] */
//                    // not quite perfect, but good enough
                    phi_op.ins.push(Rc::clone(value_allocator.current(loc)));
                }
            }
        }

        // children == nodes immediately dominated by block
//        for each Y in children(block) {
// TODO: avoid iterating the whole function's cfg?
        let mut bfs = Bfs::new(&cfg, block.start);
        while let Some(u) = bfs.next(&cfg) {
            if let Some(u_idom) = idom.immediate_dominator(u) {
                if u_idom == block.start && u != block.start {
                    if let Some(adjustments) = ssa.control_dependent_values.get(&block.start).and_then(|map| map.get(&u)) {
                        for ((loc, direction), value) in adjustments {
                            if *direction == Direction::Write {
                                // these values were not created through `value_allocator`, but are
                                // instead externally generated properties of the dfg we're
                                // processing. In the following recursive call these are live, by
                                // being on the relevant edge of `control_dependent_values`,
                                // so we have to inform the allocator of these new transient
                                // values.
                                value_allocator.track_external(loc.clone(), Rc::clone(value));
                            }
                        }
                    }
                search(
                    data,
                    basic_blocks.get_block(u),
                    ssa,
                    basic_blocks,
                    cfg,
                    idom,
                    value_allocator,
                    value_modifiers,
                    disambiguator,
                    functions,
                );
                    // this shouldn't be modified in recursive calls (we won't visit the same block
                    // [so, same bounds] twice), but it's clumsy to express that to rustc. might
                    // revisit in the future.
                    if let Some(adjustments) = ssa.control_dependent_values.get(&block.start).and_then(|map| map.get(&u)) {
                        for ((loc, direction), _value) in adjustments {
                            if *direction == Direction::Write {
                                value_allocator.free(loc.clone());
                            }
                        }
                    }
                }
            }
        }

        for assignment in assignments.into_iter() {
            value_allocator.free(assignment);
        }
    }

    let mut value_allocator = ValueAllocator::from_use_tracker(use_tracker);

    search(
        data,
        basic_blocks.get_block(entry),
        &mut ssa,
        basic_blocks,
        cfg,
        &idom,
        &mut value_allocator,
        value_modifiers,
        disambiguator,
        functions,
    );

    fn mark_phi_used<A: SSAValues>(
        phi: &PhiOp<A>,
        phis: &HashMap<A::Address, PhiLocations<A>>,
        defs: &HashMap<HashedValue<DFGRef<A>>, (A::Address, DefSource<A::Address>)>
    ) {
        for value in phi.ins.iter() {
            if !value.borrow().used {
                value.borrow_mut().used = true;
                if let Some((addr, DefSource::Phi)) = defs.get(&HashedValue { value: Rc::clone(&value) }) {
                    let dep_phi = phis.get(addr).unwrap().get(&value.borrow().location).unwrap();
                    mark_phi_used(dep_phi, phis, defs);
                }
            }
        }
    }

    for (_block, phi_locs) in ssa.phi.iter() {
        for (_loc, phi) in phi_locs.iter() {
            if phi.out.borrow().used {
                mark_phi_used(phi, &ssa.phi, &ssa.defs)
            }
        }
    }

    ssa
}
