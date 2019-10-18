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

use analyses::static_single_assignment::{HashedValue, DefSource, DFGRef, Value, SSA, SSAValues, RWMap, PhiLocations};
use analyses::static_single_assignment::data::PhiOp;
use data::{AliasInfo, Direction, Disambiguator, LocIterator};
use data::modifier::ModifierCollection;

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
                        .expect("V was reachable, so its immeidate dominator must also be");
                }
            }
        }
    }

    dominance_frontiers
}

struct UseDefTracker<A: crate::data::ValueLocations> {
    pub all_locations: HashSet<A::Location>,
    pub assignments: HashMap<A::Location, HashSet<A::Address>>,
    pub aliases: HashMap<A::Location, HashSet<A::Location>>,
}

impl <A: crate::data::ValueLocations> UseDefTracker<A> {
    pub fn new() -> Self {
        UseDefTracker {
            all_locations: HashSet::new(),
            assignments: HashMap::new(),
            aliases: HashMap::new(),
        }
    }

    pub fn track_use(&mut self, loc: A::Location) {
        self.all_locations.insert(loc);
        for alias in loc.aliases_of() {
            self.track_alias(alias, loc);
        }
    }

    pub fn track_def(&mut self, loc: A::Location, addr: A::Address) {
        self.assignments.entry(loc).or_insert_with(|| HashSet::new()).insert(addr);
        self.track_use(loc);
    }

    pub fn track_alias(&mut self, alias: A::Location, loc: A::Location) {
        self.all_locations.insert(alias);
        self.aliases.entry(alias).or_insert_with(|| HashSet::new())
            .insert(loc);
    }
}

pub fn generate_ssa<
    A: SSAValues,
    M: MemoryRange<A::Address>,
    U: ModifierCollection<A>,
    D: std::ops::Deref<Target = U>,
    LocSpec,
    Disam: Disambiguator<A::Location, LocSpec>,
>(
    data: &M,
    entry: A::Address,
    basic_blocks: &ControlFlowGraph<A::Address>,
    cfg: &GraphMap<A::Address, (), petgraph::Directed>,
    value_modifiers: D,
    disambiguator: &mut Disam,
) -> SSA<A> where
    for<'a, 'disam> &'a <A as Arch>::Instruction: LocIterator<'disam, A::Location, Disam, Item=(Option<A::Location>, Direction), LocSpec=LocSpec>
{
    let idom = petgraph::algo::dominators::simple_fast(&cfg, entry);

    let dominance_frontiers = compute_dominance_frontiers_from_idom(cfg, entry, &idom);

    // extract out dominance frontiers .... one day...

    let mut use_tracker = UseDefTracker::<A>::new();

    use arch::InstructionSpan;

    let mut has_already: HashMap<A::Address, u32> = HashMap::new();
    let mut work: HashMap<A::Address, u32> = HashMap::new();

    let mut bfs = Bfs::new(&cfg, entry);
    while let Some(k) = bfs.next(&cfg) {
        let block = basic_blocks.get_block(k);
        has_already.insert(k, 0);
        work.insert(k, 0);
        let mut iter = data.instructions_spanning::<A::Instruction>(block.start, block.end);
        while let Some((_address, instr)) = iter.next() {
            for (maybeloc, direction) in instr.iter_locs(disambiguator) {
                match (maybeloc, direction) {
                    (Some(loc), Direction::Write) => {
                        // TODO: this us the cause of some inaccuracies f.ex on x86:
                        // cl and ch both have a widest alias of ecx (or rcx) so a write to ch may
                        // clobber a prior write of cl.
                        use_tracker.track_def(loc, block.start);
                    }
                    (None, Direction::Write) => {
                        // TODO: this is a write to something, we don't know what
                        // this should set a bit to indicate potential all-clobber or something
                    }
                    (Some(loc), Direction::Read) => {
                        use_tracker.track_use(loc);
                    }
                    (None, Direction::Read) => {
                        // TODO: this is a read from something, but we don't know what
                        // not sure if there's anything we can do with this, really
                    }
                }
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
            for modifier in value_modifiers.between(block.start, next) {
                match modifier {
                    (Some(loc), Direction::Write) => {
                        use_tracker.track_def(loc, block.start);
                    }
                    (None, Direction::Write) => {
                        // TODO: this is a write to something, we don't know what
                        // this should set a bit to indicate potential all-clobber or something
                    }
                    (Some(loc), Direction::Read) => {
                        use_tracker.track_use(loc);
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
    let mut values: HashMap<A::Address, RWMap<A>> = HashMap::new();
    let mut between_block_bounds: HashMap<A::Address, HashMap<A::Address, RWMap<A>>> = HashMap::new();
    let mut defs: HashMap<HashedValue<DFGRef<A>>, (A::Address, DefSource<A::Address>)> = HashMap::new();
    let mut phi: HashMap<A::Address, PhiLocations<A>> = HashMap::new();

    let mut iter_count = 0;

    #[allow(non_snake_case)]
    let mut W: VecDeque<A::Address> = VecDeque::new();


//    for each variable in vars {
    for loc in use_tracker.all_locations.iter() {
        iter_count += 1;
//        for each X in A(variable) {
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
                        let new_value = Rc::new(RefCell::new(Value::new(*loc, Some(0xffffffff))));
                        phi.entry(*Y).or_insert_with(|| HashMap::new())
                            .insert(*loc, PhiOp { out: new_value, ins: vec![] });
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
        A: Arch + SSAValues,
        M: MemoryRange<A::Address>,
        U: ModifierCollection<A>,
        LocSpec,
        Disam: Disambiguator<A::Location, LocSpec>,
    >(
        data: &M,
        block: &BasicBlock<A::Address>,
        values: &mut HashMap<A::Address, RWMap<A>>,
        between_block_bounds: &mut HashMap<A::Address, HashMap<A::Address, RWMap<A>>>,
        defs: &mut HashMap<HashedValue<DFGRef<A>>, (A::Address, DefSource<A::Address>)>,
        phi: &mut HashMap<A::Address, PhiLocations<A>>,
        basic_blocks: &ControlFlowGraph<A::Address>,
        cfg: &GraphMap<A::Address, (), petgraph::Directed>,
        dominance_frontiers: &HashMap<A::Address, Vec<A::Address>>,
        idom: &petgraph::algo::dominators::Dominators<A::Address>,
        C: &mut HashMap<A::Location, u32>,
        S: &mut HashMap<A::Location, Vec<DFGRef<A>>>,
        value_modifiers: &impl std::ops::Deref<Target = U>,
        disambiguator: &mut Disam,
    ) where
        for<'a, 'disam> &'a <A as Arch>::Instruction: LocIterator<'disam, A::Location, Disam, Item=(Option<A::Location>, Direction), LocSpec=LocSpec>
    {
        fn new_value<A: Arch + SSAValues>(loc: A::Location, C: &mut HashMap<A::Location, u32>) -> Value<A> {
            use std::collections::hash_map::Entry;
            if let Entry::Occupied(mut e) = C.entry(loc) {
                let i = *e.get();
                *e.get_mut() += 1;
                Value::new(loc, Some(i))
            } else {
                unreachable!();
            }
        }
        let mut assignments: Vec<A::Location> = Vec::new();
        // for each statement in block {
        // also check phis at start of the block...
        if let Some(phis) = phi.get(&block.start) {
            for (loc, _data) in phis {
                // these are very clear reads vs assignments:
                let phi_dest = Rc::clone(&phi[&block.start][loc].out);
                phi_dest.replace(new_value(*loc, C));
                defs.insert(HashedValue {
                    value: Rc::clone(&phi_dest)
                }, (block.start, DefSource::Phi));
                S.get_mut(&loc).expect("S should have entries for all locations.").push(Rc::clone(&phi_dest));
                // for very assignment-heavy blocks maybe there's a good way to summarize multiple of the same location being assigned
                // eg is it faster to store this and pop it back or is it faster to just
                // decode again..?
                assignments.push(*loc); // ???
            }
        }
        let mut iter = data.instructions_spanning::<A::Instruction>(block.start, block.end);
        while let Some((address, instr)) = iter.next() {
            let mut writelog: HashSet<A::Location> = HashSet::new();
            let def_source = DefSource::Instruction;
            for (maybeloc, direction) in instr.iter_locs(disambiguator) {
                if let Some(loc) = maybeloc {
                    match direction {
                        Direction::Read => {
                            let at_address = values.entry(address).or_insert_with(||
                                HashMap::new()
                            );
                            let s_idx = {
                                S[&loc].len() - 1 - if writelog.contains(&loc) {
                                    1
                                } else {
                                    0
                                }
                            };
                            let value = Rc::clone(&S[&loc][s_idx]);
                            value.borrow_mut().used = true;
                            at_address.insert((loc, Direction::Read), value);
                        },
                        Direction::Write => {
                            if writelog.contains(&loc) {
                                continue;
                            } else {
                                writelog.insert(loc.clone());
                            }
                            let new_value = Rc::new(RefCell::new(new_value(loc, C)));
                            defs.insert(HashedValue {
                                value: Rc::clone(&new_value)
                            }, (address, def_source));
                            let at_address = values.entry(address).or_insert_with(||
                                HashMap::new()
                            );
                            at_address.insert((loc, Direction::Write), Rc::clone(&new_value));
                            S.get_mut(&loc).expect("S should have entries for all locations.").push(new_value);
                            // for very assignment-heavy blocks maybe there's a good way to summarize multiple of the same location being assigned
                            // eg is it faster to store this and pop it back or is it faster to just
                            // decode again..?
                            assignments.push(loc); // ???
                        },
                    }
                } else {
                    match direction {
                        Direction::Read => {
                            // it's a read of something, but we don't know what, 
                        },
                        Direction::Write => {
                            // a write to somewhere, we don't know where, this should def ...
                            // everything
                        }
                    }
                }
            }
            for (maybeloc, direction) in instr.iter_locs(disambiguator) {
                if let Some(loc) = maybeloc {
                    for alias in loc.aliases_of() {
                        match direction {
                            Direction::Read => {
                                let at_address = values.entry(address).or_insert_with(||
                                    HashMap::new()
                                );
                                let s_idx = {
                                    S[&alias].len() - 1 - if writelog.contains(&alias) {
                                        1
                                    } else {
                                        0
                                    }
                                };
                                let value = Rc::clone(&S[&alias][s_idx]);
                                value.borrow_mut().used = true;
                                at_address.insert((alias, Direction::Read), value);
                            },
                            Direction::Write => {
                                if writelog.contains(&alias) {
                                    continue;
                                } else {
                                    writelog.insert(alias.clone());
                                }
                                let new_value = Rc::new(RefCell::new(new_value(alias, C)));
                                defs.insert(HashedValue {
                                    value: Rc::clone(&new_value)
                                }, (address, def_source));
                                let at_address = values.entry(address).or_insert_with(||
                                    HashMap::new()
                                );
                                at_address.insert((alias, Direction::Write), Rc::clone(&new_value));
                                S.get_mut(&alias).expect("S should have entries for all locations.").push(new_value);
                                // for very assignment-heavy blocks maybe there's a good way to summarize multiple of the same location being assigned
                                // eg is it faster to store this and pop it back or is it faster to just
                                // decode again..?
                                assignments.push(alias); // ???
                            },
                        }
                    }
                }
            }
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

        {
        let introduced_bounds: &mut HashMap<A::Address, RWMap<A>> = between_block_bounds.entry(block.start).or_insert_with(|| HashMap::new());
        // succ == traditional successors-in-cfg
        for Y in cfg.neighbors(block.start) {
            let edge_modifiers: &mut RWMap<A> = introduced_bounds.entry(Y).or_insert_with(|| HashMap::new());

            for between in value_modifiers.between(block.start, Y) {
                match between {
                    (Some(loc), Direction::Read) => {
//                        let widening = loc.maximal_alias_of();
                        let value = Rc::clone(&S[&loc][S[&loc].len() - 1]);
                        value.borrow_mut().used = true;
                        edge_modifiers.insert((loc, Direction::Read), value);
                        for alias in loc.aliases_of() {
                            let value = Rc::clone(&S[&alias][S[&alias].len() - 1]);
                            value.borrow_mut().used = true;
                            edge_modifiers.insert((alias, Direction::Read), value);
                        }
                    },
                    (None, Direction::Read) => {
                        // it's a read of something, but we don't know what, 
                    },
                    (Some(loc), Direction::Write) => {
//                        let widening = loc.maximal_alias_of();
                        let value = Rc::new(RefCell::new(new_value(loc, C)));
                        defs.insert(HashedValue {
                            value: Rc::clone(&value)
                        }, (block.start, DefSource::Between(Y)));
                        edge_modifiers.insert((loc, Direction::Write), Rc::clone(&value));
                        // Note we push S here only to immediately pop it when done with this
                        // block.
                        // This is replicated when working with immediate dominators as well.
                        // The value only exists in the context of one particular control flow
                        // path.
                        S.get_mut(&loc).expect("S should have entries for all locations.").push(value);
                        // this does not modify assignments because it does not need the same
                        // block-global assignment cleanup - it should be neutral w.r.t S in all
                        // cases.
                        assignments.push(loc); // ???

                        for alias in loc.aliases_of() {
                            let value = Rc::new(RefCell::new(new_value(alias, C)));
                            defs.insert(HashedValue {
                                value: Rc::clone(&value)
                            }, (block.start, DefSource::Between(Y)));
                            edge_modifiers.insert((alias, Direction::Write), Rc::clone(&value));
                            // Note we push S here only to immediately pop it when done with this
                            // block.
                            // This is replicated when working with immediate dominators as well.
                            // The value only exists in the context of one particular control flow
                            // path.
                            S.get_mut(&alias).expect("S should have entries for all locations.").push(value);
                            // this does not modify assignments because it does not need the same
                            // block-global assignment cleanup - it should be neutral w.r.t S in all
                            // cases.
                            assignments.push(alias); // ???
                        }
                    },
                    (None, Direction::Write) => {
                        // a write to somewhere, we don't know where, this should def ...
                        // everything
                    }
                }
            }
//            j = whichpred(Y, block);
//            for each phi in Y {
            if let Some(block_phis) = phi.get_mut(&Y) {
//                for loc in phi.get(Y)
                for (loc, phi_op) in block_phis.iter_mut() {
//                    phi.operands[j] = .. /* value for S[V] */
//                    // not quite perfect, but good enough
                    let loc_stack = &S[&loc];
                    phi_op.ins.push(loc_stack[loc_stack.len() - 1].clone());
                }
            }
/*
            for (loc, direction) in edge_modifiers.keys() {
                if *direction == Direction::Write {
//                    S.get_mut(&loc).expect("S has entries for all locations").pop();
                }
            }
            */
        }
        }

        // children == nodes immediately dominated by block
//        for each Y in children(block) {
        let mut bfs = Bfs::new(&cfg, block.start);
        while let Some(u) = bfs.next(&cfg) {
            if let Some(u_idom) = idom.immediate_dominator(u) {
                if u_idom == block.start && u != block.start {
                    if let Some(adjustments) = between_block_bounds[&block.start].get(&u) {
                        for ((loc, direction), value) in adjustments {
                            if *direction == Direction::Write {
                                S.get_mut(&loc).expect("S has entries for locations").push(Rc::clone(value));
                            }
                        }
                    }
                search(
                    data,
                    basic_blocks.get_block(u),
                    values,
                    between_block_bounds,
                    defs,
                    phi,
                    basic_blocks,
                    cfg,
                    dominance_frontiers,
                    idom,
                    C,
                    S,
                    value_modifiers,
                    disambiguator,
                );
                    // this shouldn't be modified in recursive calls (we won't visit the same block
                    // [so, same bounds] twice), but it's clumsy to express that to rustc. might
                    // revisit in the future.
                    if let Some(adjustments) = between_block_bounds[&block.start].get(&u) {
                        for ((loc, direction), _value) in adjustments {
                            if *direction == Direction::Write {
                                S.get_mut(&loc).expect("S has entries for locations").pop();
                            }
                        }
                    }
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
    let mut S: HashMap<A::Location, Vec<DFGRef<A>>> = HashMap::new();

    // all_locations should be the widest aliases ONLY
//    for each variable in vars {
    for loc in use_tracker.all_locations {
        C.insert(loc, 0);
        S.insert(loc, vec![Rc::new(RefCell::new(Value::new(loc, None)))]);
    }

    search(
        data,
        basic_blocks.get_block(entry),
        &mut values,
        &mut between_block_bounds,
        &mut defs,
        &mut phi,
        basic_blocks,
        cfg,
        &dominance_frontiers,
        &idom,
        &mut C,
        &mut S,
        &value_modifiers,
        disambiguator,
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

    let phis = &phi;

    for (_block, phi_locs) in phi.iter() {
        for (_loc, phi) in phi_locs.iter() {
            if phi.out.borrow().used {
                mark_phi_used(phi, phis, &defs)
            }
        }
    }

    SSA { instruction_values: values, modifier_values: HashMap::new(), control_dependent_values: between_block_bounds, defs: defs, phi: phi }
}
