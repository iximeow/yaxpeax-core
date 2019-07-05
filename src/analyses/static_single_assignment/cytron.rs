use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use std::cmp::Eq;
use petgraph::graphmap::GraphMap;
use petgraph;
use petgraph::visit::Bfs;

use std::cell::RefCell;
use std::rc::Rc;

use std::fmt::Debug;

use yaxpeax_arch::{Arch, LengthedInstruction};
use analyses::control_flow::{BasicBlock, ControlFlowGraph};
use memory::MemoryRange;

use analyses::static_single_assignment::{HashedValue, DefSource, DFGRef, Value, SSA, SSAValues, RWMap, PhiLocations};
use analyses::static_single_assignment::data::PhiOp;
use data::{AliasInfo, Direction};
use data::modifier::ModifierCollection;

use num_traits::Zero;

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

pub fn generate_ssa<A: SSAValues, M: MemoryRange<A::Address>, U: ModifierCollection<A>>(
    data: &M,
    entry: A::Address,
    basic_blocks: &ControlFlowGraph<A::Address>,
    cfg: &GraphMap<A::Address, (), petgraph::Directed>,
    value_modifiers: &U
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
                        assignments.entry(widening).or_insert_with(|| HashSet::new()).insert(block.start);
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
    let mut values: HashMap<A::Address, RWMap<A>> = HashMap::new();
    let mut defs: HashMap<HashedValue<DFGRef<A>>, (A::Address, DefSource)> = HashMap::new();
    let mut phi: HashMap<A::Address, PhiLocations<A>> = HashMap::new();

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
                        let new_value = Rc::new(RefCell::new(Value::new(*Y, *loc, Some(0xffffffff))));
                        defs.insert(HashedValue {
                            value: Rc::clone(&new_value)
                        }, (*Y, DefSource::Phi));
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
    fn search<A: Arch + SSAValues, M: MemoryRange<A::Address>>(
        data: &M,
        block: &BasicBlock<A::Address>,
        values: &mut HashMap<A::Address, RWMap<A>>,
        defs: &mut HashMap<HashedValue<DFGRef<A>>, (A::Address, DefSource)>,
        phi: &mut HashMap<A::Address, PhiLocations<A>>,
        basic_blocks: &ControlFlowGraph<A::Address>,
        cfg: &GraphMap<A::Address, (), petgraph::Directed>,
        dominance_frontiers: &HashMap<A::Address, Vec<A::Address>>,
        idom: &petgraph::algo::dominators::Dominators<A::Address>,
        C: &mut HashMap<A::Location, u32>,
        S: &mut HashMap<A::Location, Vec<DFGRef<A>>>
    ) where <A as Arch>::Address: Copy + Ord + Hash + Eq, <A as Arch>::Instruction: Debug + LengthedInstruction<Unit=<A as Arch>::Address> {
        let mut assignments: Vec<A::Location> = Vec::new();
        // for each statement in block {
        // also check phis at start of the block...
        if let Some(phis) = phi.get(&block.start) {
            for (loc, _data) in phis {
                // these are very clear reads vs assignments:
                let widening = loc.maximal_alias_of();
                let i = C[&widening];
                let mut phi_dest = Rc::clone(&phi[&block.start][loc].out);
                phi_dest.replace(Value::new(block.start, *loc, Some(i)));
                S.get_mut(&widening).expect("S should have entries for all locations.").push(Rc::clone(&phi_dest));
                C.entry(widening).and_modify(|x| *x += 1);
                // for very assignment-heavy blocks maybe there's a good way to summarize multiple of the same location being assigned
                // eg is it faster to store this and pop it back or is it faster to just
                // decode again..?
                assignments.push(widening); // ???
            }
        }
        let mut iter = data.instructions_spanning::<A::Instruction>(block.start, block.end);
        while let Some((address, instr)) = iter.next() {
            for (maybeloc, direction) in A::decompose(&instr).into_iter().zip(std::iter::repeat(DefSource::Instruction)) {
                match (maybeloc, direction) {
                    ((Some(loc), Direction::Read), _) => {
                        let widening = loc.maximal_alias_of();
                        let at_address = values.entry(address).or_insert_with(||
                            HashMap::new()
                        );
                        at_address.insert((loc, Direction::Read), Rc::clone(&S[&widening][S[&widening].len() - 1]));
                    },
                    ((None, Direction::Read), _) => {
                        // it's a read of something, but we don't know what, 
                    },
                    ((Some(loc), Direction::Write), def_source) => {
                        let widening = loc.maximal_alias_of();
                        let i = C[&widening];
                        let new_value = Rc::new(RefCell::new(Value::new(address, loc, Some(i))));
                        defs.insert(HashedValue {
                            value: Rc::clone(&new_value)
                        }, (address, def_source));
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
                    ((None, Direction::Write), _) => {
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
                for (loc, phi_op) in block_phis.iter_mut() {
                    let widening = loc.maximal_alias_of();
//                    phi.operands[j] = .. /* value for S[V] */
//                    // not quite perfect, but good enough
                    let widen_stack = &S[&widening];
                    phi_op.ins.push(widen_stack[widen_stack.len() - 1].clone());
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
                    defs,
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
    let mut S: HashMap<A::Location, Vec<DFGRef<A>>> = HashMap::new();

    // all_locations should be the widest aliases ONLY
//    for each variable in vars {
    for loc in all_locations {
        C.insert(loc, 0);
        S.insert(loc, vec![Rc::new(RefCell::new(Value::new(A::Address::zero(), loc, None)))]);
    }

    search(
        data,
        basic_blocks.get_block(entry),
        &mut values,
        &mut defs,
        &mut phi,
        basic_blocks,
        cfg,
        &dominance_frontiers,
        &idom,
        &mut C,
        &mut S
    );

    SSA { instruction_values: values, modifier_values: HashMap::new(), defs: defs, phi: phi }
}
