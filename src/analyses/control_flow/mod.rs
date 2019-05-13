use std::collections::{HashMap, HashSet, VecDeque};
use std::cmp::Ordering;
use std::hash::Hash;

use petgraph;
use petgraph::graphmap::{GraphMap, Nodes};

use yaxpeax_arch::{Address, Arch, Decodable, LengthedInstruction};

use num_traits::{Bounded, WrappingAdd, Zero, One};

use ContextRead;
use ContextWrite;
use yaxpeax_arch::AddressDisplay;

use memory::MemoryRange;

use serialize::GraphSerializer;

use serde::ser::SerializeStruct;
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Effect<Addr> {
    stop_after: bool,
    pub dest: Option<Target<Addr>>
}

impl <Addr> Effect<Addr> {
    pub fn is_stop(&self) -> bool {
        self.stop_after
    }

    pub fn stop() -> Effect<Addr> {
        Effect {
            stop_after: true,
            dest: None
        }
    }
    pub fn stop_and(dest: Target<Addr>) -> Effect<Addr> {
        Effect {
            stop_after: true,
            dest: Some(dest)
        }
    }
    pub fn cont() -> Effect<Addr> {
        Effect {
            stop_after: false,
            dest: None
        }
    }
    pub fn cont_and(dest: Target<Addr>) -> Effect<Addr> {
        Effect {
            stop_after: false,
            dest: Some(dest)
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum Target<Addr> {
    Relative(Addr),
    Absolute(Addr),
    Multiple(Vec<Target<Addr>>), // TODO: ?? jump tables?
    Indeterminate       // Unknowns? rets? idk
}

pub trait Determinant<T, Addr> {
//    fn control_flow<T, Addr>(&self, &T) -> Effect<Addr>;
    fn control_flow(&self, Option<&T>) -> Effect<Addr>;
}

use serde::{Serialize, Deserialize};
#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub struct BasicBlock<Addr> where Addr: Copy + Clone {
    pub start: Addr,
    pub end: Addr // inclusive!!
}

impl <Addr> BasicBlock<Addr> where Addr: Copy + Clone {
    pub fn new(start_addr: Addr, end_addr: Addr) -> BasicBlock<Addr> {
        BasicBlock {
            start: start_addr,
            end: end_addr
        }
    }
}

pub struct ControlFlowGraph<A> where A: Address {
    pub blocks: Vec<BasicBlock<A>>,
    pub graph: GraphMap<A, (), petgraph::Directed>
}

impl <A: Address + Hash> Serialize for ControlFlowGraph<A> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut struc = serializer.serialize_struct("CFG<A>", 2)?;
        struc.serialize_field("blocks", &self.blocks)?;
        struc.serialize_field("graph", &GraphSerializer::from(&self.graph))?;
        struc.end()
    }
}

#[test]
fn control_flow_graph_construction() {
    let mut cfg: ControlFlowGraph<u32> = ControlFlowGraph::new();
    let nexts = cfg.with_effect(4000 - 1, 4000, &Effect::stop());
    println!("nexts0: {:?}", nexts);
    assert!(nexts.len() == 0);
    let nexts = cfg.with_effect(4008, 4008, &Effect::stop_and(
        Target::Relative(3)));
    println!("nexts1: {:?}", nexts);
    assert!(nexts.len() == 1);
    assert!(nexts[0] == 4012);
    let nexts = cfg.with_effect(4030, 4031, &Effect::cont_and(
        Target::Relative(-13i32 as u32)));
    println!("nexts2: {:?}", nexts);
    assert!(nexts.len() == 2);
    assert!(nexts.contains(&4018));
    println!("graph: {:?}", cfg.graph);
    assert!(nexts.contains(&4031));
    let expected_blocks: [(u32, u32, Vec<u32>); 4] = [
        (4000, 4008, vec![4012]),
//        (0x4009, 0x4011),
        (4012, 4017, vec![4018]),
        (4018, 4030, vec![4031, 4018]),
        (4031, 0xffffffff, vec![])
    ];

    for (start, end, nexts) in expected_blocks.iter() {
        let block = cfg.get_block(*start);
        println!("block at {}: ({}, {}). Expecting: ({}, {})", start, block.start, block.end, start, end);
        assert!(block.start == *start);
        assert!(block.end == *end);
        println!("Expected neighbors {:?}", nexts);
        let neighbors = cfg.graph.neighbors(*start).collect::<Vec<u32>>();
        println!("Actual neighbors {:?}", neighbors);
        for i in 0..nexts.len() {
            assert!(nexts[i] == neighbors[i]);
        }
    }
}

#[test]
fn control_flow_graph_construction_2() {
    let mut cfg: ControlFlowGraph<u32> = ControlFlowGraph::new();
    cfg.with_effect(1000 - 1, 1000, &Effect::stop());
    cfg.with_effect(1009, 1010, &Effect::cont_and(
        Target::Relative(-10i32 as u32)));
    cfg.with_effect(1019,  1020, &Effect::cont_and(
        Target::Relative(-11i32 as u32)));

    // TODO
//    println!("cfg:\n  graph: {:?}\n  blocks: {:?}", cfg.graph, cfg.blocks);
//    assert!(false == true);
}

#[test]
fn control_flow_graph_construction_3() {
    /*
     * OK. the issue here was something like having a block
     * [0, 9], that leads into [10, 19],
     * but addint a split that stops at 4 yielded
     * [0, 4], [4, 9], [10, 19]
     * where nothing was connected.
     */
    let mut cfg: ControlFlowGraph<u32> = ControlFlowGraph::new();
    cfg.with_effect(0, 1, &Effect::stop_and(
        Target::Absolute(10)
    ));
    /*
     * So now we have [0, 0], [1, 9], [10, ..]
     * with 0 -> 10, 1 -> 10
     */
    for n in [0, 1, 10].iter() {
        assert!(cfg.graph.contains_node(*n));
    }
    for (start, dest) in [(0, 10), (1, 10)].iter() {
        assert!(cfg.graph.contains_edge(*start, *dest));
    }

    cfg.with_effect(4, 5, &Effect::stop());
    /*
     * and now we should have [0, 0], [1, 4], [5, 9], [10, ..]
     * with 0 -> 10, 5 -> 10
     */
    for n in [0, 1, 5, 10].iter() {
        assert!(cfg.graph.contains_node(*n));
    }
    for (start, dest) in [(0, 10), (5, 10)].iter() {
        assert!(cfg.graph.contains_edge(*start, *dest));
    }
}

impl <A> ControlFlowGraph<A> where A: Address + petgraph::graphmap::NodeTrait {
    pub fn new() -> ControlFlowGraph<A> {
        let mut cfg = ControlFlowGraph {
            blocks: vec![BasicBlock::new(A::min_value(), A::max_value())],
            graph: GraphMap::new()
        };
        cfg.graph.add_node(A::min_value());
        cfg
    }

    pub fn blocks(&self) -> Nodes<A> {
        self.graph.nodes()
    }

    pub fn destinations(&self, block: A) -> Vec<A> {
        self.graph.neighbors_directed(block, petgraph::Direction::Outgoing).into_iter().collect()
    }

    /*
     * U should be a function, function_table should be an oracle
     * we can query to answer "does there exist a function at this place?"
     *
     * TODO: are there other reasons a basic block edge should be
     * disincluded?
     */
    pub fn get_function<U>(&self, start: A, function_table: &HashMap<A, U>) -> ControlFlowGraph<A> {
        let mut result: ControlFlowGraph<A> = ControlFlowGraph::new();
        result.graph.add_node(start);
        let mut walk = petgraph::visit::Bfs::new(&self.graph, start);
        for k in function_table.keys() {
            walk.discovered.insert(*k);
        }
        while let Some(next) = walk.next(&self.graph) {
            for i in self.graph.neighbors_directed(next, petgraph::Direction::Outgoing) {
                if !function_table.contains_key(&i) {
                    result.graph.add_edge(next, i, ());
                }
            }
        }
        result.blocks = vec![];
        let mut starts: Vec<A> = result.graph.nodes().collect();
        starts.sort();
        for addr in starts.into_iter() {
            result.blocks.push(BasicBlock { start: addr, end: self.get_block(addr).end });
        }
        return result;
    }

    fn get_block_idx<'a>(&'a self, addr: A) -> usize {
        self.blocks.binary_search_by(|block| {
            if addr < block.start {
                Ordering::Greater
            } else if addr > block.end {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        }).expect("<blocks> should cover the entire address space. Binary search should not fail. This is a critical algorithmic bug.")
    }
    pub fn get_block<'a>(&'a self, addr: A) -> &'a BasicBlock<A> {
        let idx = self.get_block_idx(addr);

        &self.blocks[idx]
    }
    /*
     * Adjust control flow linkage as appropriate with effect `effect`
     * applied at `at` with the next linear instruction at `next`.
     *
     * This takes `next` explicitly to avoid taking an instruction
     * just to `instr.len()` it. Add at the call site.
     *
     * This may be best explained with an example:
     *   0x1234: 54030400: jmp $+4 (imaginary ISA)
     * the effect from here is a relative branch with no continuation
     * to relative +4. the +4 is computed with respect to the start
     * of the *next* instruction (eg where PC points *after* this
     * instruction). To find the basic block this affects, we need
     * the address of the instruction that causes control flow
     * but also the address of the next instruction for relative branch
     * destination computations.
     *
     * So at = 0x1234, next = 0x1238, effect = stop_and(Relative(4)).
     */
    pub fn with_effect(&mut self, at: A, next: A, effect: &Effect<A>) -> Vec<A> {
        // splits the basic block enclosing split_loc into two basic blocks,
        // one ending directly before split_loc, and one starting at split_loc
        // continuing to the end of the original basic block.
        //
        // If split_loc is the start of the enclosing basic block, no change
        // is made (the former block would be nonsense spanning
        //  [split_loc, split_loc - 1]
        // and thus would not be added, yielding no net change.
        fn add_split<A: Address + petgraph::graphmap::NodeTrait>(graph: &mut ControlFlowGraph<A>, split_loc: A, preserve_edges: bool) -> bool {
            let idx = graph.get_block_idx(split_loc);
            let (start, end) = {
                let block = &graph.blocks[idx];
                (block.start, block.end)
            };

            if start != split_loc {
                let new_block = BasicBlock::new(split_loc, end);
                graph.blocks.insert(idx + 1, new_block);
                graph.blocks[idx].end = split_loc - A::one();
                let neighbors: Vec<A> = graph.graph.neighbors(graph.blocks[idx].start).into_iter().collect();
                for next in neighbors.into_iter() {
                    graph.graph.remove_edge(graph.blocks[idx].start, next);
                    graph.graph.add_edge(new_block.start, next, ());
                }
                if preserve_edges {
                    graph.graph.add_edge(graph.blocks[idx].start, new_block.start, ());
                } else {
//                    graph.graph.add_node(new_block.start);
                }
                true
            } else {
                false
            }
        }

        let mut result: Vec<A> = vec![];

        let enclosing_block_start: A = self.get_block(at).start;

        if effect.stop_after {
            add_split(self, next, false);
        } else {
            // if this is cont, AND there is nothing to branch to, this
            // does not really effect control flow.
            //
            // so if this is cont and there is an out-dest, this ends
            // the basic block.
            if effect.dest.is_some() {
                let dest_addr = next;
                if (add_split(self, dest_addr, true)) { // TODO: t.len());
                }
                    result.push(dest_addr);
                self.graph.add_edge(enclosing_block_start, dest_addr, ());
            }
        }

//        let enclosing_block_start: A = self.get_block(at).start;

        match &effect.dest {
            // Ok, have to clip the containing basic block
            // if this is not going to the start of an existing basic block
            Some(Target::Relative(rel)) => {
                let dest_addr = next.wrapping_add(rel);
                if add_split(self, dest_addr, true) {
                }
                    result.push(dest_addr);
                let enclosing_block_start: A = self.get_block(at).start;
//                println!("Adding edge from {:?} to {:?} (at == {:?})", enclosing_block_start, dest_addr, at);
                self.graph.add_edge(enclosing_block_start, dest_addr, ());
            },
            Some(Target::Absolute(dest)) => {
                let dest_addr = *dest;
                if add_split(self, dest_addr, true) {
                }
                    result.push(dest_addr);
//                let enclosing_block_start: A = self.get_block(at).start;
                self.graph.add_edge(enclosing_block_start, dest_addr, ());
            }
            Some(Target::Multiple(targets)) => {
                for target in targets {
                    match target {
                        Target::Relative(rel) => {
                            let dest_addr = next.wrapping_add(rel);
                            if add_split(self, dest_addr, true) {
                            }
                                result.push(dest_addr);
//                            let enclosing_block_start: A = self.get_block(at).start;
                            self.graph.add_edge(enclosing_block_start, dest_addr, ());
                        },
                        Target::Absolute(dest) => {
                            let dest_addr = *dest;
                            if add_split(self, dest_addr, true) {
                            }
                                result.push(dest_addr);
//                            let enclosing_block_start: A = self.get_block(at).start;
                            self.graph.add_edge(enclosing_block_start, dest_addr, ());
                        }
                        _ => {
                            // TODO: handle these.
                            panic!("Unhandled");
                        }
                    }
                }
            },
            _ => {
                // TODO: unhandled!
            }
        }
        result
    }
}

use std::fmt::Debug;

pub fn explore_all<'a, A, U, M, Contexts, Update, InstrCallback>(
    data: &M,
    contexts: &'a mut Contexts,
    cfg: &mut ControlFlowGraph<A::Address>,
    starts: Vec<A::Address>,
    on_instruction_discovered: &InstrCallback
) where
    A: Arch,
    M: MemoryRange<A::Address>,
    Contexts: ContextRead<A, U> + ContextWrite<A, Update>,
    A::Address: Hash + petgraph::graphmap::NodeTrait + num_traits::WrappingAdd,
    A::Instruction: Debug + Determinant<U, A::Address>,
    InstrCallback: Fn(&A::Instruction, A::Address, &Effect<A::Address>, &Contexts) -> Vec<(A::Address, Update)>
{
    let mut to_explore: VecDeque<A::Address> = VecDeque::new();
    let mut seen: HashSet<A::Address> = HashSet::new();

    for addr in starts.iter() {
        to_explore.push_back(*addr);
        seen.insert(*addr);

        if *addr > A::Address::zero() {
            // we've been told by `starts` that control flow leads here
            // so it must be the start of a basic block.
            cfg.with_effect(*addr - A::Address::one(), *addr, &Effect::stop());
        }
    }

    while let Some(addr) = to_explore.pop_front() {
        let dests = explore_control_flow(data, contexts, cfg, addr, on_instruction_discovered);
        for next in dests.into_iter() {
            if !seen.contains(&next) {
                to_explore.push_back(next);
                seen.insert(next);
            }
        }
    }
}

pub fn explore_control_flow<'a, A, U, M, Contexts, Update, InstrCallback>(
    data: &M,
    contexts: &'a mut Contexts,
    cfg: &mut ControlFlowGraph<A::Address>,
    start: A::Address,
    on_instruction_discovered: &InstrCallback
) -> Vec<A::Address> where
    A: Arch,
    M: MemoryRange<A::Address>,
    Contexts: ContextWrite<A, Update> + ContextRead<A, U>,
    A::Address: Hash + petgraph::graphmap::NodeTrait + num_traits::WrappingAdd,
    A::Instruction: Debug + Determinant<U, A::Address>,
    InstrCallback: Fn(&A::Instruction, A::Address, &Effect<A::Address>, &Contexts) -> Vec<(A::Address, Update)> {
    // we don't know if we've just observed some flow to start,
    // or that start has already been explored,
    // so for now just go through start to end like normal
    //
    // can't even assume if the block ends at the same end
    // as we find that we've already seen this, because that
    // would ambiguify single instruction basic blocks

    let mut addr = start;
    loop {
        let range = match data.range_from(addr) {
            Some(range) => range,
            None => {
                use petgraph::Direction;
                println!("Reached {}, which is not a valid address - marking start ({}) as hazard.", addr.stringy(), start.stringy());
                let problem_blocks = cfg.graph.neighbors_directed(start, Direction::Incoming).collect::<Vec<A::Address>>();
                println!("Problem blocks: {:?}", problem_blocks);
                for problem in problem_blocks.iter() {
                    cfg.graph.remove_edge(*problem, start);
                }
                return vec![];
            }
        };
        match A::Instruction::decode(range) {
            Some(instr) => {
                let effect = {
                    let ctx = contexts.at(&addr);
                    instr.control_flow(Some(&ctx))
                };
                let results = on_instruction_discovered(&instr, addr, &effect, contexts);
                for (addr, update) in results.into_iter() {
                    contexts.put(addr, update);
                }
                match effect {
                    Effect { stop_after: false, dest: None } => {
                        // we can continue!
                        addr = addr + instr.len();
                    },
                    // and for any other cases...
                    effect @ _ => {
                        return cfg.with_effect(addr, addr.wrapping_add(&instr.len()), &effect);
                    }
                }
            },
            None => {
                return vec![];
            }
        }
    }
}

//                                        v-- (Addr, T). this probably will have to go in favor of Vec<u8> and T:
//                                        Decodable?
pub fn build_global_cfgs<'a, A: Arch, U, Update, UTable>(ts: &Vec<(A::Address, A::Instruction)>, contexts: &'a UTable, start: u16) -> ControlFlowGraph<A::Address>
    where
        A::Instruction: Determinant<U, A::Address> + LengthedInstruction<Unit=A::Address>,
        A::Address: petgraph::graphmap::NodeTrait,
        UTable: ContextRead<A, U> + ContextWrite<A, Update>
{
    let mut cfg = ControlFlowGraph {
        blocks: vec![BasicBlock::new(A::Address::min_value(), A::Address::max_value())], // TODO: this should be the lower and upper bounds of rust-num Bounded one day
        graph: GraphMap::new()
    };

    // splits the basic block enclosing split_loc into two basic blocks,
    // one ending directly before split_loc, and one starting at split_loc
    // continuing to the end of the original basic block.
    //
    // If split_loc is the start of the enclosing basic block, no change
    // is made (the former block would be nonsense spanning
    //  [split_loc, split_loc - 1]
    // and thus would not be added, yielding no net change.
    fn add_split<A>(blocks: &mut Vec<BasicBlock<A>>, split_loc: A) where A: Address {
        let idx = blocks.binary_search_by(|block| {
            if split_loc < block.start {
                Ordering::Greater
            } else if split_loc > block.end {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        }).expect("<blocks> should over the entire address space. Binary search should not fail. This is a critical algorithmic bug.");

        let (start, end) = {
            let block = &blocks[idx];
            (block.start, block.end)
        };

        if start != split_loc {
            let new_block = BasicBlock::new(split_loc, end);
            blocks.insert(idx + 1, new_block);
            blocks[idx].end = split_loc - A::one();
        };
    }

    for (addr, t) in ts {
        let effect = t.control_flow(Some(&contexts.at(addr)));
        if effect.stop_after {
            add_split(&mut cfg.blocks, *addr + t.len());
        }
        match effect.dest {
            // Ok, have to clip the containing basic block
            // if this is not going to the start of an existing basic block
            Some(Target::Relative(rel)) => {
                add_split(&mut cfg.blocks, *addr + t.len() + rel);
            },
            Some(Target::Absolute(dest)) => {
                add_split(&mut cfg.blocks, dest);
            }
            Some(Target::Multiple(targets)) => {
                for target in targets {
                    match target {
                        Target::Relative(rel) => {
                            add_split(&mut cfg.blocks, *addr + t.len() + rel);
                        },
                        Target::Absolute(dest) => {
                            add_split(&mut cfg.blocks, dest);
                        }
                        _ => {
                            // TODO: handle these.
                            panic!("Unhandled");
                        }
                    }
                }
            },
            _ => {
                // TODO: unhandled!
            }
        }
    }

    // Add to digraph here.
    // ssa on this?
    // bfs to find reachable from a call?
    // all this and more, next time..

    {
    let mut block_iter = cfg.blocks.iter();

    let mut curr_block = block_iter.next().expect("basic blocks should span all instructions.");
    // addr should be increasing, so we *will* reach the end of this block eventually..
    for (addr, t) in ts {
        if addr == &curr_block.end {
            let effect = t.control_flow(Some(&contexts.at(addr)));
            if !effect.stop_after {
                cfg.graph.add_edge(curr_block.start, *addr + t.len(), ());
            }
            match effect.dest {
                Some(Target::Relative(rel)) => {
                    cfg.graph.add_edge(curr_block.start, *addr + t.len() + rel, ());
                },
                Some(Target::Absolute(dest)) => {
                    cfg.graph.add_edge(curr_block.start, dest, ());
                },
                Some(Target::Multiple(targets)) => {
                    for target in targets {
                        match target {
                            Target::Relative(rel) => {
                                cfg.graph.add_edge(curr_block.start, *addr + t.len() + rel, ());
                            },
                            Target::Absolute(dest) => {
                                cfg.graph.add_edge(curr_block.start, dest, ());
                            }
                            _ => {
                                // TODO: handle these.
                                panic!("Unhandled");
                            }
                        }
                    }
                },
                _ => {
                    // TODO: handle these cases too...
                }
            }
            curr_block = block_iter.next().expect("basic blocks should span all instructions.");
        }
    }
    }

    cfg
}
