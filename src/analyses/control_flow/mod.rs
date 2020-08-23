use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::hash::Hash;
use std::ops::Bound::Included;
use std::fmt::Debug;

use smallvec::SmallVec;

use petgraph;
use petgraph::graphmap::{GraphMap, Nodes};

use yaxpeax_arch::{Address, AddressBase, AddressDiff, AddressDiffAmount, AddressDisplay, Arch, Decoder, LengthedInstruction};

use num_traits::{Zero, One};

use ContextRead;
use ContextWrite;

use memory::MemoryRange;

pub mod deserialize;

use serialize::GraphSerializer;

use serde::Serialize;

use serde::ser::SerializeStruct;
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Effect<Addr: AddressDiffAmount + Debug> {
    pub(crate) stop_after: bool,
    // the AddressDiffAmount trait fools `Deserialize`'s proc macro, so we have to explicitly write
    // the bound serde should use.
    #[serde(bound(deserialize = "Addr: AddressDiffAmount"))]
    pub dest: Option<Target<Addr>>
}

impl <Addr: Address + Debug> Effect<Addr> {
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

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum Target<Addr: AddressDiffAmount + Debug> {
    // the AddressDiffAmount trait fools `Deserialize`'s proc macro, so we have to explicitly write
    // the bound serde should use.
    #[serde(bound(deserialize = "Addr: AddressDiffAmount"))]
    Relative(AddressDiff<Addr>),
    #[serde(bound(deserialize = "Addr: AddressDiffAmount"))]
    Absolute(Addr),
    #[serde(bound(deserialize = "Addr: AddressDiffAmount"))]
    Multiple(Vec<Target<Addr>>), // TODO: ?? jump tables?
    Indeterminate       // Unknowns? rets? idk
}

pub trait Determinant<T, Addr: AddressDiffAmount + Debug> {
    fn control_flow(&self, Option<&T>) -> Effect<Addr>;
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub struct BasicBlock<Addr> where Addr: Copy + Clone {
    pub start: Addr,
    pub end: Addr // inclusive!!
}

impl<Addr: Copy + Clone> BasicBlock<Addr> {
    pub fn new(start_addr: Addr, end_addr: Addr) -> BasicBlock<Addr> {
        BasicBlock {
            start: start_addr,
            end: end_addr
        }
    }
}

#[derive(Default)]
pub struct ControlFlowGraph<A> where A: Address {
    pub entrypoint: A,
    pub blocks: BTreeMap<A, BasicBlock<A>>,
    pub graph: GraphMap<A, (), petgraph::Directed>
}

impl <A: Address + Hash> Serialize for ControlFlowGraph<A> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut struc = serializer.serialize_struct("CFG<A>", 3)?;
        struc.serialize_field("entrypoint", &self.entrypoint)?;
        struc.serialize_field("blocks", &self.blocks)?;
        struc.serialize_field("graph", &GraphSerializer::from(&self.graph))?;
        struc.end()
    }
}

#[ignore]
#[test]
fn control_flow_graph_construction() {
    let mut cfg: ControlFlowGraph<u32> = ControlFlowGraph::new();
    let nexts = cfg.with_effect(4000 - 1, 4000, &Effect::stop());
    println!("nexts0: {:?}", nexts);
    assert!(nexts.len() == 0);
    let nexts = cfg.with_effect(4008, 4008, &Effect::stop_and(
        Target::Relative(AddressDiff::from_const(3))));
    println!("nexts1: {:?}", nexts);
    assert!(nexts.len() == 1);
    assert!(nexts[0] == 4012);
    let nexts = cfg.with_effect(4030, 4031, &Effect::cont_and(
        Target::Relative(AddressDiff::from_const(-13i32 as u32))));
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
        Target::Relative(AddressDiff::from_const(-10i32 as u32))));
    cfg.with_effect(1019,  1020, &Effect::cont_and(
        Target::Relative(AddressDiff::from_const(-11i32 as u32))));

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

impl <A> ControlFlowGraph<A> where A: Address + Debug + petgraph::graphmap::NodeTrait {
    pub fn new() -> ControlFlowGraph<A> {
        let mut blocks = BTreeMap::new();
        blocks.insert(A::min_value(), BasicBlock::new(A::min_value(), A::max_value()));
        let mut cfg = ControlFlowGraph {
            entrypoint: A::zero(),
            blocks,
            graph: GraphMap::new()
        };
        cfg.graph.add_node(A::min_value());
        cfg
    }

    pub fn from(addr: A) -> ControlFlowGraph<A> {
        let mut blocks = BTreeMap::new();
        blocks.insert(A::min_value(), BasicBlock::new(A::min_value(), A::max_value()));
        let mut cfg = ControlFlowGraph {
            entrypoint: addr,
            blocks,
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

    pub fn sources(&self, block: A) -> Vec<A> {
        self.graph.neighbors_directed(block, petgraph::Direction::Incoming).into_iter().collect()
    }

    /*
     * U should be a function, function_table should be an oracle
     * we can query to answer "does there exist a function at this place?"
     *
     * TODO: are there other reasons a basic block edge should be
     * disincluded?
     */
    pub fn get_function<U>(&self, start: A, function_table: &HashMap<A, U>) -> ControlFlowGraph<A> {
        let mut result: ControlFlowGraph<A> = ControlFlowGraph::from(start);
        result.graph = GraphMap::new();
        result.graph.add_node(start);
        result.blocks = BTreeMap::new();

        let mut bfs_deque = VecDeque::new();
        bfs_deque.push_back(start);
        let mut bfs_visited = HashSet::new();
        bfs_visited.insert(start);

        while let Some(next) = bfs_deque.pop_front() {
            for outedge in self.graph.neighbors_directed(next, petgraph::Direction::Outgoing) {
                if bfs_visited.insert(outedge) {
                    // don't walk into another function - don't have to check `next == start`
                    // because start was visited already and bfs_visited.insert() would never be
                    // true anyway.
                    if !function_table.contains_key(&outedge) {
                        bfs_deque.push_back(outedge);
                        result.graph.add_edge(next, outedge, ());
                    }
                }
            }

            result.blocks.insert(next, *self.get_block(next));
        }
        return result;
    }

    pub fn get_block<'a>(&'a self, addr: A) -> &'a BasicBlock<A> {
        let (_block_idx, block) = self.blocks.range((Included(&A::min_value()), Included(&addr))).rev().next().expect("there should be a basic block covering all addresses, this is a control flow data structure error");
        block
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
    pub fn with_effect(&mut self, at: A, next: A, effect: &Effect<A>) -> SmallVec<[A; 2]> {
        // splits the basic block enclosing split_loc into two basic blocks,
        // one ending directly before split_loc, and one starting at split_loc
        // continuing to the end of the original basic block.
        //
        // If split_loc is the start of the enclosing basic block, no change
        // is made (the former block would be nonsense spanning
        //  [split_loc, split_loc - 1]
        // and thus would not be added, yielding no net change.
        fn add_split<A: Address + petgraph::graphmap::NodeTrait>(graph: &mut ControlFlowGraph<A>, split_loc: A, preserve_edges: bool) -> bool {
            // find a block before `split_loc`. either there is one, or `split_loc` is the lowest
            // value of `A`, and there is no possible start address before it.
            let mut iter = graph.blocks.range_mut((Included(&A::min_value()), Included(&split_loc))).rev();

            let last_block: &mut BasicBlock<A> = if let Some((_, prev)) = iter.next() {
                prev
            } else {
                // no prior block, so split_loc should be the first address.
                assert_eq!(split_loc.to_linear(), A::min_value().to_linear());
                return false;
            };

            if last_block.start == split_loc {
                return false;
            }

            // ok, we have a basic block that starts before split_loc, so resize it to end at
            // `split_loc - 1`, with `[split_loc, block.end]` as the block we must insert after it
            let split_loc_end = last_block.end;
            let last_start = last_block.start;
            last_block.end = split_loc - AddressDiff::one();
            graph.blocks.insert(split_loc, BasicBlock::new(split_loc, split_loc_end));

            let neighbors: Vec<A> = graph.graph.neighbors(last_start).into_iter().collect();
            for next in neighbors.into_iter() {
                graph.graph.remove_edge(last_start, next);
                graph.graph.add_edge(split_loc, next, ());
            }
            if preserve_edges {
                graph.graph.add_edge(last_start, split_loc, ());
            } else {
//                    graph.graph.add_node(new_block.start);
            }
            true
        }

        let mut result: SmallVec<[A; 2]> = SmallVec::new();

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
                if add_split(self, dest_addr, true) { // TODO: t.len());
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
                let dest_addr = next.wrapping_offset(*rel);
                if add_split(self, dest_addr, true) {
                }
                    result.push(dest_addr);
                let enclosing_block_start: A = self.get_block(at).start;
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
            Some(Target::Multiple(_targets)) => {
                /*
                for target in targets {
                    match target {
                        Target::Relative(rel) => {
                            let dest_addr = next.wrapping_offset(*rel);
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
                */
            },
            _ => {
                // TODO: unhandled!
            }
        }
        result
    }
}

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

pub struct AnalysisBuilder<
    'memory,
    'ctx,
    A: Arch,
    M: MemoryRange<A::Address>,
    U,
    Update,
    Contexts: ContextWrite<A, Update> + ContextRead<A, U>,
> {
    memory: &'memory M,
    starts: Option<Vec<A::Address>>,
    contexts: &'ctx mut Contexts,
    on_instruction_discovered: fn(&A::Instruction, A::Address, &Effect<A::Address>, &Contexts) -> Vec<(A::Address, Update)>,
    _u: std::marker::PhantomData<U>,
}

impl<'memory, 'ctx, A, M: MemoryRange<A::Address>, U, Update, Contexts> AnalysisBuilder<'memory, 'ctx, A, M, U, Update, Contexts> where
    A: Arch,
    Contexts: ContextWrite<A, Update> + ContextRead<A, U>,
    A::Address: Hash + petgraph::graphmap::NodeTrait + num_traits::WrappingAdd,
    A::Instruction: Debug + Determinant<U, A::Address>,
{
    pub fn new(memory: &'memory M, contexts: &'ctx mut Contexts) -> Self {
        fn do_nothing<
            A: Arch,
            U,
            Update,
            Contexts: ContextWrite<A, Update> + ContextRead<A, U>
        >(_inst: &A::Instruction, _addr: A::Address, _effect: &Effect<A::Address>, _ctx: &Contexts) -> Vec<(A::Address, Update)> {
            Vec::new()
        }
        Self {
            memory,
            starts: None,
            contexts,
            on_instruction_discovered: do_nothing,
            _u: std::marker::PhantomData,
        }
    }

    pub fn with_entrypoints(mut self, starts: Vec<A::Address>) -> Self {
        self.starts = Some(starts);
        self
    }

    pub fn evaluate(self) -> ControlFlowGraph<A::Address> {
        let mut cfg = ControlFlowGraph::new();
        self.evaluate_into(&mut cfg);
        cfg
    }

    pub fn evaluate_into(self, cfg: &mut ControlFlowGraph<A::Address>) {
        let Self {
            memory,
            contexts,
            starts,
            on_instruction_discovered,
            ..
        } = self;
        if let Some(starts) = starts {
            explore_all(memory, contexts, cfg, starts, &on_instruction_discovered);
        } else {
            explore_all(memory, contexts, cfg, vec![A::Address::zero()], &on_instruction_discovered);
        }
    }
}

pub fn explore_control_flow<'a, A, U, M, Contexts, Update, InstrCallback>(
    data: &M,
    contexts: &'a mut Contexts,
    cfg: &mut ControlFlowGraph<A::Address>,
    start: A::Address,
    on_instruction_discovered: &InstrCallback
) -> SmallVec<[A::Address; 2]> where
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
                println!("Reached {}, which is not a valid address - marking start ({}) as hazard.", addr.show(), start.show());
                let problem_blocks = cfg.graph.neighbors_directed(start, Direction::Incoming).collect::<Vec<A::Address>>();
                println!("Problem blocks: {:?}", problem_blocks);
                for problem in problem_blocks.iter() {
                    cfg.graph.remove_edge(*problem, start);
                }
                return SmallVec::new();
            }
        };
        match A::Decoder::default().decode(range) {
            Ok(instr) => {
                let effect = {
                    let ctx = contexts.at(&addr);
                    println!("computing control flow at {}", addr.show());
                    instr.control_flow(Some(&ctx))
                };
                let results = on_instruction_discovered(&instr, addr, &effect, contexts);
                for (addr, update) in results.into_iter() {
                    contexts.put(addr, update);
                }
                match effect {
                    Effect { stop_after: false, dest: None } => {
                        // we can continue!
                        addr += instr.len();
                    },
                    // and for any other cases...
                    effect @ _ => {
                        return cfg.with_effect(addr, addr.wrapping_offset(instr.len()), &effect);
                    }
                }
            },
            Err(_) => {
                return SmallVec::new();
            }
        }
    }
}

//                                        v-- (Addr, T). this probably will have to go in favor of Vec<u8> and T:
//                                        Decodable?
pub fn build_global_cfgs<'a, A: Arch, U, Update, UTable>(ts: &Vec<(A::Address, A::Instruction)>, contexts: &'a UTable, _start: u16) -> ControlFlowGraph<A::Address>
    where
        A::Instruction: Determinant<U, A::Address> + LengthedInstruction<Unit=AddressDiff<A::Address>>,
        A::Address: petgraph::graphmap::NodeTrait,
        UTable: ContextRead<A, U> + ContextWrite<A, Update>
{
    let mut cfg = ControlFlowGraph::new();

    // splits the basic block enclosing split_loc into two basic blocks,
    // one ending directly before split_loc, and one starting at split_loc
    // continuing to the end of the original basic block.
    //
    // If split_loc is the start of the enclosing basic block, no change
    // is made (the former block would be nonsense spanning
    //  [split_loc, split_loc - 1]
    // and thus would not be added, yielding no net change.
    fn add_split<A>(blocks: &mut BTreeMap<A, BasicBlock<A>>, split_loc: A) where A: Address {
        // find a block before `split_loc`. either there is one, or `split_loc` is the lowest
        // value of `A`, and there is no possible start address before it.
        let mut iter = blocks.range_mut((Included(&A::min_value()), Included(&split_loc))).rev();

        let last_block: &mut BasicBlock<A> = if let Some((_, prev)) = iter.next() {
            prev
        } else {
            // no prior block, so split_loc should be the first address.
            assert_eq!(split_loc.to_linear(), A::min_value().to_linear());
            return;
        };

        if last_block.start == split_loc {
            return;
        }

        // ok, we have a basic block that starts before split_loc, so resize it to end at
        // `split_loc - 1`, with `[split_loc, block.end]` as the block we must insert after it
        let split_loc_end = last_block.end;
        last_block.end = split_loc - AddressDiff::one();
        blocks.insert(split_loc, BasicBlock::new(split_loc, split_loc_end));
    }

    for (addr, t) in ts {
        let effect = t.control_flow(Some(&contexts.at(addr)));
        if effect.stop_after {
            add_split(&mut cfg.blocks, addr.wrapping_offset(t.len()));
        }
        match effect.dest {
            // Ok, have to clip the containing basic block
            // if this is not going to the start of an existing basic block
            Some(Target::Relative(rel)) => {
                add_split(&mut cfg.blocks, addr.wrapping_offset(t.len()).wrapping_offset(rel));
            },
            Some(Target::Absolute(dest)) => {
                add_split(&mut cfg.blocks, dest);
            }
            Some(Target::Multiple(_targets)) => {
                /*
                for target in targets {
                    match target {
                        Target::Relative(rel) => {
                            add_split(&mut cfg.blocks, addr.wrapping_offset(t.len()).wrapping_offset(rel));
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
                */
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
    let mut block_iter = cfg.blocks.values();

    let mut curr_block = block_iter.next().expect("basic blocks should span all instructions.");
    // addr should be increasing, so we *will* reach the end of this block eventually..
    for (addr, t) in ts {
        if addr == &curr_block.end {
            let effect = t.control_flow(Some(&contexts.at(addr)));
            if !effect.stop_after {
                cfg.graph.add_edge(curr_block.start, addr.wrapping_offset(t.len()), ());
            }
            match effect.dest {
                Some(Target::Relative(rel)) => {
                    cfg.graph.add_edge(curr_block.start, addr.wrapping_offset(t.len()).wrapping_offset(rel), ());
                },
                Some(Target::Absolute(dest)) => {
                    cfg.graph.add_edge(curr_block.start, dest, ());
                },
                Some(Target::Multiple(_targets)) => {
                    /*
                    for target in targets {
                        match target {
                            Target::Relative(rel) => {
                                cfg.graph.add_edge(curr_block.start, addr.wrapping_offset(t.len()).wrapping_offset(rel), ());
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
                    */
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

use analyses::Value;
use analyses::ValueRes;
use analyses::control_flow;

pub struct ControlFlowAnalysis<A: Address + Debug> {
    pub(crate) effect: control_flow::Effect<A>,
}

impl <A: Address + Debug> ControlFlowAnalysis<A> {
    pub fn new() -> Self {
        Self {
            effect: control_flow::Effect::cont(),
        }
    }

    pub fn into_effect(self) -> control_flow::Effect<A> {
        self.effect
    }
}

impl <A: Address + Debug> From<AddressDiff<A>> for control_flow::Effect<A> {
    fn from(item: AddressDiff<A>) -> Self {
        control_flow::Effect::cont_and(
            control_flow::Target::Relative(item)
        )
    }
}

pub trait ToAddrDiff: yaxpeax_arch::AddressDiffAmount {
    fn translate_offset(from: u64) -> AddressDiff<Self>;
}

impl ToAddrDiff for u64 {
    fn translate_offset(from: u64) -> AddressDiff<Self> {
        AddressDiff::from_const(from)
    }
}

impl ToAddrDiff for u32 {
    fn translate_offset(from: u64) -> AddressDiff<Self> {
        AddressDiff::from_const(from as u32)
    }
}

impl <A: Address + ToAddrDiff + Debug> Value for control_flow::Effect<A> {
    fn unknown() -> Self {
        control_flow::Effect::stop()
    }

    fn from_const(c: u64) -> Self {
        control_flow::Effect::stop_and(
            control_flow::Target::Relative(A::translate_offset(c))
        )
    }

    fn from_set(effects: &[Self]) -> Self {
        debug_assert!(effects.len() != 0);
        let mut stop_after = true;
        let mut target: Option<Target<A>> = None;

        for effect in effects {
            stop_after &= effect.is_stop();

            let merged_target = match (target, effect.dest.as_ref()) {
                (None, None) => {
                    None
                }
                (None, Some(o)) => {
                    Some(o.clone())
                }
                (Some(o), None) => {
                    Some(o)
                }
                // welllll this ought to be deduplicated...
                (Some(Target::Multiple(ref l)), Some(Target::Multiple(r))) => {
                    let mut vec = l.clone();
                    vec.extend_from_slice(&r);
                    Some(Target::Multiple(vec))
                }
                (Some(Target::Multiple(l)), Some(r)) => {
                    let mut vec = l.clone();
                    vec.push(r.clone());
                    Some(Target::Multiple(vec))
                }
                (Some(ref l), Some(Target::Multiple(r))) => {
                    let mut vec = r.clone();
                    vec.push(l.clone());
                    Some(Target::Multiple(vec))
                }
                (Some(l), Some(r)) => {
                    if &l == r {
                        Some(l)
                    } else {
                        Some(Target::Multiple(vec![l, r.clone()]))
                    }
                }
            };
            target = merged_target;
        }

        Effect {
            stop_after,
            dest: target,
        }
    }

    fn to_const(&self) -> Option<u64> {
        None
    }

    #[inline(always)]
    fn add(&self, other: &Self) -> ValueRes<Self> {
        if (self.stop_after == true && self.dest.is_none()) ||
            (other.stop_after == true && other.dest.is_none()) {

            return ValueRes::literal(Self::unknown());
        }

        match (self.dest.as_ref(), other.dest.as_ref()) {
            (None, Some(control_flow::Target::Relative(rel))) |
            (Some(control_flow::Target::Relative(rel)), None) => {
                ValueRes::literal(control_flow::Effect {
                    stop_after: self.stop_after || other.stop_after,
                    dest: Some(control_flow::Target::Relative(*rel))
                })
            },
            (Some(control_flow::Target::Relative(l)), Some(control_flow::Target::Relative(r))) => {
                ValueRes::literal(control_flow::Effect {
                    stop_after: self.stop_after || other.stop_after,
                    dest: Some(control_flow::Target::Relative(
                        A::zero().wrapping_offset(*l).wrapping_offset(*r).diff(&A::zero()).unwrap_or_else(|| unsafe { std::hint::unreachable_unchecked() }) //.expect("can compute diff")
                    ))
                })
            }
            _ => {
                unsafe {
                    std::hint::unreachable_unchecked();
                    // panic!("bad add: {:?} + {:?}", self, other);
                }
            }
        }
    }
}

macro_rules! impl_control_flow {
    /*
    ($semantic:expr, $arch:ty, $inst_ty:ty, ) => {
        impl_control_flow!($semantic, $arch, |inst| { None }, );
    };
    */
    ($semantic:expr, $arch:ty, $inst_ty:ty, $fixup:expr, ) => {
        impl <T> $crate::analyses::control_flow::Determinant<T, <$arch as yaxpeax_arch::Arch>::Address> for $inst_ty {
            fn control_flow(&self, _ctx: Option<&T>) -> control_flow::Effect<<$arch as yaxpeax_arch::Arch>::Address> {
                let fixup: fn(&$inst_ty) -> Option<control_flow::Effect<<$arch as yaxpeax_arch::Arch>::Address>> = $fixup;
                if let Some(effect) = fixup(self) {
                    return effect;
                }
                let mut instr_control_flow = $crate::analyses::control_flow::ControlFlowAnalysis::new();
                $semantic(self, &mut instr_control_flow);
                instr_control_flow.into_effect()
            }
        }
    };
}
