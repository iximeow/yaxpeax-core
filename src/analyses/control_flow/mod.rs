use arch::x86_64::MergedContextTable;
use yaxpeax_x86::x86_64;
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
use arch::x86_64::display::show_instruction;
use yaxpeax_x86::long_mode::{Opcode};



use serde::ser::SerializeStruct;
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Effect<Addr: AddressDiffAmount + Debug> {
    pub stop_after: bool,
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
        println!("get_function!");
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
                        println!("add_edge 0: {:x} {:x}", next.to_linear(), outedge.to_linear());
                        result.graph.add_edge(next, outedge, ());
                    }
                }
            }
            println!("block insert 0: {:x} : {:x}", next.to_linear(), self.get_block(next).end.to_linear());
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
            // if split_loc.to_linear() == 0xf1ee5{
            //     println!("add_split @ 0xf1ee5");
            // }
            //println!();
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

            // if last_block.start.to_linear() == 0x384fd {
            //     println!("last_block = {:x}-{:x} split @ {:x}", last_block.start.to_linear(), last_block.end.to_linear(), split_loc.to_linear())
            // }

            // ok, we have a basic block that starts before split_loc, so resize it to end at
            // `split_loc - 1`, with `[split_loc, block.end]` as the block we
            // must insert after it
            // println!("original last block: {:x} : {:x}", last_block.start.to_linear(), last_block.end.to_linear());
            let split_loc_end = last_block.end;
            let last_start = last_block.start;
            last_block.end = split_loc - AddressDiff::one();
            // println!("last block: {:x} : {:x}", last_start.to_linear(), last_block.end.to_linear());
            println!("block insert because splitting block: {:x} : {:x}", split_loc.to_linear(), split_loc_end.to_linear());
            graph.blocks.insert(split_loc, BasicBlock::new(split_loc, split_loc_end));

            let neighbors: Vec<A> = graph.graph.neighbors(last_start).into_iter().collect();
            for next in neighbors.into_iter() {
                println!("Removing destination edge because of split: last_start = {:x} split_loc = {:x} {:x} {:x} -> {:x} {:x}",  last_start.to_linear(), split_loc.to_linear(), last_start.to_linear(), next.to_linear(), split_loc.to_linear(), next.to_linear());
                graph.graph.remove_edge(last_start, next);
                println!("Adding edge to bottom block: {:x} {:x}", split_loc.to_linear(), next.to_linear());
                graph.graph.add_edge(split_loc, next, ());
            }
            if preserve_edges {
                println!("add_edge from first part of start to last part: {:x} {:x}", split_loc.to_linear(), last_start.to_linear());
                graph.graph.add_edge(last_start, split_loc, ());
            } else {
//                    graph.graph.add_node(new_block.start);
            }
            true
        }

        let mut result: SmallVec<[A; 2]> = SmallVec::new();

        let enclosing_block_start: A = self.get_block(at).start;
        
        println!("with_effect: {:x} {:?} stop_after = {:?} dest = {:?}", enclosing_block_start.to_linear(), effect, effect.stop_after, effect.dest);

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
                println!("add_edge as part of branch: {:x} {:x}", enclosing_block_start.to_linear(), dest_addr.to_linear());
                self.graph.add_edge(enclosing_block_start, dest_addr, ());
            }
        }

//        let enclosing_block_start: A = self.get_block(at).start;

        match &effect.dest {
            // Ok, have to clip the containing basic block
            // if this is not going to the start of an existing basic block
            Some(Target::Relative(rel)) => {
                let dest_addr = next.wrapping_offset(*rel);
                add_split(self, dest_addr, true);
                result.push(dest_addr);
                let enclosing_block_start: A = self.get_block(at).start;
                println!("adding relative jump edge 2: {:x} {:x}", enclosing_block_start.to_linear(), dest_addr.to_linear());
                self.graph.add_edge(enclosing_block_start, dest_addr, ());
            },
            Some(Target::Absolute(dest)) => {
                let dest_addr = *dest;
                add_split(self, dest_addr, true);
                result.push(dest_addr);
//              let enclosing_block_start: A = self.get_block(at).start;
                println!("adding absolute jump edge: {:x} {:x}", enclosing_block_start.to_linear(), dest_addr.to_linear());
                self.graph.add_edge(enclosing_block_start, dest_addr, ());
            }
            Some(Target::Multiple(_targets)) => {},
            e => {
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
            println!("with_effect 1: {:x} {:x} effect::stop", (*addr - A::Address::one()).to_linear(), (*addr).to_linear());

            cfg.with_effect(*addr - A::Address::one(), *addr, &Effect::stop());
        }
    }

    while let Some(addr) = to_explore.pop_front() {
        let dests = explore_control_flow(data, contexts, cfg, addr, on_instruction_discovered);
        
        println!("CFG Yield: {:x} -> {:?}", addr.to_linear(), dests);
        // for b in cfg.blocks(){

        //     let edges = cfg.destinations(b);
        //     println!("{:x} -> {:?}", b.to_linear(), edges);
        // }
        // if addr.to_linear() == 0xfa357{
            //println!("CFG Yield: {:x} -> {:?}", addr.to_linear(), dests);
        // }
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
                    println!("Remove edge because it is a part of a problematic block: {:x} {:x}", problem.to_linear(), start.to_linear());
                    cfg.graph.remove_edge(*problem, start);
                }
                return SmallVec::new();
            }
        };
        match A::Decoder::default().decode(range) {
            Ok(instr) => {
                let effect = {
                    let ctx = contexts.at(&addr);
                    //println!("computing control flow at {}", addr.show());
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
                        println!("with_effect because we are at end of a block: {:x} {:x} {:?}", addr.to_linear(), addr.wrapping_offset(instr.len()).to_linear(), &effect );
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
// pub fn build_global_cfgs<'a, A: Arch, U, Update, UTable>(ts: &Vec<(A::Address, A::Instruction)>, contexts: &'a UTable, _start: u16) -> ControlFlowGraph<A::Address>
//     where
//         A::Instruction: Determinant<U, A::Address> + LengthedInstruction<Unit=AddressDiff<A::Address>>,
//         A::Address: petgraph::graphmap::NodeTrait,
//         UTable: ContextRead<A, U> + ContextWrite<A, Update>
// {
//     let mut cfg = ControlFlowGraph::new();

//     // splits the basic block enclosing split_loc into two basic blocks,
//     // one ending directly before split_loc, and one starting at split_loc
//     // continuing to the end of the original basic block.
//     //
//     // If split_loc is the start of the enclosing basic block, no change
//     // is made (the former block would be nonsense spanning
//     //  [split_loc, split_loc - 1]
//     // and thus would not be added, yielding no net change.
//     fn add_split<A>(blocks: &mut BTreeMap<A, BasicBlock<A>>, split_loc: A) where A: Address {
//         // find a block before `split_loc`. either there is one, or `split_loc` is the lowest
//         // value of `A`, and there is no possible start address before it.
//         let mut iter = blocks.range_mut((Included(&A::min_value()), Included(&split_loc))).rev();

//         let last_block: &mut BasicBlock<A> = if let Some((_, prev)) = iter.next() {
//             prev
//         } else {
//             // no prior block, so split_loc should be the first address.
//             assert_eq!(split_loc.to_linear(), A::min_value().to_linear());
//             return;
//         };

//         if last_block.start == split_loc {
//             return;
//         }

//         // ok, we have a basic block that starts before split_loc, so resize it to end at
//         // `split_loc - 1`, with `[split_loc, block.end]` as the block we must insert after it
//         let split_loc_end = last_block.end;
//         last_block.end = split_loc - AddressDiff::one();
//         println!("block insert 2: {:x}", split_loc.to_linear());
//         blocks.insert(split_loc, BasicBlock::new(split_loc, split_loc_end));
//     }

//     for (addr, t) in ts {
//         let effect = t.control_flow(Some(&contexts.at(addr)));
//         if effect.stop_after {
//             add_split(&mut cfg.blocks, addr.wrapping_offset(t.len()));
//         }
//         match effect.dest {
//             // Ok, have to clip the containing basic block
//             // if this is not going to the start of an existing basic block
//             Some(Target::Relative(rel)) => {
//                 add_split(&mut cfg.blocks, addr.wrapping_offset(t.len()).wrapping_offset(rel));
//             },
//             Some(Target::Absolute(dest)) => {
//                 add_split(&mut cfg.blocks, dest);
//             }
//             Some(Target::Multiple(_targets)) => {
//             },
//             _ => {
//                 // TODO: unhandled!
//             }
//         }
//     }

//     // Add to digraph here.
//     // ssa on this?
//     // bfs to find reachable from a call?
//     // all this and more, next time..

//     {
//     let mut block_iter = cfg.blocks.values();

//     let mut curr_block = block_iter.next().expect("basic blocks should span all instructions.");
//     // addr should be increasing, so we *will* reach the end of this block eventually..
//     for (addr, t) in ts {
//         if addr == &curr_block.end {
//             let effect = t.control_flow(Some(&contexts.at(addr)));
//             if !effect.stop_after {
//                 cfg.graph.add_edge(curr_block.start, addr.wrapping_offset(t.len()), ());
//             }
//             match effect.dest {
//                 Some(Target::Relative(rel)) => {
//                     cfg.graph.add_edge(curr_block.start, addr.wrapping_offset(t.len()).wrapping_offset(rel), ());
//                 },
//                 Some(Target::Absolute(dest)) => {
//                     cfg.graph.add_edge(curr_block.start, dest, ());
//                 },
//                 Some(Target::Multiple(_targets)) => {
//                     /*
//                     for target in targets {
//                         match target {
//                             Target::Relative(rel) => {
//                                 cfg.graph.add_edge(curr_block.start, addr.wrapping_offset(t.len()).wrapping_offset(rel), ());
//                             },
//                             Target::Absolute(dest) => {
//                                 cfg.graph.add_edge(curr_block.start, dest, ());
//                             }
//                             _ => {
//                                 // TODO: handle these.
//                                 panic!("Unhandled");
//                             }
//                         }
//                     }
//                     */
//                 },
//                 _ => {
//                     // TODO: handle these cases too...
//                 }
//             }
//             curr_block = block_iter.next().expect("basic blocks should span all instructions.");
//         }
//     }
//     }

//     cfg
// }

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




#[derive(Debug, Copy, Clone)]
pub struct VW_Block {
    pub start: u64,
    pub end: u64, // inclusive!!
}

impl VW_Block {
    pub fn new(start_addr: u64, end_addr: u64) -> VW_Block {
        VW_Block {
            start: start_addr,
            end: end_addr
        }
    }

    pub fn as_str(&self) -> std::string::String{
        return format!("Block[0x{:x}:0x{:x}]", self.start, self.end);
    }
}

#[derive(Default)]
pub struct VW_CFG{
    pub entrypoint: u64,
    pub blocks: BTreeMap<u64, VW_Block>,
    pub graph: GraphMap<u64, (), petgraph::Directed>
}

impl VW_CFG{

    pub fn new(entrypoint: u64) -> VW_CFG {
        VW_CFG {
            entrypoint: entrypoint,
            blocks: BTreeMap::new(),
            graph: GraphMap::new()
        }
    }

    fn prev_block(&self, addr: u64) -> Option<VW_Block>{
        for (a,b) in self.blocks.iter(){
            if (addr > b.start) && (addr < b.end){
                return Some(*b);
                }
            }
        None
    }

    pub fn get_block(&self, addr: u64) -> &VW_Block {
        &self.blocks[&addr]
    }

    pub fn destinations(&self, addr: u64) -> Vec<u64> {
        self.graph.neighbors_directed(addr, petgraph::Direction::Outgoing).into_iter().collect()
    }

    pub fn check_integrity(&self){
        for (addr,block) in self.blocks.iter(){
            //1. check that key points to start of block
            assert!(*addr == block.start);
            //2. check that block is in the graph
            assert!(self.graph.contains_node(*addr));
            //3. check that there are no overlapping blocks
            for (a2,b2) in self.blocks.iter(){
                if addr == a2 {continue};
                let before = block.end < b2.start;
                let after = block.start > b2.end;
                if !(before || after) {
                    println!("{:?} {:?}", block.as_str(), b2.as_str());
                    assert!(false);
                }
            }
        }
        //4. check that same number of blocks and graph nodes.
        //   along with check 3, shows that block nodes and graph nodes are the
        //   same
        assert!(self.blocks.keys().len() == self.graph.node_count());
    }
}

#[derive(Default)]
pub struct VW_CFG_Builder{
    pub cfg: VW_CFG,
    pub switch_targets: HashMap<u64, std::vec::Vec<i64>>,
}

impl VW_CFG_Builder{

    pub fn new(entrypoint : u64, switch_targets: &HashMap<u64, std::vec::Vec<i64>>) -> VW_CFG_Builder {
        VW_CFG_Builder {
            cfg: VW_CFG::new(entrypoint),
            switch_targets : switch_targets.clone(),
        }
    }

    //split block_to_split at split_addr
    fn apply_split(&mut self, block_to_split: &VW_Block, split_addr: u64){
        //update block index
        self.cfg.blocks.remove(&block_to_split.start);
        let b1 = VW_Block::new(block_to_split.start , split_addr - 1);
        let b2 = VW_Block::new(split_addr, block_to_split.end);
        self.cfg.blocks.insert(block_to_split.start, b1);
        self.cfg.blocks.insert(split_addr, b2);
        
        //update graph
        self.cfg.graph.remove_node(block_to_split.start);
        self.cfg.graph.add_node(block_to_split.start);
        self.cfg.graph.add_node(split_addr);
        
        let neighbors: Vec<u64> = self.cfg.graph.neighbors(block_to_split.start).into_iter().collect();
        for next in neighbors.into_iter() {
            self.cfg.graph.remove_edge(block_to_split.start, next);
            self.cfg.graph.add_edge(split_addr, next, ());
        }
        self.cfg.graph.add_edge(block_to_split.start, split_addr, ());
        
    }

    // Split if inside block (not first or last address)
    fn maybe_apply_split(&mut self, split_addr: u64){
        let block = self.cfg.prev_block(split_addr);
        if let Some(block_to_split) = block {
            self.apply_split(&block_to_split, split_addr)
        };
    }


    fn vw_decode<M>(&self, data: &M, contexts: &MergedContextTable, start: u64) -> Option<(Vec<u64>, VW_Block)> where M: MemoryRange<u64>{
        let dsts : Vec<u64> = vec![];
        let decoder = <x86_64 as Arch>::Decoder::default();
        let mut addr = start;
        //show_instruction(data,contexts,addr,None);
        loop {
            //show_instruction(data,contexts,addr,None);
            let range = match data.range_from(addr) {
                Some(range) => range,
                None => { println!("Decoding range error"); return None; }
            };
            match decoder.decode(range) {
                Ok(instr) => {
                    match get_effect(contexts, &instr, addr) {
                        Effect { stop_after: false, dest: None } => {
                            // we can continue!
                            addr += instr.len();
                            // if we hit an existing block, stop decoding
                            if self.cfg.blocks.contains_key(&addr){
                                let b = VW_Block::new(start, addr - 1);
                                //println!("Hit already existing block: 0x{:x}", addr);
                                return Some((vec![addr],b));
                            }
                        },
                        // and for any other cases...
                        effect @ _ => {
                            //println!("Maybe branch? effect = {:?} {:x}", &effect,  addr + instr.len());
                            let dsts = self.effect_to_destinations(&effect, addr + instr.len(), &instr, addr);
                            let b = VW_Block::new(start, addr + instr.len() - 1);
                            //println!("Hit branch: {:?}", dsts);
                            return Some((dsts,b))
                        }
                    }
                },
                Err(e) => { println!("Decode error: {:?} @ {:x}", e, addr);
                panic!("Decode error!"); 
                return None; }
            }
        }
    }

    fn process_job<M>(&mut self, data: &M, contexts: &MergedContextTable, start: u64) -> Vec<u64> where M: MemoryRange<u64>{
        //if addr is inside of a block, apply split and return no destinations
        self.maybe_apply_split(start);

        let decode_result = self.vw_decode(data, contexts, start);
        //println!("Decode: {:x} -> {:?}", start, decode_result);
        if let None = decode_result{ 
            return vec![];
        }
        let (dsts,block) = decode_result.unwrap();
        self.cfg.blocks.insert(start, block);
        self.cfg.graph.add_node(start);
        for dst in dsts.clone() {
            self.cfg.graph.add_edge(start, dst, ());
            self.maybe_apply_split(dst);
        }
        dsts
    }

    //get destinations at end of block
    //next = start of next instruction
    fn effect_to_destinations(&self, effect : &Effect<u64>, next: u64, instr: &yaxpeax_x86::long_mode::Instruction, addr: u64) -> Vec<u64>{
        let mut dsts : Vec<u64> = vec![];
        if !effect.stop_after{
            dsts.push(next)
        }
        match &effect.dest {
            Some(Target::Relative(rel)) => {
                let dest_addr = next.wrapping_offset(*rel);
                dsts.push(dest_addr);
                return dsts;
            },
            Some(Target::Absolute(dest)) => {
                dsts.push(*dest);
                return dsts;
            }
            Some(Target::Multiple(_targets)) =>  return vec![],
            Some(Indeterminate) => return vec![],
            None => {
                match instr.opcode{
                    Opcode::JMP => (),
                    Opcode::RETURN | Opcode::UD2 => (),
                    _ => panic!("Unknown indirect control flow transfer {:?}", instr.opcode),
                }
                if let Opcode::JMP = instr.opcode{
                    println!("Indirect Jump = {:x}", addr);
                    if !self.switch_targets.contains_key(&addr){
                        panic!("No entry in switch_targets for the switch @ {:x}", addr);
                    }
                    let targets: Vec<u64> = self.switch_targets.get(&addr).unwrap().into_iter().map(|x| *x as u64).rev().collect();
                    println!("Indirect jumps discovered = {:?}", targets);
                    return targets;
                }
                return vec![];
            } 
        }
    }

}

fn get_effect(contexts: &MergedContextTable, instr: &yaxpeax_x86::long_mode::Instruction, addr: u64) -> Effect<u64>{
    let ctx = contexts.at(&addr);
    instr.control_flow(Some(&ctx))
}

pub fn get_cfg<M>(
    data: &M,
    contexts: &MergedContextTable,
    entrypoint: u64,
    switch_targets: &HashMap<u64, std::vec::Vec<i64>>,
) -> VW_CFG where M: MemoryRange<u64>,
{
    let mut cfg_builder = VW_CFG_Builder::new(entrypoint, switch_targets);
    let mut to_explore: VecDeque<u64> = VecDeque::new();
    let mut seen: HashSet<u64> = HashSet::new();
    to_explore.push_back(entrypoint);

    while let Some(addr) = to_explore.pop_front() {
        let dests = cfg_builder.process_job(data, contexts, addr);

        let out_addrs: Vec<std::string::String> = dests.clone().into_iter().map(|x| format!("{:x}", x)).rev().collect();
        //println!("Processed Job: {:x} -> {:?}", addr, out_addrs);
               
        for next in dests.into_iter() {
            if !seen.contains(&next) {
                to_explore.push_back(next);
                seen.insert(next);
            }
        }
    }
    cfg_builder.cfg.check_integrity();
    cfg_builder.cfg
}

