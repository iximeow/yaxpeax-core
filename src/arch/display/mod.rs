use yaxpeax_arch::{Arch, AddressDisplay, ColorSettings, Decodable, LengthedInstruction, ShowContextual};
use analyses::control_flow::{BasicBlock, ControlFlowGraph, Determinant};
use std::collections::HashMap;
use arch::InstructionSpan;
use memory::{MemoryRepr, MemoryRange};
use num_traits::Zero;

use std::fmt;

pub mod function;

pub trait BaseDisplay<F, U> where
    Self: Arch,
    Self::Address: std::hash::Hash + petgraph::graphmap::NodeTrait {
    fn render_frame<Data: Iterator<Item=u8>, W: fmt::Write>(
        dest: &mut W,
        addr: Self::Address,
        instr: &Self::Instruction,
        bytes: &mut Data,
        ctx: Option<&U>,
    ) -> fmt::Result;
}

pub fn show_block<M: MemoryRange<A::Address>, A: Arch + BaseDisplay<F, Contexts>, F, Contexts>(
    data: &M,
    ctx: &Contexts,
    _function_table: &HashMap<A::Address, F>,
    cfg: &ControlFlowGraph<A::Address>,
    block: &BasicBlock<A::Address>,
    colors: Option<&ColorSettings>
) where 
    A::Address: std::hash::Hash + petgraph::graphmap::NodeTrait,
    A::Instruction: Determinant<Contexts, A::Address> + ShowContextual<A::Address, Contexts, String> {
    println!("Basic block --\n  start: {}\n  end:   {}", block.start.stringy(), block.end.stringy());
    println!("  next:");
    for neighbor in cfg.graph.neighbors(block.start) {
        println!("    {}", neighbor.stringy());
    }
    let mut iter = data.instructions_spanning::<A::Instruction>(block.start, block.end);
    while let Some((address, instr)) = iter.next() {
        let mut instr_text = String::new();
        A::render_frame(
            &mut instr_text,
            address,
            instr,
            &mut data.range(address..(address + instr.len())).unwrap(),
            Some(ctx),
        ).unwrap();
        instr.contextualize(colors, address, Some(ctx), &mut instr_text).unwrap();
        println!(" {}", instr_text);
        println!("Control flow: {:?}", instr.control_flow(Some(&ctx)));
    }
}

pub fn show_instruction<M: MemoryRange<A::Address>, A: Arch + BaseDisplay<F, Contexts>, F, Contexts>(
    data: &M,
    ctx: &Contexts,
    address: A::Address,
    _function_table: &HashMap<A::Address, F>,
    colors: Option<&ColorSettings>
) where
    A::Address: std::hash::Hash + petgraph::graphmap::NodeTrait,
    A::Instruction: ShowContextual<A::Address, Contexts, String> {
    match A::Instruction::decode(data.range_from(address).unwrap()) {
        Some(instr) => {
            let mut instr_text = String::new();
            A::render_frame(
                &mut instr_text,
                address,
                &instr,
                &mut data.range(address..(address + instr.len())).unwrap(),
                Some(ctx),
            ).unwrap();
            instr.contextualize(colors, address, Some(ctx), &mut instr_text).unwrap();
            println!(" {}", instr_text);
        },
        None => {
            println!("Decode error at {}", address);
        }
    };
}

pub fn show_linear<M: MemoryRange<A::Address>, A: Arch + BaseDisplay<F, Contexts>, F, Contexts>(
    data: &M,
    ctx: &Contexts,
    start_addr: A::Address,
    end_addr: A::Address,
    _function_table: &HashMap<A::Address, F>,
    colors: Option<&ColorSettings>
) -> Vec<(A::Address, Vec<String>)> where
    A::Address: std::hash::Hash + petgraph::graphmap::NodeTrait,
    A::Instruction: ShowContextual<A::Address, Contexts, String> {
    let mut result: Vec<(A::Address, Vec<String>)> = Vec::new();
    let mut continuation = start_addr;
    while continuation < end_addr {
        let mut iter = data.instructions_spanning::<A::Instruction>(continuation, end_addr);
        loop {
            let (address, instr) = match iter.next() {
                Some((address, instr)) => {
                    (address, instr)
                },
                None => {
                    result.push((
                        continuation,
                        vec![format!("Decode error for data starting at {}, byte: {:#02x}", continuation.stringy(), data.read(continuation).unwrap())]
                    ));

                    continuation += A::Instruction::min_size();
                        /*
                        opcode: Opcode::Invalid(
                            (data[(continuation as usize)] as u16) |
                            ((data[(continuation as usize) + 1] as u16) << 8)
                        ),
                        */
                    break; // ... the iterator doesn't distinguish
                           // between None and Invalid ...
                }
            };

            let mut instr_text = "".to_string();
            A::render_frame(
                &mut instr_text,
                address,
                instr,
                &mut data.range(address..(address + instr.len())).unwrap(),
                Some(ctx),
            ).unwrap();
            instr_text.push(' ');
            instr.contextualize(colors, address, Some(ctx), &mut instr_text).unwrap();
            result.push((
                address,
                instr_text.split("\n").map(|s| s.to_string()).collect()
            ));
            continuation += instr.len();
        }
    }
    result
}

pub fn show_function<M: MemoryRepr<A::Address> + MemoryRange<A::Address>, A: Arch + BaseDisplay<F, Contexts>, F, Contexts>(
    data: &M,
    ctx: &Contexts,
    function_table: &HashMap<A::Address, F>,
    cfg: &ControlFlowGraph<A::Address>,
    addr: A::Address,
    colors: Option<&ColorSettings>
) where
    A::Address: std::hash::Hash + petgraph::graphmap::NodeTrait,
    A::Instruction: ShowContextual<A::Address, Contexts, String> {

    let fn_graph = cfg.get_function(addr, function_table);

    let mut blocks: Vec<A::Address> = fn_graph.blocks.iter().map(|x| x.start).collect();
    blocks.sort();

    for blockaddr in blocks.iter() {
        let block = cfg.get_block(*blockaddr);
        println!("  -- block: {} --", blockaddr.stringy());
        if block.start == A::Address::zero() { continue; }
//        println!("Showing block: {:#x}-{:#x} for {:#x}", block.start, block.end, *blockaddr);
//        continue;
        let mut iter = data.instructions_spanning::<A::Instruction>(block.start, block.end);
//                println!("Block: {:#04x}", next);
//                println!("{:#04x}", block.start);
        while let Some((address, instr)) = iter.next() {
            let mut instr_text = String::new();
            A::render_frame(
                &mut instr_text,
                address,
                instr,
                &mut data.range(address..(address + instr.len())).unwrap(),
                Some(ctx),
            ).unwrap();
            instr.contextualize(colors, address, Some(ctx), &mut instr_text).unwrap();
            println!(" {}", instr_text);
        }
        let next: Vec<A::Address> = cfg.destinations(*blockaddr);
        for n in next {
            println!("  -> {}", n.stringy());
        }
        println!("  ------------");
    }
}
