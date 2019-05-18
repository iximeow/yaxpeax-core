use yaxpeax_arch::{Arch, AddressDisplay, ColorSettings, Decodable, LengthedInstruction, ShowContextual};
use analyses::control_flow::{BasicBlock, ControlFlowGraph, Determinant};
use std::collections::HashMap;
use ContextRead;
use arch::InstructionSpan;
use memory::{MemoryRepr, MemoryRange};
use num_traits::Zero;

pub trait BaseDisplay<F, U> where
    Self: Arch,
    Self::Address: std::hash::Hash + petgraph::graphmap::NodeTrait {
    fn render_frame<Data: Iterator<Item=u8>>(
        addr: Self::Address,
        instr: &Self::Instruction,
        bytes: &mut Data,
        ctx: Option<&U>,
        function_table: &HashMap<Self::Address, F>
    ) -> ();
}

pub fn show_block<M: MemoryRange<A::Address>, A: Arch + BaseDisplay<F, U>, U, F, Contexts>(
    data: &M,
    ctx: &Contexts,
    function_table: &HashMap<A::Address, F>,
    cfg: &ControlFlowGraph<A::Address>,
    block: &BasicBlock<A::Address>,
    colors: Option<&ColorSettings>
) where 
    A::Address: std::hash::Hash + petgraph::graphmap::NodeTrait,
    A::Instruction: Determinant<U, A::Address> + ShowContextual<A::Address, Contexts, String>,
    Contexts: ContextRead<A, U> {
    println!("Basic block --\n  start: {}\n  end:   {}", block.start.stringy(), block.end.stringy());
    println!("  next:");
    for neighbor in cfg.graph.neighbors(block.start) {
        println!("    {}", neighbor.stringy());
    }
    let mut iter = data.instructions_spanning::<A::Instruction>(block.start, block.end);
    while let Some((address, instr)) = iter.next() {
        A::render_frame(
            address,
            instr,
            &mut data.range(address..(address + instr.len())).unwrap(),
            Some(&ctx.at(&address)),
            function_table
        );
        let mut instr_text = String::new();
        instr.contextualize(colors, address, Some(ctx), &mut instr_text).unwrap();
        println!(" {}", instr_text);
        use analyses::control_flow::Determinant;
        println!("Control flow: {:?}", instr.control_flow(Some(&ctx.at(&address))));
    }
}

pub fn show_instruction<M: MemoryRange<A::Address>, A: Arch + BaseDisplay<F, U>, U, F, Contexts>(
    data: &M,
    ctx: &Contexts,
    address: A::Address,
    function_table: &HashMap<A::Address, F>,
    colors: Option<&ColorSettings>
) where
    A::Address: std::hash::Hash + petgraph::graphmap::NodeTrait,
    A::Instruction: Determinant<U, A::Address> + ShowContextual<A::Address, Contexts, String>,
    Contexts: ContextRead<A, U> {
    match A::Instruction::decode(data.range_from(address).unwrap()) {
        Some(instr) => {
            A::render_frame(
                address,
                &instr,
                &mut data.range(address..(address + instr.len())).unwrap(),
                Some(&ctx.at(&address)),
                function_table
            );
            let mut instr_text = String::new();
            instr.contextualize(colors, address, Some(ctx), &mut instr_text).unwrap();
            println!(" {}", instr_text);
        },
        None => {
            println!("Decode error at {}", address);
        }
    };
}

pub fn show_linear<M: MemoryRange<A::Address>, A: Arch + BaseDisplay<F, U>, U, F, Contexts>(
    data: &M,
    ctx: &Contexts,
    start_addr: A::Address,
    end_addr: A::Address,
    function_table: &HashMap<A::Address, F>,
    colors: Option<&ColorSettings>
) where
    A::Address: std::hash::Hash + petgraph::graphmap::NodeTrait,
    A::Instruction: Determinant<U, A::Address> + ShowContextual<A::Address, Contexts, String>,
    Contexts: ContextRead<A, U> {
    let mut continuation = start_addr;
    while continuation < end_addr {
        let mut iter = data.instructions_spanning::<A::Instruction>(continuation, end_addr);
        loop {
            let (address, instr) = match iter.next() {
                Some((address, instr)) => {
                    (address, instr)
                },
                None => {
                    println!("Decode error for data starting at {}, byte: {:#02x}", continuation.stringy(), data.read(continuation).unwrap());

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

            A::render_frame(
                address,
                instr,
                &mut data.range(address..(address + instr.len())).unwrap(),
                Some(&ctx.at(&address)),
                function_table
            );
            let mut instr_text = String::new();
            instr.contextualize(colors, address, Some(ctx), &mut instr_text).unwrap();
            println!(" {}", instr_text);
            continuation += instr.len();
        }
    }
}

pub fn show_function<M: MemoryRepr<A::Address> + MemoryRange<A::Address>, A: Arch + BaseDisplay<F, U>, U, F, Contexts>(
    data: &M,
    ctx: &Contexts,
    function_table: &HashMap<A::Address, F>,
    cfg: &ControlFlowGraph<A::Address>,
    addr: A::Address,
    colors: Option<&ColorSettings>
) where
    A::Address: std::hash::Hash + petgraph::graphmap::NodeTrait,
    A::Instruction: ShowContextual<A::Address, Contexts, String>,
    Contexts: ContextRead<A, U> {

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
            A::render_frame(
                address,
                instr,
                &mut data.range(address..(address + instr.len())).unwrap(),
                Some(&ctx.at(&address)),
                function_table
            );
            let mut instr_text = String::new();
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
