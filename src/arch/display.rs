use yaxpeax_arch::{Arch, Address, AddressDisplay, Decodable, LengthedInstruction};
use analyses::control_flow::{BasicBlock, ControlFlowGraph, Determinant};
use std::collections::HashMap;
use ContextTable;
use arch::InstructionSpan;

pub trait BaseDisplay<F, U> where Self: Arch, Self::Address: std::hash::Hash + petgraph::graphmap::NodeTrait {
    fn render_frame(
        addr: Self::Address,
        instr: &Self::Instruction,
        bytes: &[u8],
        ctx: Option<&U>,
        function_table: &HashMap<Self::Address, F>
    ) -> ();

    fn render_instruction(
        instr: &Self::Instruction,
        ctx: Option<&U>,
        function_table: &HashMap<Self::Address, F>
    ) -> ();
}

pub fn show_block<A: Arch + BaseDisplay<F, U>, U, F, Update, Contexts: ContextTable<A, U, Update>>(
    data: &[u8],
    ctx: &Contexts,
    function_table: &HashMap<A::Address, F>,
    cfg: &ControlFlowGraph<A::Address>,
    block: &BasicBlock<A::Address>
) where A::Address: std::hash::Hash + petgraph::graphmap::NodeTrait, A::Instruction: Determinant<U, A::Address> {
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
            &data[(address.to_linear() as usize)..(address.to_linear() as usize + instr.len().to_linear() as usize)],
            Some(&ctx.at(&address)),
            function_table
        );
        A::render_instruction(
            instr,
            Some(&ctx.at(&address)),
            function_table
        );
        use analyses::control_flow::Determinant;
        println!("Control flow: {:?}", instr.control_flow(Some(&ctx.at(&address))));
    }
}

pub fn show_linear<A: Arch + BaseDisplay<F, U>, U, F, Update, Contexts: ContextTable<A, U, Update>>(
    data: &[u8],
    ctx: &Contexts,
    start_addr: A::Address,
    end_addr: A::Address,
    function_table: &HashMap<A::Address, F>
) where
    A::Address: std::hash::Hash + petgraph::graphmap::NodeTrait,
    A::Instruction: Determinant<U, A::Address>,
    usize: From<A::Address> {
    let mut continuation = start_addr;
    while continuation < end_addr {
        let mut iter = data.instructions_spanning::<A::Instruction>(continuation, end_addr);
        loop {
            let (address, instr) = match iter.next() {
                Some((address, instr)) => {
                    (address, instr)
                },
                None => {
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
                &data[(address.into())..((address + instr.len()).into())],
                Some(&ctx.at(&address)),
                function_table
            );
            A::render_instruction(
                instr,
                Some(&ctx.at(&address)),
                function_table
            );
            continuation += instr.len();
        }
    }
}
