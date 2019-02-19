use std::collections::HashMap;
use std::hash::Hash;

use termion::color;

use SyntaxedRender;
use SyntaxedSSARender;
use yaxpeax_arch::{Arch, LengthedInstruction};
use arch;
use arch::display::BaseDisplay;
use arch::InstructionSpan;
use arch::pic17;
use arch::pic17::{ContextRead, PartialInstructionContext};
use yaxpeax_pic17::{Instruction, Opcode, Operand, PIC17};
use analyses::control_flow;
use analyses::control_flow::{BasicBlock, ControlFlowGraph, Determinant};

use analyses::static_single_assignment::cytron::SSA;

impl <T> SyntaxedSSARender<PIC17, T, pic17::Function> for yaxpeax_pic17::Instruction where T: pic17::PartialInstructionContext {
    fn render_with_ssa_values(
        &self,
        address: <PIC17 as Arch>::Address,
        context: Option<&T>,
        function_table: &HashMap<<PIC17 as Arch>::Address, pic17::Function>,
        ssa: &SSA<PIC17>) -> String {

        fn render_function(address: <PIC17 as Arch>::Address, function_table: &HashMap<<PIC17 as Arch>::Address, pic17::Function>) -> String {
            match function_table.get(&address) {
                Some(fn_dec) => {
                    format!("{}{}{}",
                        color::Fg(&color::LightYellow as &color::Color),
                        fn_dec.decl_string(),
                        color::Fg(&color::Reset as &color::Color)
                    )
                },
                None => { format!("#{:08x}", address) }
            }
        }

        use analyses::static_single_assignment::cytron::Direction;
        fn render_operand<T: PartialInstructionContext>(address: <PIC17 as Arch>::Address, operand: &Operand, context: Option<&T>, ssa: &SSA<PIC17>, direction: Direction) -> String {
            fn signed_hex(num: i16) -> String {
                if num >= 0 {
                    format!("+{:#x}", num)
                } else {
                    format!("-{:#x}", -num)
                }
            }
            fn register_name(num: u8) -> String {
                format!("{}", num)
            }

            fn numbered_register_name<T: PartialInstructionContext>(address: <PIC17 as Arch>::Address, reg: u8, context: Option<&T>, ssa: &SSA<PIC17>, direction: Direction) -> String {
                format!("{}_ERR_UNIMPLEMENTED", //{}",
                    register_name(reg)
                    /*
                    match ssa.values.get(&address).and_then(|addr_values| addr_values.get(&(pic17::Location::Register(reg), direction))) {
                        Some(data) => format!("{}", data.borrow().version()),
                        None => format!("ERR_{:?}", direction)
                    }
                    */
                )
            }

            match operand {
                Operand::ImmediateU8(i) => {
                    format!("#{:02x}", i)
                },
                Operand::ImmediateU32(i) => {
                    format!("#{:08x}", i)
                },
                Operand::File(f) => {
                    let name = match ::arch::pic17::cpu::try_debank(*f, context) {
                        Some(num) => ::arch::pic17::named_file(num).to_string(),
                        None => format!("[banked 0x{:02x}]", f)
                    };
                    format!("{}{}{}",
                        color::Fg(color::Yellow),
                        name,
                        color::Fg(color::Reset)
                    )
                },
                Operand::W => {
                    format!("{}W{}",
                        color::Fg(color::Yellow),
                        color::Fg(color::Reset)
                    )
                },
                _ => { "".to_owned() }
            }
        }

        let start_color = pic17::syntaxed_render::opcode_color(self.opcode);
        let mut result = format!("{}{}{}",
             start_color,
             self.opcode,
             color::Fg(color::Reset)
         );

        match self.opcode {
            Opcode::LCALL => {
                result.push_str(" ");

                let control_flow = self.control_flow(context);
                match control_flow.dest {
                    Some(control_flow::Target::Absolute(addr)) => {
                        result.push_str(&render_function(addr * 2, function_table));
                    }
                    Some(control_flow::Target::Indeterminate) => {
                        match self.operands[0] {
                            Operand::ImmediateU8(i) => {
                                // TODO: distinguish from pclath==0 lcall and pclath==??? lcall
                                result.push_str(&format!("#{:08x}", (i as u16) * 2));
                            },
                            // LCALL's first operand is always a u8
                            _ => { unreachable!(); }
                        }
                    },
                    // LCALL always has control flow, and either it's
                    // an absolute destination, or unknown (contextual)
                    _ => { unreachable!(); }
                }
            },
            Opcode::CALL |
            Opcode::GOTO => {
                match self.operands[0] {
                    Operand::ImmediateU32(i) => {
                        result.push(' ');
                        result.push_str(&render_function((i as u16) * 2, function_table));
                    },
                    _ => {
                        unreachable!()
                    }
                };
            },
            _ => {
                let (rw0, rw1) = match self.opcode {
                    _ => { (Direction::Read, Direction::Read) }
                };
                match self.operands[0] {
                    Operand::Nothing => { return result; },
                    x @ _ => {
                        result.push(' ');
                        result.push_str(&render_operand(address, &x, context, ssa, rw0));
                    }
                };
                match self.operands[1] {
                    Operand::Nothing => { return result; },
                    x @ _ => {
                        result.push(',');
                        result.push(' ');
                        result.push_str(&render_operand(address, &x, context, ssa, rw1));
                    }
                };
            }
        };
        result
    }
}

impl <T> BaseDisplay<pic17::Function, T> for PIC17 where T: pic17::PartialInstructionContext {
    fn render_frame(
        addr: u16,
        instr: &<PIC17 as Arch>::Instruction,
        bytes: &[u8],
        ctx: Option<&T>,
        function_table: &HashMap<<PIC17 as Arch>::Address, pic17::Function>
    ) where T: pic17::PartialInstructionContext {
        if let Some(comment) = ctx.and_then(|x| x.comment()) {
            println!("{:04x}: {}{}{}",
                addr,
                color::Fg(&color::Blue as &color::Color),
                comment,
                color::Fg(&color::Reset as &color::Color)
            );
        }
        if let Some(fn_dec) = function_table.get(&addr) {
            println!("      {}{}{}",
                color::Fg(&color::LightYellow as &color::Color),
                fn_dec.decl_string(),
                color::Fg(&color::Reset as &color::Color)
            );
        }
        print!(
            "{:04x}: {}{}: |{}|",
            addr,
            bytes.get(0).map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
            bytes.get(1).map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
            ctx.map(|c| c.indicator_tag()).unwrap_or("   ".to_owned())
        );
    }

    fn render_instruction(
        instr: &<PIC17 as Arch>::Instruction,
        ctx: Option<&T>,
        function_table: &HashMap<<PIC17 as Arch>::Address, pic17::Function>
    ) where T: pic17::PartialInstructionContext {
        println!(" {}", instr.render(ctx, &function_table))
    }
}

use analyses::static_single_assignment::cytron::SSAValues;
pub fn render_instruction_with_ssa_values<T>(
    address: <PIC17 as Arch>::Address,
    instr: &<PIC17 as Arch>::Instruction,
    ctx: Option<&T>,
    function_table: &HashMap<u16, pic17::Function>,
    ssa: &SSA<PIC17>
) where
    T: pic17::PartialInstructionContext,
    <PIC17 as SSAValues>::Location: Eq + Hash,
    <PIC17 as Arch>::Address: Eq + Hash,
    <PIC17 as Arch>::Instruction: SyntaxedSSARender<PIC17, T, pic17::Function> {
    println!(" {}", instr.render_with_ssa_values(address, ctx, function_table, ssa))
}

pub fn show_linear_with_blocks(
    data: &[u8],
    ctx: &pic17::MergedContextTable,
    function_table: &HashMap<<PIC17 as Arch>::Address, pic17::Function>,
    cfg: &ControlFlowGraph<<PIC17 as Arch>::Address>,
    start_addr: <PIC17 as Arch>::Address,
    end_addr: <PIC17 as Arch>::Address) {
    let mut continuation = start_addr;
    while continuation < end_addr {
        // Do we have a block here?
        let block = cfg.get_block(continuation);
        // now, get_block doesn't consult if it's something we've explored
        // in the cfg or just free unused space, so let's check that...
        if cfg.graph.contains_node(block.start) {
        }

        let end = if block.end < end_addr {
            block.end
        } else {
            end_addr
        };
        // haha actually we don't do anything one way or the other.
        // so just use the end of this block as an indication of where
        // to stop linear disassembly here
        //
        // start at continuation because this linear disassembly
        // might start at the middle of a preexisting block
        arch::display::show_linear(data, &ctx, continuation, end, function_table);

        // and continue on right after this block
        continuation = block.end + <PIC17 as Arch>::Address::from(1u16);
    }
}

pub fn show_functions(
    data: &[u8],
    ctx: &pic17::MergedContextTable,
    cfg: &ControlFlowGraph<<PIC17 as Arch>::Address>,
    addr: <PIC17 as Arch>::Address) {

}

pub fn show_function_by_ssa(
    data: &[u8],
    ctx: &pic17::MergedContextTable,
    function_table: &HashMap<<PIC17 as Arch>::Address, pic17::Function>,
    cfg: &ControlFlowGraph<<PIC17 as Arch>::Address>,
    addr: <PIC17 as Arch>::Address,
    ssa: &SSA<PIC17>) {

    let fn_graph = cfg.get_function(addr, function_table);

    let mut blocks: Vec<<PIC17 as Arch>::Address> = fn_graph.blocks.iter().map(|x| x.start).collect();
    blocks.sort();

    for blockaddr in blocks.iter() {
        let block = cfg.get_block(*blockaddr);
        if block.start == 0x00 { continue; }
        /*
        println!("Basic block --\n  start: {:#x}\n  end:   {:#x}", block.start, block.end);
        println!("  next:");
        for neighbor in cfg.graph.neighbors(block.start) {
            println!("    {:#x}", neighbor);
        }
        */

        if ssa.phi.contains_key(&block.start) {
            println!("Phi: {:?}", ssa.phi[&block.start].keys());
        }

        let mut iter = data.instructions_spanning::<yaxpeax_pic17::Instruction>(block.start, block.end);
//                println!("Block: {:#04x}", next);
//                println!("{:#04x}", block.start);
        while let Some((address, instr)) = iter.next() {
            let mut computed = pic17::ComputedContext::new();
            PIC17::render_frame(
                address,
                instr,
                &data[(address as usize)..(address as usize + instr.len() as usize)],
                Some(&ctx.at(&address)),
                function_table
            );
            render_instruction_with_ssa_values(
                address,
                instr,
                Some(&ctx.at(&address)),
                function_table,
                ssa
            );
            if ssa.values.contains_key(&address) {
                // println!("  values: {:?}", ssa.values[&address]);
            }
           //println!("{:#04x}: {}", address, instr);
        }
//        println!("------------------------------");
    }
}

pub fn show_function(
    data: &[u8],
    ctx: &pic17::MergedContextTable,
    function_table: &HashMap<<PIC17 as Arch>::Address, pic17::Function>,
    cfg: &ControlFlowGraph<<PIC17 as Arch>::Address>,
    addr: <PIC17 as Arch>::Address) {

    let fn_graph = cfg.get_function(addr, function_table);

    let mut blocks: Vec<<PIC17 as Arch>::Address> = fn_graph.blocks.iter().map(|x| x.start).collect();
    blocks.sort();

    for blockaddr in blocks.iter() {
        let block = cfg.get_block(*blockaddr);
        if block.start == 0x00 { continue; }
//        println!("Showing block: {:#x}-{:#x} for {:#x}", block.start, block.end, *blockaddr);
//        continue;
        let mut iter = data.instructions_spanning::<yaxpeax_pic17::Instruction>(block.start, block.end);
//                println!("Block: {:#04x}", next);
//                println!("{:#04x}", block.start);
        while let Some((address, instr)) = iter.next() {
            let mut computed = pic17::ComputedContext::new();
            PIC17::render_frame(
                address,
                instr,
                &data[(address as usize)..(address as usize + instr.len() as usize)],
                Some(&ctx.at(&address)),
                function_table
            );
            PIC17::render_instruction(
                instr,
                Some(&ctx.at(&address)),
                function_table
            );
           //println!("{:#04x}: {}", address, instr);
        }
    }
}
