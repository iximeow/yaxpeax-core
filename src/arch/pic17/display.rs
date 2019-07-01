use std::collections::HashMap;
use std::hash::Hash;

use termion::color;

use SyntaxedSSARender;
use yaxpeax_arch::{Arch, ColorSettings, LengthedInstruction};
use arch;
use arch::display::BaseDisplay;
use arch::InstructionSpan;
use arch::pic17;
use arch::pic17::{ContextRead, PartialInstructionContext};
use yaxpeax_pic17::{Opcode, Operand, PIC17};
use analyses::control_flow;
use analyses::control_flow::{ControlFlowGraph, Determinant};
use memory::MemoryRange;
use data::ValueLocations;
use data::Direction;

use analyses::static_single_assignment::SSA;

// TODO: should this whole thing be deleted lol
impl <T> SyntaxedSSARender<PIC17, T, pic17::Function> for yaxpeax_pic17::Instruction where T: pic17::PartialInstructionContext {
    fn render_with_ssa_values(
        &self,
        address: <PIC17 as Arch>::Address,
        _colors: Option<&ColorSettings>,
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

        fn render_operand<T: PartialInstructionContext>(_address: <PIC17 as Arch>::Address, operand: &Operand, context: Option<&T>, _ssa: &SSA<PIC17>, _direction: Direction) -> String {
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

        let start_color = yaxpeax_pic17::opcode_color(self.opcode);
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
    fn render_frame<Data: Iterator<Item=u8>>(
        addr: u16,
        _instr: &<PIC17 as Arch>::Instruction,
        bytes: &mut Data,
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
            bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
            bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
            ctx.map(|c| c.indicator_tag()).unwrap_or("   ".to_owned())
        );
    }
}

pub fn render_instruction_with_ssa_values<T>(
    address: <PIC17 as Arch>::Address,
    instr: &<PIC17 as Arch>::Instruction,
    colors: Option<&ColorSettings>,
    ctx: Option<&T>,
    function_table: &HashMap<u16, pic17::Function>,
    ssa: &SSA<PIC17>
) where
    T: pic17::PartialInstructionContext,
    <PIC17 as ValueLocations>::Location: Eq + Hash,
    <PIC17 as Arch>::Address: Eq + Hash,
    <PIC17 as Arch>::Instruction: SyntaxedSSARender<PIC17, T, pic17::Function> {
    println!(" {}", instr.render_with_ssa_values(address, colors, ctx, function_table, ssa))
}

pub fn show_linear_with_blocks<M: MemoryRange<<PIC17 as Arch>::Address>>(
    data: &M,
    ctx: &pic17::MergedContextTable,
    function_table: &HashMap<<PIC17 as Arch>::Address, pic17::Function>,
    cfg: &ControlFlowGraph<<PIC17 as Arch>::Address>,
    start_addr: <PIC17 as Arch>::Address,
    end_addr: <PIC17 as Arch>::Address,
    colors: Option<&ColorSettings>) {
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
        arch::display::show_linear(data, ctx, continuation, end, function_table, colors);

        // and continue on right after this block
        continuation = block.end + <PIC17 as Arch>::Address::from(1u16);
    }
}

pub fn show_functions(
    _data: &[u8],
    _ctx: &pic17::MergedContextTable,
    _cfg: &ControlFlowGraph<<PIC17 as Arch>::Address>,
    _addr: <PIC17 as Arch>::Address) {

}

pub fn show_function_by_ssa<M: MemoryRange<<PIC17 as Arch>::Address>>(
    data: &M,
    colors: Option<&ColorSettings>,
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
            PIC17::render_frame(
                address,
                instr,
                &mut data.range(address..(address + instr.len())).unwrap(),
                Some(&ctx.at(&address)),
                function_table
            );
            render_instruction_with_ssa_values(
                address,
                instr,
                colors,
                Some(&ctx.at(&address)),
                function_table,
                ssa
            );
            if ssa.instruction_values.contains_key(&address) {
                // println!("  values: {:?}", ssa.instruction_values[&address]);
            }
           //println!("{:#04x}: {}", address, instr);
        }
//        println!("------------------------------");
    }
}
