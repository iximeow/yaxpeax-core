use std::hash::Hash;
use std::rc::Rc;

use termion::color;

use SyntaxedSSARender;
use yaxpeax_arch::{Arch, ColorSettings, LengthedInstruction, ShowContextual, YaxColors};
use arch::display::BaseDisplay;
use arch::CommentQuery;
use arch::Function;
use arch::FunctionQuery;
use arch::FunctionRepr;
use arch::InstructionSpan;
use arch::msp430;
use arch::msp430::syntaxed_render;
use arch::msp430::{PartialInstructionContext};
use yaxpeax_msp430::{Instruction, Opcode, Operand, Width, MSP430, NoContext};
use analyses::control_flow::ControlFlowGraph;
use analyses::static_single_assignment::SSA;
use std::collections::HashMap;
use memory::MemoryRange;
use data::{Direction, ValueLocations};
use std::fmt;

impl <T> SyntaxedSSARender<MSP430, T, Function> for yaxpeax_msp430::Instruction where T: msp430::PartialInstructionContext {
    fn render_with_ssa_values(
        &self,
        address: <MSP430 as Arch>::Address,
        _colors: Option<&ColorSettings>,
        context: Option<&T>,
        _function_table: &HashMap<<MSP430 as Arch>::Address, Function>,
        ssa: &SSA<MSP430>) -> String {

        fn render_operand<T: PartialInstructionContext>(address: <MSP430 as Arch>::Address, operand: &Operand, context: Option<&T>, ssa: &SSA<MSP430>, direction: Direction) -> String {
            fn signed_hex(num: i16) -> String {
                if num >= 0 {
                    format!("+{:#x}", num)
                } else {
                    format!("-{:#x}", -num)
                }
            }
            fn register_name(num: u8) -> &'static str {
                match num {
                    0 => "pc",
                    1 => "sp",
                    2 => "sr",
                    3 => "cg",
                     4 => "r4",   5 => "r5",   6 => "r6",   7 => "r7",
                     8 => "r8",   9 => "r9",  10 => "r10", 11 => "r11",
                    12 => "r12", 13 => "r13", 14 => "r14", 15 => "r15",
                    _ => unreachable!()
                }
            }

            fn numbered_register_name<T: PartialInstructionContext>(address: <MSP430 as Arch>::Address, reg: u8, _context: Option<&T>, ssa: &SSA<MSP430>, direction: Direction) -> String {
                format!("{}_{}",
                    register_name(reg),
                    match ssa.get_value(address, msp430::Location::Register(reg), direction) {
                        Some(data) => data.borrow().version().map(|v| v.to_string()).unwrap_or("input".to_string()),
                        None => format!("ERR_{:?}", direction)
                    }
                )
            }

            match operand {
                Operand::Register(reg) => { numbered_register_name(address, *reg, context, ssa, direction) },
                Operand::Indexed(reg, offset) => {
                    format!("{}({})", signed_hex(*offset as i16), numbered_register_name(address, *reg, context, ssa, Direction::Read))
                },
                Operand::RegisterIndirect(reg) => {
                    format!("@{}", numbered_register_name(address, *reg, context, ssa, Direction::Read))
                },
                Operand::IndirectAutoinc(reg) => {
                    format!("@{}+", numbered_register_name(address, *reg, context, ssa, Direction::Read))
                },
                Operand::Offset(offset) => {
                    match context.and_then(|ctx| ctx.address()) {
                        Some(address) => {
                            // TODO: Uhhhh.. is this supposed to be instr len, not 2?
                            format!("{:#x}", address.wrapping_add((*offset as u16).wrapping_mul(2)).wrapping_add(2))
                        },
                        None => {
                            format!("{}(pc)", signed_hex(*offset as i16))
                        }
                    }
                },
                Operand::Symbolic(offset) => {
                    match context.and_then(|ctx| ctx.address()) {
                        Some(address) => {
                            format!("{:#x}", address.wrapping_add(*offset))
                        },
                        None => {
                            format!("{}(pc)", signed_hex(*offset as i16))
                        }
                    }
                },
                Operand::Immediate(imm) => {
                    format!("#{:#x}", imm)
                },
                Operand::Absolute(offset) => {
                    format!("&{:#x}", offset)
                },
                Operand::Const4 => {
                    "4".to_owned()
                },
                Operand::Const8 => {
                    "8".to_owned()
                },
                Operand::Const0 => {
                    "0".to_owned()
                },
                Operand::Const1 => {
                    "1".to_owned()
                },
                Operand::Const2 => {
                    "2".to_owned()
                },
                Operand::ConstNeg1 => {
                    "-1".to_owned()
                },
                Operand::Nothing => {
                    "<No Operand>".to_owned()
                }
            }
        }

        // try to recover some of the "emulated" instructions... fall back with a naive render
        match self {
            Instruction { opcode: Opcode::MOV, operands: [Operand::Const0, Operand::Const0], op_width: _ } => {
                format!("{}{}{}", color::Fg(color::Blue), "nop", color::Fg(color::Reset))
            },
            Instruction { opcode: Opcode::MOV, operands: [Operand::Const0, dest], op_width: _ } => {
                let start_color = syntaxed_render::opcode_color(Opcode::MOV);
                format!("{}{}{} {}", start_color, "clr", color::Fg(color::Reset), render_operand(address, &dest, context, ssa, Direction::Write))
            },
            Instruction { opcode: Opcode::MOV, operands: [Operand::IndirectAutoinc(1), Operand::Register(0)], op_width: Width::W } => {
                // this is a pop
                let start_color = syntaxed_render::opcode_color(Opcode::CALL);
                format!("{}{}{}", start_color, "ret", color::Fg(color::Reset))
            },
            Instruction { opcode: Opcode::MOV, operands: [Operand::IndirectAutoinc(1), dest], op_width: Width::W } => {
                // this is a pop
                let start_color = syntaxed_render::opcode_color(Opcode::PUSH);
                format!("{}{}{} {}", start_color, "pop", color::Fg(color::Reset), render_operand(address, &dest, context, ssa, Direction::Write))
            },
            Instruction { opcode: Opcode::MOV, operands: [src, Operand::Register(0)], op_width: Width::W } => {
                // br [src]
                let start_color = syntaxed_render::opcode_color(Opcode::JMP);
                format!("{}{}{} {}", start_color, "br", color::Fg(color::Reset), render_operand(address, &src, context, ssa, Direction::Read))
            }
            _ => {
                let start_color = syntaxed_render::opcode_color(self.opcode);
                let mut result = format!("{}{}{}{}", start_color, self.opcode, match self.op_width {
                    Width::W => "",
                    Width::B => ".b"
                }, color::Fg(color::Reset));

                let (rw0, rw1) = match self.opcode {
                    Opcode::Invalid(_) => { (Direction::Read, Direction::Read) },
                    Opcode::CALL => { (Direction::Read, Direction::Read) },
                    Opcode::RETI => { (Direction::Read, Direction::Read) },
                    Opcode::JNE => { (Direction::Read, Direction::Read) },
                    Opcode::JEQ => { (Direction::Read, Direction::Read) },
                    Opcode::JNC => { (Direction::Read, Direction::Read) },
                    Opcode::JC => { (Direction::Read, Direction::Read) },
                    Opcode::JN => { (Direction::Read, Direction::Read) },
                    Opcode::JGE => { (Direction::Read, Direction::Read) },
                    Opcode::JL => { (Direction::Read, Direction::Read) },
                    Opcode::JMP => { (Direction::Read, Direction::Read) },
                    Opcode::MOV => { (Direction::Read, Direction::Write) },
                    Opcode::RRA => { (Direction::Read, Direction::Read) },
                    Opcode::SXT => { (Direction::Read, Direction::Read) },
                    Opcode::PUSH => { (Direction::Read, Direction::Read) },
                    Opcode::AND => { (Direction::Read, Direction::Read) },
                    Opcode::XOR => { (Direction::Read, Direction::Read) },
                    Opcode::BIT => { (Direction::Read, Direction::Read) },
                    Opcode::BIC => { (Direction::Read, Direction::Read) },
                    Opcode::RRC => { (Direction::Read, Direction::Read) },
                    Opcode::SWPB => { (Direction::Read, Direction::Read) },
                    Opcode::BIS => { (Direction::Read, Direction::Read) },
                    Opcode::ADD => { (Direction::Read, Direction::Read) },
                    Opcode::ADDC => { (Direction::Read, Direction::Read) },
                    Opcode::SUBC => { (Direction::Read, Direction::Read) },
                    Opcode::SUB => { (Direction::Read, Direction::Read) },
                    Opcode::DADD => { (Direction::Read, Direction::Read) },
                    Opcode::CMP => { (Direction::Read, Direction::Read) }
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
                result
            }
        }
    }
}

impl <T: std::fmt::Write, C: fmt::Display, Y: YaxColors<C>> ShowContextual<u16, msp430::MergedContextTable, C, T, Y> for Instruction {
    fn contextualize(&self, colors: &Y, address: <MSP430 as Arch>::Address, ctx: Option<&msp430::MergedContextTable>, out: &mut T) -> std::fmt::Result {
        let _ctxs: [Option<String>; 3] = [None, None, None];
        self.contextualize(colors, address, ctx, out)
    }
}
impl <F: FunctionRepr, T> BaseDisplay<Function, T> for MSP430 where T: FunctionQuery<<MSP430 as Arch>::Address, Function=F> + CommentQuery<<MSP430 as Arch>::Address> {
    fn render_frame<Data: Iterator<Item=u8> + ?Sized, W: fmt::Write>(
        dest: &mut W,
        addr: u16,
        _instr: &<MSP430 as Arch>::Instruction,
        bytes: &mut Data,
        ctx: Option<&T>,
    ) -> fmt::Result {
        if let Some(ctx) = ctx {
            if let Some(comment) = ctx.comment_for(addr) {
                writeln!(dest, "{:04x}: {}{}{}",
                    addr,
                    color::Fg(&color::Blue as &dyn color::Color),
                    comment,
                    color::Fg(&color::Reset as &dyn color::Color)
                )?;
            }
            if let Some(fn_dec) = ctx.function_at(addr) {
                writeln!(dest, "      {}{}{}",
                    color::Fg(&color::LightYellow as &dyn color::Color),
                    // TODO: show values?
                    fn_dec.decl_string(false),
                    color::Fg(&color::Reset as &dyn color::Color)
                )?;
            }
        }
        write!(
            dest,
//            "{:04x}: {}{} {}{} {}{}: |{}|",
            "{:04x}: {}{} {}{} {}{}: | |",
            addr,
            bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
            bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
            bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
            bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
            bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
            bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
            // ctx.map(|c| c.indicator_tag()).unwrap_or(" ")
        )
    }
}

pub fn render_instruction_with_ssa_values<T>(
    address: <MSP430 as Arch>::Address,
    instr: &<MSP430 as Arch>::Instruction,
    colors: Option<&ColorSettings>,
    ctx: Option<&T>,
    function_table: &HashMap<u16, Function>,
    ssa: &SSA<MSP430>
) where
    T: msp430::PartialInstructionContext,
    <MSP430 as ValueLocations>::Location: Eq + Hash,
    <MSP430 as Arch>::Address: Eq + Hash,
    <MSP430 as Arch>::Instruction: SyntaxedSSARender<MSP430, T, Function> {
    println!(" {}", instr.render_with_ssa_values(address, colors, ctx, &function_table, ssa))
}

pub fn show_linear_with_blocks<M: MemoryRange<<MSP430 as Arch>::Address>>(
    _data: &M,
    _ctx: &msp430::MergedContextTable, //HashMap<<MSP430 as Arch>::Address, msp430::PartialContext>,
    cfg: &ControlFlowGraph<<MSP430 as Arch>::Address>,
    start_addr: <MSP430 as Arch>::Address,
    end_addr: <MSP430 as Arch>::Address,
    _function_table: &HashMap<<MSP430 as Arch>::Address, Function>,
    _colors: Option<&ColorSettings>) {
    let continuation = start_addr;
    while continuation < end_addr {
        // Do we have a block here?
        let block = cfg.get_block(continuation);
        // now, get_block doesn't consult if it's something we've explored
        // in the cfg or just free unused space, so let's check that...
        if cfg.graph.contains_node(block.start) {
        }

        let _end = if block.end < end_addr {
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
        unimplemented!("\
        arch::display::show_linear(data, ctx, continuation, end, function_table, colors);\
        ");

        // and continue on right after this block
        /*
         * TODO: these handle continuting on or not to the next basic block
         *
        if block.end == 0xffff {
            break;
        }
        continuation = block.end + <MSP430 as Arch>::Address::from(1u16);
        */
    }
}

pub fn show_functions<M: MemoryRange<<MSP430 as Arch>::Address>>(
    _data: &[u8],
    _user_infos: &HashMap<<MSP430 as Arch>::Address, msp430::PartialContext>,
    _cfg: &ControlFlowGraph<<MSP430 as Arch>::Address>,
    _addr: <MSP430 as Arch>::Address) {

}

pub fn show_function_by_ssa<M: MemoryRange<<MSP430 as Arch>::Address>>(
    data: &M,
    user_infos: &HashMap<<MSP430 as Arch>::Address, Rc<msp430::PartialContext>>,
    ssa: &SSA<MSP430>,
    cfg: &ControlFlowGraph<<MSP430 as Arch>::Address>,
    addr: <MSP430 as Arch>::Address,
    _colors: Option<&ColorSettings>
    ) {

    let fn_graph = cfg.get_function::<Function>(addr, &HashMap::new());

    let mut blocks: Vec<<MSP430 as Arch>::Address> = fn_graph.blocks.keys().cloned().collect();
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

        let mut iter = data.instructions_spanning(<MSP430 as Arch>::Decoder::default(), block.start, block.end);
//                println!("Block: {:#04x}", next);
//                println!("{:#04x}", block.start);
        while let Some((address, instr)) = iter.next() {
            let _user = user_infos.get(&address);
            let mut instr_text = String::new();
            MSP430::render_frame(
                &mut instr_text,
                address,
                instr,
                &mut data.range(address..(address + instr.len())).unwrap(),
                Option::<&msp430::MergedContextTable>::None
            ).unwrap();
            print!("{}", instr_text);
            /* TODO: figure out what's going on here...
            render_instruction_with_ssa_values(
                address,
                instr,
                colors,
                Option::<&&msp430::MergedContext>::None,
                &HashMap::new(),
                ssa
            );
            */
            if ssa.instruction_values.contains_key(&address) {
                // println!("  values: {:?}", ssa.instruction_values[&address]);
            }
           //println!("{:#04x}: {}", address, instr);
        }
//        println!("------------------------------");
    }
}

pub fn show_function<M: MemoryRange<<MSP430 as Arch>::Address>, C: fmt::Display, Y: YaxColors<C>>(
    data: &M,
    _user_infos: &HashMap<<MSP430 as Arch>::Address, Rc<msp430::PartialContext>>,
    cfg: &ControlFlowGraph<<MSP430 as Arch>::Address>,
    addr: <MSP430 as Arch>::Address,
    colors: &Y) {

    let fn_graph = cfg.get_function::<Function>(addr, &HashMap::new());

    let mut blocks: Vec<<MSP430 as Arch>::Address> = fn_graph.blocks.keys().cloned().collect();
    blocks.sort();

    for blockaddr in blocks.iter() {
        let block = cfg.get_block(*blockaddr);
        if block.start == 0x00 { continue; }
//        println!("Showing block: {:#x}-{:#x} for {:#x}", block.start, block.end, *blockaddr);
//        continue;
        let mut iter = data.instructions_spanning(<MSP430 as Arch>::Decoder::default(), block.start, block.end);
//                println!("Block: {:#04x}", next);
//                println!("{:#04x}", block.start);
        while let Some((address, instr)) = iter.next() {
            let mut instr_text = String::new();
            MSP430::render_frame(
                &mut instr_text,
                address,
                instr,
                &mut data.range(address..(address + instr.len())).unwrap(),
                Option::<&msp430::MergedContextTable>::None,
            ).unwrap();
            instr.contextualize(colors, address, Some(&NoContext), &mut instr_text).unwrap();
            println!(" {}", instr_text);
           //println!("{:#04x}: {}", address, instr);
        }
    }
}
