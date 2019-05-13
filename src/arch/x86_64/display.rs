use std::collections::HashMap;
use std::hash::Hash;
use std::fmt::Display;
use std::rc::Rc;

use termion::color;

use num_traits::Zero;

use SyntaxedRender;
use SyntaxedSSARender;
use memory::MemoryRepr;
use yaxpeax_arch::{ColorSettings, Colorize, Colored};
use yaxpeax_arch::{AddressDisplay, Arch, Decodable, LengthedInstruction, ShowContextual, YaxColors};
use yaxpeax_arch::display::*;
use arch;
use arch::display::BaseDisplay;
use arch::InstructionSpan;
use arch::x86_64;
use arch::x86_64::{ContextRead, MergedContextTable};
use arch::x86_64::analyses::data_flow::Location;
use yaxpeax_x86::{Instruction, Opcode, Operand};
use yaxpeax_x86::{RegSpec, RegisterBank};
use yaxpeax_x86::x86_64 as x86_64Arch;
use analyses::control_flow;
use analyses::control_flow::{BasicBlock, ControlFlowGraph, Determinant};
use analyses::static_single_assignment::cytron::{Direction, SSA};

use memory::MemoryRange;

use std::fmt;

impl <T> BaseDisplay<x86_64::Function, T> for x86_64Arch {
    fn render_frame<Data: Iterator<Item=u8>>(
        addr: <x86_64Arch as Arch>::Address,
        instr: &<x86_64Arch as Arch>::Instruction,
        bytes: &mut Data,
        ctx: Option<&T>,
        function_table: &HashMap<<x86_64Arch as Arch>::Address, x86_64::Function>
    ) {
        /*
        if let Some(comment) = ctx.and_then(|x| x.comment()) {
            println!("{:04x}: {}{}{}",
                addr,
                color::Fg(&color::Blue as &color::Color),
                comment,
                color::Fg(&color::Reset as &color::Color)
            );
        }
        */
        if let Some(fn_dec) = function_table.get(&addr) {
            println!("      {}{}{}",
                color::Fg(&color::LightYellow as &color::Color),
                "<function>",
                //fn_dec.decl_string(),
                color::Fg(&color::Reset as &color::Color)
            );
        }
        print!("{:08x}: ", addr);
        for i in 0..16 {
            if i < instr.length {
                match bytes.next() {
                    Some(b) => {
                        print!("{:02x}", b);
                    },
                    None => { print!("  "); }
                }
            } else {
                print!("  ");
            }
        }
        print!(": | |");
    }
}

pub struct InstructionContext<'a, 'b, 'c, 'd> {
    instr: &'a Instruction,
    addr: <x86_64Arch as Arch>::Address,
    contexts: Option<&'b MergedContextTable>,
    ssa: Option<&'c SSA<x86_64Arch>>,
    colors: Option<&'d ColorSettings>
}

pub enum Use {
    Read,
    Write,
    ReadWrite
}


impl <'a, 'b, 'c, 'd> Display for InstructionContext<'a, 'b, 'c, 'd> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fn colorize_i8(num: i8, colors: Option<&ColorSettings>) -> String {
            match num {
                0 => format!("{}", colors.zero("0")),
                1 => format!("{}", colors.one("1")),
                -1 => format!("{}", colors.minus_one("-1")),
                _ => format!("{}", colors.number(format!("{:#x}", num)))
            }
        }

        fn colorize_u8(num: u8, colors: Option<&ColorSettings>) -> String {
            match num {
                0 => format!("{}", colors.zero("0")),
                1 => format!("{}", colors.one("1")),
                0xff => format!("{}", colors.minus_one("0xff")),
                _ => format!("{}", colors.number(format!("{:#x}", num)))
            }
        }

        fn colorize_i16(num: i16, colors: Option<&ColorSettings>) -> String {
            match num {
                0 => format!("{}", colors.zero("0")),
                1 => format!("{}", colors.one("1")),
                -1 => format!("{}", colors.minus_one("-1")),
                _ => format!("{}", colors.number(format!("{:#x}", num)))
            }
        }

        fn colorize_u16(num: u16, colors: Option<&ColorSettings>) -> String {
            match num {
                0 => format!("{}", colors.zero("0")),
                1 => format!("{}", colors.one("1")),
                0xffff => format!("{}", colors.minus_one("0xffff")),
                _ => format!("{}", colors.number(format!("{:#x}", num)))
            }
        }

        fn colorize_i32(num: i32, colors: Option<&ColorSettings>) -> String {
            match num {
                0 => format!("{}", colors.zero("0")),
                1 => format!("{}", colors.one("1")),
                -1 => format!("{}", colors.minus_one("-1")),
                _ => format!("{}", colors.number(format!("{:#x}", num)))
            }
        }

        fn colorize_u32(num: u32, colors: Option<&ColorSettings>) -> String {
            match num {
                0 => format!("{}", colors.zero("0")),
                1 => format!("{}", colors.one("1")),
                0xffffffff => format!("{}", colors.minus_one("0xffffffff")),
                _ => format!("{}", colors.number(format!("{:#x}", num)))
            }
        }

        fn colorize_i64(num: i64, colors: Option<&ColorSettings>) -> String {
            match num {
                0 => format!("{}", colors.zero("0")),
                1 => format!("{}", colors.one("1")),
                -1 => format!("{}", colors.minus_one("-1")),
                _ => format!("{}", colors.number(format!("{:#x}", num)))
            }
        }

        fn colorize_u64(num: u64, colors: Option<&ColorSettings>) -> String {
            match num {
                0 => format!("{}", colors.zero("0")),
                1 => format!("{}", colors.one("1")),
                0xffffffff_ffffffff => format!("{}", colors.minus_one("0xffffffffffffffff")),
                _ => format!("{}", colors.number(format!("{:#x}", num)))
            }
        }

        fn render_function<'a, 'b, 'c, 'd>(address: <x86_64Arch as Arch>::Address, ctx: &InstructionContext<'a, 'b, 'c, 'd>) -> Option<Colored<String>> {
            ctx.contexts
                .and_then(|ctx| ctx.functions.get(&address).map(|f| f.name.to_owned()))
                .or_else(|| { ctx.contexts.and_then(|ctx| ctx.symbols.get(&address).map(|sym| sym.to_string())) })
                .map(|name| { ctx.colors.function(name) })
        }

        fn contextualize_operand<'a, 'b, 'c, 'd>(op: &Operand, ctx: &InstructionContext<'a, 'b, 'c, 'd>, usage: Use, fmt: &mut fmt::Formatter) -> fmt::Result {
            fn numbered_register_name<'a, 'b, 'c, 'd>(address: <x86_64Arch as Arch>::Address, reg: RegSpec, context: &InstructionContext<'a, 'b, 'c, 'd>, direction: Direction) -> Colored<String> {
                let text = context.ssa.map(|ssa| {
                    let num = ssa.values.get(&address)
                        .and_then(|addr_values| addr_values.get(&(Location::Register(reg), direction)))
                        .map(|data| data.borrow().version());
                    format!("{}_{}",
                        reg,
                        num.map(|n| n.to_string()).unwrap_or_else(|| {
                            format!("ERR_{:?}", direction)
                        })
                    )
                }).unwrap_or_else(|| { reg.to_string() });

                context.colors.register(text)
            }

            match op {
                Operand::ImmediateI8(i) => {
                    write!(fmt, "{}", colorize_i8(*i, ctx.colors))
                },
                Operand::ImmediateU8(i) => {
                    write!(fmt, "{}", colorize_u8(*i, ctx.colors))
                },
                Operand::ImmediateI16(i) => {
                    write!(fmt, "{}", colorize_i16(*i, ctx.colors))
                },
                Operand::ImmediateU16(i) => {
                    write!(fmt, "{}", colorize_u16(*i, ctx.colors))
                },
                Operand::ImmediateI32(i) => {
                    write!(fmt, "{}", colorize_i32(*i, ctx.colors))
                },
                Operand::ImmediateU32(i) => {
                    write!(fmt, "{}", colorize_u32(*i, ctx.colors))
                },
                Operand::ImmediateI64(i) => {
                    write!(fmt, "{}", colorize_i64(*i, ctx.colors))
                },
                Operand::ImmediateU64(i) => {
                    write!(fmt, "{}", colorize_u64(*i, ctx.colors))
                },
                Operand::Register(spec) => {
                    match usage {
                        Use::Read => {
                            write!(fmt, "{}",
                                numbered_register_name(ctx.addr, *spec, &ctx, Direction::Read)
                            )
                        },
                        Use::Write => {
                            write!(fmt, "{}",
                                numbered_register_name(ctx.addr, *spec, &ctx, Direction::Write)
                            )
                        },
                        Use::ReadWrite => {
                            if ctx.ssa.is_some() {
                                write!(fmt, "{} (-> {})",
                                    numbered_register_name(ctx.addr, *spec, &ctx, Direction::Read),
                                    numbered_register_name(ctx.addr, *spec, &ctx, Direction::Write)
                                )
                            } else {
                                write!(fmt, "{}", numbered_register_name(ctx.addr, *spec, &ctx, Direction::Write))
                            }
                        }
                    }
                },
                Operand::DisplacementU32(disp) => {
                    write!(fmt, "[{}]", ctx.colors.address((*disp as u64).stringy()))
                },
                Operand::DisplacementU64(disp) => {
                    write!(fmt, "[{}]", ctx.colors.address((*disp as u64).stringy()))
                },
                Operand::RegDeref(spec) => {
                    write!(fmt, "[{}]",
                        numbered_register_name(ctx.addr, *spec, &ctx, Direction::Read)
                    )
                }
                Operand::RegDisp(RegSpec { bank: RegisterBank::RIP, num: _ }, disp) => {
                    let addr = ctx.addr.wrapping_add(*disp as i64 as u64).wrapping_add(ctx.instr.len());
                    let text = ctx.contexts
                        .and_then(|ctx| ctx.symbols.get(&addr))
                        .map(|sym| { ctx.colors.symbol(format!("&{}", sym)) })
                        .unwrap_or_else(|| { ctx.colors.address(addr.stringy()) });
                    write!(fmt, "[{}]", text)
                }
                Operand::RegDisp(spec, disp) => {
                    write!(fmt, "[{} {}]",
                        numbered_register_name(ctx.addr, *spec, &ctx, Direction::Read),
                        format_number_i32(*disp, NumberStyleHint::HexSignedWithSignSplit)
                    )
                },
                Operand::RegScale(spec, scale) => {
                    write!(fmt, "[{} * {}]",
                        numbered_register_name(ctx.addr, *spec, &ctx, Direction::Read),
                        scale
                    )
                },
                Operand::RegScaleDisp(spec, scale, disp) => {
                    write!(fmt, "[{} * {} {}]",
                        numbered_register_name(ctx.addr, *spec, &ctx, Direction::Read),
                        scale,
                        format_number_i32(*disp, NumberStyleHint::HexSignedWithSignSplit)
                    )
                },
                Operand::RegIndexBase(base, index) => {
                    write!(fmt, "[{} + {}]",
                        numbered_register_name(ctx.addr, *base, &ctx, Direction::Read),
                        numbered_register_name(ctx.addr, *index, &ctx, Direction::Read)
                    )
                },
                Operand::RegIndexBaseDisp(base, index, disp) => {
                    write!(fmt, "[{} + {} {}]",
                        numbered_register_name(ctx.addr, *base, &ctx, Direction::Read),
                        numbered_register_name(ctx.addr, *index, &ctx, Direction::Read),
                        format_number_i32(*disp, NumberStyleHint::HexSignedWithSignSplit)
                    )
                }
                Operand::RegIndexBaseScale(base, index, scale) => {
                    write!(fmt, "[{} + {} * {}]",
                        numbered_register_name(ctx.addr, *base, &ctx, Direction::Read),
                        numbered_register_name(ctx.addr, *index, &ctx, Direction::Read),
                        scale
                    )
                }
                Operand::RegIndexBaseScaleDisp(base, index, scale, disp) => {
                    write!(fmt, "[{} + {} * {} {}]",
                        numbered_register_name(ctx.addr, *base, &ctx, Direction::Read),
                        numbered_register_name(ctx.addr, *index, &ctx, Direction::Read),
                        scale,
                        format_number_i32(*disp, NumberStyleHint::HexSignedWithSignSplit)
                    )
                }
                Operand::Nothing => {
                    Ok(())
                },
                Operand::Many(_) => {
                    panic!("asdf");
                }
            }
        }
        /*
         * if self.ssa.is_some() {
         *     ... ...
         * }
         */
        self.instr.opcode.colorize(self.colors, fmt)?;

        match self.instr.opcode {
            Opcode::CALL |
            Opcode::JMP |
            Opcode::JO |
            Opcode::JNO |
            Opcode::JB |
            Opcode::JNB |
            Opcode::JZ |
            Opcode::JNZ |
            Opcode::JA |
            Opcode::JNA |
            Opcode::JS |
            Opcode::JNS |
            Opcode::JP |
            Opcode::JNP |
            Opcode::JL |
            Opcode::JGE |
            Opcode::JLE |
            Opcode::JG => {
                match &self.instr.operands[0] {
                    Operand::ImmediateI8(i) => {
                        let addr = (*i as i64 as u64).wrapping_add(self.instr.len()).wrapping_add(self.addr);
                        let text = self.contexts.and_then(|context| {
                            render_function(addr, self)
                        }).unwrap_or_else(|| { self.colors.address(addr.stringy()) });
                        return write!(fmt, " {}", text);
                    },
                    Operand::ImmediateI16(i) => {
                        let addr = (*i as i64 as u64).wrapping_add(self.instr.len()).wrapping_add(self.addr);
                        let text = self.contexts.and_then(|context| {
                            render_function(addr, self)
                        }).unwrap_or_else(|| { self.colors.address(addr.stringy()) });
                        return write!(fmt, " {}", text);
                    },
                    Operand::ImmediateI32(i) => {
                        let addr = (*i as i64 as u64).wrapping_add(self.instr.len()).wrapping_add(self.addr);
                        let text = self.contexts.and_then(|context| {
                            render_function(addr, self)
                        }).unwrap_or_else(|| { self.colors.address(addr.stringy()) });
                        return write!(fmt, " {}", text);
                    },
                    Operand::ImmediateI64(i) => {
                        let addr = (*i as u64).wrapping_add(self.instr.len()).wrapping_add(self.addr);
                        let text = self.contexts.and_then(|context| {
                            render_function(addr, self)
                        }).unwrap_or_else(|| { self.colors.address(addr.stringy()) });
                        return write!(fmt, " {}", text);
                    },
                    Operand::RegDisp(RegSpec { bank: RegisterBank::RIP, num: _ }, disp) => {
                        let addr = self.addr.wrapping_add(*disp as i64 as u64).wrapping_add(self.instr.len());
                        let text = self.contexts.and_then(|context| {
                            context.symbols.get(&addr)
                                .map(|sym| { self.colors.function(sym).to_string() })
                        })
                            .unwrap_or_else(|| colorize_u64(addr, self.colors));
                        return write!(fmt, " [{}]", text);
                    }
                    op @ _ => {
                        write!(fmt, " ")?;
                        return contextualize_operand(op, self, Use::Read, fmt);
                    }
                }
            },
            Opcode::LEA |
            Opcode::MOVZX_b |
            Opcode::MOVZX_w |
            Opcode::MOVSX |
            Opcode::MOV => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], self, Use::Write, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands[1], self, Use::Read, fmt)
            }
            Opcode::XCHG => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], self, Use::ReadWrite, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands[1], self, Use::ReadWrite, fmt)
            }
            Opcode::SAR |
            Opcode::SAL |
            Opcode::SHR |
            Opcode::SHL |
            Opcode::RCR |
            Opcode::RCL |
            Opcode::ROR |
            Opcode::ROL |
            Opcode::ADC |
            Opcode::SBB |
            Opcode::ADD |
            Opcode::SUB |
            Opcode::AND |
            Opcode::XOR |
            Opcode::OR |
            Opcode::CMOVA |
            Opcode::CMOVB |
            Opcode::CMOVG |
            Opcode::CMOVGE |
            Opcode::CMOVL |
            Opcode::CMOVLE |
            Opcode::CMOVNA |
            Opcode::CMOVNB |
            Opcode::CMOVNO |
            Opcode::CMOVNP |
            Opcode::CMOVNS |
            Opcode::CMOVNZ |
            Opcode::CMOVO |
            Opcode::CMOVP |
            Opcode::CMOVS |
            Opcode::CMOVZ => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], self, Use::ReadWrite, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands[1], self, Use::Read, fmt)
            }
            Opcode::CMP |
            Opcode::TEST => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], self, Use::Read, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands[1], self, Use::Read, fmt)
            }
            Opcode::CMPXCHG => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], self, Use::Read, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands[1], self, Use::ReadWrite, fmt)
            }
            Opcode::LSL => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], self, Use::Write, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands[1], self, Use::Write, fmt)
            }
            Opcode::LAR => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], self, Use::Write, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands[1], self, Use::Read, fmt)
            }
            Opcode::SETO |
            Opcode::SETNO |
            Opcode::SETB |
            Opcode::SETAE |
            Opcode::SETZ |
            Opcode::SETNZ |
            Opcode::SETBE |
            Opcode::SETA |
            Opcode::SETS |
            Opcode::SETNS |
            Opcode::SETP |
            Opcode::SETNP |
            Opcode::SETL |
            Opcode::SETGE |
            Opcode::SETLE |
            Opcode::SETG => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], self, Use::Read, fmt)
            }
            Opcode::NOP |
            Opcode::RETURN => {
                match &self.instr.operands[0] {
                    Operand::Nothing => { return Ok(()); },
                    op @ _ => {
                        write!(fmt, " ");
                        contextualize_operand(&self.instr.operands[0], self, Use::Read, fmt)
                   }
                }
            }
            Opcode::INC |
            Opcode::DEC |
            Opcode::NEG |
            Opcode::NOT => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], self, Use::ReadWrite, fmt)
            }
            Opcode::CALLF | // TODO: this is wrong.
            Opcode::JMPF | // TODO: this is wrong.
            Opcode::PUSH => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], self, Use::Read, fmt)
            }
            Opcode::POP => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], self, Use::Write, fmt)
            }
            Opcode::RETF | // TODO: this is wrong.
            Opcode::CMPS |
            Opcode::SCAS |
            Opcode::MOVS |
            Opcode::LODS |
            Opcode::STOS |
            Opcode::JMPE |
            Opcode::INS |
            Opcode::OUTS => {
                // well, these don't have any *explicit* operands...
                Ok(())
            },
            Opcode::INVLPG |
            Opcode::FXSAVE |
            Opcode::FXRSTOR |
            Opcode::STMXCSR |
            Opcode::XSAVE |
            Opcode::XSTOR |
            Opcode::XSAVEOPT |
            Opcode::SMSW |
            Opcode::SLDT |
            Opcode::STR |
            Opcode::SGDT |
            Opcode::SIDT => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], self, Use::Write, fmt)
            }
            Opcode::VERR |
            Opcode::VERW |
            Opcode::LDMXCSR |
            Opcode::LMSW |
            Opcode::LLDT |
            Opcode::LTR |
            Opcode::LGDT |
            Opcode::LIDT => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], self, Use::Read, fmt)
            }
            Opcode::RDMSR |
            Opcode::WRMSR |
            Opcode::RDTSC |
            Opcode::RDPMC |
            Opcode::LFENCE |
            Opcode::MFENCE |
            Opcode::SFENCE |
            Opcode::CLFLUSH |
            Opcode::SWAPGS |
            Opcode::RDTSCP |
            Opcode::ENTER |
            Opcode::LEAVE |
            Opcode::PUSHF |
            Opcode::POPF |
            Opcode::CBW |
            Opcode::CDW |
            Opcode::LAHF |
            Opcode::SAHF |
            Opcode::IRET |
            Opcode::INTO |
            Opcode::INT |
            Opcode::SYSCALL |
            Opcode::SYSRET |
            Opcode::CPUID |
            Opcode::WBINVD |
            Opcode::INVD |
            Opcode::CLTS |
            Opcode::UD2 |
            Opcode::HLT |
            Opcode::WAIT |
            Opcode::CLC |
            Opcode::STC |
            Opcode::CLI |
            Opcode::STI |
            Opcode::CLD |
            Opcode::STD |
            Opcode::Invalid => {
                Ok(())
            }
            Opcode::MUL |
            Opcode::IMUL |
            Opcode::DIV |
            Opcode::IDIV => {
                unreachable!();
            }
        }
    }
}

pub fn show_block<M: MemoryRange<<x86_64Arch as Arch>::Address>>(
    data: &M,
    ctx: &MergedContextTable,
    ssa: Option<&SSA<x86_64Arch>>,
    cfg: &ControlFlowGraph<<x86_64Arch as Arch>::Address>,
    block: &BasicBlock<<x86_64Arch as Arch>::Address>,
    colors: Option<&ColorSettings>
) {
    println!("Basic block --\n  start: {}\n  end:   {}", block.start.stringy(), block.end.stringy());
    println!("  next:");
    for neighbor in cfg.graph.neighbors(block.start) {
        println!("    {}", neighbor.stringy());
    }
    let mut iter = data.instructions_spanning::<<x86_64Arch as Arch>::Instruction>(block.start, block.end);
    while let Some((address, instr)) = iter.next() {
        x86_64Arch::render_frame(
            address,
            instr,
            &mut data.range(address..(address + instr.len())).unwrap(),
            Some(&ctx.at(&address)),
            &ctx.functions
        );
        println!(" {}", InstructionContext {
            instr: &instr,
            addr: address,
            contexts: Some(ctx),
            ssa: ssa,
            colors: colors
        });
        use analyses::control_flow::Determinant;
        println!("Control flow: {:?}", instr.control_flow(Some(&ctx.at(&address))));
    }
}

pub fn show_instruction<M: MemoryRange<<x86_64Arch as Arch>::Address>>(
    data: &M,
    ctx: &MergedContextTable,
    address: <x86_64Arch as Arch>::Address,
    colors: Option<&ColorSettings>
) {
    match <x86_64Arch as Arch>::Instruction::decode(data.range_from(address).unwrap()) {
        Some(instr) => {
            x86_64Arch::render_frame(
                address,
                &instr,
                &mut data.range(address..(address + instr.len())).unwrap(),
                Some(&ctx.at(&address)),
                &ctx.functions
            );
            println!(" {}", InstructionContext {
                instr: &instr,
                addr: address,
                contexts: Some(ctx),
                ssa: None,
                colors: colors
            });
        },
        None => {
            println!("Decode error at {}", address);
        }
    };
}

pub fn show_linear<M: MemoryRange<<x86_64Arch as Arch>::Address>>(
    data: &M,
    ctx: &MergedContextTable,
    start_addr: <x86_64Arch as Arch>::Address,
    end_addr: <x86_64Arch as Arch>::Address,
    colors: Option<&ColorSettings>
) {
    let mut continuation = start_addr;
    while continuation < end_addr {
        let mut iter = data.instructions_spanning::<<x86_64Arch as Arch>::Instruction>(continuation, end_addr);
        loop {
            let (address, instr) = match iter.next() {
                Some((address, instr)) => {
                    (address, instr)
                },
                None => {
                    println!("Decode error for data starting at {}, byte: {:#02x}", continuation.stringy(), data.read(continuation).unwrap());

                    continuation += <x86_64Arch as Arch>::Instruction::min_size();
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

            x86_64Arch::render_frame(
                address,
                instr,
                &mut data.range(address..(address + instr.len())).unwrap(),
                Some(&ctx.at(&address)),
                &ctx.functions
            );
            println!(" {}", InstructionContext {
                instr: &instr,
                addr: address,
                contexts: Some(ctx),
                ssa: None,
                colors: colors
            });
            continuation += instr.len();
        }
    }
}

pub fn show_function<M: MemoryRepr<<x86_64Arch as Arch>::Address> + MemoryRange<<x86_64Arch as Arch>::Address>>(
    data: &M,
    ctx: &MergedContextTable,
    ssa: Option<&SSA<x86_64Arch>>,
    cfg: &ControlFlowGraph<<x86_64Arch as Arch>::Address>,
    addr: <x86_64Arch as Arch>::Address,
    colors: Option<&ColorSettings>
) {
    let fn_graph = cfg.get_function(addr, &ctx.functions);

    let mut blocks: Vec<<x86_64Arch as Arch>::Address> = fn_graph.blocks.iter().map(|x| x.start).collect();
    blocks.sort();

    for blockaddr in blocks.iter() {
        let block = cfg.get_block(*blockaddr);
//        println!("  -- block: {} --", blockaddr.stringy());
        if block.start == <x86_64Arch as Arch>::Address::zero() { continue; }
//        println!("Showing block: {:#x}-{:#x} for {:#x}", block.start, block.end, *blockaddr);
//        continue;
        let mut iter = data.instructions_spanning::<<x86_64Arch as Arch>::Instruction>(block.start, block.end);
//                println!("Block: {:#04x}", next);
//                println!("{:#04x}", block.start);
        let end = iter.end;
        while let Some((address, instr)) = iter.next() {
            x86_64Arch::render_frame(
                address,
                instr,
                &mut data.range(address..(address + instr.len())).unwrap(),
                Some(&ctx.at(&address)),
                &ctx.functions
            );
            println!(" {}", InstructionContext {
                instr: &instr,
                addr: address,
                contexts: Some(ctx),
                ssa: ssa,
                colors: colors
            });
            if address.wrapping_add(instr.len()) > end {
                println!("┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄");
            }
        }
        let next: Vec<<x86_64Arch as Arch>::Address> = cfg.destinations(*blockaddr);
        for n in next {
//            println!("  -> {}", n.stringy());
        }
//        println!("  ------------");
    }
}
