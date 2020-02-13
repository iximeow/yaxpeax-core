use yaxpeax_arm::armv7::{ARMv7, Instruction, ConditionedOpcode, Operand, Opcode};
use yaxpeax_arch::{Arch, AddressDisplay, ColorSettings, Colorize, Decoder, LengthedInstruction, ShowContextual, YaxColors};

use arch::display::BaseDisplay;
use arch::arm;
use arch::arm::v7::DisplayCtx;
use arch::arm::v7::analyses::data_flow::{Data, Location};
use arch::FunctionImpl;
use arch::SymbolQuery;
use std::fmt;

use analyses::data_flow::Use;
use analyses::static_single_assignment::{DFGRef, SSA};
use analyses::control_flow::ControlFlowGraph;
use arch::AddressNamer;
use arch::CommentQuery;
use arch::FunctionQuery;
use arch::FunctionRepr;
use arch::display::function::{FunctionInstructionDisplay, FunctionView};
use arch::arm::v7::MergedContextTable;
use memory::{MemoryRange, MemoryRepr};
use data::{Direction, ValueLocations};
use display::location::{LocationHighlighter, NoHighlights, StyledDisplay};

use std::marker::PhantomData;

use termion::color;

use std::rc::Rc;

use tracing::{event, Level};

impl <F: FunctionRepr, T: FunctionQuery<<ARMv7 as Arch>::Address, Function=F> + CommentQuery<<ARMv7 as Arch>::Address>> BaseDisplay<F, T> for ARMv7 {
    fn render_frame<Data: Iterator<Item=u8> + ?Sized, W: fmt::Write>(
        dest: &mut W,
        addr: <ARMv7 as Arch>::Address,
        _instr: &<ARMv7 as Arch>::Instruction,
        bytes: &mut Data,
        ctx: Option<&T>,
    ) -> fmt::Result {
        /*
         * if there's a comment, show that
         */
        // TODO: totally replace this?
        if let Some(ctx) = ctx {
            if let Some(comment) = ctx.comment_for(addr) {
                writeln!(dest, "{:04x}: {}{}{}",
                    addr,
                    color::Fg(color::Blue),
                    comment,
                    color::Fg(color::Reset)
                ).unwrap();
            }
            if let Some(fn_dec) = ctx.function_at(addr) {
                writeln!(dest, "      {}{}{}",
                    color::Fg(&color::LightYellow as &dyn color::Color),
                    // TODO: show values?
                    fn_dec.decl_string(false),
                    color::Fg(&color::Reset as &dyn color::Color)
                ).unwrap();
            }
        }
        write!(
            dest,
//            "{:08x}: {}{}{}{}: |{}|",
            "{:08x}: {}{}{}{}: | |",
                addr,
                bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
                bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
                bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
                bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
//                ctx.map(|c| c.indicator_tag()).unwrap_or(" ")
        )
    }
}

impl <'a, T: std::fmt::Write, C: fmt::Display, Y: YaxColors<C>> ShowContextual<u32, DisplayCtx<'a>, C, T, Y> for Instruction {
    fn contextualize(&self, colors: &Y, _address: u32, _context: Option<&DisplayCtx<'a>>, out: &mut T) -> std::fmt::Result {
        self.contextualize(colors, _address, Some(&yaxpeax_arm::armv7::NoContext), out)
    }
}

pub struct InstructionContext<'a, 'b, 'c, 'd, 'e, Context: AddressNamer<<ARMv7 as Arch>::Address>, Highlighter: LocationHighlighter<<ARMv7 as ValueLocations>::Location>> {
    instr: &'a Instruction,
    addr: <ARMv7 as Arch>::Address,
    contexts: Option<&'b Context>,
    ssa: Option<&'c SSA<ARMv7>>,
    colors: Option<&'d ColorSettings>,
    highlight: &'e Highlighter,
}

impl <'a, 'b, 'c, 'd, 'e, Context: AddressNamer<<ARMv7 as Arch>::Address>, Highlighter: LocationHighlighter<<ARMv7 as ValueLocations>::Location>> InstructionContext<'a, 'b, 'c, 'd, 'e, Context, Highlighter> {
    pub fn numbered_register_name(
        &self,
        reg: u8,
        direction: Direction
    ) -> impl fmt::Display {
        let text = self.ssa.map(|ssa| {
            let num = ssa.get_value(self.addr, Location::Register(reg), direction)
                .map(|data| data.borrow().version());
            format!("{}_{}",
                reg,
                num.map(|n| n.map(|v| v.to_string()).unwrap_or("input".to_string())).unwrap_or_else(|| {
                    format!("ERR_{:?}", direction)
                })
            )
        }).unwrap_or_else(|| { reg.to_string() });

        self.colors.register(text)
    }
}

pub struct RegValueDisplay<'a, 'b, 'c, C: fmt::Display, Y: YaxColors<C>> {
    pub reg: &'a u8,
    pub value: &'b Option<DFGRef<ARMv7>>,
    pub colors: &'c Y,
    _pd: std::marker::PhantomData<C>
}

impl <'a, 'b, 'c, C: fmt::Display, Y: YaxColors<C>> fmt::Display for RegValueDisplay<'a, 'b, 'c, C, Y> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.value {
            Some(value) => {
                write!(
                    fmt,
                    "{}",
                    self.colors.register(
                        format!(
                            "r{}_{}",
                            self.reg,
                            value.borrow().version()
                                .map(|ver| ver.to_string())
                                .unwrap_or_else(|| "input".to_string())
                        )
                    )
                )?;
                if let Some(data) = value.borrow().data.as_ref() {
                    write!(fmt, " (= {})", DataDisplay { data: &data, colors: self.colors, _color: std::marker::PhantomData })?;
                }
                Ok(())
            },
            None => {
                write!(fmt, "r{}", self.reg)
            }
        }
    }
}

pub enum MemValueDisplay<'a, 'b, 'c, 'd, C: fmt::Display, Y: YaxColors<C>> {
    Address(u64),
    Reg(StyledDisplay<'a, RegValueDisplay<'b, 'c, 'd, C, Y>>),
    RegOffset(StyledDisplay<'a, RegValueDisplay<'b, 'c, 'd, C, Y>>, u64),
}

impl <'a, 'b, 'c, 'd, C: fmt::Display, Y: YaxColors<C>> fmt::Display for MemValueDisplay<'a, 'b, 'c, 'd, C, Y> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MemValueDisplay::Address(disp) => {
                write!(fmt, "[{}]", disp)
            }
            MemValueDisplay::Reg(reg) => {
                write!(fmt, "[{}]", reg)
            }
            MemValueDisplay::RegOffset(reg, disp) => {
                write!(fmt, "[{} + {:#x}]", reg, disp)
            }
        }
    }
}

pub struct DataDisplay<'a, 'b, C: fmt::Display, Y: YaxColors<C>> {
    pub data: &'a Data,
    pub colors: &'b Y,
    _color: std::marker::PhantomData<C>,
}

impl <'a, 'b, C: fmt::Display, Y: YaxColors<C>> fmt::Display for DataDisplay<'a, 'b, C, Y> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.data {
            Data::Alias(alias) => {
                if let Location::Register(alias_reg) = alias.borrow().location {
                    write!(fmt, "{}", RegValueDisplay {
                        reg: &alias_reg,
                        value: &Some(Rc::clone(alias)),
                        colors: self.colors,
                        _pd: std::marker::PhantomData,
                    })?;
                } else {
                    unreachable!("Register alias must be another register");
                }
            },
            Data::Concrete(v) => {
                write!(fmt, "{:#x}", v)?;
            }
        }
        Ok(())
    }
}

impl <
    'a, 'b, 'c, 'd, 'e,
    Context: SymbolQuery<<ARMv7 as Arch>::Address> + FunctionQuery<<ARMv7 as Arch>::Address, Function=FunctionImpl<<ARMv7 as ValueLocations>::Location>>,
    Highlighter: LocationHighlighter<<ARMv7 as ValueLocations>::Location>
> fmt::Display for InstructionContext<'a, 'b, 'c, 'd, 'e, Context, Highlighter> {
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

        fn contextualize_operand<
            'a, 'b, 'c, 'd, 'e,
            F: FunctionRepr,
            C: FunctionQuery<<ARMv7 as Arch>::Address, Function=F> + SymbolQuery<<ARMv7 as Arch>::Address>,
            Highlighter: LocationHighlighter<<ARMv7 as ValueLocations>::Location>
        >(
            op: &[Operand; 4],
            op_idx: u8,
            ctx: &InstructionContext<'a, 'b, 'c, 'd, 'e, C, Highlighter>,
            usage: Use,
            fmt: &mut fmt::Formatter
        ) -> fmt::Result {
            let _op_highlight = ctx.highlight.operand(op_idx, "TODO");
            panic!("oh no");
            /*
            match op {
                Operands::OneOperand(reg) => {
                    match usage {
                        Use::Read => {
                            let value = ctx.ssa.map(|ssa| {
                                ssa.try_get_use(ctx.addr, Location::Register(*reg))
                            });
                            if let Some(value) = value {
                                write!(fmt, "{}", ctx.highlight.location(
                                    &(Location::Register(*reg), Direction::Read),
                                    &RegValueDisplay {
                                        reg: reg,
                                        value: &value,
                                        colors: &ctx.colors,
                                        _pd: std::marker::PhantomData,
                                    }
                                ))
                            } else {
                                event!(Level::ERROR, reg=reg, addr=ctx.addr, "broken definer for reg {} at {}", reg, ctx.addr);
                                write!(fmt, "r{}_ERR", reg)
                            }
                        },
                        Use::Write => {
                            let value = ctx.ssa.map(|ssa| {
                                ssa.get_def(ctx.addr, Location::Register(*reg)).as_rc()
                            });
                            write!(fmt, "{}", ctx.highlight.location(
                                &(Location::Register(*reg), Direction::Write),
                                &RegValueDisplay {
                                    reg: reg,
                                    value: &value,
                                    colors: &ctx.colors,
                                    _pd: std::marker::PhantomData,
                                }
                            ))
                        },
                        Use::ReadWrite => {
                            let read = ctx.ssa.map(|ssa| {
                                ssa.get_use(ctx.addr, Location::Register(*reg)).as_rc()
                            });
                            let write = ctx.ssa.map(|ssa| {
                                ssa.get_def(ctx.addr, Location::Register(*reg)).as_rc()
                            });
                            write!(fmt, "{} (-> {})",
                                ctx.highlight.location(
                                    &(Location::Register(*reg), Direction::Read),
                                    &RegValueDisplay {
                                        reg: reg,
                                        value: &read,
                                        colors: &ctx.colors,
                                        _pd: std::marker::PhantomData,
                                    }
                                ),
                                ctx.highlight.location(
                                    &(Location::Register(*reg), Direction::Write),
                                    &RegValueDisplay {
                                        reg: reg,
                                        value: &write,
                                        colors: &ctx.colors,
                                        _pd: std::marker::PhantomData,
                                    }
                                ),
                            )
                        }
                    }
                },
                Operands::TwoOperand(rd, rs) => {
                    match usage {
                        Use::Read => {
                            let value = ctx.ssa.map(|ssa| {
                                ssa.get_use(ctx.addr, Location::Register(*rs)).as_rc()
                            });
                            write!(fmt, "{}", ctx.highlight.location(
                                &(Location::Register(*rs), Direction::Read),
                                &RegValueDisplay {
                                    reg: rs,
                                    value: &value,
                                    colors: &ctx.colors,
                                    _pd: std::marker::PhantomData,
                                }
                            ))
                        },
                        Use::Write => {
                            let value = ctx.ssa.map(|ssa| {
                                ssa.get_def(ctx.addr, Location::Register(*rd)).as_rc()
                            });
                            write!(fmt, "{}", ctx.highlight.location(
                                &(Location::Register(*rd), Direction::Write),
                                &RegValueDisplay {
                                    reg: rd,
                                    value: &value,
                                    colors: &ctx.colors,
                                    _pd: std::marker::PhantomData,
                                }
                            ))
                        },
                        Use::ReadWrite => {
                            panic!("asdlfjkasldfkjaslfkd");
                            /*
                            let read = ctx.ssa.map(|ssa| {
                                ssa.get_use(ctx.addr, Location::Register(*reg)).as_rc()
                            });
                            let write = ctx.ssa.map(|ssa| {
                                ssa.get_def(ctx.addr, Location::Register(*reg)).as_rc()
                            });
                            write!(fmt, "{} (-> {})",
                                ctx.highlight.location(
                                    &(Location::Register(*reg), Direction::Read),
                                    &RegValueDisplay {
                                        reg: reg,
                                        value: &read,
                                        colors: &ctx.colors,
                                        _pd: std::marker::PhantomData,
                                    }
                                ),
                                ctx.highlight.location(
                                    &(Location::Register(*reg), Direction::Write),
                                    &RegValueDisplay {
                                        reg: reg,
                                        value: &write,
                                        colors: &ctx.colors,
                                        _pd: std::marker::PhantomData,
                                    }
                                ),
                            )
                            */
                        }
                    }
                },
                Operands::RegImm(rd, imm) => {
                    if op_idx == 0 {
                        match usage {
                            Use::Read => {
                                let value = ctx.ssa.map(|ssa| {
                                    ssa.try_get_use(ctx.addr, Location::Register(*rd))
                                });
                                if let Some(value) = value {
                                    write!(fmt, "{}", ctx.highlight.location(
                                        &(Location::Register(*rd), Direction::Write),
                                        &RegValueDisplay {
                                            reg: rd,
                                            value: &value,
                                            colors: &ctx.colors,
                                            _pd: std::marker::PhantomData,
                                        }
                                    ))
                                } else {
                                    event!(Level::ERROR, reg=rd, addr=ctx.addr, "broken definer for reg {} at {}", rd, ctx.addr);
                                    write!(fmt, "r{}_ERR", rd)
                                }
                            },
                            Use::Write => {
                                let value = ctx.ssa.map(|ssa| {
                                    ssa.try_get_def(ctx.addr, Location::Register(*rd))
                                });
                                if let Some(value) = value {
                                    write!(fmt, "{}", ctx.highlight.location(
                                        &(Location::Register(*rd), Direction::Write),
                                        &RegValueDisplay {
                                            reg: rd,
                                            value: &value,
                                            colors: &ctx.colors,
                                            _pd: std::marker::PhantomData,
                                        }
                                    ))
                                } else {
                                    event!(Level::ERROR, reg=rd, addr=ctx.addr, "broken definer for reg {} at {}", rd, ctx.addr);
                                    write!(fmt, "r{}_ERR", rd)
                                }
                            },
                            Use::ReadWrite => {
                                panic!("regimm readwrite unsupported");
                            }
                        }
                    } else {
                        write!(fmt, "{:#x}", imm)
                    }
                }
                Operands::RegRegList(rd, list) => {
                    write!(fmt, "{{")?;
                    let mut i = 0;
                    let mut tail = false;
                    while i < 16 {
                        let present = (list & (1 << i)) != 0;
                        if present {
                            if tail {
                                write!(fmt, ", ")?;
                            } else {
                                tail = true;
                            }
                            // read/write
                            match usage {
                                Use::Read => {
                                    let value = ctx.ssa.map(|ssa| {
                                        ssa.try_get_use(ctx.addr, Location::Register(i))
                                    });
                                    if let Some(value) = value {
                                        write!(fmt, "{}", ctx.highlight.location(
                                            &(Location::Register(i), Direction::Read),
                                            &RegValueDisplay {
                                                reg: &i,
                                                value: &value,
                                                colors: &ctx.colors,
                                                _pd: std::marker::PhantomData,
                                            }
                                        ))?;
                                    } else {
                                        event!(Level::ERROR, reg=i, addr=ctx.addr, "broken definer for reg {} at {}", rd, ctx.addr);
                                        write!(fmt, "r{}_ERR", rd)?;
                                    }
                                },
                                Use::Write => {
                                    let value = ctx.ssa.map(|ssa| {
                                        ssa.try_get_def(ctx.addr, Location::Register(i))
                                    });
                                    if let Some(value) = value {
                                        write!(fmt, "{}", ctx.highlight.location(
                                            &(Location::Register(i), Direction::Write),
                                            &RegValueDisplay {
                                                reg: &i,
                                                value: &value,
                                                colors: &ctx.colors,
                                                _pd: std::marker::PhantomData,
                                            }
                                        ))?;
                                    } else {
                                        event!(Level::ERROR, reg=i, addr=ctx.addr, "broken definer for reg {} at {}", rd, ctx.addr);
                                        write!(fmt, "r{}_ERR", rd)?;
                                    }
                                },
                                Use::ReadWrite => {
                                    panic!("no thank you");
                                }
                            }
                        }
                        i += 1;
                    }
                    write!(fmt, "}}")
                }
                Operands::BranchOffset(offs) => {
                    write!(fmt, "{:#x}", ctx.addr.wrapping_add(4).wrapping_add((*offs << 2) as u32))
                }
                o => {
                    write!(fmt, "unsupported operand {:?}", o)
                }
            }?;
            */

            Ok(())
        }
        /*
         * if self.ssa.is_some() {
         *     ... ...
         * }
         */

        panic!("oh no ooo");
        /*
        match self.instr.opcode {
            Opcode::LDR(true, false, false) => {
                match self.instr.operands {
                    Operands::TwoRegImm(13, Rt, 4) => {
                        ConditionedOpcode(Opcode::POP, self.instr.condition).colorize(&self.colors, fmt)?;
                        write!(fmt, " ")?;
                        return contextualize_operand(&Operands::OneOperand(Rt), 0, self, Use::Write, fmt);
                    },
                    _ => {}
                }
            },
            Opcode::STR(false, true, true) => {
                match self.instr.operands {
                    Operands::TwoRegImm(13, Rt, 4) => {
                        ConditionedOpcode(Opcode::PUSH, self.instr.condition).colorize(&self.colors, fmt)?;
                        write!(fmt, " ")?;
                        return contextualize_operand(&Operands::OneOperand(Rt), 0, self, Use::Read, fmt);
                    },
                    _ => {}
                }
            },
            Opcode::LDM(true, false, true, _usermode) => {
                // TODO: what indicates usermode in the ARM syntax?
                match self.instr.operands {
                    Operands::RegRegList(13, list) => {
                        ConditionedOpcode(Opcode::POP, self.instr.condition).colorize(&self.colors, fmt)?;
                        write!(fmt, " ")?;
                        return contextualize_operand(&self.instr.operands, 1, self, Use::Write, fmt);
                    }
                    _ => {}
                }
            }
            Opcode::STM(false, true, true, _usermode) => {
                // TODO: what indicates usermode in the ARM syntax?
                match self.instr.operands {
                    Operands::RegRegList(13, list) => {
                        ConditionedOpcode(Opcode::PUSH, self.instr.condition).colorize(&self.colors, fmt)?;
                        write!(fmt, " ")?;
                        return contextualize_operand(&self.instr.operands, 1, self, Use::Read, fmt);
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        */

        ConditionedOpcode(self.instr.opcode, self.instr.condition).colorize(&self.colors, fmt)?;

        match self.instr.opcode {
            Opcode::PUSH => {
                return contextualize_operand(&self.instr.operands, 0, self, Use::Read, fmt);
            }
            Opcode::POP => {
                return contextualize_operand(&self.instr.operands, 0, self, Use::Write, fmt);
            }
            Opcode::BLX |
            Opcode::BL |
            Opcode::B => {
                let dest_namer = |addr| {
                    self.contexts.and_then(|context| {
                        context.function_at(addr).map(|f| {
                            self.colors.function(f.with_value_names(self.ssa.map(|fn_ssa| fn_ssa.query_at(self.addr))).decl_string(false))
                        })
                            .or_else(|| {
                                context.address_name(addr).map(|name| self.colors.function(name))
                            })
                    }).unwrap_or_else(|| { self.colors.address(addr.show().to_string()) })
                };
                /*
                let relative_namer = |i| {
                    let addr = (i as u32).wrapping_add(self.instr.len()).wrapping_add(self.addr);
                    dest_namer(addr)
                };
                */

                panic!("aa");
                /*
                match &self.instr.operands {
                    Operands::BranchOffset(offs) => {
                        let dest = self.addr.wrapping_add(4).wrapping_add((*offs << 2) as u32);
                        write!(fmt, " {}", dest_namer(dest))
                    }
                    op @ _ => {
                        write!(fmt, " ")?;
                        return contextualize_operand(op, 0, self, Use::Read, fmt);
                    }
                }
                */
            },
            Opcode::AND |
            Opcode::EOR |
            Opcode::ORR |
            Opcode::LSL |
            Opcode::LSR |
            Opcode::ROR |
            Opcode::ASR |
            Opcode::RRX |
            Opcode::BIC |
            Opcode::ADR |
            Opcode::SUB |
            Opcode::RSB |
            Opcode::ADD |
            Opcode::ADC |
            Opcode::SBC |
            Opcode::RSC => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands, 0, self, Use::Write, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands, 1, self, Use::Read, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands, 2, self, Use::Read, fmt)
            }
            Opcode::TST |
            Opcode::TST |
            Opcode::CMP |
            Opcode::CMN => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands, 0, self, Use::Read, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands, 1, self, Use::Read, fmt)
            }
            Opcode::MOV => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands, 0, self, Use::Write, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands, 1, self, Use::Read, fmt)
            }
            o => {
                event!(Level::ERROR, inst=?self.instr, opcode=?o, "unsupported ssa display for opcode");
                write!(fmt, "<error>")
            }
        }
    }
}

pub fn show_instruction<M: MemoryRange<<ARMv7 as Arch>::Address>>(
    data: &M,
    ctx: &MergedContextTable,
    address: <ARMv7 as Arch>::Address,
    colors: Option<&ColorSettings>
) {
    match <ARMv7 as Arch>::Decoder::default().decode(data.range_from(address).unwrap()) {
        Ok(instr) => {
            let mut instr_text = String::new();
            ARMv7::render_frame(
                &mut instr_text,
                address,
                &instr,
                &mut data.range(address..(address + instr.len())).unwrap(),
                Some(&ctx.display_ctx()),
            ).unwrap();
            print!("{}", instr_text);
            println!(" {}", InstructionContext {
                instr: &instr,
                addr: address,
                contexts: Some(&ctx.display_ctx()),
                ssa: None,
                colors: colors,
                highlight: &NoHighlights,
            });
        },
        Err(e) => {
            println!("Decode error at {}, {}", address, e);
        }
    };
}

impl <
    Context: FunctionQuery<<ARMv7 as Arch>::Address, Function=FunctionImpl<<ARMv7 as ValueLocations>::Location>> + SymbolQuery<<ARMv7 as Arch>::Address>,
> FunctionInstructionDisplay<ARMv7, Context> for ARMv7 {
    fn display_instruction_in_function<W: fmt::Write, Highlighter: LocationHighlighter<<ARMv7 as ValueLocations>::Location>>(
        dest: &mut W,
        instr: &<ARMv7 as Arch>::Instruction,
        address: <ARMv7 as Arch>::Address,
        context: &Context,
        ssa: Option<&SSA<ARMv7>>,
        colors: Option<&ColorSettings>,
        highlight: &Highlighter,
    ) -> fmt::Result {
        write!(dest, "{}", InstructionContext {
            instr: &instr,
            addr: address,
            contexts: Some(context),
            ssa: ssa,
            colors: colors,
            highlight: highlight,
        }).unwrap();
        /*
        if let Some(ssa) = ssa {
            if is_conditional_op(instr.opcode) {
                for (loc, dir) in <x86_64Arch as ValueLocations>::decompose(instr) {
                    if let (Some(flag), Direction::Read) = (loc, dir) {
                        if [Location::ZF, Location::PF, Location::CF, Location::SF, Location::OF].contains(&flag) {
                            let use_site = ssa.try_get_def_site(ssa.get_use(address, flag).value);
                            if let Some(use_site) = use_site {
                                write!(dest, "\n    uses {:?}, defined by {} at {:#x}", flag, use_site.1, use_site.0)?;
                            } else {
                                write!(dest, "\n    uses {:?}, MISSING DEFINITION", flag)?;
                            }
                        }
                    }
                }
            }
        }
        */
        Ok(())
    }
}

pub fn show_function<'a, 'b, 'c, 'd, 'e, M: MemoryRepr<<ARMv7 as Arch>::Address> + MemoryRange<<ARMv7 as Arch>::Address>>(
    data: &'a M,
    ctx: &'b MergedContextTable,
    ssa: Option<&'d SSA<ARMv7>>,
    fn_graph: &'c ControlFlowGraph<<ARMv7 as Arch>::Address>,
    colors: Option<&'e ColorSettings>
) -> FunctionView<'a, 'c, 'd, 'e, FunctionImpl<<ARMv7 as ValueLocations>::Location>, DisplayCtx<'b>, ARMv7, M> {
    FunctionView {
        _function_type: PhantomData,
        data,
        ctx: ctx.display_ctx(),
        fn_graph,
        ssa,
        colors,
        highlight_instrs: Vec::new(),
        highlight_locs: Vec::new(),
    }
}
