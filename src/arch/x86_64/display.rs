use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

use termion::color;

use num_traits::Zero;

use memory::MemoryRepr;
use yaxpeax_arch::{ColorSettings, Colorize, Colored};
use yaxpeax_arch::{AddressDisplay, Arch, Decodable, LengthedInstruction, YaxColors};
use yaxpeax_arch::display::*;
use arch::display::BaseDisplay;
use arch::FunctionRepr;
use arch::InstructionSpan;
use arch::FunctionQuery;
use arch::CommentQuery;
use arch::SymbolQuery;
use arch::AddressNamer;
use arch::x86_64;
use arch::x86_64::{ContextRead, MergedContextTable};
use arch::x86_64::analyses::data_flow::{Data, Location, SymbolicExpression, ValueRange};
use analyses::static_single_assignment::SSAValues;
use yaxpeax_x86::{Instruction, Opcode, Operand};
use yaxpeax_x86::{RegSpec, RegisterBank};
use yaxpeax_x86::x86_64 as x86_64Arch;
use analyses::control_flow::{BasicBlock, ControlFlowGraph};
use analyses::static_single_assignment::{DFGRef, SSA};
use data::Direction;
use data::types::{Typed, TypeAtlas, TypeSpec};
use arch::display::function::FunctionInstructionDisplay;
use arch::display::function::FunctionView;

use memory::MemoryRange;

use data::ValueLocations;

use std::fmt;
use std::fmt::Write;
use std::marker::PhantomData;
use num_traits::WrappingAdd;

use yaxpeax_arch::ShowContextual;
impl <T: std::fmt::Write> ShowContextual<u64, MergedContextTable, T> for Instruction {
    fn contextualize(&self, colors: Option<&ColorSettings>, _address: u64, context: Option<&MergedContextTable>, out: &mut T) -> std::fmt::Result {
        self.contextualize(colors, _address, Option::<&[Option<String>]>::None, out)
    }
}

impl <T: FunctionQuery<<x86_64Arch as Arch>::Address> + CommentQuery<<x86_64Arch as Arch>::Address>> BaseDisplay<x86_64::Function, T> for x86_64Arch {
    fn render_frame<Data: Iterator<Item=u8>, W: fmt::Write>(
        dest: &mut W,
        addr: <x86_64Arch as Arch>::Address,
        instr: &<x86_64Arch as Arch>::Instruction,
        bytes: &mut Data,
        ctx: Option<&T>,
    ) -> fmt::Result {
        if let Some(ctx) = ctx {
            if let Some(comment) = ctx.comment_for(addr) {
                writeln!(dest, "{}: {}{}{}",
                    addr.stringy(),
                    color::Fg(&color::Blue as &color::Color),
                    comment,
                    color::Fg(&color::Reset as &color::Color)
                );
            }
            if let Some(fn_dec) = ctx.function_at(addr) {
                writeln!(dest, "      {}{}{}",
                    color::Fg(&color::LightYellow as &color::Color),
                    fn_dec.decl_string(),
                    color::Fg(&color::Reset as &color::Color)
                )?;
            }
        }
        write!(dest, "{:08x}: ", addr)?;
        for i in 0..16 {
            if i < instr.length {
                match bytes.next() {
                    Some(b) => {
                        write!(dest, "{:02x}", b)?;
                    },
                    None => { write!(dest, "  ")?; }
                }
            } else {
                write!(dest, "  ")?;
            }
        }
        write!(dest, ": | |")?;
        Ok(())
    }
}

pub struct InstructionContext<'a, 'b, 'c, 'd, Context: AddressNamer<<x86_64Arch as Arch>::Address>> {
    instr: &'a Instruction,
    addr: <x86_64Arch as Arch>::Address,
    contexts: Option<&'b Context>,
    ssa: Option<&'c SSA<x86_64Arch>>,
    colors: Option<&'d ColorSettings>
}

pub struct RegValueDisplay<'a, 'b, 'c> {
    pub reg: &'a RegSpec,
    pub value: &'b Option<DFGRef<x86_64Arch>>,
    pub colors: Option<&'c ColorSettings>,
}

impl <'a, 'b, 'c> fmt::Display for RegValueDisplay<'a, 'b, 'c> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.value {
            Some(value) => {
                write!(
                    fmt,
                    "{}",
                    self.colors.register(
                        format!(
                            "{}_{}",
                            self.reg,
                            value.borrow().version()
                                .map(|ver| ver.to_string())
                                .unwrap_or_else(|| "input".to_string())
                        )
                    )
                )?;
                if let Some(data) = value.borrow().data.as_ref() {
                    write!(fmt, " (= {})", DataDisplay { data: &data, colors: self.colors })?;
                }
                Ok(())
            },
            None => {
                write!(fmt, "{}", self.reg)
            }
        }
    }
}

pub struct ValueRangeDisplay<'a, 'b> {
    pub range: &'a ValueRange,
    pub colors: Option<&'b ColorSettings>,
}

impl <'a, 'b> fmt::Display for ValueRangeDisplay<'a, 'b> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.range {
            ValueRange::Between(start, end) => {
                write!(fmt, "[{}, {}]", DataDisplay { data: &start, colors: self.colors }, DataDisplay { data: &end, colors: self.colors })
            },
            ValueRange::Precisely(v) => {
                write!(fmt, "{}", DataDisplay { data: &v, colors: self.colors })
            }
        }
    }
}

pub struct DataDisplay<'a, 'b> {
    pub data: &'a Data,
    pub colors: Option<&'b ColorSettings>,
}

impl <'a, 'b> fmt::Display for DataDisplay<'a, 'b> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let type_atlas = TypeAtlas::new();
        match self.data {
            Data::Alias(alias) => {
                if let Location::Register(alias_reg) = alias.borrow().location {
                    write!(fmt, "{}", RegValueDisplay {
                        reg: &alias_reg,
                        value: &Some(Rc::clone(alias)),
                        colors: self.colors
                    })?;
                } else {
                    unreachable!("Register alias must be another register");
                }
            },
            Data::Str(string) => { write!(fmt, "\"{}\"", string)?; }
            Data::Expression(expr) => {
                let real_ty = expr.type_of(&type_atlas);
                if real_ty != TypeSpec::Unknown {
                    write!(fmt, "{}", expr.show(&type_atlas))?;
                } else {
                    write!(fmt, "{}", expr.show(&type_atlas))?;
                }
            },
            Data::Concrete(v, ty) => {
                if let Some(_real_ty) = ty {
                    write!(fmt, "{:#x}", v)?;
                } else {
                    write!(fmt, "{:#x}", v)?;
                }
            }
            Data::ValueSet(values) => {
                if values.len() == 0 {
                    unreachable!("Value sets cannot be empty, logical bug");
                } else if values.len() == 1 {
                    write!(fmt, "{}", ValueRangeDisplay {
                        range: &values[0],
                        colors: self.colors
                    })?;
                } else {
                    write!(fmt, "{{ {}", ValueRangeDisplay {
                        range: &values[0],
                        colors: self.colors
                    })?;
                    for value in &values[1..] {
                        write!(fmt, ", {}", ValueRangeDisplay { range: value, colors: self.colors })?;
                    }
                    write!(fmt, " }}")?;
                }
            }
        }
        Ok(())
    }
}

pub enum Use {
    Read,
    Write,
    ReadWrite
}


impl <'a, 'b, 'c, 'd, Context: SymbolQuery<<x86_64Arch as Arch>::Address> + FunctionQuery<<x86_64Arch as Arch>::Address>> Display for InstructionContext<'a, 'b, 'c, 'd, Context> {
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

        fn contextualize_operand<'a, 'b, 'c, 'd, C: FunctionQuery<<x86_64Arch as Arch>::Address> + SymbolQuery<<x86_64Arch as Arch>::Address>>(op: &Operand, op_idx: u8, ctx: &InstructionContext<'a, 'b, 'c, 'd, C>, usage: Use, fmt: &mut fmt::Formatter) -> fmt::Result {
            fn numbered_register_name<'a, 'b, 'c, 'd, C: FunctionQuery<<x86_64Arch as Arch>::Address> + SymbolQuery<<x86_64Arch as Arch>::Address>>(address: <x86_64Arch as Arch>::Address, reg: RegSpec, context: &InstructionContext<'a, 'b, 'c, 'd, C>, direction: Direction) -> Colored<String> {
                let text = context.ssa.map(|ssa| {
                    let num = ssa.get_value(address, Location::Register(reg), direction)
                        .map(|data| data.borrow().version());
                    format!("{}_{}",
                        reg,
                        num.map(|n| n.map(|v| v.to_string()).unwrap_or("input".to_string())).unwrap_or_else(|| {
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
                            let value = ctx.ssa.map(|ssa| {
                                ssa.get_use(ctx.addr, Location::Register(*spec)).as_rc()
                            });
                            write!(fmt, "{}", RegValueDisplay {
                                reg: spec,
                                value: &value,
                                colors: ctx.colors,
                            })
                        },
                        Use::Write => {
                            let value = ctx.ssa.map(|ssa| {
                                ssa.get_def(ctx.addr, Location::Register(*spec)).as_rc()
                            });
                            write!(fmt, "{}", RegValueDisplay {
                                reg: spec,
                                value: &value,
                                colors: ctx.colors,
                            })
                        },
                        Use::ReadWrite => {
                            let read = ctx.ssa.map(|ssa| {
                                ssa.get_use(ctx.addr, Location::Register(*spec)).as_rc()
                            });
                            let write = ctx.ssa.map(|ssa| {
                                ssa.get_def(ctx.addr, Location::Register(*spec)).as_rc()
                            });
                            write!(fmt, "{} (-> {})",
                                RegValueDisplay {
                                    reg: spec,
                                    value: &read,
                                    colors: ctx.colors,
                                },
                                RegValueDisplay {
                                    reg: spec,
                                    value: &write,
                                    colors: ctx.colors,
                                },
                            )
                        }
                    }
                },
                Operand::DisplacementU32(disp) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    write!(fmt, "[{}]", ctx.colors.address((*disp as u64).stringy()))
                },
                Operand::DisplacementU64(disp) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    write!(fmt, "[{}]", ctx.colors.address((*disp as u64).stringy()))
                },
                Operand::RegDeref(spec) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    write!(fmt, "[{}]",
                        RegValueDisplay {
                            reg: spec,
                            value: &ctx.ssa.map(|ssa| ssa.get_use(ctx.addr, Location::Register(*spec)).as_rc()),
                            colors: ctx.colors
                        }
                    )
                }
                Operand::RegDisp(RegSpec { bank: RegisterBank::RIP, num: _ }, disp) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    let addr = ctx.addr.wrapping_add(*disp as i64 as u64).wrapping_add(ctx.instr.len());
                    let text = ctx.contexts
                        .and_then(|ctx| ctx.symbol_for(addr))
                        .map(|sym| { ctx.colors.symbol(format!("&{}", sym)) })
                        .unwrap_or_else(|| { ctx.colors.address(addr.stringy()) });
                    write!(fmt, "[{}]", text)
                }
                Operand::RegDisp(spec, disp) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    // HACK: support writing memory operands like `reg.field` if possible:
                    let mut drawn = false;
                    if let Some(ssa) = ctx.ssa.as_ref() {
                        let use_val = ssa.get_use(ctx.addr, Location::Register(*spec));
                        match use_val.get_data() {
                            Some(Data::Expression(expr)) => {
                                let type_atlas = TypeAtlas::new();
                                if let SymbolicExpression::Add(base, offset) = expr.offset(*disp as i64 as u64) {
                                    if let Some(field) = type_atlas.get_field(&base.type_of(&type_atlas), offset as u32) {
                                        drawn = true;
                                        let val_rc = use_val.as_rc();
                                        if let Some(name) = field.name.as_ref() {
                                            write!(fmt, "[{}_{}.{}]", spec, val_rc.borrow().version().unwrap_or(0xffffffff), name)?;
                                        } else {
                                            write!(fmt, "[{}_{} + {:#x}]", spec, val_rc.borrow().version().unwrap_or(0xffffffff), offset)?;
                                        }
                                    }
                                }
                            }
                            _ => { }
                        }
                    }

                    if !drawn {
                        write!(fmt, "[{} {}]",
                            RegValueDisplay {
                                reg: spec,
                                value: &ctx.ssa.map(|ssa| ssa.get_use(ctx.addr, Location::Register(*spec)).as_rc()),
                                colors: ctx. colors
                            },
                            format_number_i32(*disp, NumberStyleHint::HexSignedWithSignSplit)
                        )?;
                    }

                    Ok(())
                },
                Operand::RegScale(spec, scale) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    write!(fmt, "[{} * {}]",
                        numbered_register_name(ctx.addr, *spec, &ctx, Direction::Read),
                        scale
                    )
                },
                Operand::RegScaleDisp(spec, scale, disp) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    write!(fmt, "[{} * {} {}]",
                        numbered_register_name(ctx.addr, *spec, &ctx, Direction::Read),
                        scale,
                        format_number_i32(*disp, NumberStyleHint::HexSignedWithSignSplit)
                    )
                },
                Operand::RegIndexBase(base, index) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    write!(fmt, "[{} + {}]",
                        numbered_register_name(ctx.addr, *base, &ctx, Direction::Read),
                        numbered_register_name(ctx.addr, *index, &ctx, Direction::Read)
                    )
                },
                Operand::RegIndexBaseDisp(base, index, disp) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    write!(fmt, "[{} + {} {}]",
                        numbered_register_name(ctx.addr, *base, &ctx, Direction::Read),
                        numbered_register_name(ctx.addr, *index, &ctx, Direction::Read),
                        format_number_i32(*disp, NumberStyleHint::HexSignedWithSignSplit)
                    )
                }
                Operand::RegIndexBaseScale(base, index, scale) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    write!(fmt, "[{} + {} * {}]",
                        numbered_register_name(ctx.addr, *base, &ctx, Direction::Read),
                        numbered_register_name(ctx.addr, *index, &ctx, Direction::Read),
                        scale
                    )
                }
                Operand::RegIndexBaseScaleDisp(base, index, scale, disp) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
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
            Opcode::Jcc(_) => {
                match &self.instr.operands[0] {
                    Operand::ImmediateI8(i) => {
                        let addr = (*i as i64 as u64).wrapping_add(self.instr.len()).wrapping_add(self.addr);
                        let text = self.contexts.and_then(|context| {
                            context.address_name(addr).map(|name| self.colors.function(name))
                        }).unwrap_or_else(|| { self.colors.address(addr.stringy()) });
                        return write!(fmt, " {}", text);
                    },
                    Operand::ImmediateI16(i) => {
                        let addr = (*i as i64 as u64).wrapping_add(self.instr.len()).wrapping_add(self.addr);
                        let text = self.contexts.and_then(|context| {
                            context.address_name(addr).map(|name| self.colors.function(name))
                        }).unwrap_or_else(|| { self.colors.address(addr.stringy()) });
                        return write!(fmt, " {}", text);
                    },
                    Operand::ImmediateI32(i) => {
                        let addr = (*i as i64 as u64).wrapping_add(self.instr.len()).wrapping_add(self.addr);
                        let text = self.contexts.and_then(|context| {
                            context.address_name(addr).map(|name| self.colors.function(name))
                        }).unwrap_or_else(|| { self.colors.address(addr.stringy()) });
                        return write!(fmt, " {}", text);
                    },
                    Operand::ImmediateI64(i) => {
                        let addr = (*i as u64).wrapping_add(self.instr.len()).wrapping_add(self.addr);
                        let text = self.contexts.and_then(|context| {
                            context.address_name(addr).map(|name| self.colors.function(name))
                        }).unwrap_or_else(|| { self.colors.address(addr.stringy()) });
                        return write!(fmt, " {}", text);
                    },
                    Operand::RegDisp(RegSpec { bank: RegisterBank::RIP, num: _ }, disp) => {
                        let addr = self.addr.wrapping_add(*disp as i64 as u64).wrapping_add(self.instr.len());
                        let text = self.contexts.and_then(|context| {
                            context.address_name(addr)
                                .map(|name| { self.colors.function(name).to_string() })
                        })
                            .unwrap_or_else(|| colorize_u64(addr, self.colors));
                        return write!(fmt, " [{}]", text);
                    }
                    op @ _ => {
                        write!(fmt, " ")?;
                        return contextualize_operand(op, 0, self, Use::Read, fmt);
                    }
                }
            },
            Opcode::LEA |
            Opcode::MOVDDUP |
            Opcode::MOVSLDUP |
            Opcode::MOVSD |
            Opcode::MOVSS |
            Opcode::CVTSI2SS |
            Opcode::CVTTSS2SI |
            Opcode::CVTSS2SI |
            Opcode::CVTSS2SD |
            Opcode::CVTSI2SD |
            Opcode::CVTTSD2SI |
            Opcode::CVTSD2SI |
            Opcode::CVTSD2SS |
            Opcode::LDDQU |
            Opcode::MOVSX_b |
            Opcode::MOVSX_w |
            Opcode::MOVZX_b |
            Opcode::MOVZX_w |
            Opcode::MOVSX |
            Opcode::MOVSXD |
            Opcode::MOV => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], 0, self, Use::Write, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands[1], 1, self, Use::Read, fmt)
            }
            Opcode::XCHG => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], 0, self, Use::ReadWrite, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands[1], 1, self, Use::ReadWrite, fmt)
            }
            Opcode::ADDSD |
            Opcode::SUBSD |
            Opcode::MULSD |
            Opcode::DIVSD |
            Opcode::MINSD |
            Opcode::MAXSD |
            Opcode::ADDSS |
            Opcode::SUBSS |
            Opcode::MULSS |
            Opcode::DIVSS |
            Opcode::MINSS |
            Opcode::MAXSS |
            Opcode::HADDPS |
            Opcode::HSUBPS |
            Opcode::ADDSUBPS |
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
            Opcode::OR => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], 0, self, Use::ReadWrite, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands[1], 1, self, Use::Read, fmt)
            }

            Opcode::SQRTSD |
            Opcode::SQRTSS |
            Opcode::MOVcc(_) => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], 0, self, Use::Write, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands[1], 1, self, Use::Read, fmt)
            }
            Opcode::BT |
            Opcode::BTS |
            Opcode::BTR |
            Opcode::BTC |
            Opcode::BSR |
            Opcode::BSF |
            Opcode::CMP |
            Opcode::TEST => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], 0, self, Use::Read, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands[1], 1, self, Use::Read, fmt)
            }
            Opcode::CMPXCHG => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], 0, self, Use::ReadWrite, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands[1], 1, self, Use::Read, fmt)
            }
            Opcode::XADD |
            Opcode::LSL => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], 0, self, Use::Write, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands[1], 1, self, Use::Write, fmt)
            }
            Opcode::LAR => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], 0, self, Use::Write, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operands[1], 1, self, Use::Read, fmt)
            }
            Opcode::SETcc(_) => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], 0, self, Use::Write, fmt)
            }
            Opcode::NOP => {
                // TODO: work around the fact that NOP doesn't decompose into ssa operations ...
                // because it's a nop.
                return Ok(())
            }
            Opcode::RETURN => {
                match &self.instr.operands[0] {
                    Operand::Nothing => { return Ok(()); },
                    _ => {
                        write!(fmt, " ")?;
                        contextualize_operand(&self.instr.operands[0], 0, self, Use::Read, fmt)
                   }
                }
            }
            Opcode::INC |
            Opcode::DEC |
            Opcode::NEG |
            Opcode::NOT => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], 0, self, Use::ReadWrite, fmt)
            }
            Opcode::CALLF | // TODO: this is wrong.
            Opcode::JMPF => { // TODO: this is wrong.
                Ok(())
            }
            Opcode::PUSH => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], 0, self, Use::Read, fmt)
            }
            Opcode::POP => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], 0, self, Use::Write, fmt)
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
                contextualize_operand(&self.instr.operands[0], 0, self, Use::Write, fmt)
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
                contextualize_operand(&self.instr.operands[0], 0, self, Use::Read, fmt)
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
            Opcode::DIV |
            Opcode::MUL => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operands[0], 0, self, Use::Read, fmt)
            },
            Opcode::IMUL |
            Opcode::IDIV => {
                write!(fmt, " ")?;
                if let Operand::Nothing = &self.instr.operands[1] {
                    contextualize_operand(&self.instr.operands[0], 0, self, Use::Read, fmt)?;
                    Ok(())
                } else {
                    contextualize_operand(&self.instr.operands[0], 0, self, Use::Read, fmt)?;
                    write!(fmt, ", ")?;
                    contextualize_operand(&self.instr.operands[1], 0, self, Use::Read, fmt)
                }
                // TODO: 3-operand mul/div?
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
        let mut instr_string = String::new();
        x86_64Arch::render_frame(
            &mut instr_string,
            address,
            instr,
            &mut data.range(address..(address + instr.len())).unwrap(),
            Some(ctx),
        );
        writeln!(instr_string, " {}", InstructionContext {
            instr: &instr,
            addr: address,
            contexts: Some(ctx),
            ssa: ssa,
            colors: colors
        });
        use analyses::control_flow::Determinant;
        writeln!(instr_string, "Control flow: {:?}", instr.control_flow(Some(&ctx.at(&address))));
        print!("{}", instr_string);
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
            let mut instr_text = String::new();
            x86_64Arch::render_frame(
                &mut instr_text,
                address,
                &instr,
                &mut data.range(address..(address + instr.len())).unwrap(),
                Some(ctx),
            );
            print!("{}", instr_text);
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

fn is_conditional_op(op: Opcode) -> bool {
    match op {
        Opcode::Jcc(_) |
        Opcode::MOVcc(_) |
        Opcode::SETcc(_) => true,
        _ => false
    }
}

impl <
    Context: FunctionQuery<<x86_64Arch as Arch>::Address> + SymbolQuery<<x86_64Arch as Arch>::Address>,
> FunctionInstructionDisplay<x86_64Arch, Context> for x86_64Arch {
    fn display_instruction_in_function<W: fmt::Write>(
        dest: &mut W,
        instr: &<x86_64Arch as Arch>::Instruction,
        address: <x86_64Arch as Arch>::Address,
        context: &Context,
        ssa: Option<&SSA<x86_64Arch>>,
        colors: Option<&ColorSettings>,
    ) -> fmt::Result {
        write!(dest, "{}", InstructionContext {
            instr: &instr,
            addr: address,
            contexts: Some(context),
            ssa: ssa,
            colors: colors
        });
        if let Some(ssa) = ssa {
            if is_conditional_op(instr.opcode) {
                for (loc, dir) in <x86_64Arch as ValueLocations>::decompose(instr) {
                    if let (Some(flag), Direction::Read) = (loc, dir) {
                        if [Location::ZF, Location::PF, Location::CF, Location::SF, Location::OF].contains(&flag) {
                            let use_site = ssa.get_def_site(ssa.get_use(address, flag).value);
                            write!(dest, "\n    uses {:?}, defined by {} at {:#x}", flag, use_site.1, use_site.0)?;
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

pub fn show_function<'a, 'b, 'c, 'd, 'e, M: MemoryRepr<<x86_64Arch as Arch>::Address> + MemoryRange<<x86_64Arch as Arch>::Address>>(
    data: &'a M,
    ctx: &'b MergedContextTable,
    ssa: Option<&'d SSA<x86_64Arch>>,
    fn_graph: &'c ControlFlowGraph<<x86_64Arch as Arch>::Address>,
    colors: Option<&'e ColorSettings>
) -> FunctionView<'a, 'b, 'c, 'd, 'e, x86_64::Function, MergedContextTable, x86_64Arch, M> {
    FunctionView {
        _function_type: PhantomData,
        data,
        ctx,
        fn_graph,
        ssa,
        colors,
        highlight_instrs: Vec::new(),
        highlight_locs: Vec::new(),
    }
}
