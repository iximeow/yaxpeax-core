use std::fmt::Display;
use std::rc::Rc;

use termion::color;

use arch::FunctionImpl;
use memory::MemoryRepr;
use yaxpeax_arch::{ColorSettings, Colorize};
use yaxpeax_arch::{Arch, AddressDiff, AddressBase, AddressDisplay, Decoder, LengthedInstruction, NoColors, YaxColors};
use yaxpeax_arch::display::*;
use arch::display::BaseDisplay;
use arch::FunctionRepr;
use arch::InstructionSpan;
use arch::FunctionQuery;
use arch::CommentQuery;
use arch::SymbolQuery;
use arch::AddressNamer;
use arch::x86_64::{ContextRead, DisplayCtx, MergedContextTable};
use arch::x86_64::analyses::data_flow::{Data, Location, SymbolicExpression, ValueRange, ANY};
use analyses::control_flow::Determinant;
use yaxpeax_x86::long_mode::{Instruction, Opcode, Operand};
use yaxpeax_x86::long_mode::{RegSpec, Segment};
use yaxpeax_x86::x86_64 as x86_64Arch;
use analyses::control_flow::{BasicBlock, ControlFlowGraph};
use analyses::static_single_assignment::{DFGRef, SSA};
use data::Direction;
use data::types::{Typed, TypeAtlas, TypeSpec};
use display::location::{LocationHighlighter, NoHighlights, StyledDisplay};
use arch::display::function::FunctionInstructionDisplay;
use arch::display::function::FunctionView;

use memory::MemoryRange;

use data::ValueLocations;

use std::fmt;
use std::fmt::Write;
use std::marker::PhantomData;

use yaxpeax_arch::ShowContextual;
impl <'a, T: std::fmt::Write, C: fmt::Display, Y: YaxColors<C>> ShowContextual<u64, DisplayCtx<'a>, C, T, Y> for Instruction {
    fn contextualize(&self, colors: &Y, _address: u64, _context: Option<&DisplayCtx<'a>>, out: &mut T) -> std::fmt::Result {
        self.contextualize(colors, _address, Option::<&[Option<String>]>::None, out)
    }
}

impl <F: FunctionRepr, T: FunctionQuery<<x86_64Arch as Arch>::Address, Function=F> + CommentQuery<<x86_64Arch as Arch>::Address>> BaseDisplay<F, T> for x86_64Arch {
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
                    addr.show(),
                    color::Fg(&color::Blue as &dyn color::Color),
                    comment,
                    color::Fg(&color::Reset as &dyn color::Color)
                ).unwrap();
            }
            if let Some(fn_dec) = ctx.function_at(addr) {
                writeln!(dest, "      {}{}{}",
                    color::Fg(&color::LightYellow as &dyn color::Color),
                    // TODO: configurable? show iff non-default?
                    fn_dec.decl_string(true),
                    color::Fg(&color::Reset as &dyn color::Color)
                )?;
            }
        }
        write!(dest, "{}: ", addr.show())?;
        for i in 0..16 {
            if i < 0.wrapping_offset(instr.len()) {
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

pub struct InstructionContext<'a, 'b, 'c, 'd, 'e, Context: AddressNamer<<x86_64Arch as Arch>::Address>, Highlighter: LocationHighlighter<<x86_64Arch as ValueLocations>::Location>> {
    instr: &'a Instruction,
    addr: <x86_64Arch as Arch>::Address,
    contexts: Option<&'b Context>,
    ssa: Option<&'c SSA<x86_64Arch>>,
    colors: Option<&'d ColorSettings>,
    highlight: &'e Highlighter,
}

impl <'a, 'b, 'c, 'd, 'e, Context: AddressNamer<<x86_64Arch as Arch>::Address>, Highlighter: LocationHighlighter<<x86_64Arch as ValueLocations>::Location>> InstructionContext<'a, 'b, 'c, 'd, 'e, Context, Highlighter> {
    pub fn numbered_register_name(
        &self,
        reg: RegSpec,
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
                    write!(fmt, " (= {})", data.display(self.colors))?;
                }
                Ok(())
            },
            None => {
                write!(fmt, "{}", self.reg)
            }
        }
    }
}

pub enum MemValueDisplay<'a, 'b, 'c, 'd> {
    Address(Option<Segment>, u64),
    Reg(Option<Segment>, StyledDisplay<'a, RegValueDisplay<'b, 'c, 'd>>),
    RegOffset(Option<Segment>, StyledDisplay<'a, RegValueDisplay<'b, 'c, 'd>>, u64),
}

impl <'a, 'b, 'c, 'd> fmt::Display for MemValueDisplay<'a, 'b, 'c, 'd> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MemValueDisplay::Address(seg, disp) => {
                if let Some(seg) = seg {
                    write!(fmt, "{}:", seg)?;
                }
                write!(fmt, "[{}]", disp)
            }
            MemValueDisplay::Reg(seg, reg) => {
                if let Some(seg) = seg {
                    write!(fmt, "{}:", seg)?;
                }
                write!(fmt, "[{}]", reg)
            }
            MemValueDisplay::RegOffset(seg, reg, disp) => {
                if let Some(seg) = seg {
                    write!(fmt, "{}:", seg)?;
                }
                write!(fmt, "[{} + {:#x}]", reg, disp)
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

#[derive(PartialEq, Clone, Copy)]
pub enum Use {
    Read,
    Write,
    ReadWrite
}

fn operand_use(inst: &<x86_64Arch as Arch>::Instruction, op_idx: u8) -> Use {
    match inst.opcode() {
        Opcode::CALL |
        Opcode::JMP |
        Opcode::JO |
        Opcode::JNO |
        Opcode::JZ |
        Opcode::JNZ |
        Opcode::JA |
        Opcode::JNA |
        Opcode::JB |
        Opcode::JNB |
        Opcode::JP |
        Opcode::JNP |
        Opcode::JS |
        Opcode::JNS |
        Opcode::JG |
        Opcode::JLE |
        Opcode::JGE |
        Opcode::JL => {
            // we can assume op_idx is valid (so, 0), and as a result...
            debug_assert!(op_idx == 0);
            Use::Read
        }
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
            [Use::Write, Use::Read][op_idx as usize]
        }
        Opcode::XADD |
        Opcode::XCHG => {
            [Use::ReadWrite, Use::ReadWrite][op_idx as usize]
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
            [Use::ReadWrite, Use::Read][op_idx as usize]
        }

        Opcode::SQRTSD |
        Opcode::SQRTSS |
        Opcode::CMOVO |
        Opcode::CMOVNO |
        Opcode::CMOVZ |
        Opcode::CMOVNZ |
        Opcode::CMOVA |
        Opcode::CMOVNA |
        Opcode::CMOVNB |
        Opcode::CMOVB |
        Opcode::CMOVP |
        Opcode::CMOVNP |
        Opcode::CMOVS |
        Opcode::CMOVNS |
        Opcode::CMOVG |
        Opcode::CMOVLE |
        Opcode::CMOVGE |
        Opcode::CMOVL => {
            [Use::Write, Use::Read][op_idx as usize]
        }
        Opcode::BT |
        Opcode::BTS |
        Opcode::BTR |
        Opcode::BTC |
        Opcode::BSR |
        Opcode::BSF |
        Opcode::CMP |
        Opcode::TEST => {
            [Use::Read, Use::Read][op_idx as usize]
        }
        Opcode::CMPXCHG => {
            [Use::ReadWrite, Use::Read][op_idx as usize]
        }
        Opcode::LSL => {
            [Use::Write, Use::Write][op_idx as usize]
        }
        Opcode::LAR => {
            [Use::Write, Use::Read][op_idx as usize]
        }
        Opcode::SETO |
        Opcode::SETNO |
        Opcode::SETZ |
        Opcode::SETNZ |
        Opcode::SETA |
        Opcode::SETBE |
        Opcode::SETAE |
        Opcode::SETB |
        Opcode::SETP |
        Opcode::SETNP |
        Opcode::SETS |
        Opcode::SETNS |
        Opcode::SETG |
        Opcode::SETLE |
        Opcode::SETGE |
        Opcode::SETL => {
            debug_assert!(op_idx == 0);
            Use::Write
        }
        Opcode::NOP => {
            panic!("instruction has no operands");
        }
        Opcode::RETURN => {
            // this is either not called (no-arg return) or called for the first operand in return
            // (so op_idx == 0)
            debug_assert!(op_idx == 0);
            Use::Read
        }
        Opcode::INC |
        Opcode::DEC |
        Opcode::NEG |
        Opcode::NOT => {
            debug_assert!(op_idx == 0);
            Use::ReadWrite
        }
        Opcode::CALLF | // TODO: this is wrong.
        Opcode::JMPF => { // TODO: this is wrong.
            Use::Read
        }
        Opcode::PUSH => {
            debug_assert!(op_idx == 0);
            Use::Read
        }
        Opcode::POP => {
            debug_assert!(op_idx == 0);
            Use::Write
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
            Use::Read
        },
        Opcode::INVLPG |
        Opcode::FXSAVE |
        Opcode::FXRSTOR |
        Opcode::STMXCSR |
        Opcode::XSAVE |
        Opcode::XRSTOR |
        Opcode::XSAVEOPT |
        Opcode::SMSW |
        Opcode::SLDT |
        Opcode::STR |
        Opcode::SGDT |
        Opcode::SIDT => {
            Use::Write
        }
        Opcode::VERR |
        Opcode::VERW |
        Opcode::LDMXCSR |
        Opcode::LMSW |
        Opcode::LLDT |
        Opcode::LTR |
        Opcode::LGDT |
        Opcode::LIDT => {
            Use::Read
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
        Opcode::CDQ |
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
            // TODO: this is wrong.
            Use::Read
        }
        Opcode::DIV |
        Opcode::MUL => {
            // TODO: questionable.
            Use::Read
        },
        Opcode::IMUL |
        Opcode::IDIV => {
            // TODO: questionable.
            Use::Read
        }
        o => {
            unimplemented!("yet-unsupported opcode {:?}", o);
        }
    }
}

use arch::x86_64::analyses;
pub fn locations_of(inst: &<x86_64Arch as Arch>::Instruction, op_idx: u8) -> Vec<(analyses::data_flow::Location, Direction)> {
    let mut locs: Vec<(analyses::data_flow::Location, Direction)> = vec![];
    let op = &inst.operand(op_idx);
    let usage = operand_use(inst, op_idx);
    fn push_operand_locs(op: &Operand, usage: Use, locs: &mut Vec<(analyses::data_flow::Location, Direction)>) {
        match op {
            Operand::Register(reg) => {
                match usage {
                    Use::Read => {
                        locs.push((Location::Register(*reg), Direction::Read));
                    },
                    Use::Write => {
                        locs.push((Location::Register(*reg), Direction::Write));
                    }
                    Use::ReadWrite => {
                        locs.push((Location::Register(*reg), Direction::Read));
                        locs.push((Location::Register(*reg), Direction::Write));
                    }
                }
            },
            Operand::RegDeref(reg) => {
                locs.push((Location::Register(*reg), Direction::Read));
            },
            Operand::RegDisp(reg, _) => {
                locs.push((Location::Register(*reg), Direction::Read));
            },
            Operand::RegScale(reg, _) => {
                locs.push((Location::Register(*reg), Direction::Read));
            }
            Operand::RegScaleDisp(base, _, _) => {
                locs.push((Location::Register(*base), Direction::Read));
            }
            Operand::RegIndexBase(base, index) => {
                locs.push((Location::Register(*base), Direction::Read));
                locs.push((Location::Register(*index), Direction::Read));
            }
            Operand::RegIndexBaseDisp(base, index, _) => {
                locs.push((Location::Register(*base), Direction::Read));
                locs.push((Location::Register(*index), Direction::Read));
            }
            Operand::RegIndexBaseScale(base, index, _) => {
                locs.push((Location::Register(*base), Direction::Read));
                locs.push((Location::Register(*index), Direction::Read));
            }
            Operand::RegIndexBaseScaleDisp(base, index, _, _) => {
                locs.push((Location::Register(*base), Direction::Read));
                locs.push((Location::Register(*index), Direction::Read));
            }
            Operand::Nothing => {
                panic!("`Operand::Nothing` contains no locations");
            }
            Operand::ImmediateI8(_) |
            Operand::ImmediateU8(_) |
            Operand::ImmediateI16(_) |
            Operand::ImmediateU16(_) |
            Operand::ImmediateI32(_) |
            Operand::ImmediateU32(_) |
            Operand::ImmediateI64(_) |
            Operand::ImmediateU64(_) |
            Operand::DisplacementU32(_) |
            Operand::DisplacementU64(_) => {
                // this is an immediate, displacement. no locations.
            }
            op => {
                panic!("{:?} has unknown locations", op);
            }
        }
    }
    push_operand_locs(op, usage, &mut locs);
    locs
}

use display::location::{OperandScroll, OperandCursor};
impl <'a> OperandScroll<(<x86_64Arch as Arch>::Instruction, Option<&'a SSA<x86_64Arch>>)> for OperandCursor {
    fn first(_instr: &(<x86_64Arch as Arch>::Instruction, Option<&'a SSA<x86_64Arch>>)) -> OperandCursor {
        OperandCursor {
            operand: 0,
            location: None,
            implicit: false
        }
    }

    // TODO: neither of these handle the register operand == register location equivalence i'd like
    // to see
    fn next(&mut self, inst_data: &(<x86_64Arch as Arch>::Instruction, Option<&'a SSA<x86_64Arch>>)) -> bool {
        let (inst, ssa) = inst_data;

        if let Some(_ssa) = ssa {
            // some instructions have no operands, so we can just bail early:
            if !inst.operand_present(0) {
                // nope, not scrolling!
                false
            } else {
                // instruction has operands. we can assume here that 'operand' selects an actual
                // operand.
                let _curr_op = &inst.operand(self.operand);

                let mut locations = locations_of(inst, self.operand);

                if locations.len() == 0 {
                    // TODO: this is wrong - would prohibit seeking over an immediate as the first
                    // argument?
                    return false;
                }

                if let Some(location) = self.location.clone() {
                    let mut next_loc = location;
                    let mut next_operand = self.operand;
                    let curr_loc = locations[location as usize];
                    while locations[next_loc as usize] == curr_loc {
                        next_loc += 1;
                        if next_loc as usize >= locations.len() {
                            // try to advance operands..
                            next_operand += 1;
                            if next_operand >= inst.operand_count() || !inst.operand_present(next_operand) {
                                // we've hit the end! no more locations!
                                return false;
                            }
                            locations = locations_of(inst, next_operand);

                            if locations.len() == 0 {
                                // TODO: this is wrong - would prohibit seeking over an immediate as the first
                                // argument?
                                return false;
                            }

                            next_loc = 0;
                        }
                    }

                    self.operand = next_operand;
                    self.location = Some(next_loc);

                    true
                } else {
                    if locations.len() > 0 {
                        self.location = Some(0);
                        true
                    } else {
                        false
                    }
                }
            }

        } else {
            let next_operand = self.operand + 1;
            if inst.operand_count() > next_operand && inst.operand_present(next_operand) {
                self.operand = next_operand;
                true
            } else {
                false
            }
        }
    }

    // TODO: neither of these handle the register operand == register location equivalence i'd like
    // to see
    fn prev(&mut self, inst_data: &(<x86_64Arch as Arch>::Instruction, Option<&'a SSA<x86_64Arch>>)) -> bool {
        let (inst, ssa) = inst_data;

        if let Some(_ssa) = ssa {
            // some instructions have no operands, so we can just bail early:
            if !inst.operand_present(0) {
                // nope, not scrolling!
                false
            } else {
                // instruction has operands. we can assume here that 'operand' selects an actual
                // operand.
                let _curr_op = &inst.operand(self.operand);

                let mut locations = locations_of(inst, self.operand);

                if let Some(location) = self.location.clone() {
                    let mut next_loc = location;
                    let mut next_operand = self.operand;
                    let curr_loc = locations[location as usize];
                    while locations[next_loc as usize] == curr_loc {
                        if next_loc == 0 {
                            // try to advance operands..
                            if next_operand == 0 {
                                return false;
                            }
                            next_operand -= 1;
                            if !inst.operand_present(next_operand) {
                                // we've hit the end! no more locations!
                                return false;
                            }
                            locations = locations_of(inst, next_operand);
                            next_loc = locations.len() as u8 - 1;
                        } else {
                            next_loc -= 1;
                        }
                    }

                    self.operand = next_operand;
                    self.location = Some(next_loc);

                    true
                } else {
                    if locations.len() > 0 {
                        self.location = Some(0);
                        true
                    } else {
                        false
                    }
                }
            }


        } else {
            if self.operand == 0 {
                false
            } else {
                self.operand -= 1;
                true
            }
        }
    }
}

impl <
    'a, 'b, 'c, 'd, 'e,
    Context: SymbolQuery<<x86_64Arch as Arch>::Address> + FunctionQuery<<x86_64Arch as Arch>::Address, Function=FunctionImpl<<x86_64Arch as ValueLocations>::Location>>,
    Highlighter: LocationHighlighter<<x86_64Arch as ValueLocations>::Location>
> Display for InstructionContext<'a, 'b, 'c, 'd, 'e, Context, Highlighter> {
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
            C: FunctionQuery<<x86_64Arch as Arch>::Address, Function=F> + SymbolQuery<<x86_64Arch as Arch>::Address>,
            Highlighter: LocationHighlighter<<x86_64Arch as ValueLocations>::Location>
        >(
            op: &Operand,
            op_idx: u8,
            ctx: &InstructionContext<'a, 'b, 'c, 'd, 'e, C, Highlighter>,
            usage: Use,
            fmt: &mut fmt::Formatter
        ) -> fmt::Result {
            let _op_highlight = ctx.highlight.operand(op_idx, "TODO");
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
                            write!(fmt, "{}", ctx.highlight.location(
                                &(Location::Register(*spec), Direction::Read),
                                &RegValueDisplay {
                                    reg: spec,
                                    value: &value,
                                    colors: ctx.colors,
                                }
                            ))
                        },
                        Use::Write => {
                            let value = ctx.ssa.map(|ssa| {
                                ssa.get_def(ctx.addr, Location::Register(*spec)).as_rc()
                            });
                            write!(fmt, "{}", ctx.highlight.location(
                                &(Location::Register(*spec), Direction::Write),
                                &RegValueDisplay {
                                    reg: spec,
                                    value: &value,
                                    colors: ctx.colors,
                                }
                            ))
                        },
                        Use::ReadWrite => {
                            let read = ctx.ssa.map(|ssa| {
                                ssa.get_use(ctx.addr, Location::Register(*spec)).as_rc()
                            });
                            let write = ctx.ssa.map(|ssa| {
                                ssa.get_def(ctx.addr, Location::Register(*spec)).as_rc()
                            });
                            write!(fmt, "{} (-> {})",
                                ctx.highlight.location(
                                    &(Location::Register(*spec), Direction::Read),
                                    &RegValueDisplay {
                                        reg: spec,
                                        value: &read,
                                        colors: ctx.colors,
                                    }
                                ),
                                ctx.highlight.location(
                                    &(Location::Register(*spec), Direction::Write),
                                    &RegValueDisplay {
                                        reg: spec,
                                        value: &write,
                                        colors: ctx.colors,
                                    }
                                ),
                            )
                        }
                    }
                },
                Operand::DisplacementU32(disp) => {
                    let mem_disp = MemValueDisplay::Address(
                        ctx.instr.segment_override_for_op(op_idx),
                        *disp as u64
                    );
                    let read_thunk = || {
                        ctx.highlight.location(
                            &(Location::Memory(ANY), Direction::Read),
                            &mem_disp,
                        )
                    };
                    let write_thunk = || {
                        ctx.highlight.location(
                            &(Location::Memory(ANY), Direction::Write),
                            &mem_disp,
                        )
                    };
                    match usage {
                        Use::Read => {
                            write!(fmt, "{}", read_thunk())
                        },
                        Use::Write => {
                            write!(fmt, "{}", write_thunk())
                        },
                        Use::ReadWrite => {
                            write!(fmt, "{} (-> {})",
                                read_thunk(),
                                write_thunk()
                            )
                        }
                    }
                },
                Operand::DisplacementU64(disp) => {
                    let mem_disp = MemValueDisplay::Address(
                        ctx.instr.segment_override_for_op(op_idx),
                        *disp as u64
                    );
                    let read_thunk = || {
                        ctx.highlight.location(
                            &(Location::Memory(ANY), Direction::Read),
                            &mem_disp,
                        )
                    };
                    let write_thunk = || {
                        ctx.highlight.location(
                            &(Location::Memory(ANY), Direction::Write),
                            &mem_disp,
                        )
                    };
                    match usage {
                        Use::Read => {
                            write!(fmt, "{}", read_thunk())
                        },
                        Use::Write => {
                            write!(fmt, "{}", write_thunk())
                        },
                        Use::ReadWrite => {
                            write!(fmt, "{} (-> {})",
                                read_thunk(),
                                write_thunk()
                            )
                        }
                    }
                },
                Operand::RegDeref(spec) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    let value = ctx.ssa.map(|ssa| ssa.get_use(ctx.addr, Location::Register(*spec)).as_rc());
                    let reg_disp = RegValueDisplay {
                        reg: spec,
                        value: &value,
                        colors: ctx.colors
                    };
                    let reg_disp = ctx.highlight.location(
                        &(Location::Register(*spec), Direction::Read),
                        &reg_disp,
                    );
                    let mem_disp = MemValueDisplay::Reg(
                        ctx.instr.segment_override_for_op(op_idx),
                        reg_disp,
                    );
                    let read_thunk = || {
                        ctx.highlight.location(
                            &(Location::Memory(ANY), Direction::Read),
                            &mem_disp,
                        )
                    };
                    let write_thunk = || {
                        ctx.highlight.location(
                            &(Location::Memory(ANY), Direction::Write),
                            &mem_disp,
                        )
                    };
                    match usage {
                        Use::Read => {
                            write!(fmt, "{}", read_thunk())
                        },
                        Use::Write => {
                            write!(fmt, "{}", write_thunk())
                        }
                        Use::ReadWrite => {
                            write!(fmt, "{} (-> {})", read_thunk(), write_thunk())
                        }
                    }
                }
                Operand::RegDisp(RegSpec::RIP, disp) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    let addr = ctx.addr.wrapping_offset(AddressDiff::from_const(*disp as i64 as u64)).wrapping_offset(ctx.instr.len());
                    let text = ctx.contexts
                        .and_then(|ctx| ctx.symbol_for(addr))
                        .map(|sym| { ctx.colors.symbol(format!("&{}", sym)) })
                        .unwrap_or_else(|| { ctx.colors.address(addr.show().to_string()) });
                    let text = ctx.highlight.location(
                        &(Location::Register(RegSpec::rip()), Direction::Read),
                        &text
                    );
                    let text = format!("[{}]", text);
                    let read_thunk = || {
                        ctx.highlight.location(
                            &(Location::Memory(ANY), Direction::Read),
                            &text,
                        )
                    };
                    let write_thunk = || {
                        ctx.highlight.location(
                            &(Location::Memory(ANY), Direction::Write),
                            &text,
                        )
                    };
                    match usage {
                        Use::Read => {
                            write!(fmt, "{}", read_thunk())
                        },
                        Use::Write => {
                            write!(fmt, "{}", write_thunk())
                        }
                        Use::ReadWrite => {
                            write!(fmt, "{} (-> {})", read_thunk(), write_thunk())
                        }
                    }
                }
                Operand::RegDisp(spec, disp) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    // let value = ctx.ssa.map(|ssa| ssa.get_use(ctx.addr, Location::Register(*spec)).as_rc());
                    let reg_text = ctx.numbered_register_name(*spec, Direction::Read);
                    let reg = ctx.highlight.location(
                        &(Location::Register(*spec), Direction::Read),
                        &reg_text,
                    );
                    // HACK: support writing memory operands like `reg.field` if possible:
                    let mut drawn = false;
                    if let Some(ssa) = ctx.ssa.as_ref() {
                        let use_val = ssa.get_use(ctx.addr, Location::Register(*spec));
                        match use_val.get_data().as_ref() {
                            Some(Data::Expression(expr)) => {
                                let type_atlas = TypeAtlas::new();
                                if let SymbolicExpression::Add(base, offset) = expr.clone().offset(*disp as i64 as u64) {
                                    if let Some(field) = type_atlas.get_field(&base.type_of(&type_atlas), offset as u32) {
                                        drawn = true;
//                                        let _val_rc = use_val.as_rc();
                                        let text = if let Some(name) = field.name.as_ref() {
                                            format!("[{}.{}]", reg, name)
                                        } else {
                                            format!("[{} + {:#x}]", reg, offset)
                                        };
                                        let read_thunk = || {
                                            ctx.highlight.location(
                                                &(Location::Memory(ANY), Direction::Read),
                                                &text
                                            )
                                        };
                                        let write_thunk = || {
                                            ctx.highlight.location(
                                                &(Location::Memory(ANY), Direction::Read),
                                                &text
                                            )
                                        };
                                        match usage {
                                            Use::Read => { write!(fmt, "{}", read_thunk())?; },
                                            Use::Write => { write!(fmt, "{}", write_thunk())?; },
                                            Use::ReadWrite => { write!(fmt, "{} (-> {})", read_thunk(), write_thunk())?; },
                                        }
                                    }
                                }
                            }
                            _ => { }
                        };
                    }

                    if !drawn {
                        struct Formatted<'a, Data: fmt::Display> {
                            reg: &'a Data,
                            disp: i32,
                        }

                        impl<'a, Data: fmt::Display> fmt::Display for Formatted<'a, Data> {
                            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                                write!(f, "[{} ", self.reg)?;
                                format_number_i32(&NoColors, f, self.disp, NumberStyleHint::HexSignedWithSignSplit)?;
                                write!(f, "]")
                            }
                        }

                        let fmt_struct = Formatted {
                            reg: &reg,
                            disp: *disp,
                        };

                        match usage {
                            Use::Read => {
                                write!(fmt, "{}", ctx.highlight.location(
                                    &(Location::Memory(ANY), Direction::Read),
                                    &fmt_struct,
                                ))?;
                            },
                            Use::Write => {
                                write!(fmt, "{}", ctx.highlight.location(
                                    &(Location::Memory(ANY), Direction::Write),
                                    &fmt_struct,
                                ))?;
                            },
                            Use::ReadWrite => {
                                write!(fmt, "{} (-> {})",
                                    ctx.highlight.location(
                                        &(Location::Memory(ANY), Direction::Read),
                                        &fmt_struct,
                                    ),
                                    ctx.highlight.location(
                                        &(Location::Memory(ANY), Direction::Write),
                                        &fmt_struct,
                                    ),
                                )?;
                            }
                        }
                    }

                    Ok(())
                },
                Operand::RegScale(spec, scale) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    let reg_text = ctx.numbered_register_name(*spec, Direction::Read);
                    let reg = ctx.highlight.location(
                        &(Location::Register(*spec), Direction::Read),
                        &reg_text,
                    );
                    write!(fmt, "[{} * {}]", reg, scale)
                },
                Operand::RegScaleDisp(spec, scale, disp) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    let reg_text = ctx.numbered_register_name(*spec, Direction::Read);
                    let reg = ctx.highlight.location(
                        &(Location::Register(*spec), Direction::Read),
                        &reg_text
                    );
                    write!(fmt, "[{} * {} ",
                        reg,
                        scale,
                    )?;
                    format_number_i32(&ctx.colors, fmt, *disp, NumberStyleHint::HexSignedWithSignSplit)?;
                    write!(fmt, "]")
                },
                Operand::RegIndexBase(base, index) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    let base_text = ctx.numbered_register_name(*base, Direction::Read);
                    let base = ctx.highlight.location(
                        &(Location::Register(*base), Direction::Read),
                        &base_text,
                    );
                    let index_text = ctx.numbered_register_name(*index, Direction::Read);
                    let index = ctx.highlight.location(
                        &(Location::Register(*index), Direction::Read),
                        &index_text,
                    );
                    write!(fmt, "[{} + {}]", base, index)
                },
                Operand::RegIndexBaseDisp(base, index, disp) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    let base_text = ctx.numbered_register_name(*base, Direction::Read);
                    let base = ctx.highlight.location(
                        &(Location::Register(*base), Direction::Read),
                        &base_text,
                    );
                    let index_text = ctx.numbered_register_name(*index, Direction::Read);
                    let index = ctx.highlight.location(
                        &(Location::Register(*index), Direction::Read),
                        &index_text,
                    );
                    write!(fmt, "[{} + {} ",
                        base,
                        index,
                    )?;
                    format_number_i32(&ctx.colors, fmt, *disp, NumberStyleHint::HexSignedWithSignSplit)?;
                    write!(fmt, "]")
                }
                Operand::RegIndexBaseScale(base, index, scale) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    let base_text = ctx.numbered_register_name(*base, Direction::Read);
                    let base = ctx.highlight.location(
                        &(Location::Register(*base), Direction::Read),
                        &base_text,
                    );
                    let index_text = ctx.numbered_register_name(*index, Direction::Read);
                    let index = ctx.highlight.location(
                        &(Location::Register(*index), Direction::Read),
                        &index_text,
                    );
                    write!(fmt, "[{} + {} * {}]",
                        base,
                        index,
                        scale
                    )
                }
                Operand::RegIndexBaseScaleDisp(base, index, scale, disp) => {
                    if let Some(prefix) = ctx.instr.segment_override_for_op(op_idx) {
                        write!(fmt, "{}", ctx.colors.address(format!("{}:", prefix)))?;
                    }
                    let base_text = ctx.numbered_register_name(*base, Direction::Read);
                    let base = ctx.highlight.location(
                        &(Location::Register(*base), Direction::Read),
                        &base_text,
                    );
                    let index_text = ctx.numbered_register_name(*index, Direction::Read);
                    let index = ctx.highlight.location(
                        &(Location::Register(*index), Direction::Read),
                        &index_text,
                    );
                    write!(fmt, "[{} + {} * {}",
                        base,
                        index,
                        scale
                    )?;
                    format_number_i32(&ctx.colors, fmt, *disp, NumberStyleHint::HexSignedWithSignSplit)?;
                    write!(fmt, "]")
                }
                Operand::Nothing => {
                    Ok(())
                },
                op => {
                    panic!("attempted to display unknown operand kind: {}", op);
                }
            }?;

            Ok(())
        }

        self.instr.opcode().colorize(&self.colors, fmt)?;

        match self.instr.opcode() {
            Opcode::CALL |
            Opcode::JMP |
            Opcode::JO |
            Opcode::JNO |
            Opcode::JB |
            Opcode::JNB |
            Opcode::JZ |
            Opcode::JNZ |
            Opcode::JNA |
            Opcode::JA |
            Opcode::JS |
            Opcode::JNS |
            Opcode::JP |
            Opcode::JNP |
            Opcode::JL |
            Opcode::JGE |
            Opcode::JLE |
            Opcode::JG => {
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
                let relative_namer = |i| {
                    let addr = self.addr.wrapping_offset(AddressDiff::from_const(i as u64)).wrapping_offset(self.instr.len());
                    dest_namer(addr)
                };

                match &self.instr.operand(0) {
                    Operand::ImmediateI8(i) => {
                        return write!(fmt, " {}", relative_namer(*i as i64));
                    },
                    Operand::ImmediateI16(i) => {
                        return write!(fmt, " {}", relative_namer(*i as i64));
                    },
                    Operand::ImmediateI32(i) => {
                        return write!(fmt, " {}", relative_namer(*i as i64));
                    },
                    Operand::ImmediateI64(i) => {
                        return write!(fmt, " {}", relative_namer(*i as i64));
                    },
                    Operand::RegDisp(RegSpec::RIP, disp) => {
                        let addr = self.addr.wrapping_offset(AddressDiff::from_const(*disp as i64 as u64)).wrapping_offset(self.instr.len());
                        return write!(fmt, " [{}]", dest_namer(addr));
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
                contextualize_operand(&self.instr.operand(0), 0, self, Use::Write, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operand(1), 1, self, Use::Read, fmt)
            }
            Opcode::XADD |
            Opcode::XCHG => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operand(0), 0, self, Use::ReadWrite, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operand(1), 1, self, Use::ReadWrite, fmt)
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
                contextualize_operand(&self.instr.operand(0), 0, self, Use::ReadWrite, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operand(1), 1, self, Use::Read, fmt)
            }

            Opcode::SQRTSD |
            Opcode::SQRTSS |
            Opcode::CMOVO |
            Opcode::CMOVNO |
            Opcode::CMOVB |
            Opcode::CMOVNB |
            Opcode::CMOVZ |
            Opcode::CMOVNZ |
            Opcode::CMOVNA |
            Opcode::CMOVA |
            Opcode::CMOVS |
            Opcode::CMOVNS |
            Opcode::CMOVP |
            Opcode::CMOVNP |
            Opcode::CMOVL |
            Opcode::CMOVGE |
            Opcode::CMOVLE |
            Opcode::CMOVG => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operand(0), 0, self, Use::Write, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operand(1), 1, self, Use::Read, fmt)
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
                contextualize_operand(&self.instr.operand(0), 0, self, Use::Read, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operand(1), 1, self, Use::Read, fmt)
            }
            Opcode::CMPXCHG => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operand(0), 0, self, Use::ReadWrite, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operand(1), 1, self, Use::Read, fmt)
            }
            Opcode::LSL => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operand(0), 0, self, Use::Write, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operand(1), 1, self, Use::Write, fmt)
            }
            Opcode::LAR => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operand(0), 0, self, Use::Write, fmt)?;
                write!(fmt, ", ")?;
                contextualize_operand(&self.instr.operand(1), 1, self, Use::Read, fmt)
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
                contextualize_operand(&self.instr.operand(0), 0, self, Use::Write, fmt)
            }
            Opcode::NOP => {
                // TODO: work around the fact that NOP doesn't decompose into ssa operations ...
                // because it's a nop.
                return Ok(())
            }
            Opcode::RETURN => {
                match &self.instr.operand(0) {
                    Operand::Nothing => { return Ok(()); },
                    _ => {
                        write!(fmt, " ")?;
                        contextualize_operand(&self.instr.operand(0), 0, self, Use::Read, fmt)
                   }
                }
            }
            Opcode::INC |
            Opcode::DEC |
            Opcode::NEG |
            Opcode::NOT => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operand(0), 0, self, Use::ReadWrite, fmt)
            }
            Opcode::CALLF | // TODO: this is wrong.
            Opcode::JMPF => { // TODO: this is wrong.
                Ok(())
            }
            Opcode::PUSH => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operand(0), 0, self, Use::Read, fmt)
            }
            Opcode::POP => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operand(0), 0, self, Use::Write, fmt)
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
            Opcode::XRSTOR |
            Opcode::XSAVEOPT |
            Opcode::SMSW |
            Opcode::SLDT |
            Opcode::STR |
            Opcode::SGDT |
            Opcode::SIDT => {
                write!(fmt, " ")?;
                contextualize_operand(&self.instr.operand(0), 0, self, Use::Write, fmt)
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
                contextualize_operand(&self.instr.operand(0), 0, self, Use::Read, fmt)
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
            Opcode::CDQ |
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
                contextualize_operand(&self.instr.operand(0), 0, self, Use::Read, fmt)
            },
            Opcode::IMUL |
            Opcode::IDIV => {
                write!(fmt, " ")?;
                if !self.instr.operand_present(1) {
                    contextualize_operand(&self.instr.operand(0), 0, self, Use::Read, fmt)?;
                    Ok(())
                } else {
                    contextualize_operand(&self.instr.operand(0), 0, self, Use::Read, fmt)?;
                    write!(fmt, ", ")?;
                    contextualize_operand(&self.instr.operand(1), 0, self, Use::Read, fmt)
                }
                // TODO: 3-operand mul/div?
            }
            _o => {
                // unimplemented!("yet-unsupported opcode {:?}", o);
                write!(fmt, "missing operand info")
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
    println!("Basic block --\n  start: {}\n  end:   {}", block.start.show(), block.end.show());
    println!("  next:");
    for neighbor in cfg.graph.neighbors(block.start) {
        println!("    {}", neighbor.show());
    }
    let mut iter = data.instructions_spanning(<x86_64Arch as Arch>::Decoder::default(), block.start, block.end);
    while let Some((address, instr)) = iter.next() {
        let mut instr_string = String::new();
        x86_64Arch::render_frame(
            &mut instr_string,
            address,
            instr,
            &mut data.range(address..(address + instr.len())).unwrap(),
            Some(&ctx.display_ctx()),
        ).unwrap();
        writeln!(instr_string, " {}", InstructionContext {
            instr: &instr,
            addr: address,
            contexts: Some(&ctx.display_ctx()),
            ssa: ssa,
            colors: colors,
            highlight: &NoHighlights,
        }).unwrap();
        writeln!(instr_string, "Control flow: {:?}", instr.control_flow(Some(&ctx.at(&address)))).unwrap();
        print!("{}", instr_string);
    }
}

pub fn show_instruction<M: MemoryRange<<x86_64Arch as Arch>::Address>>(
    data: &M,
    ctx: &MergedContextTable,
    address: <x86_64Arch as Arch>::Address,
    colors: Option<&ColorSettings>
) {
    match <x86_64Arch as Arch>::Decoder::default().decode(data.range_from(address).unwrap()) {
        Ok(instr) => {
            let mut instr_text = String::new();
            x86_64Arch::render_frame(
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

fn is_conditional_op(op: Opcode) -> bool {
    match op {
        Opcode::JO |
        Opcode::JNO |
        Opcode::JB |
        Opcode::JNB |
        Opcode::JZ |
        Opcode::JNZ |
        Opcode::JNA |
        Opcode::JA |
        Opcode::JS |
        Opcode::JNS |
        Opcode::JP |
        Opcode::JNP |
        Opcode::JL |
        Opcode::JGE |
        Opcode::JLE |
        Opcode::JG |
        Opcode::CMOVO |
        Opcode::CMOVNO |
        Opcode::CMOVB |
        Opcode::CMOVNB |
        Opcode::CMOVZ |
        Opcode::CMOVNZ |
        Opcode::CMOVNA |
        Opcode::CMOVA |
        Opcode::CMOVS |
        Opcode::CMOVNS |
        Opcode::CMOVP |
        Opcode::CMOVNP |
        Opcode::CMOVL |
        Opcode::CMOVGE |
        Opcode::CMOVLE |
        Opcode::CMOVG |
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
        Opcode::SETG => true,
        _ => false
    }
}

impl <
    Context: FunctionQuery<<x86_64Arch as Arch>::Address, Function=FunctionImpl<<x86_64Arch as ValueLocations>::Location>> + SymbolQuery<<x86_64Arch as Arch>::Address>,
> FunctionInstructionDisplay<x86_64Arch, Context> for x86_64Arch {
    fn display_instruction_in_function<W: fmt::Write, Highlighter: LocationHighlighter<<x86_64Arch as ValueLocations>::Location>>(
        dest: &mut W,
        instr: &<x86_64Arch as Arch>::Instruction,
        address: <x86_64Arch as Arch>::Address,
        context: &Context,
        ssa: Option<&SSA<x86_64Arch>>,
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
        if let Some(ssa) = ssa {
            if is_conditional_op(instr.opcode()) {
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
        Ok(())
    }
}

pub fn show_function<'a, 'b, 'c, 'd, 'e, M: MemoryRepr<<x86_64Arch as Arch>::Address> + MemoryRange<<x86_64Arch as Arch>::Address>>(
    data: &'a M,
    ctx: &'b MergedContextTable,
    ssa: Option<&'d SSA<x86_64Arch>>,
    fn_graph: &'c ControlFlowGraph<<x86_64Arch as Arch>::Address>,
    colors: Option<&'e ColorSettings>
) -> FunctionView<'a, 'c, 'd, 'e, FunctionImpl<<x86_64Arch as ValueLocations>::Location>, DisplayCtx<'b>, x86_64Arch, M> {
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
