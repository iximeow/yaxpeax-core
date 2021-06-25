use yaxpeax_arm::armv7::{ARMv7, Instruction, ConditionedOpcode, Operand, Opcode};
use yaxpeax_arch::{Arch, AddressDisplay, ColorSettings, Colorize, Decoder, LengthedInstruction, ShowContextual, YaxColors};

use arch::display::BaseDisplay;
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

// TODO:
#[allow(dead_code)]
#[allow(unreachable_code)]
#[allow(unused_variables)]
impl <
    'a, 'b, 'c, 'd, 'e,
    Context: SymbolQuery<<ARMv7 as Arch>::Address> + FunctionQuery<<ARMv7 as Arch>::Address, Function=FunctionImpl<<ARMv7 as ValueLocations>::Location>>,
    Highlighter: LocationHighlighter<<ARMv7 as ValueLocations>::Location>
> fmt::Display for InstructionContext<'a, 'b, 'c, 'd, 'e, Context, Highlighter> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!("contextualized display of armv7 instructions");
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
