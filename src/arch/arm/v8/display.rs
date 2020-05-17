use yaxpeax_arm::armv8::a64::{ARMv8, Instruction, NoContext};
use yaxpeax_arch::{Arch, ColorSettings, Decoder, LengthedInstruction, ShowContextual, YaxColors};

use arch::display::BaseDisplay;
use arch::display::function::FunctionInstructionDisplay;
use arch::display::function::FunctionView;
use arch::arm;
use arch::arm::v8::DisplayCtx;
use arch::CommentQuery;
use arch::FunctionQuery;
use arch::FunctionImpl;
use arch::FunctionRepr;
use arch::AddressNamer;
use arch::SymbolQuery;
use display::location::{NoHighlights, LocationHighlighter};
use analyses::static_single_assignment::SSA;
use data::ValueLocations;

use memory::MemoryRange;
use memory::MemoryRepr;
use analyses::control_flow::ControlFlowGraph;

use std::marker::PhantomData;
use std::fmt;

use arch::arm::v8::MergedContextTable;

use termion::color;

impl <F: FunctionRepr, T: FunctionQuery<<ARMv8 as Arch>::Address, Function=F> + CommentQuery<<ARMv8 as Arch>::Address>> BaseDisplay<F, T> for ARMv8 {
    fn render_frame<Data: Iterator<Item=u8>, W: fmt::Write>(
        dest: &mut W,
        addr: <ARMv8 as Arch>::Address,
        _instr: &<ARMv8 as Arch>::Instruction,
        bytes: &mut Data,
        ctx: Option<&T>,
    ) -> fmt::Result {
        /*
         * if there's a comment, show that
         */
        // TODO: totally replace this?
        if let Some(ctx) = ctx {
            if let Some(fn_dec) = ctx.function_at(addr) {
                write!(dest, "      {}{}{}",
                    color::Fg(&color::LightYellow as &dyn color::Color),
                    // TODO: show locations?
                    fn_dec.decl_string(false),
                    color::Fg(&color::Reset as &dyn color::Color)
                )?;
            }
        }
        write!(
            dest, "{:08x}: {}{}{}{}: | |",
                addr,
                bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
                bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
                bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
                bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
//                ctx.map(|c| c.indicator_tag()).unwrap_or(" ")
        )
    }
}

pub struct InstructionContext<'a, 'b, 'c, 'd, 'e, Context: AddressNamer<<ARMv8 as Arch>::Address>, Highlighter: LocationHighlighter<<ARMv8 as ValueLocations>::Location>> {
    instr: &'a Instruction,
    addr: <ARMv8 as Arch>::Address,
    contexts: Option<&'b Context>,
    ssa: Option<&'c SSA<ARMv8>>,
    colors: Option<&'d ColorSettings>,
    highlight: &'e Highlighter,
}

impl <T: std::fmt::Write, C: fmt::Display, Y: YaxColors<C>> ShowContextual<u64, MergedContextTable, C, T, Y> for Instruction {
    fn contextualize(&self, colors: &Y, address: u64, ctx: Option<&MergedContextTable>, out: &mut T) -> std::fmt::Result {
        self.contextualize(colors, address, ctx, out)
    }
}

impl <
    'a, 'b, 'c, 'd, 'e,
    Context: SymbolQuery<<ARMv8 as Arch>::Address> + FunctionQuery<<ARMv8 as Arch>::Address, Function=FunctionImpl<<ARMv8 as ValueLocations>::Location>>,
    Highlighter: LocationHighlighter<<ARMv8 as ValueLocations>::Location>
> fmt::Display for InstructionContext<'a, 'b, 'c, 'd, 'e, Context, Highlighter> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", self.instr)
    }
}

pub fn show_instruction<M: MemoryRange<<ARMv8 as Arch>::Address>>(
    data: &M,
    ctx: &MergedContextTable,
    address: <ARMv8 as Arch>::Address,
    colors: Option<&ColorSettings>
) {
    match <ARMv8 as Arch>::Decoder::default().decode(data.range_from(address).unwrap()) {
        Ok(instr) => {
            let mut instr_text = String::new();
            ARMv8::render_frame(
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
    Context: FunctionQuery<<ARMv8 as Arch>::Address, Function=FunctionImpl<<ARMv8 as ValueLocations>::Location>> + SymbolQuery<<ARMv8 as Arch>::Address>,
> FunctionInstructionDisplay<ARMv8, Context> for ARMv8 {
    fn display_instruction_in_function<W: fmt::Write, Highlighter: LocationHighlighter<<ARMv8 as ValueLocations>::Location>>(
        dest: &mut W,
        instr: &<ARMv8 as Arch>::Instruction,
        address: <ARMv8 as Arch>::Address,
        context: &Context,
        ssa: Option<&SSA<ARMv8>>,
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

pub fn show_function<'a, 'b, 'c, 'd, 'e, M: MemoryRepr<<ARMv8 as Arch>::Address> + MemoryRange<<ARMv8 as Arch>::Address>>(
    data: &'a M,
    ctx: &'b MergedContextTable,
    ssa: Option<&'d SSA<ARMv8>>,
    fn_graph: &'c ControlFlowGraph<<ARMv8 as Arch>::Address>,
    colors: Option<&'e ColorSettings>
) -> FunctionView<'a, 'c, 'd, 'e, FunctionImpl<<ARMv8 as ValueLocations>::Location>, DisplayCtx<'b>, ARMv8, M> {
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
