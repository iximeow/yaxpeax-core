use yaxpeax_arm::armv8::a64::{ARMv8, Instruction, NoContext};
use yaxpeax_arch::{Arch, ColorSettings, ShowContextual, YaxColors};

use arch::display::BaseDisplay;
use arch::arm;
use arch::FunctionQuery;
use arch::FunctionRepr;

use std::fmt;

use arch::arm::v8::MergedContextTable;

use termion::color;

impl <F: FunctionRepr, T: FunctionQuery<<ARMv8 as Arch>::Address, Function=F>> BaseDisplay<arm::v8::Function, T> for ARMv8 {
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

impl <T: std::fmt::Write, C: fmt::Display, Y: YaxColors<C>> ShowContextual<u64, MergedContextTable, C, T, Y> for Instruction {
    fn contextualize(&self, colors: &Y, address: u64, ctx: Option<&MergedContextTable>, out: &mut T) -> std::fmt::Result {
        self.contextualize(colors, address, ctx, out)
    }
}
