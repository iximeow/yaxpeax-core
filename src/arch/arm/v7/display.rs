use yaxpeax_arm::armv7::{ARMv7, Instruction};
use yaxpeax_arch::{Arch, ColorSettings, ShowContextual};

use arch::display::BaseDisplay;
use arch::arm;
use std::collections::HashMap;
use std::fmt;

use arch::CommentQuery;
use arch::FunctionQuery;
use arch::FunctionRepr;
use arch::arm::v7::MergedContextTable;

use termion::color;

impl <T: FunctionQuery<<ARMv7 as Arch>::Address> + CommentQuery<<ARMv7 as Arch>::Address>> BaseDisplay<arm::v7::Function, T> for ARMv7 {
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
                );
            }
            if let Some(fn_dec) = ctx.function_at(addr) {
                writeln!(dest, "      {}{}{}",
                    color::Fg(&color::LightYellow as &color::Color),
                    // TODO: show values?
                    fn_dec.decl_string(false),
                    color::Fg(&color::Reset as &color::Color)
                );
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

impl <T: std::fmt::Write> ShowContextual<u32, MergedContextTable, T> for Instruction {
    fn contextualize(&self, colors: Option<&ColorSettings>, address: u32, _context: Option<&MergedContextTable>, out: &mut T) -> std::fmt::Result {
        let ctxs: Vec<Option<String>> = vec![];
        self.contextualize(colors, address, Some(&ctxs[..]), out)
    }
}
