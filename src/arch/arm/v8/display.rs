use yaxpeax_arm::armv8::a64::{ARMv8, Instruction};
use yaxpeax_arch::{Arch, ColorSettings, ShowContextual};

use arch::display::BaseDisplay;
use arch::arm;
use std::collections::HashMap;

use arch::arm::v8::MergedContextTable;

use termion::color;

impl <T: arm::v8::PartialInstructionContext> BaseDisplay<arm::v8::Function, T> for ARMv8 {
    fn render_frame<Data: Iterator<Item=u8>>(
        addr: <ARMv8 as Arch>::Address,
        _instr: &<ARMv8 as Arch>::Instruction,
        bytes: &mut Data,
        ctx: Option<&T>,
        function_table: &HashMap<<ARMv8 as Arch>::Address, arm::v8::Function>
    ) {
        /*
         * if there's a comment, show that
         */
        // TODO: totally replace this?
        if let Some(_fn_dec) = function_table.get(&addr) {
            println!("      {}{}{}",
                color::Fg(&color::LightYellow as &color::Color),
                "___",
                color::Fg(&color::Reset as &color::Color)
            );
        }
        print!(
            "{:08x}: {}{}{}{}: |{}|",
                addr,
                bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
                bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
                bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
                bytes.next().map(|x| format!("{:02x}", x)).unwrap_or("  ".to_owned()),
                ctx.map(|c| c.indicator_tag()).unwrap_or(" ")
        );
    }
}

impl <T: std::fmt::Write> ShowContextual<u64, MergedContextTable, T> for Instruction {
    fn contextualize(&self, colors: Option<&ColorSettings>, address: u64, _context: Option<&MergedContextTable>, out: &mut T) -> std::fmt::Result {
        let ctxs: Vec<Option<String>> = vec![];
        self.contextualize(colors, address, Some(&ctxs[..]), out)
    }
}
