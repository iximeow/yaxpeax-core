use yaxpeax_arm::armv7::{ARMv7, Instruction};
use yaxpeax_arch::{AddressDisplay, Arch, ColorSettings, ShowContextual};

use arch::display::BaseDisplay;
use arch::arm;
use std::collections::HashMap;

use arch::arm::v7::MergedContextTable;

use termion::color;

impl <T: arm::v7::PartialInstructionContext> BaseDisplay<arm::v7::Function, T> for ARMv7 {
    fn render_frame<Data: Iterator<Item=u8>>(
        addr: <ARMv7 as Arch>::Address,
        instr: &<ARMv7 as Arch>::Instruction,
        bytes: &mut Data,
        ctx: Option<&T>,
        function_table: &HashMap<<ARMv7 as Arch>::Address, arm::v7::Function>
    ) {
        /*
         * if there's a comment, show that
         */
        if let Some(fn_dec) = function_table.get(&addr) {
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

impl <T: std::fmt::Write> ShowContextual<u32, MergedContextTable, T> for Instruction {
    fn contextualize(&self, colors: Option<&ColorSettings>, address: u32, context: Option<&MergedContextTable>, out: &mut T) -> std::fmt::Result {
        let ctxs: Vec<Option<String>> = vec![];
        self.contextualize(colors, address, Some(&ctxs[..]), out)
    }
}
