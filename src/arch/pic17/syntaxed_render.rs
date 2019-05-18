use termion::color;
use yaxpeax_arch::Arch;
use yaxpeax_arch::ColorSettings;
use yaxpeax_arch::ShowContextual;
use yaxpeax_pic17::PIC17;
use yaxpeax_pic17::{Opcode, Operand, Instruction};
use arch::pic17::MergedContextTable;

use ContextRead;

use std::fmt;

impl <T: fmt::Write> ShowContextual<u16, MergedContextTable, T> for Instruction {
    fn contextualize(&self, colors: Option<&ColorSettings>, address: <PIC17 as Arch>::Address, context: Option<&MergedContextTable>, out: &mut T) -> fmt::Result {
        let operand_replacements: Option<[Option<String>; 2]> = match context {
            Some(context) => {
                use analyses::control_flow;
                use analyses::control_flow::Determinant;

                let mut operand_replacements: [Option<String>; 2] = [None, None];

                // Don't look...
                fn render_function(address: <PIC17 as Arch>::Address /*, function_table: &HashMap<<PIC17 as Arch>::Address, Function>*/) -> String {
                    /*
                    match function_table.get(&address) {
                        Some(fn_dec) => {
                            format!("{}{}{}",
                                color::Fg(&color::LightYellow as &color::Color),
                                fn_dec.decl_string(),
                                color::Fg(&color::Reset as &color::Color)
                            )
                        },
                        None => {*/ format!("#{:08x}", address) //}
                    //}
                }

                fn contextualize_operand(operand: &Operand, address: <PIC17 as Arch>::Address, context: Option<&MergedContextTable>) -> Option<String> {
                    match operand {
                        Operand::File(f) => {
                            let name = ::arch::pic17::cpu::try_debank(*f, context.map(|x| x.at(&address)).as_ref())
                                .map(|num| ::arch::pic17::named_file(num).to_string());
                            name.map(|name| {
                                format!("{}{}{}", color::Fg(color::Yellow), name, color::Fg(color::Reset))
                            })
                        },
                        _ => {
                            None
                        }
                    }
                }

                match self.opcode {
                    Opcode::LCALL => {
                        let control_flow = self.control_flow(Some(&context.at(&address)));
                        match control_flow.dest {
                            Some(control_flow::Target::Absolute(addr)) => {
                                operand_replacements[0] = Some(render_function(addr * 2));
                            }
                            _ => { }
                        }
                    },
                    Opcode::CALL |
                    Opcode::GOTO => {
                        match self.operands[0] {
                            Operand::ImmediateU32(i) => {
                                operand_replacements[0] = Some(render_function((i as u16) * 2));
                            },
                            _ => { unreachable!() }
                        }
                    },
                    _ => {
                        match &self.operands[0] {
                            Operand::Nothing => { },
                            x @ _ => { operand_replacements[0] = contextualize_operand(x, address, Some(context)) }
                        };
                        match &self.operands[0] {
                            Operand::Nothing => { },
                            x @ _ => { operand_replacements[0] = contextualize_operand(x, address, Some(context)) }
                        };
                    }
                };
                Some(operand_replacements)
            }
            None => {
                None
            }
        };
        let reffified: Option<&[Option<String>]> = operand_replacements.as_ref().map(|x| &x[..]);
        self.contextualize(colors, address, reffified, out)
    }
}
