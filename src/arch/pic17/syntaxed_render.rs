use termion::color;
use yaxpeax_arch::Arch;
use yaxpeax_pic17::PIC17;
use yaxpeax_pic17::{Opcode, Operand, Instruction};
use arch::pic17::{Function, PartialInstructionContext};
use std::collections::HashMap;

pub fn opcode_color(opcode: Opcode) -> &'static color::Fg<&'static color::Color> {
    match opcode {
        Opcode::Invalid(_, _) => &color::Fg(&color::Red),
        Opcode::NOP => &color::Fg(&color::Blue),
        Opcode::CPFSLT |
        Opcode::CPFSEQ |
        Opcode::CPFSGT |
        Opcode::TSTFSZ |
        Opcode::BTFSS |
        Opcode::BTFSC |
        Opcode::RETLW |
        Opcode::LCALL |
        Opcode::GOTO |
        Opcode::CALL |
        Opcode::RETURN => &color::Fg(&color::Green),
        Opcode::SLEEP |
        Opcode::CLRWDT |
        Opcode::RETFIE => &color::Fg(&color::Cyan),
        Opcode::MOVWF |
        Opcode::MOVFP |
        Opcode::MOVPF |
        Opcode::MOVLW |
        Opcode::MOVLB |
        Opcode::MOVLR => &color::Fg(&color::LightMagenta),
        Opcode::BSF |
        Opcode::BCF |
        Opcode::IORWF |
        Opcode::ANDWF |
        Opcode::XORWF |
        Opcode::IORLW |
        Opcode::XORLW |
        Opcode::ANDLW |
        Opcode::CLRF |
        Opcode::SETF |
        Opcode::BTG |
        Opcode::COMF |
        Opcode::RRCF |
        Opcode::RLCF |
        Opcode::RRNCF |
        Opcode::RLNCF |
        Opcode::SWAPF => &color::Fg(&color::LightYellow),

        Opcode::INFSNZ |
        Opcode::DCFSNZ |
        Opcode::DECFSZ |
        Opcode::INCFSZ |
        Opcode::SUBWFB |
        Opcode::SUBWF |
        Opcode::DECF |
        Opcode::ADDWF |
        Opcode::ADDWFC |
        Opcode::INCF |
        Opcode::MULWF |
        Opcode::NEGW |
        Opcode::DAW |
        Opcode::ADDLW |
        Opcode::SUBLW |
        Opcode::MULLW => &color::Fg(&color::Yellow),

        Opcode::TLRDL |
        Opcode::TLRDH |
        Opcode::TLWTL |
        Opcode::TLWTH |
        Opcode::TABLRDL |
        Opcode::TABLRDLI |
        Opcode::TABLRDH |
        Opcode::TABLRDHI |
        Opcode::TABLWTL |
        Opcode::TABLWTLI |
        Opcode::TABLWTH |
        Opcode::TABLWTHI => &color::Fg(&color::Magenta),
    }
}

impl <T> ::SyntaxedRender<<PIC17 as Arch>::Address, T, Function> for Instruction where T: PartialInstructionContext {
    fn render(&self, context: Option<&T>, function_table: &HashMap<<PIC17 as Arch>::Address, Function>) -> String {
        use analyses::control_flow;
        use analyses::control_flow::Determinant;

        let start_color = opcode_color(self.opcode);
        let mut result = format!("{}{}{}", start_color, self.opcode, color::Fg(color::Reset));

        fn render_function(address: <PIC17 as Arch>::Address, function_table: &HashMap<<PIC17 as Arch>::Address, Function>) -> String {
            match function_table.get(&address) {
                Some(fn_dec) => {
                    format!("{}{}{}",
                        color::Fg(&color::LightYellow as &color::Color),
                        fn_dec.decl_string(),
                        color::Fg(&color::Reset as &color::Color)
                    )
                },
                None => { format!("#{:08x}", address) }
            }
        }

        fn render_operand<T>(operand: &Operand, context: Option<&T>) -> String where T: PartialInstructionContext {
            match operand {
                Operand::ImmediateU8(i) => {
                    format!("#{:02x}", i)
                },
                Operand::ImmediateU32(i) => {
                    format!("#{:08x}", i)
                },
                Operand::File(f) => {
                    let name = match ::arch::pic17::cpu::try_debank(*f, context) {
                        Some(num) => ::arch::pic17::named_file(num).to_string(),
                        None => format!("[banked 0x{:02x}]", f)
                    };
                    format!("{}{}{}", color::Fg(color::Yellow), name, color::Fg(color::Reset))
                },
                Operand::W => {
                    format!("{}W{}", color::Fg(color::Yellow), color::Fg(color::Reset))
                },
                _ => {
                    "".to_owned()
                }
            }
        }

        match self.opcode {
            Opcode::LCALL => {
                result.push_str(" ");

                let control_flow = self.control_flow(context);
                match control_flow.dest {
                    Some(control_flow::Target::Absolute(addr)) => {
                        result.push_str(&render_function(addr * 2, function_table));
                    }
                    Some(control_flow::Target::Indeterminate) => {
                        match self.operands[0] {
                            Operand::ImmediateU8(i) => {
                                // TODO: distinguish from pclath==0 lcall and pclath==??? lcall
                                result.push_str(&format!("#{:08x}", (i as u16) * 2));
                            },
                            // LCALL's first operand is always a u8
                            _ => { unreachable!(); }
                        }
                    },
                    // LCALL always has control flow, and either it's
                    // an absolute destination, or unknown (contextual)
                    _ => { unreachable!(); }
                }
            },
            Opcode::CALL |
            Opcode::GOTO => {
                match self.operands[0] {
                    Operand::ImmediateU32(i) => {
                        result.push(' ');
                        result.push_str(&render_function((i as u16) * 2, function_table));
                    },
                    _ => {
                        unreachable!()
                    }
                };
            },
            _ => {
                match self.operands[0] {
                    Operand::Nothing => {
                        return result;
                    },
                    x @ _ => {
                        result.push(' ');
                        result.push_str(&render_operand(&x, context));
                    }
                };

                match self.operands[1] {
                    Operand::Nothing => {
                        return result;
                    },
                    x @ _ => {
                        result.push(',');
                        result.push(' ');
                        result.push_str(&render_operand(&x, context));
                    }
                };
            }
        };
        result
    }
}
