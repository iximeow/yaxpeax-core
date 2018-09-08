use termion::color;
use yaxpeax_msp430_mc::{Opcode, Operand, Instruction, Width};
use std::collections::HashMap;

pub fn opcode_color(opcode: Opcode) -> &'static color::Fg<&'static color::Color> {
    match opcode {
        Opcode::Invalid(u16) => { &color::Fg(&color::Red) }
        Opcode::CALL |
        Opcode::RETI => { &color::Fg(&color::LightGreen) }
        Opcode::JNE |
        Opcode::JEQ |
        Opcode::JNC |
        Opcode::JC |
        Opcode::JN |
        Opcode::JGE |
        Opcode::JL |
        Opcode::JMP => { &color::Fg(&color::Green) }
        Opcode::MOV => { &color::Fg(&color::LightMagenta) }
        Opcode::RRA |
        Opcode::SXT |
        Opcode::PUSH |
        Opcode::AND |
        Opcode::XOR |
        Opcode::BIT |
        Opcode::BIC |
        Opcode::RRC |
        Opcode::SWPB |
        Opcode::BIS => { &color::Fg(&color::LightYellow) }
        Opcode::ADD |
        Opcode::ADDC |
        Opcode::SUBC |
        Opcode::SUB |
        Opcode::DADD |
        Opcode::CMP => { &color::Fg(&color::Yellow) }
    }
}

use arch::msp430::PartialInstructionContext;
impl <T> ::SyntaxedRender<u16, T, ()> for Instruction where T: PartialInstructionContext {
    fn render(&self, context: Option<&T>, function_table: &HashMap<u16, ()>) -> String {
        fn render_operand<T: PartialInstructionContext>(operand: &Operand, context: Option<&T>) -> String {
            fn signed_hex(num: i16) -> String {
                if num >= 0 {
                    format!("+{:#x}", num)
                } else {
                    format!("-{:#x}", -num)
                }
            }
            match operand {
                Operand::Register(0) => { "pc".to_owned() },
                Operand::Register(1) => { "sp".to_owned() },
                Operand::Register(2) => { "sr".to_owned() },
                Operand::Register(3) => { "cg".to_owned() }, // this really shouldn't come up, since any read/write to CG should normalize to const
                Operand::Register(reg) => {
                    format!("r{}", reg)
                },
                Operand::Indexed(0, offset) => {
                    format!("{}(pc)", signed_hex(*offset as i16))
                },
                Operand::Indexed(1, offset) => {
                    format!("{}(sp)", signed_hex(*offset as i16))
                },
                Operand::Indexed(2, offset) => {
                    format!("{}(sr)", signed_hex(*offset as i16))
                },
                Operand::Indexed(3, offset) => {
                    format!("{}(cg)", signed_hex(*offset as i16))
                },
                Operand::Indexed(reg, offset) => {
                    format!("{}(r{})", signed_hex(*offset as i16), reg)
                },
                Operand::RegisterIndirect(0) => { "@pc".to_owned() }
                Operand::RegisterIndirect(1) => { "@sp".to_owned() }
                Operand::RegisterIndirect(2) => { "@sr".to_owned() }
                Operand::RegisterIndirect(3) => { "@cg".to_owned() }
                Operand::RegisterIndirect(reg) => {
                    format!("@r{}", reg)
                },
                Operand::IndirectAutoinc(0) => { "@pc+".to_owned() }
                Operand::IndirectAutoinc(1) => { "@sp+".to_owned() }
                Operand::IndirectAutoinc(2) => { "@sr+".to_owned() }
                Operand::IndirectAutoinc(3) => { "@cg+".to_owned() } // this one just makes no sense
                Operand::IndirectAutoinc(reg) => {
                    format!("@r{}+", reg)
                },
                Operand::Offset(offset) => {
                    match context.and_then(|ctx| ctx.address()) {
                        Some(address) => {
                            // TODO: Uhhhh.. is this supposed to be instr len, not 2?
                            format!("{:#x}", address.wrapping_add((*offset as u16).wrapping_mul(2)).wrapping_add(2))
                        },
                        None => {
                            format!("{}(pc)", signed_hex(*offset as i16))
                        }
                    }
                },
                Operand::Symbolic(offset) => {
                    match context.and_then(|ctx| ctx.address()) {
                        Some(address) => {
                            format!("{:#x}", address.wrapping_add(*offset))
                        },
                        None => {
                            format!("{}(pc)", signed_hex(*offset as i16))
                        }
                    }
                },
                Operand::Immediate(imm) => {
                    format!("#{:#x}", imm)
                },
                Operand::Absolute(offset) => {
                    format!("&{:#x}", offset)
                },
                Operand::Const4 => {
                    "4".to_owned()
                },
                Operand::Const8 => {
                    "8".to_owned()
                },
                Operand::Const0 => {
                    "0".to_owned()
                },
                Operand::Const1 => {
                    "1".to_owned()
                },
                Operand::Const2 => {
                    "2".to_owned()
                },
                Operand::ConstNeg1 => {
                    "-1".to_owned()
                },
                Operand::Nothing => {
                    "<No Operand>".to_owned()
                }
            }
        }

        // try to recover some of the "emulated" instructions... fall back with a naive render
        match self {
            Instruction { opcode: Opcode::MOV, operands: [Operand::Const0, Operand::Const0], op_width: _ } => {
                format!("{}{}{}", color::Fg(color::Blue), "nop", color::Fg(color::Reset))
            },
            Instruction { opcode: Opcode::MOV, operands: [Operand::Const0, dest], op_width: _ } => {
                let start_color = opcode_color(Opcode::MOV);
                format!("{}{}{} {}", start_color, "clr", color::Fg(color::Reset), render_operand(&dest, context))
            },
            Instruction { opcode: Opcode::MOV, operands: [Operand::IndirectAutoinc(1), Operand::Register(0)], op_width: Width::W } => {
                // this is a pop
                let start_color = opcode_color(Opcode::CALL);
                format!("{}{}{}", start_color, "ret", color::Fg(color::Reset))
            },
            Instruction { opcode: Opcode::MOV, operands: [Operand::IndirectAutoinc(1), dest], op_width: Width::W } => {
                // this is a pop
                let start_color = opcode_color(Opcode::PUSH);
                format!("{}{}{} {}", start_color, "pop", color::Fg(color::Reset), render_operand(&dest, context))
            },
            Instruction { opcode: Opcode::MOV, operands: [src, Operand::Register(0)], op_width: Width::W } => {
                // br [src]
                let start_color = opcode_color(Opcode::JMP);
                format!("{}{}{} {}", start_color, "br", color::Fg(color::Reset), render_operand(&src, context))
            }
            x @ _ => {
                let start_color = opcode_color(self.opcode);
                let mut result = format!("{}{}{}{}", start_color, self.opcode, match self.op_width {
                    Width::W => "",
                    Width::B => ".b"
                }, color::Fg(color::Reset));

                match self.operands[0] {
                    Operand::Nothing => { return result; },
                    x @ _ => {
                        result.push(' ');
                        result.push_str(&render_operand(&x, context));
                    }
                };
                match self.operands[1] {
                    Operand::Nothing => { return result; },
                    x @ _ => {
                        result.push(',');
                        result.push(' ');
                        result.push_str(&render_operand(&x, context));
                    }
                };
                result
            }
        }
    }
}

