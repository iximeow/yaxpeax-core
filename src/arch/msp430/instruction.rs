use std::collections::HashMap;
use termion::color;
use arch::{Arch, Decodable, LengthedInstruction};

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

#[derive(Debug, Copy, Clone)]
pub struct Instruction {
    pub opcode: Opcode,
    pub op_width: Width,
    pub operands: [Operand; 2]
}

#[derive(Debug, Copy, Clone)]
pub enum Width {
    W, B
}

use std;
use std::fmt::{Display, Formatter};
impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.opcode)?;
        match self.op_width {
            Width::B => { write!(f, ".b")? },
            Width::W => { }
        };
        match self.operands[0] {
            Operand::Nothing => return Ok(()),
            x @ _ => {
                write!(f, " {}", x)?;
            }
        };
        match self.operands[1] {
            Operand::Nothing => return Ok(()),
            x @ _ => {
                write!(f, ", {}", x)?;
            }
        };
        Ok(())
    }
}

impl Instruction {
    pub fn blank() -> Instruction {
        Instruction {
            opcode: Opcode::Invalid(0xffff),
            op_width: Width::W,
            operands: [Operand::Nothing, Operand::Nothing]
        }
    }
}

impl LengthedInstruction for Instruction {
    type Unit = <::arch::msp430::MSP430 as Arch>::Address;
    fn len(&self) -> Self::Unit {
        let mut size = 2;
        match self.operands[0] {
            Operand::Indexed(_, _) |
            Operand::Symbolic(_) |
            Operand::Immediate(_) |
            Operand::Absolute(_) => { size += 2; },
            _ => {}
        };
        match self.operands[1] {
            Operand::Indexed(_, _) |
            Operand::Symbolic(_) |
            Operand::Immediate(_) |
            Operand::Absolute(_) => { size += 2; },
            _ => {}
        };
        size
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Opcode {
    Invalid(u16),
    RRC,
    SWPB,
    RRA,
    SXT,
    PUSH,
    CALL,
    RETI,
    JNE,
    JEQ,
    JNC,
    JC,
    JN,
    JGE,
    JL,
    JMP,
    MOV,
    ADD,
    ADDC,
    SUBC,
    SUB,
    CMP,
    DADD,
    BIT,
    BIC,
    BIS,
    XOR,
    AND
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Opcode::Invalid(a) => { write!(f, "invalid({:04x})", a) },
            Opcode::RRC => { write!(f, "rrc") },
            Opcode::SWPB => { write!(f, "swpb") },
            Opcode::RRA => { write!(f, "rra") },
            Opcode::SXT => { write!(f, "sxt") },
            Opcode::PUSH => { write!(f, "push") },
            Opcode::CALL => { write!(f, "call") },
            Opcode::RETI => { write!(f, "reti") },
            Opcode::JNE => { write!(f, "jne") },
            Opcode::JEQ => { write!(f, "jeq") },
            Opcode::JNC => { write!(f, "jnc") },
            Opcode::JC => { write!(f, "jc") },
            Opcode::JN => { write!(f, "jn") },
            Opcode::JGE => { write!(f, "jge") },
            Opcode::JL => { write!(f, "jl") },
            Opcode::JMP => { write!(f, "jmp") },
            Opcode::MOV => { write!(f, "mov") },
            Opcode::ADD => { write!(f, "add") },
            Opcode::ADDC => { write!(f, "addc") },
            Opcode::SUBC => { write!(f, "subc") },
            Opcode::SUB => { write!(f, "sub") },
            Opcode::CMP => { write!(f, "cmp") },
            Opcode::DADD => { write!(f, "dadd") },
            Opcode::BIT => { write!(f, "bit") },
            Opcode::BIC => { write!(f, "bic") },
            Opcode::BIS => { write!(f, "bis") },
            Opcode::XOR => { write!(f, "xor") },
            Opcode::AND => { write!(f, "and") }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Operand {
    Register(u8),
    Indexed(u8, u16),
    RegisterIndirect(u8),
    IndirectAutoinc(u8),
    Symbolic(u16),
    Immediate(u16),
    Absolute(u16),
    Offset(i16),
    Const4,
    Const8,
    Const0,
    Const1,
    Const2,
    ConstNeg1,
    Nothing
}

impl Display for Operand {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        fn signed_hex(num: i16) -> String {
            if num >= 0 {
                format!("+{:#x}", num)
            } else {
                format!("-{:#x}", -num)
            }
        }
        match self {
            Operand::Register(reg) => {
                write!(f, "R{}", reg)
            },
            Operand::Indexed(reg, offset) => {
                write!(f, "{}(R{})", signed_hex(*offset as i16), reg)
            },
            Operand::RegisterIndirect(reg) => {
                write!(f, "@R{}", reg)
            },
            Operand::IndirectAutoinc(reg) => {
                write!(f, "@R{}+", reg)
            },
            Operand::Offset(offset) => {
                write!(f, "${}", signed_hex(*offset as i16))
            },
            Operand::Symbolic(offset) => {
                write!(f, "{}(PC)", signed_hex(*offset as i16))
            },
            Operand::Immediate(imm) => {
                write!(f, "#0x{:x}", imm)
            },
            Operand::Absolute(offset) => {
                write!(f, "&0x{:x}", offset)
            },
            Operand::Const4 => {
                write!(f, "4")
            },
            Operand::Const8 => {
                write!(f, "8")
            },
            Operand::Const0 => {
                write!(f, "0")
            },
            Operand::Const1 => {
                write!(f, "1")
            },
            Operand::Const2 => {
                write!(f, "2")
            },
            Operand::ConstNeg1 => {
                write!(f, "-1")
            },
            Operand::Nothing => {
                write!(f, "<No Operand>")
            }
        }
    }
}

#[test]
fn test_decode() {
    let data = [0x02, 0x12];
    let mut instr = Instruction::blank();
    instr.decode_into(&data);
    assert!(instr.opcode == Opcode::PUSH);

    let data = [0xb1, 0x92, 0x8d, 0x49];
    let mut instr = Instruction::blank();
    instr.decode_into(&data);
    assert!(instr.opcode == Opcode::CMP);

    let data = [0x12, 0x00, 0x3f, 0x40];
    let mut instr = Instruction::blank();
    instr.decode_into(&data);
    assert!(instr.opcode == Opcode::RRC);

    let data = [0x20, 0x0e];
    let mut instr = Instruction::blank();
    instr.decode_into(&data);
    assert!(instr.opcode == Opcode::PUSH);
}
impl Decodable for Instruction {
    fn decode<'a, T: IntoIterator<Item=&'a u8>>(bytes: T) -> Option<Self> {
        let mut instr = Instruction::blank();
        match instr.decode_into(bytes) {
            Some(_) => Some(instr),
            None => None
        }
    }
    fn decode_into<'a, T: IntoIterator<Item=&'a u8>>(&mut self, bytes: T) -> Option<()> {
        let mut bytes_iter = bytes.into_iter();
        let word: Vec<&'a u8> = bytes_iter.by_ref().take(2).collect();

        let fullword = match word[..] {
            [] | [_] => { return None; },
            [low, high] => (*high as u16) << 8 | (*low as u16),
            _ => unreachable!()
        };

        fn decode_operand<'a, T: Iterator<Item=&'a u8>>(bytes: &mut T, reg: u8, mode: u8, oper: &mut Operand) -> bool {
            *oper = match reg {
                0 => {
                    if mode == 0 {
                        Operand::Register(reg)
                    } else if mode == 1 {
                        let next = match bytes.take(2).collect::<Vec<&u8>>()[..] {
                            [] | [_] => { return false; },
                            [low, high] => { ((*high as u16) << 8) | (*low as u16) },
                            _ => { unreachable!() }
                        };
                        Operand::Symbolic(next)
                    } else if mode == 2 {
                        Operand::RegisterIndirect(reg)
                    } else if mode == 3 {
                        let next = match bytes.take(2).collect::<Vec<&u8>>()[..] {
                            [] | [_] => { return false; },
                            [low, high] => { ((*high as u16) << 8) | (*low as u16) },
                            _ => { unreachable!() }
                        };
                        Operand::Immediate(next)
                    } else {
                        return false;
                    }
                },
                2 => {
                    match mode {
                        0 => { Operand::Register(reg) },
                        1 => {
                            let next = match bytes.take(2).collect::<Vec<&u8>>()[..] {
                                [] | [_] => { return false; },
                                [low, high] => { ((*high as u16) << 8) | (*low as u16) },
                                _ => { unreachable!() }
                            };
                            Operand::Absolute(next)
                        },
                        2 => { Operand::Const8 },
                        3 => { Operand::Const4 },
                        _ => { unreachable!() }
                    }
                },
                3 => {
                    match mode {
                        0 => { Operand::Const0 },
                        1 => { Operand::Const1 },
                        2 => { Operand::Const2 },
                        3 => { Operand::ConstNeg1 },
                        _ => { unreachable!() }
                    }
                },
                _ => {
                    match mode {
                        0 => { Operand::Register(reg) },
                        1 => {
                            let next = match bytes.take(2).collect::<Vec<&u8>>()[..] {
                                [] | [_] => { return false; },
                                [low, high] => { ((*high as u16) << 8) | (*low as u16) },
                                _ => { unreachable!() }
                            };
                            Operand::Indexed(reg, next)
                        },
                        2 => { Operand::RegisterIndirect(reg) },
                        3 => { Operand::IndirectAutoinc(reg) },
                        _ => { unreachable!() }
                    }
                }
            };
            return true;
        }

        self.op_width = Width::W;

        match fullword {
            /*
            instrword if instrword < 0x1000 => {
                // MSP430X instructions go here
                self.opcode = Opcode::Invalid(instrword);
                self.operands[0] = Operand::Nothing;
                self.operands[1] = Operand::Nothing;
                return None;
            }, */
            instrword if instrword < 0x2000 => {
                // microcorruption msp430 is non-standard and accepts invalid instructions..
                let (opcode_idx, operands) = ((instrword & 0x0380) >> 7, instrword & 0x7f);
                match opcode_idx {
                    x if x < 6 => {
                        self.opcode = [
                            Opcode::RRC,
                            Opcode::SWPB,
                            Opcode::RRA,
                            Opcode::SXT,
                            Opcode::PUSH,
                            Opcode::CALL
                        ][x as usize];
                        self.op_width = if operands & 0b01000000 == 0 {
                            Width::W
                        } else {
                            if x == 1 || x == 3 || x == 5 {
                                self.opcode = Opcode::Invalid(instrword);
                                return None;
                            }
                            Width:: B
                        };
                        #[allow(non_snake_case)]
                        let (As, source) = (
                            ((instrword & 0x0030) >> 4) as u8,
                            (instrword & 0x000f) as u8
                        );
                        if !decode_operand(&mut bytes_iter, source, As, &mut self.operands[0]) {
                            self.opcode = Opcode::Invalid(instrword);
                            return None;
                        };
                        self.operands[1] = Operand::Nothing;
                        Some(())
                    },
                    6 => {
                        if operands == 0 {
                            self.opcode = Opcode::RETI;
                            self.operands[0] = Operand::Nothing;
                            self.operands[1] = Operand::Nothing;
                            Some(())
                        } else {
                            self.opcode = Opcode::Invalid(instrword);
                            return None;
                        }
                    }
                    7 => {
                        self.opcode = Opcode::Invalid(instrword);
                        return None;
                    }
                    _ => {
                        unreachable!();
                    }
                }
            },
            instrword if instrword < 0x4000 => {
                let (opcode_idx, offset) = ((instrword & 0x1c00) >> 10, instrword & 0x3ff);
                self.opcode = [
                    Opcode::JNE,
                    Opcode::JEQ,
                    Opcode::JNC,
                    Opcode::JC,
                    Opcode::JN,
                    Opcode::JGE,
                    Opcode::JL,
                    Opcode::JMP
                ][opcode_idx as usize];
                self.operands[0] = Operand::Offset(((offset as i16) << 6) >> 6);
                self.operands[1] = Operand::Nothing;
                Some(())
            },
            instrword @ _ => {
                let (opcode_idx, operands) = ((instrword & 0xf000) >> 12, instrword & 0x0fff);
                self.opcode = [
                    Opcode::MOV,
                    Opcode::ADD,
                    Opcode::ADDC,
                    Opcode::SUBC,
                    Opcode::SUB,
                    Opcode::CMP,
                    Opcode::DADD,
                    Opcode::BIT,
                    Opcode::BIC,
                    Opcode::BIS,
                    Opcode::XOR,
                    Opcode::AND
                ][(opcode_idx - 4) as usize];
                self.op_width = if operands & 0b01000000 == 0 { Width::W } else { Width:: B };
                #[allow(non_snake_case)]
                let (source, Ad, As, dest) = (
                    ((instrword & 0x0f00) >> 8) as u8,
                    ((instrword & 0x0080) >> 7) as u8,
                    ((instrword & 0x0030) >> 4) as u8,
                    (instrword & 0x000f) as u8
                );
                if !decode_operand(&mut bytes_iter, source, As, &mut self.operands[0]) {
                    self.opcode = Opcode::Invalid(instrword);
                    return None;
                }
                if !decode_operand(&mut bytes_iter, dest, Ad, &mut self.operands[1]) {
                    self.opcode = Opcode::Invalid(instrword);
                    return None;
                }
                Some(())
            }
        }
    }
}

