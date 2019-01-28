use arch::{Arch, Decodable, LengthedInstruction};
use termion::color;

use arch::pic17;
use arch::pic17::{PIC17, PartialInstructionContext, SFRS};

use std::collections::HashMap;

#[derive(Debug, Copy, Clone)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operands: [Operand; 2]
}

use std;
use std::fmt::{Display, Formatter};
impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.opcode)?;
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

impl <T> ::SyntaxedRender<<PIC17 as Arch>::Address, T, pic17::Function> for Instruction where T: PartialInstructionContext {
    fn render(&self, context: Option<&T>, function_table: &HashMap<<PIC17 as Arch>::Address, pic17::Function>) -> String {
        use analyses::control_flow;
        use analyses::control_flow::Determinant;

        let start_color = opcode_color(self.opcode);
        let mut result = format!("{}{}{}", start_color, self.opcode, color::Fg(color::Reset));

        fn render_function(address: <PIC17 as Arch>::Address, function_table: &HashMap<<PIC17 as Arch>::Address, pic17::Function>) -> String {
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

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub enum Dependence {
    W, // aka the WREG SFR, knowing this should imply knowing that memory and vice versa
    Memory(u16), // just as the CPU, linear memory. may be SFR or data.
    Program(u16), // some word of program memory (tblrd)
    Stack(u8), // some value off the stack (return). index is relative to TOS.
    Carry, // depends on the carry bit. we might know this while not knowing ALUSTA.
    BSR_SFR, // the half of BSR used for bank selection of SFRs.
    BSR_GPR, // the half of BSR used for bank selection of general memory.
    Unknown // depends on something, we don't know what.
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum Update {
    W,
    Memory(u16),
    Program(u16),
    Carry,
    BSR_SFR,
    BSR_GPR,
    Unknown
}

impl LengthedInstruction for Instruction {
    type Unit = <PIC17 as Arch>::Address;
    fn len(&self) -> Self::Unit {
        match self.opcode {
            _ => 2
        }
    }
}

impl Instruction {
    pub fn blank() -> Instruction {
        Instruction {
            opcode: Opcode::NOP,
            operands: [Operand::Nothing, Operand::Nothing]
        }
    }

    pub fn is_call(&self) -> bool {
        match self.opcode {
            Opcode::CALL | Opcode::LCALL => { true },
            _ => { false }
        }
    }

    pub fn updates<T>(&self, ctx: &T) -> Vec<Update> where T: PartialInstructionContext {
        fn register_updates<T>(op: Operand, ctx: &T) -> Vec<Update> where T: PartialInstructionContext {
            use arch::pic17::cpu::try_debank;
            match op {
                Operand::W => { vec![Update::W] },
                Operand::File(addr) => {
                    let initial_addr = try_debank(addr, Some(ctx));
                    let inner_addr = initial_addr.and_then(|debanked| match debanked {
                        SFRS::INDF0 => { ctx.memory(SFRS::FSR0).and_then(|addr| { try_debank(addr, Some(ctx)) }) },
                        SFRS::INDF1 => { ctx.memory(SFRS::FSR1).and_then(|addr| { try_debank(addr, Some(ctx)) }) },
                        x @ _ => { Some(x) }
                    });
                    match (initial_addr, inner_addr) {
                        (None, _) |
                        (_, None) => { vec![Update::Unknown] },
                        (_, Some(SFRS::INDF0)) |
                        (_, Some(SFRS::INDF1)) => { vec![] },
                        (Some(SFRS::INDF0), Some(x)) |
                        (Some(SFRS::INDF1), Some(x)) => { vec![Update::Memory(x)] },
                        (Some(x), Some(_)) => { vec![Update::Memory(x)] }
                    }
                },
                Operand::Nothing => { vec![] },
                _ => { unreachable!() }
            }
        }
        match self.opcode {
            Opcode::GOTO |
            Opcode::CALL |
            Opcode::SLEEP |
            Opcode::CLRWDT | // TODO: Verify.
            Opcode::NOP => { vec![] }
            Opcode::MOVLW |
            Opcode::RETLW => {
                vec![Update::W]
            },
            Opcode::MOVLB => { vec![Update::BSR_SFR] },
            Opcode::MOVLR => { vec![Update::BSR_GPR] },
            Opcode::RETURN |
            Opcode::RETFIE => { vec![] } // TODO: stack??
            Opcode::RRCF |
            Opcode::RLCF |
            Opcode::SUBWFB |
            Opcode::ADDWFC => {
                let mut updates = register_updates(self.operands[1], ctx);
                updates.push(Update::Carry);
                updates
            }
            Opcode::LCALL => {
                vec![]
            }
            Opcode::MOVWF => {
                register_updates(self.operands[0], ctx)
            }
            Opcode::MOVFP |
            Opcode::MOVPF => {
                // TODO: handle cases where, say, source is INDF_ with inc/dec
                register_updates(self.operands[1], ctx)
            }
            Opcode::ADDLW |
            Opcode::SUBLW |
            Opcode::IORLW |
            Opcode::XORLW |
            Opcode::ANDLW => {
                vec![Update::W, Update::Carry]
            }
            Opcode::MULLW => {
                vec![Update::W, Update::Memory(SFRS::PRODL), Update::Memory(SFRS::PRODH)]
            }
            Opcode::TSTFSZ |
            Opcode::BTFSS |
            Opcode::BTFSC => {
                // TODO: handle cases where source is INDF_ with inc/dec
                vec![]
            }

            Opcode::BTG |
            Opcode::BSF |
            Opcode::BCF => {
                register_updates(self.operands[0], ctx)
            }

            Opcode::CLRF => {
                let mut updates = register_updates(self.operands[0], ctx);
                match self.operands[1] {
                    Operand::W => {
                        updates.push(Update::W);
                    }
                    _ => { }
                };
                updates
            }
            Opcode::TLRDL |
            Opcode::TLRDH |
            Opcode::TABLRDL |
            Opcode::TABLRDH => {
                register_updates(self.operands[0], ctx)
            }
            Opcode::TABLRDLI |
            Opcode::TABLRDHI => {
                let mut dest = register_updates(self.operands[0], ctx);
                dest.push(Update::Memory(SFRS::TBLPTRL));
                dest.push(Update::Memory(SFRS::TBLPTRH));
                dest
            }
            Opcode::TLWTL |
            Opcode::TLWTH |
            Opcode::TABLWTL |
            Opcode::TABLWTH => {
                vec![]
            },
            Opcode::TABLWTLI |
            Opcode::TABLWTHI => {
                vec![Update::Memory(SFRS::TBLPTRL), Update::Memory(SFRS::TBLPTRH)]
            }

            Opcode::INFSNZ |
            Opcode::DCFSNZ |
            Opcode::DECFSZ |
            Opcode::INCFSZ |
            Opcode::DECF |
            Opcode::INCF |
            Opcode::SUBWF |
            Opcode::IORWF |
            Opcode::ANDWF |
            Opcode::XORWF |
            Opcode::ADDWF => {
                // TODO: C, Z,...
                register_updates(self.operands[1], ctx)
            }
            Opcode::MULWF => {
                vec![Update::Memory(SFRS::PRODL), Update::Memory(SFRS::PRODH)]
            }

            Opcode::NEGW |
            Opcode::CPFSLT |
            Opcode::CPFSEQ |
            Opcode::CPFSGT |
            Opcode::COMF |
            Opcode::SWAPF |
            Opcode::RRNCF |
            Opcode::RLNCF |
            Opcode::SETF |

            Opcode::DAW | // TODO: yeah, phoning this one in.
            Opcode::Invalid(_, _) => {
                // TODO: more precise definitions here..
                vec![Update::Unknown]
            }
        }
    }

    // TODO: Failing a symbolic implementation of CPU,
    // the next best thing is being able to identify all src/dep of instructions...
    // TODO: does this add gross indirection to accesses of ctx.<member>()?
    //          f.ex bsr_sfr() should be inline-able...
    pub fn dependencies<T: PartialInstructionContext>(&self, ctx: &T) -> Vec<Dependence> {
        fn register_write_deps<T: PartialInstructionContext>(addr: u8, ctx: &T) -> Vec<Dependence> {
            if addr < 0x10 {
                if addr == 0x00 {
                    vec![Dependence::Memory(SFRS::FSR0), Dependence::BSR_GPR]
                } else if addr == 0x08 {
                    vec![Dependence::Memory(SFRS::FSR1), Dependence::BSR_GPR]
                } else {
                    vec![]
                }
            } else if addr < 0x20 {
                ctx.bsr_sfr().map(|_| {
                    vec![]
                }).unwrap_or_else(|| {
                    vec![Dependence::BSR_SFR, Dependence::Unknown]
                })
            } else {
                ctx.bsr_gpr().map(|_| {
                    vec![]
                }).unwrap_or_else(|| {
                    vec![Dependence::BSR_GPR, Dependence::Unknown]
                })
            }
        }
        fn register_deps(addr: u8, ctx: &PartialInstructionContext) -> Vec<Dependence> {
            if addr < 0x10 {
                if addr == 0x00 {
                    vec![Dependence::Memory(SFRS::FSR0), Dependence::BSR_GPR]
                } else if addr == 0x08 {
                    vec![Dependence::Memory(SFRS::FSR1), Dependence::BSR_GPR]
                } else {
                    vec![Dependence::Memory(addr as u16)]
                }
            } else if addr < 0x20 {
                ctx.bsr_sfr().map(|bsr| {
                    let full_addr = addr as u16 | ((bsr as u16) << 8);
                    vec![Dependence::Memory(full_addr)]
                }).unwrap_or_else(|| {
                    vec![Dependence::BSR_SFR, Dependence::Unknown]
                })
            } else {
                ctx.bsr_gpr().map(|bsr| {
                    let full_addr = addr as u16 | ((bsr as u16) << 8);
                    vec![Dependence::Memory(full_addr)]
                }).unwrap_or_else(|| {
                    vec![Dependence::BSR_GPR, Dependence::Unknown]
                })
            }
        }
        match self.opcode {
            Opcode::GOTO |
            Opcode::CALL |
            Opcode::MOVLB |
            Opcode::MOVLR |
            Opcode::MOVLW |
            Opcode::SLEEP |
            Opcode::CLRWDT | // TODO: Verify.
            Opcode::NOP => { vec![] }
            Opcode::RETLW |
            Opcode::RETURN |
            Opcode::RETFIE => { vec![Dependence::Stack(0)] }
            Opcode::SUBWFB |
            Opcode::ADDWFC => {
                let mut deps = register_deps(self.operands[0].file_value(), ctx);
                deps.push(Dependence::W);
                deps.push(Dependence::Carry);
                deps
            }
            Opcode::RRCF |
            Opcode::RLCF => {
                let mut deps = register_deps(self.operands[0].file_value(), ctx);
                deps.push(Dependence::Carry);
                deps
            }
            Opcode::LCALL => {
                vec![Dependence::Memory(SFRS::PCLATH)]
            }
            Opcode::MOVWF => {
                let mut deps = register_write_deps(self.operands[0].file_value(), ctx);
                deps.push(Dependence::W);
                deps
            }
            Opcode::NEGW |
            Opcode::CPFSLT |
            Opcode::CPFSEQ |
            Opcode::CPFSGT |
            Opcode::SUBWF |
            Opcode::IORWF |
            Opcode::ANDWF |
            Opcode::XORWF |
            Opcode::ADDWF |
            Opcode::MULWF => {
                let mut deps = register_deps(self.operands[0].file_value(), ctx);
                deps.push(Dependence::W);
                deps
            }
            Opcode::DECF |
            Opcode::COMF |
            Opcode::INCF |
            Opcode::DECFSZ |
            Opcode::SWAPF |
            Opcode::INCFSZ |
            Opcode::RRNCF |
            Opcode::RLNCF |
            Opcode::INFSNZ |
            Opcode::DCFSNZ |
            Opcode::CLRF |
            Opcode::SETF => {
                register_deps(self.operands[0].file_value(), ctx)
            }

            Opcode::BTG |
            Opcode::TSTFSZ |
            Opcode::BSF |
            Opcode::BCF |
            Opcode::BTFSS |
            Opcode::BTFSC => {
                register_deps(self.operands[0].file_value(), ctx)
            }

            Opcode::MOVFP |
            Opcode::MOVPF => {
                let mut op0_deps = register_deps(self.operands[0].file_value(), ctx);
                let mut op1_deps = register_write_deps(self.operands[1].file_value(), ctx);
                op0_deps.append(&mut op1_deps);
                op0_deps
            }

            Opcode::ADDLW |
            Opcode::SUBLW |
            Opcode::IORLW |
            Opcode::XORLW |
            Opcode::ANDLW |
            Opcode::MULLW => {
                vec![Dependence::W]
            }

            Opcode::DAW | // TODO: yeah, phoning this one in.
            Opcode::Invalid(_, _) |
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
            Opcode::TABLWTHI => {
                // TODO: more precise definitions here..
                vec![Dependence::Unknown]
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Opcode {
    Invalid(u8, u8),
    NOP,
    RETURN,
    SLEEP,
    CLRWDT,
    RETFIE,
    MOVWF,
    SUBWFB,
    SUBWF,
    DECF,
    IORWF,
    ANDWF,
    XORWF,
    ADDWF,
    ADDWFC,
    COMF,
    INCF,
    DECFSZ,
    RRCF,
    RLCF,
    SWAPF,
    INCFSZ,
    RRNCF,
    RLNCF,
    INFSNZ,
    DCFSNZ,
    CLRF,
    SETF,
    NEGW,
    DAW,
    BTG,
    CPFSLT,
    CPFSEQ,
    CPFSGT,
    MULWF,
    TSTFSZ,
    BSF,
    BCF,
    BTFSS,
    BTFSC,
    MOVFP,
    MOVPF,
    MOVLW,
    ADDLW,
    SUBLW,
    IORLW,
    XORLW,
    ANDLW,
    RETLW,
    LCALL,
    MOVLB,
    MOVLR,
    MULLW,
    TLRDL,
    TLRDH,
    TLWTL,
    TLWTH,
    TABLRDL,
    TABLRDLI,
    TABLRDH,
    TABLRDHI,
    TABLWTL,
    TABLWTLI,
    TABLWTH,
    TABLWTHI,
    GOTO,
    CALL
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Opcode::Invalid(a, b) => { write!(f, "invalid({:02x}{:02x})", a, b) },
            Opcode::NOP => { write!(f, "nop") },
            Opcode::RETURN => { write!(f, "return") },
            Opcode::SLEEP => { write!(f, "sleep") },
            Opcode::CLRWDT => { write!(f, "clrwdt") },
            Opcode::RETFIE => { write!(f, "retfie") },
            Opcode::MOVFP => { write!(f, "movfp") },
            Opcode::MOVPF => { write!(f, "movpf") },
            Opcode::TLRDL => { write!(f, "tlrdl") },
            Opcode::TLRDH => { write!(f, "tlrdh") },
            Opcode::TLWTL => { write!(f, "tlwtl") },
            Opcode::TLWTH => { write!(f, "tlwth") },
            Opcode::TABLRDL => { write!(f, "tablrdl") },
            Opcode::TABLRDLI => { write!(f, "tablrdli") },
            Opcode::TABLRDH => { write!(f, "tablrdh") },
            Opcode::TABLRDHI => { write!(f, "tablrdhi") },
            Opcode::TABLWTL => { write!(f, "tablwtl") },
            Opcode::TABLWTLI => { write!(f, "tablwtli") },
            Opcode::TABLWTH => { write!(f, "tablwth") },
            Opcode::TABLWTHI => { write!(f, "tablwthi") },
            Opcode::MOVWF => { write!(f, "movwf") },
            Opcode::SUBWFB => { write!(f, "subwfb") },
            Opcode::SUBWF => { write!(f, "subwf") },
            Opcode::DECF => { write!(f, "decf") },
            Opcode::IORWF => { write!(f, "iorwf") },
            Opcode::ANDWF => { write!(f, "andwf") },
            Opcode::XORWF => { write!(f, "xorwf") },
            Opcode::ADDWF => { write!(f, "addwf") },
            Opcode::ADDWFC => { write!(f, "addwfc") },
            Opcode::COMF => { write!(f, "comf") },
            Opcode::INCF => { write!(f, "incf") },
            Opcode::DECFSZ => { write!(f, "decfsz") },
            Opcode::RRCF => { write!(f, "rrcf") },
            Opcode::RLCF => { write!(f, "rlcf") },
            Opcode::SWAPF => { write!(f, "swapf") },
            Opcode::INCFSZ => { write!(f, "incfsz") },
            Opcode::RRNCF => { write!(f, "rrncf") },
            Opcode::RLNCF => { write!(f, "rlncf") },
            Opcode::INFSNZ => { write!(f, "infsnz") },
            Opcode::DCFSNZ => { write!(f, "dcfsnz") },
            Opcode::CLRF => { write!(f, "clrf") },
            Opcode::SETF => { write!(f, "setf") },
            Opcode::NEGW => { write!(f, "negw") },
            Opcode::DAW => { write!(f, "daw") },
            Opcode::BTG => { write!(f, "btg") },
            Opcode::CPFSLT => { write!(f, "cpfslt") },
            Opcode::CPFSEQ => { write!(f, "cpfseq") },
            Opcode::CPFSGT => { write!(f, "cpfsgt") },
            Opcode::MULWF => { write!(f, "mulwf") },
            Opcode::TSTFSZ => { write!(f, "tstfsz") },
            Opcode::BSF => { write!(f, "bsf") },
            Opcode::BCF => { write!(f, "bcf") },
            Opcode::BTFSS => { write!(f, "btfss") },
            Opcode::BTFSC => { write!(f, "btfsc") },
            Opcode::MOVLW => { write!(f, "movlw") },
            Opcode::ADDLW => { write!(f, "addlw") },
            Opcode::SUBLW => { write!(f, "sublw") },
            Opcode::IORLW => { write!(f, "iorlw") },
            Opcode::XORLW => { write!(f, "xorlw") },
            Opcode::ANDLW => { write!(f, "andlw") },
            Opcode::RETLW => { write!(f, "retlw") },
            Opcode::LCALL => { write!(f, "lcall") },
            Opcode::MOVLB => { write!(f, "movlb") },
            Opcode::MOVLR => { write!(f, "movlr") },
            Opcode::MULLW => { write!(f, "mullw") },
            Opcode::GOTO => { write!(f, "goto") },
            Opcode::CALL => { write!(f, "call") }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Operand {
    ImmediateU8(u8),
    ImmediateU32(u32),
    File(u8),
    W,
    Nothing
}

impl Operand {
    pub fn file_value(&self) -> u8 {
        match self {
            Operand::File(f) => *f,
            _ => { unreachable!() }
        }
    }
    pub fn imm8_value(&self) -> u8 {
        match self {
            Operand::ImmediateU8(i) => *i,
            _ => { unreachable!() }
        }
    }
    pub fn imm32_value(&self) -> u32 {
        match self {
            Operand::ImmediateU32(i) => *i,
            _ => { unreachable!() }
        }
    }
}

impl Display for Operand {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Operand::ImmediateU8(imm) => {
                write!(f, "#0x{:x}", imm)
            },
            Operand::ImmediateU32(imm) => {
                write!(f, "#0x{:x}", imm)
            },
            Operand::W => {
                write!(f, "W")
            },
            Operand::File(file) => {
                write!(f, "[banked 0x{:x}]", file)
            },
            Operand::Nothing => {
                write!(f, "<No Operand>")
            }
        }
    }
}

impl Decodable for Instruction {
    fn decode<'a, T: IntoIterator<Item=&'a u8>>(bytes: T) -> Option<Self> {
        let mut blank = Instruction::blank();
        match blank.decode_into(bytes) {
            Some(_) => Some(blank),
            None => None
        }
    }
    fn decode_into<'a, T: IntoIterator<Item=&'a u8>>(&mut self, bytes: T) -> Option<()> {
        let mut bytes_iter = bytes.into_iter();
        let word: Vec<&'a u8> = bytes_iter.by_ref().take(2).collect();
        if word.len() != 2 {
            return None;
        }

        match *word[1] {
            0x00 => {
                self.operands = [Operand::Nothing, Operand::Nothing];
                match *word[0] {
                    0x00 => {
                        self.opcode = Opcode::NOP;
                        Some(())
                    },
                    0b00000010 => {
                        self.opcode = Opcode::RETURN;
                        Some(())
                    },
                    0b00000011 => {
                        self.opcode = Opcode::SLEEP;
                        Some(())
                    },
                    0b00000100 => {
                        self.opcode = Opcode::CLRWDT;
                        Some(())
                    },
                    0b00000101 => {
                        self.opcode = Opcode::RETFIE;
                        Some(())
                    },
                    _ => {
                        self.opcode = Opcode::Invalid(*word[1], *word[0]);
                        None
                    }
                }
            },
            0x01 => {
                self.opcode = Opcode::MOVWF;
                self.operands = [Operand::File(*word[0]), Operand::Nothing];
                Some(())
            },
            x if x < 0x30 => {
                // TODO: consume
                let d = x & 0x01 == 0x01;
                self.opcode = [
                    Opcode::SUBWFB,
                    Opcode::SUBWF,
                    Opcode::DECF,
                    Opcode::IORWF,
                    Opcode::ANDWF,
                    Opcode::XORWF,
                    Opcode::ADDWF,
                    Opcode::ADDWFC,
                    Opcode::COMF,
                    Opcode::INCF,
                    Opcode::DECFSZ,
                    Opcode::RRCF,
                    Opcode::RLCF,
                    Opcode::SWAPF,
                    Opcode::INCFSZ,
                    Opcode::RRNCF,
                    Opcode::RLNCF,
                    Opcode::INFSNZ,
                    Opcode::DCFSNZ,
                    Opcode::CLRF,
                    Opcode::SETF,
                    Opcode::NEGW,
                    Opcode::DAW
                ][(x >> 1) as usize - 1];
                self.operands[0] = Operand::File(*word[0]);
                self.operands[1] = if d {
                    Operand::File(*word[0])
                } else {
                    Operand::W
                };
                Some(())
            },
            x if x < 0x40 => {
                self.operands = [Operand::File(*word[0]), Operand::Nothing];
                self.opcode = match *word[1] {
                    0x30 => Opcode::CPFSLT,
                    0x31 => Opcode::CPFSEQ,
                    0x32 => Opcode::CPFSGT,
                    0x33 => Opcode::MULWF,
                    0x34 => Opcode::TSTFSZ,
                    0x35 => { Opcode::Invalid(*word[1], *word[0]); return None },
                    0x36 => { Opcode::Invalid(*word[1], *word[0]); return None },
                    0x37 => { Opcode::Invalid(*word[1], *word[0]); return None },
                    0x38 => { self.operands[1] = Operand::ImmediateU8(0); Opcode::BTG },
                    0x39 => { self.operands[1] = Operand::ImmediateU8(1); Opcode::BTG },
                    0x3a => { self.operands[1] = Operand::ImmediateU8(2); Opcode::BTG },
                    0x3b => { self.operands[1] = Operand::ImmediateU8(3); Opcode::BTG },
                    0x3c => { self.operands[1] = Operand::ImmediateU8(4); Opcode::BTG },
                    0x3d => { self.operands[1] = Operand::ImmediateU8(5); Opcode::BTG },
                    0x3e => { self.operands[1] = Operand::ImmediateU8(6); Opcode::BTG },
                    0x3f => { self.operands[1] = Operand::ImmediateU8(7); Opcode::BTG },
                    _ => { unreachable!(); }
                };
                Some(())
            },
            x if x < 0x60 => {
                self.opcode = Opcode::MOVPF;
                self.operands[0] = Operand::File((*word[1]) & 0x1f);
                self.operands[1] = Operand::File(*word[0]);
                Some(())
            },
            x if x < 0x80 => {
                self.opcode = Opcode::MOVFP;
                self.operands[0] = Operand::File(*word[0]);
                self.operands[1] = Operand::File((*word[1]) & 0x1f);
                Some(())
            },
            x if x < 0xa0 => {
                self.opcode = [
                    Opcode::BSF,
                    Opcode::BCF,
                    Opcode::BTFSS,
                    Opcode::BTFSC,
                ][(((*word[1]) >> 3) & 0x3) as usize];
                self.operands[0] = Operand::File(*word[0]);
                self.operands[1] = Operand::ImmediateU8((*word[1]) & 0x7);
                Some(())
            },
            x if x < 0xb0 => {
                self.opcode = [
                    Opcode::TLRDL,
                    Opcode::TLRDL,
                    Opcode::TLRDH,
                    Opcode::TLRDH,
                    Opcode::TLWTL,
                    Opcode::TLWTL,
                    Opcode::TLWTH,
                    Opcode::TLWTH,
                    Opcode::TABLRDL,
                    Opcode::TABLRDLI,
                    Opcode::TABLRDH,
                    Opcode::TABLRDHI,
                    Opcode::TABLWTL,
                    Opcode::TABLWTLI,
                    Opcode::TABLWTH,
                    Opcode::TABLWTHI
                ][(x & 0x0f) as usize];
                self.operands = [Operand::File(*word[0]), Operand::Nothing];
                Some(())
            },
            x if x < 0xc0 => {
                self.opcode = [
                    Opcode::MOVLW,
                    Opcode::ADDLW,
                    Opcode::SUBLW,
                    Opcode::IORLW,
                    Opcode::XORLW,
                    Opcode::ANDLW,
                    Opcode::RETLW,
                    Opcode::LCALL,
                    Opcode::MOVLB, // BSR only gets low four...
                    Opcode::Invalid(*word[1], *word[0]),
                    Opcode::MOVLR, // These are weird ones. The Bank Select
                    Opcode::MOVLR, // Register only gets the high four bits.
                    Opcode::MULLW,
                    Opcode::Invalid(*word[1], *word[0]),
                    Opcode::Invalid(*word[1], *word[0]),
                    Opcode::Invalid(*word[1], *word[0])
                ][((*word[1]) & 0x0f) as usize];
                self.operands = [Operand::ImmediateU8(*word[0]), Operand::Nothing];
                Some(())
            },
            x if x < 0xe0 => {
                self.opcode = Opcode::GOTO;
                self.operands = [Operand::ImmediateU32(*word[0] as u32 | (((*word[1] as u32) & 0x1f) << 8)), Operand::Nothing];
                Some(())
            },
            _ => {
                self.opcode = Opcode::CALL;
                self.operands = [Operand::ImmediateU32(*word[0] as u32 | (((*word[1] as u32) & 0x1f) << 8)), Operand::Nothing];
                Some(())
            }
        }
    }
}

