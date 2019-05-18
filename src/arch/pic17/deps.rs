use arch::pic17::PartialInstructionContext;
use arch::pic17::SFRS;
use yaxpeax_pic17::{Instruction, Opcode, Operand};

pub fn updates_of<T>(instr: &Instruction, ctx: &T) -> Vec<Update> where T: PartialInstructionContext {
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
    match instr.opcode {
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
            let mut updates = register_updates(instr.operands[1], ctx);
            updates.push(Update::Carry);
            updates
        }
        Opcode::LCALL => {
            vec![]
        }
        Opcode::MOVWF => {
            register_updates(instr.operands[0], ctx)
        }
        Opcode::MOVFP |
        Opcode::MOVPF => {
            // TODO: handle cases where, say, source is INDF_ with inc/dec
            register_updates(instr.operands[1], ctx)
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
            register_updates(instr.operands[0], ctx)
        }

        Opcode::CLRF => {
            let mut updates = register_updates(instr.operands[0], ctx);
            match instr.operands[1] {
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
            register_updates(instr.operands[0], ctx)
        }
        Opcode::TABLRDLI |
        Opcode::TABLRDHI => {
            let mut dest = register_updates(instr.operands[0], ctx);
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
            register_updates(instr.operands[1], ctx)
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
pub fn dependencies_of<T: PartialInstructionContext>(instr: &Instruction, ctx: &T) -> Vec<Dependence> {
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
    match instr.opcode {
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
            let mut deps = register_deps(instr.operands[0].file_value(), ctx);
            deps.push(Dependence::W);
            deps.push(Dependence::Carry);
            deps
        }
        Opcode::RRCF |
        Opcode::RLCF => {
            let mut deps = register_deps(instr.operands[0].file_value(), ctx);
            deps.push(Dependence::Carry);
            deps
        }
        Opcode::LCALL => {
            vec![Dependence::Memory(SFRS::PCLATH)]
        }
        Opcode::MOVWF => {
            let mut deps = register_write_deps(instr.operands[0].file_value(), ctx);
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
            let mut deps = register_deps(instr.operands[0].file_value(), ctx);
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
            register_deps(instr.operands[0].file_value(), ctx)
        }

        Opcode::BTG |
        Opcode::TSTFSZ |
        Opcode::BSF |
        Opcode::BCF |
        Opcode::BTFSS |
        Opcode::BTFSC => {
            register_deps(instr.operands[0].file_value(), ctx)
        }

        Opcode::MOVFP |
        Opcode::MOVPF => {
            let mut op0_deps = register_deps(instr.operands[0].file_value(), ctx);
            let mut op1_deps = register_write_deps(instr.operands[1].file_value(), ctx);
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

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash, Serialize, Deserialize)]
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
