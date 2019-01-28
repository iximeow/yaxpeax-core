use arch::{Arch, Decodable, pic18};
use arch::LengthedInstruction;

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

impl LengthedInstruction for Instruction {
    type Unit = <::arch::pic18::cpu::CPU as Arch>::Address;
    fn len(&self) -> Self::Unit {
        match self.opcode {
            Opcode::MOVFF
                | Opcode::MOVSF
                | Opcode::MOVSD
                | Opcode::CALL
                | Opcode::LFSR
                | Opcode::GOTO => {
                4
            },
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
}

#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone)]
pub enum Opcode {
    Invalid(u8, u8),
    NOP,
    MOVFF,
    MOVSF,
    MOVSD,
    CALL,
    LFSR,
    GOTO,
    CALLW,
    CLRWDT,
    DAW,
    POP,
    PUSH,
    RESET,
    RETFIE,
    RETFIE_FAST,
    RETURN,
    RETURN_FAST,
    SLEEP,
    TBLRD_I_S,
    TBLRD_S,
    TBLRD_S_D,
    TBLRD_S_I,
    TBLWT_I_S,
    TBLWT_S,
    TBLWT_S_D,
    TBLWT_S_I,
    MOVLB,
    ADDLW,
    MOVLW,
    MULLW,
    RETLW,
    ANDLW,
    XORLW,
    IORLW,
    SUBLW,
    IORWF,
    ANDWF,
    XORWF,
    COMF,
    MULWF,
    ADDWFC,
    ADDWF,
    INCF,
    DECF,
    DECFSZ,
    RRCF,
    RLCF,
    SWAPF,
    INCFSZ,
    RRNCF,
    RLNCF,
    INFSNZ,
    DCFSNZ,
    MOVF,
    SUBFWB,
    SUBWFB,
    SUBWF,
    CPFSLT,
    CPFSEQ,
    CPFSGT,
    TSTFSZ,
    SETF,
    CLRF,
    NEGF,
    MOVWF,
    BTG,
    BSF,
    BCF,
    BTFSS,
    BTFSC,
    BZ,
    BNZ,
    BC,
    BNC,
    BOV,
    BNOV,
    BN,
    BNN,
    BRA,
    RCALL
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Opcode::Invalid(a, b) => { write!(f, "invalid({:02x}{:02x})", a, b) },
            Opcode::NOP => { write!(f, "nop") },
            Opcode::MOVFF => { write!(f, "movff") },
            Opcode::MOVSF => { write!(f, "movsf") },
            Opcode::MOVSD => { write!(f, "movsd") },
            Opcode::CALL => { write!(f, "call") },
            Opcode::LFSR => { write!(f, "lfsr") },
            Opcode::GOTO => { write!(f, "goto") },
            Opcode::CALLW => { write!(f, "callw") },
            Opcode::CLRWDT => { write!(f, "clrwdt") },
            Opcode::DAW => { write!(f, "daw") },
            Opcode::POP => { write!(f, "pop") },
            Opcode::PUSH => { write!(f, "push") },
            Opcode::RESET => { write!(f, "reset") },
            Opcode::RETFIE => { write!(f, "retfie") },
            Opcode::RETFIE_FAST => { write!(f, "retfie_fast") },
            Opcode::RETURN => { write!(f, "return") },
            Opcode::RETURN_FAST => { write!(f, "return_fast") },
            Opcode::SLEEP => { write!(f, "sleep") },
            Opcode::TBLRD_I_S => { write!(f, "tblrd_i_s") },
            Opcode::TBLRD_S => { write!(f, "tblrd_s") },
            Opcode::TBLRD_S_D => { write!(f, "tblrd_s_d") },
            Opcode::TBLRD_S_I => { write!(f, "tblrd_s_i") },
            Opcode::TBLWT_I_S => { write!(f, "tblwt_i_s") },
            Opcode::TBLWT_S => { write!(f, "tblwt_s") },
            Opcode::TBLWT_S_D => { write!(f, "tblwt_s_d") },
            Opcode::TBLWT_S_I => { write!(f, "tblwt_s_i") },
            Opcode::MOVLB => { write!(f, "movlb") },
            Opcode::ADDLW => { write!(f, "addlw") },
            Opcode::MOVLW => { write!(f, "movlw") },
            Opcode::MULLW => { write!(f, "mullw") },
            Opcode::RETLW => { write!(f, "retlw") },
            Opcode::ANDLW => { write!(f, "andlw") },
            Opcode::XORLW => { write!(f, "xorlw") },
            Opcode::IORLW => { write!(f, "iorlw") },
            Opcode::SUBLW => { write!(f, "sublw") },
            Opcode::IORWF => { write!(f, "iorwf") },
            Opcode::ANDWF => { write!(f, "andwf") },
            Opcode::XORWF => { write!(f, "xorwf") },
            Opcode::COMF => { write!(f, "comf") },
            Opcode::MULWF => { write!(f, "mulwf") },
            Opcode::ADDWFC => { write!(f, "addwfc") },
            Opcode::ADDWF => { write!(f, "addwf") },
            Opcode::INCF => { write!(f, "incf") },
            Opcode::DECF => { write!(f, "decf") },
            Opcode::DECFSZ => { write!(f, "decfsz") },
            Opcode::RRCF => { write!(f, "rrcf") },
            Opcode::RLCF => { write!(f, "rlcf") },
            Opcode::SWAPF => { write!(f, "swapf") },
            Opcode::INCFSZ => { write!(f, "incfsz") },
            Opcode::RRNCF => { write!(f, "rrncf") },
            Opcode::RLNCF => { write!(f, "rlncf") },
            Opcode::INFSNZ => { write!(f, "infsnz") },
            Opcode::DCFSNZ => { write!(f, "dcfsnz") },
            Opcode::MOVF => { write!(f, "movf") },
            Opcode::SUBFWB => { write!(f, "subfwb") },
            Opcode::SUBWFB => { write!(f, "subwfb") },
            Opcode::SUBWF => { write!(f, "subwf") },
            Opcode::CPFSLT => { write!(f, "cpfslt") },
            Opcode::CPFSEQ => { write!(f, "cpfseq") },
            Opcode::CPFSGT => { write!(f, "cpfsgt") },
            Opcode::TSTFSZ => { write!(f, "tstfsz") },
            Opcode::SETF => { write!(f, "setf") },
            Opcode::CLRF => { write!(f, "clrf") },
            Opcode::NEGF => { write!(f, "negf") },
            Opcode::MOVWF => { write!(f, "movwf") },
            Opcode::BTG => { write!(f, "btg") },
            Opcode::BSF => { write!(f, "bsf") },
            Opcode::BCF => { write!(f, "bcf") },
            Opcode::BTFSS => { write!(f, "btfss") },
            Opcode::BTFSC => { write!(f, "btfsc") },
            Opcode::BZ => { write!(f, "bz") },
            Opcode::BNZ => { write!(f, "bnz") },
            Opcode::BC => { write!(f, "bc") },
            Opcode::BNC => { write!(f, "bnc") },
            Opcode::BOV => { write!(f, "bov") },
            Opcode::BNOV => { write!(f, "bnov") },
            Opcode::BN => { write!(f, "bn") },
            Opcode::BNN => { write!(f, "bnn") },
            Opcode::BRA => { write!(f, "bra") },
            Opcode::RCALL => { write!(f, "rcall") }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Operand {
    ImmediateU8(u8),
    ImmediateU32(u32),
    FileFSR(u8),
    File(u8, bool), // a == banked
    AbsoluteFile(u16),
    RedirectableFile(u8, bool, bool), // a == banked, d == direction
    Nothing
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
            Operand::FileFSR(fsr) => {
                write!(f, "[FSR{}]", fsr)
            },
            Operand::File(file, banked) => {
                if *banked {
                    write!(f, "[banked 0x{:x}]", file)
                } else {
                    write!(f, "[{}]", pic18::named_file(
                        if *file < 0x80 {
                            (*file as u16)
                        } else {
                            (*file as u16) | 0xf00u16
                        })
                    )
                }
            },
            Operand::AbsoluteFile(file) => {
                write!(f, "[{}]", pic18::named_file(*file))
            },
            Operand::RedirectableFile(file, banked, direction) => {
                if *direction {
                    write!(f, "[todo -> F] ")?
                } else {
                    write!(f, "[todo -> W] ")?
                };

                if *banked {
                    write!(f, "[banked 0x{:x}]", file)
                } else {
                    write!(f, "[{}]", pic18::named_file(
                        if *file < 0x80 {
                            (*file as u16)
                        } else {
                            (*file as u16) | 0xf00u16
                        })
                    )
                }
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

//            println!("Decoding {:x?}", word);
        match *word[1] {
            0x00 => {
                match *word[0] {
                    0x00 => {
                        self.opcode = Opcode::NOP;
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
                        self.opcode = Opcode::PUSH;
                        Some(())
                    },
                    0b00000110 => {
                        self.opcode = Opcode::POP;
                        Some(())
                    },
                    0b00000111 => {
                        self.opcode = Opcode::DAW;
                        Some(())
                    },
                    0b00001000 => {
                        self.opcode = Opcode::TBLRD_S;
                        Some(())
                    },
                    0b00001001 => {
                        self.opcode = Opcode::TBLRD_S_I;
                        Some(())
                    },
                    0b00001010 => {
                        self.opcode = Opcode::TBLRD_S_D;
                        Some(())
                    },
                    0b00001011 => {
                        self.opcode = Opcode::TBLRD_I_S;
                        Some(())
                    },
                    0b00001100 => {
                        self.opcode = Opcode::TBLWT_S;
                        Some(())
                    },
                    0b00001101 => {
                        self.opcode = Opcode::TBLWT_S_I;
                        Some(())
                    },
                    0b00001110 => {
                        self.opcode = Opcode::TBLWT_S_D;
                        Some(())
                    },
                    0b00001111 => {
                        self.opcode = Opcode::TBLWT_I_S;
                        Some(())
                    },
                    0b00010000 => {
                        self.opcode = Opcode::RETFIE;
                        Some(())
                    },
                    0b00010001 => {
                        self.opcode = Opcode::RETFIE_FAST;
                        Some(())
                    },
                    0b00010010 => {
                        self.opcode = Opcode::RETURN;
                        Some(())
                    },
                    0b00010011 => {
                        self.opcode = Opcode::RETURN_FAST;
                        Some(())
                    },
                    0b00010100 => {
                        self.opcode = Opcode::CALLW;
                        Some(())
                    },
                    0b11111111 => {
                        self.opcode = Opcode::RESET;
                        Some(())
                    },
                    _ => {
                        self.opcode = Opcode::Invalid(*word[0], *word[1]);
                        None
                    }
                }
            },
            0x01 => {
                self.opcode = Opcode::MOVLB;
                // this ignores high nibble of low word. ok by isa, but...
                self.operands[0] = Operand::ImmediateU8(*word[0] & 0x0f);
                Some(())
            },
            0x02 | 0x03 => {
                self.opcode = Opcode::MULWF;
                let a = (word[1] & 0x01) == 1;
                self.operands[0] = Operand::File(*word[0], a);
                Some(())
            },
            0x04 | 0x05 | 0x06 | 0x07 => {
                self.opcode = Opcode::DECF;
                let d = ((word[1] >> 1) & 0x01u8) == 1u8;
                let a = (word[1] & 0x01) == 1;
                self.operands[0] = Operand::RedirectableFile(*word[0], a, d);
                Some(())
            },
            0x08 => {
                self.opcode = Opcode::SUBLW;
                self.operands[0] = Operand::ImmediateU8(*word[0]);
                Some(())
            },
            0x09 => {
                self.opcode = Opcode::IORLW;
                self.operands[0] = Operand::ImmediateU8(*word[0]);
                Some(())
            },
            0x0a => {
                self.opcode = Opcode::XORLW;
                self.operands[0] = Operand::ImmediateU8(*word[0]);
                Some(())
            },
            0x0b => {
                self.opcode = Opcode::ANDLW;
                self.operands[0] = Operand::ImmediateU8(*word[0]);
                Some(())
            },
            0x0c => {
                self.opcode = Opcode::RETLW;
                self.operands[0] = Operand::ImmediateU8(*word[0]);
                Some(())
            },
            0x0d => {
                self.opcode = Opcode::MULLW;
                self.operands[0] = Operand::ImmediateU8(*word[0]);
                Some(())
            },
            0x0e => {
                self.opcode = Opcode::MOVLW;
                self.operands[0] = Operand::ImmediateU8(*word[0]);
                Some(())
            },
            0x0f => {
                self.opcode = Opcode::ADDLW;
                self.operands[0] = Operand::ImmediateU8(*word[0]);
                Some(())
            },
            x if x >= 0x10 && x < 0b01100000 => {
                let da = x & 0b0011;
                let opc = (x >> 2) - 4;
                self.opcode = [
                    Opcode::IORWF,
                    Opcode::ANDWF,
                    Opcode::XORWF,
                    Opcode::COMF,
                    Opcode::ADDWFC,
                    Opcode::ADDWF,
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
                    Opcode::MOVF,
                    Opcode::SUBFWB,
                    Opcode::SUBWFB,
                    Opcode::SUBWF
                ][opc as usize];
                self.operands[0] = Operand::RedirectableFile(*word[0], (da & 0x01) == 0x01, (da & 0x02) == 0x02);
                Some(())
            },
            x if x >= 0b01100000 && x < 0b01110000 => {
                let a = x & 1;
                let opc = (x >> 1) & 0b0000111;
                self.opcode = [
                    Opcode::CPFSLT,
                    Opcode::CPFSEQ,
                    Opcode::CPFSGT,
                    Opcode::TSTFSZ,
                    Opcode::SETF,
                    Opcode::CLRF,
                    Opcode::NEGF,
                    Opcode::MOVWF
                ][opc as usize];
                self.operands[0] = Operand::File(*word[0], a == 1);
                Some(())
            },
            x if x >= 0b01110000 && x < 0b11000000 => {
                let a = x & 1;
                let opc = ((x >> 4) & 0b00001111) - 0b111;
                self.opcode = [
                    Opcode::BTG,
                    Opcode::BSF,
                    Opcode::BCF,
                    Opcode::BTFSS,
                    Opcode::BTFSC
                ][opc as usize];
                let bit = (x >> 1) & 0b0000111;
                self.operands[0] = Operand::File(*word[0], a == 1);
                self.operands[1] = Operand::ImmediateU8(bit);
                Some(())
            },
            x if x >= 0b11000000 && x < 0b11010000 => {
                self.opcode = Opcode::MOVFF;
                let word2: Vec<&'a u8> = bytes_iter.take(2).collect();
                if word2.len() != 2 {
                    return None;
                }
                if word2[1] & 0xf0 != 0xf0 {
                    return None;
                }

                let src = (*word[0] as u16) | ((*word[1] as u16 & 0x0f) << 8);
                let dest = (*word2[0] as u16) | ((*word2[1] as u16 & 0x0f) << 8);
                self.operands[0] = Operand::AbsoluteFile(src);
                self.operands[1] = Operand::AbsoluteFile(dest);
                Some(())
            },
            x if x >= 0b11010000 && x < 0b11100000 => {
                self.opcode = [
                    Opcode::BRA,
                    Opcode::RCALL
                ][((x >> 3) & 1) as usize];
                self.operands[0] = Operand::ImmediateU32((((x & 0b111) as u32) << 8) | *word[0] as u32);
                Some(())
            },
            x if x >= 0b11100000 && x < 0b11101000 => {
                let opc = x & 0b00000111;
                self.opcode = [
                    Opcode::BZ,
                    Opcode::BNZ,
                    Opcode::BC,
                    Opcode::BNC,
                    Opcode::BOV,
                    Opcode::BNOV,
                    Opcode::BN,
                    Opcode::BNN
                ][opc as usize];
                self.operands[0] = Operand::ImmediateU8(*word[0]);
                Some(())
            },
            0xee => {
                let f_k_msb = *word[0];
                let word2: Vec<&'a u8> = bytes_iter.take(2).collect();
                if word2.len() != 2 {
                    return None;
                }

                if (word2[1] & 0xf0) != 0xf0 {
                    return None; // invalid instruction
                }

                self.opcode = Opcode::LFSR;

                let f = (f_k_msb >> 4) & 0b0011;
                let k_msb = f_k_msb & 0b1111;
                let k_lsb = word2[0];

                self.operands[0] = Operand::FileFSR(f);
                self.operands[1] = Operand::ImmediateU32(((k_msb as u32) << 8) | (*k_lsb as u32));
                Some(())
            }
            /* ... */
            0xeb | 0xec => {
                // TODO: respect s bit
                let k_lsb = *word[0];
                let word2: Vec<&'a u8> = bytes_iter.take(2).collect();
                if word2.len() != 2 {
                    return None;
                }

                if (word2[1] & 0xf0) != 0xf0 {
                    return None; // invalid instruction
                }

                let k_msb = (((*word2[1] & 0xf) as u32) << 8) | *word2[0] as u32;

                self.opcode = Opcode::CALL;
                self.operands[0] = Operand::ImmediateU32(((k_msb << 8) | k_lsb as u32) << 1);
                Some(())
            }
            0xef => {
                let k_lsb = *word[0];
                let word2: Vec<&'a u8> = bytes_iter.take(2).collect();
                if word2.len() != 2 {
                    return None;
                }

                if (word2[1] & 0xf0) != 0xf0 {
                    return None; // invalid instruction
                }

                let k_msb = (((*word2[1] & 0xf) as u32) << 8) | *word2[0] as u32;

                self.opcode = Opcode::GOTO;
                self.operands[0] = Operand::ImmediateU32(((k_msb << 8) | k_lsb as u32) << 1);
                Some(())
            }
            _ => None
        }
    }
}

