use yaxpeax_x86::long_mode::{register_class, Instruction, Opcode, Operand, RegSpec};
use arch::x86_64::analyses::data_flow::{ANY, Location, cond_to_flags};
use data::Direction;
use tracing::{event, Level};

pub(crate) fn decompose_locations(instr: &Instruction) -> Vec<(Option<Location>, Direction)> {
    fn decompose_write(op: &Operand) -> Vec<(Option<Location>, Direction)> {
        match op {
            Operand::ImmediateI8(_) |
            Operand::ImmediateU8(_) |
            Operand::ImmediateI16(_) |
            Operand::ImmediateU16(_) |
            Operand::ImmediateI32(_) |
            Operand::ImmediateU32(_) |
            Operand::ImmediateI64(_) |
            Operand::ImmediateU64(_) => {
                // no instruction encodes this, this should be an error
                unreachable!()
            }
            Operand::Register(spec) => {
                vec![(Some(Location::Register(*spec)), Direction::Write)]
            },
            Operand::DisplacementU32(_) |
            Operand::DisplacementU64(_) => {
                // TODO: lazy
                vec![(Some(Location::Memory(ANY)), Direction::Write)]
            },
            Operand::RegDeref(spec) => {
                vec![
                    (Some(Location::Register(*spec)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Write)
                ]
            },
            Operand::RegDisp(spec, _) => {
                vec![
                    (Some(Location::Register(*spec)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Write)
                ]
            },
            Operand::RegScale(spec, _) => {
                vec![
                    (Some(Location::Register(*spec)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Write)
                ]
            },
            Operand::RegIndexBase(base, index) => {
                vec![
                    (Some(Location::Register(*base)), Direction::Read),
                    (Some(Location::Register(*index)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Write)
                ]
            },
            Operand::RegIndexBaseDisp(base, index, _) => {
                vec![
                    (Some(Location::Register(*base)), Direction::Read),
                    (Some(Location::Register(*index)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Write)
                ]
            },
            Operand::RegScaleDisp(base, _, _) => {
                vec![
                    (Some(Location::Register(*base)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Write)
                ]
            },
            Operand::RegIndexBaseScale(base, index, _) => {
                vec![
                    (Some(Location::Register(*base)), Direction::Read),
                    (Some(Location::Register(*index)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Write)
                ]
            },
            Operand::RegIndexBaseScaleDisp(base, index, _, _) => {
                vec![
                    (Some(Location::Register(*base)), Direction::Read),
                    (Some(Location::Register(*index)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Write)
                ]
            },
            Operand::Nothing => {
                vec![]
            }
            other => {
                panic!("unhandled operand type {:?}", other);
            }
        }
    }
    fn decompose_read(op: &Operand) -> Vec<(Option<Location>, Direction)> {
        match op {
            Operand::ImmediateI8(_) |
            Operand::ImmediateU8(_) |
            Operand::ImmediateI16(_) |
            Operand::ImmediateU16(_) |
            Operand::ImmediateI32(_) |
            Operand::ImmediateU32(_) |
            Operand::ImmediateI64(_) |
            Operand::ImmediateU64(_) => {
                vec![]
            }
            Operand::Register(spec) => {
                vec![(Some(Location::Register(*spec)), Direction::Read)]
            },
            Operand::DisplacementU32(_) |
            Operand::DisplacementU64(_) => {
                // TODO: lazy
                vec![(Some(Location::Memory(ANY)), Direction::Read)]
            },
            Operand::RegDeref(spec) => {
                vec![
                    (Some(Location::Register(*spec)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Read)
                ]
            },
            Operand::RegDisp(spec, _) => {
                vec![
                    (Some(Location::Register(*spec)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Read)
                ]
            },
            Operand::RegScale(spec, _) => {
                vec![
                    (Some(Location::Register(*spec)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Read)
                ]
            },
            Operand::RegIndexBase(base, index) => {
                vec![
                    (Some(Location::Register(*base)), Direction::Read),
                    (Some(Location::Register(*index)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Read)
                ]
            },
            Operand::RegIndexBaseDisp(base, index, _) => {
                vec![
                    (Some(Location::Register(*base)), Direction::Read),
                    (Some(Location::Register(*index)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Read)
                ]
            },
            Operand::RegScaleDisp(base, _, _) => {
                vec![
                    (Some(Location::Register(*base)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Read)
                ]
            },
            Operand::RegIndexBaseScale(base, index, _) => {
                vec![
                    (Some(Location::Register(*base)), Direction::Read),
                    (Some(Location::Register(*index)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Read)
                ]
            },
            Operand::RegIndexBaseScaleDisp(base, index, _, _) => {
                vec![
                    (Some(Location::Register(*base)), Direction::Read),
                    (Some(Location::Register(*index)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Read)
                ]
            },
            Operand::Nothing => {
                vec![]
            }
            other => {
                panic!("unhandled operand type {:?}", other);
            }
        }
    }
    fn decompose_readwrite(op: &Operand) -> Vec<(Option<Location>, Direction)> {
        match op {
            Operand::ImmediateI8(_) |
            Operand::ImmediateU8(_) |
            Operand::ImmediateI16(_) |
            Operand::ImmediateU16(_) |
            Operand::ImmediateI32(_) |
            Operand::ImmediateU32(_) |
            Operand::ImmediateI64(_) |
            Operand::ImmediateU64(_) => {
                // no instruction encodes this, this should be an error
                unreachable!()
            }
            Operand::Register(spec) => {
                vec![
                    (Some(Location::Register(*spec)), Direction::Read),
                    (Some(Location::Register(*spec)), Direction::Write)
                ]
            },
            Operand::DisplacementU32(_) |
            Operand::DisplacementU64(_) => {
                // TODO: lazy
                vec![
                    (Some(Location::Memory(ANY)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Write),
                ]
            },
            Operand::RegDeref(spec) => {
                vec![
                    (Some(Location::Register(*spec)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Write)
                ]
            },
            Operand::RegDisp(spec, _) => {
                vec![
                    (Some(Location::Register(*spec)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Write)
                ]
            },
            Operand::RegScale(spec, _) => {
                vec![
                    (Some(Location::Register(*spec)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Write)
                ]
            },
            Operand::RegIndexBase(base, index) => {
                vec![
                    (Some(Location::Register(*base)), Direction::Read),
                    (Some(Location::Register(*index)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Write)
                ]
            },
            Operand::RegIndexBaseDisp(base, index, _) => {
                vec![
                    (Some(Location::Register(*base)), Direction::Read),
                    (Some(Location::Register(*index)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Write)
                ]
            },
            Operand::RegScaleDisp(base, _, _) => {
                vec![
                    (Some(Location::Register(*base)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Write)
                ]
            },
            Operand::RegIndexBaseScale(base, index, _) => {
                vec![
                    (Some(Location::Register(*base)), Direction::Read),
                    (Some(Location::Register(*index)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Write)
                ]
            },
            Operand::RegIndexBaseScaleDisp(base, index, _, _) => {
                vec![
                    (Some(Location::Register(*base)), Direction::Read),
                    (Some(Location::Register(*index)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Read),
                    (Some(Location::Memory(ANY)), Direction::Write)
                ]
            },
            Operand::Nothing => {
                vec![]
            }
            other => {
                panic!("unhandled operand type {:?}", other);
            }
        }
    }
    match instr.opcode() {
        Opcode::SQRTSD |
        Opcode::SQRTSS |
        Opcode::MOVDDUP |
        Opcode::MOVSLDUP |
        Opcode::MOVSD |
        Opcode::MOVSS |
        Opcode::CVTSI2SS |
        Opcode::CVTTSS2SI |
        Opcode::CVTSS2SI |
        Opcode::CVTSS2SD |
        Opcode::CVTSI2SD |
        Opcode::CVTTSD2SI |
        Opcode::CVTSD2SI |
        Opcode::CVTSD2SS |
        Opcode::CVTPS2PD |
        Opcode::LDDQU |
        Opcode::LEA |
        Opcode::MOVSX |
        Opcode::MOVZX |
        Opcode::MOVSXD |
        Opcode::MOVAPS |
        Opcode::MOVUPS |
        Opcode::MOVDQA |
        Opcode::MOVDQU |
        Opcode::MOVAPD |
        Opcode::MOVQ |
        Opcode::MOV => {
            let mut locs = decompose_read(&instr.operand(1));
            locs.append(&mut decompose_write(&instr.operand(0)));
            locs
        }
        Opcode::XADD |
        Opcode::XCHG => {
            let mut locs = decompose_readwrite(&instr.operand(1));
            locs.append(&mut decompose_readwrite(&instr.operand(0)));
            locs
        }
        Opcode::STI |
        Opcode::CLI => {
            vec![
                (Some(Location::IF), Direction::Write)
            ]
        }
        Opcode::STD |
        Opcode::CLD => {
            vec![
                (Some(Location::DF), Direction::Write)
            ]
        }
        Opcode::STC |
        Opcode::CLC => {
            vec![
                (Some(Location::CF), Direction::Write)
            ]
        }
        Opcode::BT |
        Opcode::BTS |
        Opcode::BTR |
        Opcode::BTC |
        Opcode::BSR |
        Opcode::BSF |
        Opcode::TZCNT => {
            let mut locs = decompose_read(&instr.operand(0));
            locs.append(&mut decompose_read(&instr.operand(1)));
            locs.push((Some(Location::ZF), Direction::Write));
            locs
        }
        Opcode::SAR |
        Opcode::SAL |
        Opcode::SHR |
        Opcode::SHL => {
            // TODO: inspect imm8 and CL if context permits to decide
            // flags more granularly
            let mut locs = decompose_readwrite(&instr.operand(0));
            locs.append(&mut decompose_read(&instr.operand(1)));
            locs.push((Some(Location::CF), Direction::Write));
            locs.push((Some(Location::SF), Direction::Write));
            locs.push((Some(Location::ZF), Direction::Write));
            locs.push((Some(Location::PF), Direction::Write));
            locs.push((Some(Location::AF), Direction::Write));
            locs
        }
        Opcode::RCR |
        Opcode::RCL |
        Opcode::ROR |
        Opcode::ROL => {
            // TODO: inspect imm8 and CL if context permits to decide
            // flags more granularly
            let mut locs = decompose_readwrite(&instr.operand(0));
            locs.append(&mut decompose_read(&instr.operand(1)));
            locs.push((Some(Location::CF), Direction::Write));
            locs.push((Some(Location::OF), Direction::Write));
            locs
        }
        Opcode::ADC |
        Opcode::SBB => {
            // TODO: this is lazy and assumes writes of all flags
            // this may not be true
            let mut locs = decompose_read(&instr.operand(1));
            locs.append(&mut decompose_readwrite(&instr.operand(0)));
            locs.push((Some(Location::CF), Direction::Read));
            locs.push((Some(Location::CF), Direction::Write));
            locs.push((Some(Location::OF), Direction::Write));
            locs.push((Some(Location::SF), Direction::Write));
            locs.push((Some(Location::ZF), Direction::Write));
            locs.push((Some(Location::PF), Direction::Write));
            locs.push((Some(Location::AF), Direction::Write));
            locs
        },
        Opcode::MULPS |
        Opcode::ORPS |
        Opcode::PADDQ |
        Opcode::PCMPEQD |
        Opcode::PCMPGTD |
        Opcode::PCMPGTW |
        Opcode::PMULLW |
        Opcode::PSLLD |
        Opcode::PSLLDQ |
        Opcode::PSLLQ |
        Opcode::PSLLW |
        Opcode::PSRAD |
        Opcode::PSRLD |
        Opcode::PSRLDQ |
        Opcode::PSRLQ |
        Opcode::PSRLW |
        Opcode::PUNPCKHBW |
        Opcode::PUNPCKHDQ |
        Opcode::PUNPCKHQDQ |
        Opcode::PUNPCKHWD |
        Opcode::PUNPCKLBW |
        Opcode::PUNPCKLDQ |
        Opcode::PUNPCKLQDQ |
        Opcode::PUNPCKLWD |
        Opcode::PXOR |
        Opcode::SUBPS |
        Opcode::UNPCKHPS |
        Opcode::UNPCKLPS |
        Opcode::XORPS => {
            let mut locs = decompose_read(&instr.operand(1));
            locs.append(&mut decompose_readwrite(&instr.operand(0)));
            locs
        }
        Opcode::ADD |
        Opcode::SUB |
        Opcode::AND |
        Opcode::XOR |
        Opcode::OR => {
            // TODO: this is lazy and assumes writes of all flags
            // this may not be true
            let mut locs = decompose_read(&instr.operand(1));
            locs.append(&mut decompose_readwrite(&instr.operand(0)));
            locs.push((Some(Location::CF), Direction::Write));
            locs.push((Some(Location::OF), Direction::Write));
            locs.push((Some(Location::SF), Direction::Write));
            locs.push((Some(Location::ZF), Direction::Write));
            locs.push((Some(Location::PF), Direction::Write));
            locs.push((Some(Location::AF), Direction::Write));
            locs
        }

        Opcode::IMUL => {
            // TODO: this.
            let mut locs = if !instr.operand_present(1) {
                decompose_read(&instr.operand(0))
            } else {
                let mut ls = decompose_readwrite(&instr.operand(0));
                ls.append(&mut decompose_read(&instr.operand(1)));
                ls
            };
            locs.push((Some(Location::Register(RegSpec::rax())), Direction::Read));
            locs.push((Some(Location::Register(RegSpec::rax())), Direction::Write));
            locs.push((Some(Location::Register(RegSpec::rdx())), Direction::Write));
            locs.push((Some(Location::CF), Direction::Write));
            locs.push((Some(Location::OF), Direction::Write));
            locs.push((Some(Location::SF), Direction::Write));
            locs.push((Some(Location::ZF), Direction::Write));
            locs.push((Some(Location::PF), Direction::Write));
            locs.push((Some(Location::AF), Direction::Write));
            locs
        },
        Opcode::IDIV |
        Opcode::DIV => {
            // TODO: this is lazy and assumes writes of all flags
            // this may not be true
            // TODO: this may not read *dx (if this is idiv r/m 8)
            let mut locs = if !instr.operand_present(1) {
                decompose_read(&instr.operand(0))
            } else {
                let mut ls = decompose_readwrite(&instr.operand(0));
                ls.append(&mut decompose_read(&instr.operand(1)));
                ls
            };
            locs.push((Some(Location::Register(RegSpec::rax())), Direction::Read));
            locs.push((Some(Location::Register(RegSpec::rax())), Direction::Write));
            locs.push((Some(Location::Register(RegSpec::rdx())), Direction::Read));
            locs.push((Some(Location::CF), Direction::Write));
            locs.push((Some(Location::OF), Direction::Write));
            locs.push((Some(Location::SF), Direction::Write));
            locs.push((Some(Location::ZF), Direction::Write));
            locs.push((Some(Location::PF), Direction::Write));
            locs.push((Some(Location::AF), Direction::Write));
            locs

        }
        Opcode::MUL => {
            // TODO: this is lazy and assumes writes of all flags
            // this may not be true
            let mut locs = decompose_read(&instr.operand(0));
            locs.push((Some(Location::Register(RegSpec::rax())), Direction::Read));
            locs.push((Some(Location::Register(RegSpec::rax())), Direction::Write));
            locs.push((Some(Location::Register(RegSpec::rdx())), Direction::Write));
            locs.push((Some(Location::CF), Direction::Write));
            locs.push((Some(Location::OF), Direction::Write));
            locs.push((Some(Location::SF), Direction::Write));
            locs.push((Some(Location::ZF), Direction::Write));
            locs.push((Some(Location::PF), Direction::Write));
            locs.push((Some(Location::AF), Direction::Write));
            locs
        }

        Opcode::PUSH => {
            let mut locs = decompose_read(&instr.operand(0));
            locs.push((Some(Location::Register(RegSpec::rsp())), Direction::Read));
            locs.push((Some(Location::Register(RegSpec::rsp())), Direction::Write));
            locs.push((Some(Location::Memory(ANY)), Direction::Write));
            locs
        },
        Opcode::POP => {
            let mut locs = decompose_write(&instr.operand(0));
            locs.push((Some(Location::Register(RegSpec::rsp())), Direction::Read));
            locs.push((Some(Location::Register(RegSpec::rsp())), Direction::Write));
            locs.push((Some(Location::Memory(ANY)), Direction::Read));
            locs
        },
        Opcode::INC |
        Opcode::DEC => {
            let mut locs = decompose_readwrite(&instr.operand(0));
            locs.push((Some(Location::OF), Direction::Write));
            locs.push((Some(Location::SF), Direction::Write));
            locs.push((Some(Location::ZF), Direction::Write));
            locs.push((Some(Location::AF), Direction::Write));
            locs.push((Some(Location::PF), Direction::Write));
            locs
        }
        Opcode::ENTER => {
            vec![
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Register(RegSpec::rbp())), Direction::Read),
                (Some(Location::Register(RegSpec::rbp())), Direction::Write),
                (Some(Location::Memory(ANY)), Direction::Write)
            ]
        }
        Opcode::LEAVE => {
            vec![
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Register(RegSpec::rbp())), Direction::Write),
                (Some(Location::Memory(ANY)), Direction::Read)
            ]
        }
        Opcode::POPF => {
            vec![
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Memory(ANY)), Direction::Read),
                (Some(Location::Register(RegSpec::rflags())), Direction::Write)
            ]
        }
        Opcode::PUSHF => {
            vec![
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Memory(ANY)), Direction::Write),
                (Some(Location::Register(RegSpec::rflags())), Direction::Read)
            ]
        }
        Opcode::CBW |
        Opcode::CDQ => {
            vec![
                (Some(Location::Register(RegSpec::rax())), Direction::Read),
                (Some(Location::Register(RegSpec::rax())), Direction::Write)
            ]
        }
        Opcode::LAHF => {
            vec![
                (Some(Location::Register(RegSpec::ax())), Direction::Write),
                (Some(Location::Register(RegSpec::rflags())), Direction::Read)
            ]
        }
        Opcode::SAHF => {
            vec![
                (Some(Location::Register(RegSpec::ax())), Direction::Read),
                (Some(Location::Register(RegSpec::rflags())), Direction::Write)
            ]
        }
        Opcode::TEST |
        Opcode::CMP => {
            /* not strictly correct for either */
            let mut locs = decompose_read(&instr.operand(0));
            locs.append(&mut decompose_read(&instr.operand(1)));
            locs.push((Some(Location::CF), Direction::Write));
            locs.push((Some(Location::OF), Direction::Write));
            locs.push((Some(Location::AF), Direction::Write));
            locs.push((Some(Location::SF), Direction::Write));
            locs.push((Some(Location::ZF), Direction::Write));
            locs.push((Some(Location::PF), Direction::Write));
            locs
        }
        Opcode::NEG => {
            let mut locs = decompose_readwrite(&instr.operand(0));
            locs.push((Some(Location::CF), Direction::Write));
            locs
        }
        Opcode::NOT => {
            decompose_readwrite(&instr.operand(0))
        }
        Opcode::CMPXCHG => {
            let mut locs = match &instr.operand(1) {
                Operand::Register(reg) => {
                    match reg.class() {
                        register_class::Q => {
                            vec![
                                (Some(Location::Register(RegSpec::q(reg.num()))), Direction::Read),
                                (Some(Location::Register(RegSpec::rax())), Direction::Read)
                            ]
                        }
                        register_class::D => {
                            vec![
                                (Some(Location::Register(RegSpec::d(reg.num()))), Direction::Read),
                                (Some(Location::Register(RegSpec::eax())), Direction::Read)
                            ]
                        }
                        register_class::W => {
                            vec![
                                (Some(Location::Register(RegSpec::w(reg.num()))), Direction::Read),
                                (Some(Location::Register(RegSpec::ax())), Direction::Read)
                            ]
                        }
                        register_class::B => {
                            vec![
                                (Some(Location::Register(RegSpec::b(reg.num()))), Direction::Read),
                                (Some(Location::Register(RegSpec::al())), Direction::Read)
                            ]
                        }
                        register_class::RB => {
                            vec![
                                (Some(Location::Register(RegSpec::rb(reg.num()))), Direction::Read),
                                (Some(Location::Register(RegSpec::al())), Direction::Read)
                            ]
                        }
                        _ => { unreachable!() }
                    }
                }
                _ => { unreachable!() }
            };
            locs.push((Some(Location::ZF), Direction::Write));
            locs.append(&mut decompose_readwrite(&instr.operand(0)));
            locs
        },
        Opcode::CALLF | // TODO: this is wrong
        Opcode::CALL => {
            let mut locs = decompose_read(&instr.operand(0));
            locs.push((Some(Location::Register(RegSpec::rsp())), Direction::Read));
            locs.push((Some(Location::Register(RegSpec::rsp())), Direction::Write));
            locs.push((Some(Location::Memory(ANY)), Direction::Write));
            locs
        }
        Opcode::JMP => {
            decompose_read(&instr.operand(0))
        },
        Opcode::JMPF => { // TODO: this is wrong.
            vec![]
        },
        Opcode::IRET | // TODO: this is wrong
        Opcode::RETF | // TODO: this is wrong
        Opcode::RETURN => {
            vec![
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Memory(ANY)), Direction::Read)
            ]
        }
        Opcode::LFENCE |
        Opcode::MFENCE |
        Opcode::SFENCE |
        Opcode::NOP |
        Opcode::WAIT => { vec![] }
        Opcode::CLFLUSH |
        Opcode::FXSAVE |
        Opcode::FXRSTOR |
        Opcode::XSAVE |
        Opcode::XRSTOR |
        Opcode::XSAVEOPT |
        Opcode::STMXCSR |
        Opcode::SGDT |
        Opcode::SIDT => {
            decompose_write(&instr.operand(0))
        }
        Opcode::LDMXCSR |
        Opcode::LGDT |
        Opcode::LIDT => {
            decompose_read(&instr.operand(0))
        }
        // TODO: this is wrong
        Opcode::SMSW => {
            decompose_write(&instr.operand(0))
        }
        // TODO: this is wrong
        Opcode::LMSW => {
            decompose_read(&instr.operand(0))
        }
        Opcode::SWAPGS => {
            vec![
                (Some(Location::Register(RegSpec::gs())), Direction::Read),
                (Some(Location::Register(RegSpec::gs())), Direction::Write)
            ]
        }
        Opcode::RDTSCP => {
            vec![
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rcx())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Write)
            ]
        }
        Opcode::WRMSR => {
            vec![
                (Some(Location::Register(RegSpec::rax())), Direction::Read),
                (Some(Location::Register(RegSpec::rdx())), Direction::Read)
            ]
        }
        Opcode::RDMSR => {
            vec![
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Write),
                (Some(Location::Register(RegSpec::rcx())), Direction::Read)
            ]
        }
        Opcode::RDTSC => {
            vec![
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Write)
            ]
        }
        Opcode::RDPMC => {
            vec![
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Write),
                (Some(Location::Register(RegSpec::rcx())), Direction::Read)
            ]
        }
        Opcode::VERR |
        Opcode::VERW => {
            let mut locs = decompose_read(&instr.operand(0));
            locs.push((Some(Location::ZF), Direction::Write));
            locs
        }
        Opcode::SLDT |
        Opcode::STR |
        Opcode::INVLPG => {
            decompose_write(&instr.operand(0))
        }
        Opcode::LLDT |
        Opcode::LTR => {
            decompose_read(&instr.operand(0))
        }
        // these are immediate-only or have no operands
        Opcode::JMPE |
        Opcode::UD2 |
        Opcode::INT |
        Opcode::INTO |
        Opcode::HLT |
        Opcode::Invalid => {
            vec![]
        },
        Opcode::JO |
        Opcode::JNO |
        Opcode::JZ |
        Opcode::JNZ |
        Opcode::JA |
        Opcode::JNA |
        Opcode::JNB |
        Opcode::JB |
        Opcode::JP |
        Opcode::JNP |
        Opcode::JS |
        Opcode::JNS |
        Opcode::JG |
        Opcode::JLE |
        Opcode::JGE |
        Opcode::JL => {
            cond_to_flags(instr.opcode().condition().unwrap()).to_vec()
        }

        Opcode::CMOVO |
        Opcode::CMOVNO |
        Opcode::CMOVZ |
        Opcode::CMOVNZ |
        Opcode::CMOVA |
        Opcode::CMOVNA |
        Opcode::CMOVNB |
        Opcode::CMOVB |
        Opcode::CMOVP |
        Opcode::CMOVNP |
        Opcode::CMOVS |
        Opcode::CMOVNS |
        Opcode::CMOVG |
        Opcode::CMOVLE |
        Opcode::CMOVGE |
        Opcode::CMOVL => {
            let mut locs = decompose_write(&instr.operand(0));
            locs.append(&mut decompose_read(&instr.operand(1)));
            locs.extend_from_slice(cond_to_flags(instr.opcode().condition().unwrap()));
            locs
        }
        Opcode::SETO |
        Opcode::SETNO |
        Opcode::SETZ |
        Opcode::SETNZ |
        Opcode::SETA |
        Opcode::SETBE |
        Opcode::SETAE |
        Opcode::SETB |
        Opcode::SETP |
        Opcode::SETNP |
        Opcode::SETS |
        Opcode::SETNS |
        Opcode::SETG |
        Opcode::SETLE |
        Opcode::SETGE |
        Opcode::SETL => {
            let mut locs = decompose_write(&instr.operand(0));
            locs.extend_from_slice(cond_to_flags(instr.opcode().condition().unwrap()));
            locs
        }
        Opcode::LSL => {
            let mut locs = decompose_write(&instr.operand(0));
            locs.append(&mut decompose_read(&instr.operand(1)));
            locs.push((Some(Location::ZF), Direction::Write));
            locs
        }
        Opcode::LAR => {
            let mut locs = decompose_write(&instr.operand(0));
            locs.append(&mut decompose_read(&instr.operand(1)));
            locs.push((Some(Location::ZF), Direction::Read));
            locs
        }
        Opcode::ADDSD |
        Opcode::SUBSD |
        Opcode::MULSD |
        Opcode::DIVSD |
        Opcode::MINSD |
        Opcode::MAXSD |
        Opcode::ADDSS |
        Opcode::SUBSS |
        Opcode::MULSS |
        Opcode::DIVSS |
        Opcode::MINSS |
        Opcode::MAXSS |
        Opcode::HADDPS |
        Opcode::HSUBPS |
        Opcode::ADDSUBPS => {
            let mut locs = decompose_readwrite(&instr.operand(0));
            locs.append(&mut decompose_read(&instr.operand(1)));
            locs
        }
        Opcode::CMPS |
        Opcode::SCAS |
        Opcode::MOVS |
        Opcode::LODS |
        Opcode::STOS |
        Opcode::INS |
        Opcode::OUTS => {
            // TODO: incomplete
            vec![]
        }
        o @ Opcode::CPUID |
        o @ Opcode::WBINVD |
        o @ Opcode::INVD |
        o @ Opcode::SYSRET |
        o @ Opcode::CLTS |
        o @ Opcode::SYSCALL |
        o => {
            event!(Level::ERROR, opcode = ?o, component = "instruction description", "missing operand decomposition");
            vec![]
        }
    }
}
