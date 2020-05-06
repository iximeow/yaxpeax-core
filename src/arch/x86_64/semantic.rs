/*
 * TODO: remove rip as a register in yaxpeax-x86, then a ToPC impl that maps Operand -> not-pc?
 * necessary for efficient compilation of `evaluate`, lest Operand::Register be treated as
 * potentially control-flow-influencing.
 */

use yaxpeax_x86::long_mode::{Arch as amd64};
use yaxpeax_x86::long_mode::{Opcode, Operand, RegSpec};
use yaxpeax_arch::Arch;
use yaxpeax_arch::AddressDiff;
use yaxpeax_arch::LengthedInstruction;
use arch::x86_64::analyses::data_flow::Location;
use arch::x86_64::analyses::data_flow::ANY;
use analyses::{DFG, Value, ValueRes};

pub enum CompletionStatus {
    Incomplete,
    Complete,
}

pub(crate) fn evaluate<V: Value + From<AddressDiff<<amd64 as Arch>::Address>>, D: DFG<V, Location=Location>>(instr: &<amd64 as Arch>::Instruction, dfg: &mut D) -> CompletionStatus {
    // TODO: get a disambiguator somehow?
    #[inline(always)]
    fn effective_address<V: Value, D: DFG<V, Location=Location>>(dfg: &mut D, operand: &Operand) -> V {
        match *operand {
            Operand::DisplacementU32(disp) => {
                V::from_const(disp as u64)
            },
            Operand::DisplacementU64(disp) => {
                V::from_const(disp as u64)
            },
            Operand::RegDeref(reg) => {
                dfg.read(&Location::Register(reg))
            }
            Operand::RegDisp(base, offset) => {
                let base = dfg.read(&Location::Register(base));
                base.add(&V::from_const(offset as i64 as u64)).value()
            }
            Operand::RegScale(base, scale) => {
                let base = dfg.read(&Location::Register(base));
                base.mul(&V::from_const(scale as u64)).value()
            }
            Operand::RegScaleDisp(base, scale, disp) => {
                let base = dfg.read(&Location::Register(base));
                base.mul(&V::from_const(scale as u64)).value().add(&V::from_const(disp as i64 as u64)).value()
            }
            Operand::RegIndexBase(base, index) => {
                let base = dfg.read(&Location::Register(base));
                let index = dfg.read(&Location::Register(index));
                base.add(&index).value()
            }
            Operand::RegIndexBaseDisp(base, index, disp) => {
                let base = dfg.read(&Location::Register(base));
                let index = dfg.read(&Location::Register(index));
                base.add(&index).value().add(&V::from_const(disp as i64 as u64)).value()
            }
            Operand::RegIndexBaseScale(base, index, scale) => {
                let base = dfg.read(&Location::Register(base));
                let index = dfg.read(&Location::Register(index));
                base.add(&index.mul(&V::from_const(scale as u64)).value()).value()
            }
            Operand::RegIndexBaseScaleDisp(base, index, scale, disp) => {
                let base = dfg.read(&Location::Register(base));
                let index = dfg.read(&Location::Register(index));
                base.add(&index.mul(&V::from_const(scale as u64)).value()).value().add(&V::from_const(disp as i64 as u64)).value()
            }
            _ => {
                unsafe {
                    std::hint::unreachable_unchecked();
//                unreachable!("effective address of an immediate is an error: {:?}", operand);
                }
            }
        }
    }

    // TODO: get a disambiguator somehow
    #[inline(always)]
    fn write_operand<V: Value, D: DFG<V, Location=Location>>(dfg: &mut D, operand: &Operand, value: V) {
        match operand {
            Operand::Register(reg) => {
                dfg.write(&Location::Register(*reg), value)
            },
            Operand::ImmediateI8(_) |
            Operand::ImmediateU8(_) |
            Operand::ImmediateI16(_) |
            Operand::ImmediateU16(_) |
            Operand::ImmediateI32(_) |
            Operand::ImmediateU32(_) |
            Operand::ImmediateI64(_) |
            Operand::ImmediateU64(_) => {
                unsafe {
                    std::hint::unreachable_unchecked();
//                unreachable!("attempt to write to an immediate. x86_64 semantic is incorrect.");
                }
            }
            op => {
                let _ea = effective_address(dfg, op);
                let disambiguated = Location::Memory(ANY);
                dfg.write(&disambiguated, value)
            }
        }
    }

    #[inline(always)]
    fn read_jump_rel_operand<V: Value, D: DFG<V, Location=Location>>(dfg: &mut D, operand: &Operand) -> V {
        match operand {
            Operand::ImmediateI8(imm) => {
                V::from_const(*imm as i64 as u64)
            }
            Operand::ImmediateI32(imm) => {
                V::from_const(*imm as i64 as u64)
            }
            _ => {
                unsafe {
                    std::hint::unreachable_unchecked();
                }
            }
        }
    }

    // TODO: get a disambiguator somehow
    #[inline(always)]
    fn read_operand<V: Value, D: DFG<V, Location=Location>>(dfg: &mut D, operand: &Operand) -> V {
        match operand {
            Operand::Register(reg) => {
                dfg.read(&Location::Register(*reg))
            },
            Operand::ImmediateI8(imm) => {
                V::from_const(*imm as i64 as u64)
            }
            Operand::ImmediateU8(imm) => {
                V::from_const(*imm as u64)
            }
            Operand::ImmediateI16(imm) => {
                V::from_const(*imm as i64 as u64)
            }
            Operand::ImmediateU16(imm) => {
                V::from_const(*imm as u64)
            }
            Operand::ImmediateI32(imm) => {
                V::from_const(*imm as i64 as u64)
            }
            Operand::ImmediateU32(imm) => {
                V::from_const(*imm as u64)
            }
            Operand::ImmediateI64(imm) => {
                V::from_const(*imm as u64)
            }
            Operand::ImmediateU64(imm) => {
                V::from_const(*imm)
            }
            op => {
                let _ea = effective_address(dfg, op);
                let disambiguated = Location::Memory(ANY);
                dfg.read(&disambiguated)
            }
        }
    }

    fn pop<V: Value, D: DFG<V, Location=Location>>(dfg: &mut D) -> V {
        let value = read_operand(dfg, &Operand::RegDeref(RegSpec::rsp()));
        let rsp = Operand::Register(RegSpec::rsp());
        let adjusted_rsp = read_operand(dfg, &rsp).add(&V::from_const(rsp.width() as u64)).value();
        write_operand(dfg, &rsp, adjusted_rsp);
        value
    }

    fn push<V: Value, D: DFG<V, Location=Location>>(dfg: &mut D, value: V) {
        let rsp = Operand::Register(RegSpec::rsp());
        let adjusted_rsp = read_operand(dfg, &rsp).sub(&V::from_const(rsp.width() as u64)).value();
        write_operand(dfg, &rsp, adjusted_rsp);
        write_operand(dfg, &Operand::RegDeref(RegSpec::rsp()), value);
    }

    fn conditional_loc_write<
        V: Value,
        D: DFG<V, Location=Location>,
        DestFn: FnOnce() -> Location,
        R: FnOnce(&mut D) -> V,
        AR: FnOnce(&mut D) -> V
    >(dfg: &mut D, condition: V, dest: DestFn, result: R, antiresult: AR) {
        let res = match condition.as_bool() {
            Some(true) => {
                result(dfg)
            }
            Some(false) => {
                antiresult(dfg)
            }
            None => {
                V::from_set(&[result(dfg), antiresult(dfg)])
            }
        };
        dfg.write(&dest(), res);
    }

    fn conditional_write<
        V: Value,
        D: DFG<V, Location=Location>,
        DestFn: FnOnce() -> Operand,
        R: FnOnce(&mut D) -> V,
        AR: FnOnce(&mut D) -> V
    >(dfg: &mut D, condition: V, dest: DestFn, result: R, antiresult: AR) {
        let res = match condition.as_bool() {
            Some(true) => {
                result(dfg)
            }
            Some(false) => {
                antiresult(dfg)
            }
            None => {
                V::from_set(&[result(dfg), antiresult(dfg)])
            }
        };
        write_operand(dfg, &dest(), res);
    }

    match instr.opcode {
        Opcode::Invalid => {
            // TODO: something??
        }
        Opcode::RETURN => {
            let ra = pop(dfg);
            dfg.write(&Location::RIP, ra);
        },
        Opcode::HLT => {
            dfg.write(&Location::RIP, V::unknown());
            return CompletionStatus::Incomplete;
        },
        Opcode::CALL => {
            let ra = dfg.read(&Location::RIP);
            push(dfg, ra);
            let jump_target = read_operand(dfg, &instr.operand(0));
            dfg.write(&Location::RIP, jump_target);
        },
        Opcode::CALLF => {
            dfg.write(&Location::RIP, V::unknown());
            return CompletionStatus::Incomplete;
        },
        Opcode::JMP => {
            let jump_target = read_operand(dfg, &instr.operand(0));
            dfg.write(&Location::RIP, jump_target);
        },
        Opcode::JMPF => {
            dfg.write(&Location::RIP, V::unknown());
            return CompletionStatus::Incomplete;
        },
        Opcode::IMUL => {
        },
        Opcode::JO => {
            let of = dfg.read(&Location::OF).eq(&V::from_const(0));
            let condition = of;
            conditional_loc_write(
                dfg,
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&read_jump_rel_operand(dfg, &instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JNO => {
            let of = dfg.read(&Location::OF);
            let condition = of.not();
            conditional_loc_write(
                dfg,
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&read_jump_rel_operand(dfg, &instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JB => {
            let cf = dfg.read(&Location::CF);
            let condition = cf;
            conditional_loc_write(
                dfg,
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&read_jump_rel_operand(dfg, &instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JNB => {
            let cf = dfg.read(&Location::CF);
            let condition = cf.not();
            conditional_loc_write(
                dfg,
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&read_jump_rel_operand(dfg, &instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JZ => {
            let zf = dfg.read(&Location::ZF);
            let condition = zf;
            conditional_loc_write(
                dfg,
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&read_jump_rel_operand(dfg, &instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JNZ => {
            let zf = dfg.read(&Location::ZF);
            let condition = zf.not();
            conditional_loc_write(
                dfg,
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&read_jump_rel_operand(dfg, &instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JA => {
            let cf = dfg.read(&Location::CF).eq(&V::from_const(0));
            let zf = dfg.read(&Location::ZF).eq(&V::from_const(0));
            let condition = zf.and(&cf).value();
            conditional_loc_write(
                dfg,
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&read_jump_rel_operand(dfg, &instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JNA => {
            let cf = dfg.read(&Location::CF);
            let zf = dfg.read(&Location::ZF);
            let condition = zf.or(&cf).value();
            conditional_loc_write(
                dfg,
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&read_jump_rel_operand(dfg, &instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JS => {
            let sf = dfg.read(&Location::SF);
            let condition = sf;
            conditional_loc_write(
                dfg,
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&read_jump_rel_operand(dfg, &instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JNS => {
            let sf = dfg.read(&Location::SF);
            let condition = sf.not();
            conditional_loc_write(
                dfg,
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&read_jump_rel_operand(dfg, &instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JP => {
            let pf = dfg.read(&Location::PF);
            let condition = pf;
            conditional_loc_write(
                dfg,
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&read_jump_rel_operand(dfg, &instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JNP => {
            let pf = dfg.read(&Location::PF);
            let condition = pf.not();
            conditional_loc_write(
                dfg,
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&read_jump_rel_operand(dfg, &instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JL => {
            let sf = dfg.read(&Location::SF);
            let of = dfg.read(&Location::OF);
            let condition = sf.ne(&of);
            conditional_loc_write(
                dfg,
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&read_jump_rel_operand(dfg, &instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JGE => {
            let sf = dfg.read(&Location::SF);
            let of = dfg.read(&Location::OF);
            let condition = sf.eq(&of);
            conditional_loc_write(
                dfg,
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&read_jump_rel_operand(dfg, &instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JLE => {
            let zf = dfg.read(&Location::ZF);
            let sf = dfg.read(&Location::SF);
            let of = dfg.read(&Location::OF);
            let condition = zf.not().and(&sf.eq(&of)).value();
            conditional_loc_write(
                dfg,
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&read_jump_rel_operand(dfg, &instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JG => {
            let zf = dfg.read(&Location::ZF);
            let sf = dfg.read(&Location::SF);
            let of = dfg.read(&Location::OF);
            let condition = zf.not().and(&sf.eq(&of)).value();
            conditional_loc_write(
                dfg,
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&read_jump_rel_operand(dfg, &instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        _ => {
            return CompletionStatus::Incomplete;
        }
    };
    CompletionStatus::Incomplete
}
