/*
 * TODO: remove rip as a register in yaxpeax-x86, then a ToPC impl that maps Operand -> not-pc?
 * necessary for efficient compilation of `evaluate`, lest Operand::Register be treated as
 * potentially control-flow-influencing.
 */

use yaxpeax_x86::long_mode::{Arch as amd64};
use yaxpeax_x86::long_mode::{Opcode, Operand, RegSpec};
use yaxpeax_arch::Arch;
use yaxpeax_arch::AddressDiff;
use arch::x86_64::analyses::data_flow::Location;
use arch::x86_64::analyses::data_flow::ANY;
use analyses::{DFG, Value, DFGLocationQuery, DFGLocationQueryMut};
use analyses::CompletionStatus;
use analyses::ValueRes;
use analyses::IntoValueIndex;
use analyses::IndirectQuery;

pub mod specialized;

pub trait DFGAccessExt<V: Value> where Self: DFGLocationQuery<V, amd64> {
    // TODO: get a disambiguator somehow?
    fn effective_address(&self, operand: &Operand) -> V {
        match *operand {
            Operand::DisplacementU32(disp) => {
                V::from_const(disp as u64)
            },
            Operand::DisplacementU64(disp) => {
                V::from_const(disp as u64)
            },
            Operand::RegDeref(reg) => {
                self.read(&Location::Register(reg))
            }
            Operand::RegDisp(base, offset) => {
                let base = self.read(&Location::Register(base));
                base.add(&V::from_const(offset as i64 as u64)).value()
            }
            Operand::RegScale(base, scale) => {
                let base = self.read(&Location::Register(base));
                base.mul(&V::from_const(scale as u64)).value()
            }
            Operand::RegScaleDisp(base, scale, disp) => {
                let base = self.read(&Location::Register(base));
                base.mul(&V::from_const(scale as u64)).value().add(&V::from_const(disp as i64 as u64)).value()
            }
            Operand::RegIndexBase(base, index) => {
                let base = self.read(&Location::Register(base));
                let index = self.read(&Location::Register(index));
                base.add(&index).value()
            }
            Operand::RegIndexBaseDisp(base, index, disp) => {
                let base = self.read(&Location::Register(base));
                let index = self.read(&Location::Register(index));
                base.add(&index).value().add(&V::from_const(disp as i64 as u64)).value()
            }
            Operand::RegIndexBaseScale(base, index, scale) => {
                let base = self.read(&Location::Register(base));
                let index = self.read(&Location::Register(index));
                base.add(&index.mul(&V::from_const(scale as u64)).value()).value()
            }
            Operand::RegIndexBaseScaleDisp(base, index, scale, disp) => {
                let base = self.read(&Location::Register(base));
                let index = self.read(&Location::Register(index));
                base.add(&index.mul(&V::from_const(scale as u64)).value()).value().add(&V::from_const(disp as i64 as u64)).value()
            }
            _ => {
                unreachable!("effective address of an immediate is an error: {:?}", operand);
//                unsafe {
//                    std::hint::unreachable_unchecked();
//                }
            }
        }
    }

    // TODO: get a disambiguator somehow
    #[inline(always)]
    fn read_operand(&self, operand: &Operand) -> V {
        match operand {
            Operand::Register(reg) => {
                self.read(&Location::Register(*reg))
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
                let ea = self.effective_address(op);
                self.indirect(&Location::Memory(ANY)).load(ea.qword())
            }
        }
    }
}

pub trait DFGAccessExtMut<V: Value> where Self: DFGLocationQueryMut<V, amd64> {
    // TODO: get a disambiguator somehow
    #[inline(always)]
    fn write_operand(&mut self, operand: &Operand, value: V) {
        match operand {
            Operand::Register(reg) => {
                self.write(&Location::Register(*reg), value)
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
                let ea = self.effective_address(op);
                self.indirect(&Location::Memory(ANY)).store(ea.qword(), &value)
            }
        }
    }

    #[inline(always)]
    fn read_jump_rel_operand(&self, operand: &Operand) -> V {
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

    fn pop(&mut self) -> V {
        let value = self.read_operand(&Operand::RegDeref(RegSpec::rsp()));
        let rsp = Operand::Register(RegSpec::rsp());
        let adjusted_rsp = self.read_operand(&rsp).add(&V::from_const(rsp.width() as u64)).value();
        self.write_operand(&rsp, adjusted_rsp);
        value
    }

    fn push(&mut self, value: V) {
        let rsp = Operand::Register(RegSpec::rsp());
        let adjusted_rsp = self.read_operand(&rsp).sub(&V::from_const(rsp.width() as u64)).value();
        self.write_operand(&rsp, adjusted_rsp);
        self.write_operand(&Operand::RegDeref(RegSpec::rsp()), value);
    }

    fn write_alu_result(&mut self, dest_op: Operand, value: ValueRes<V>) {
        let value = self.write_alu_flags(value);
        self.write_operand(&dest_op, value);
    }

    fn write_alu_flags(&mut self, value: ValueRes<V>) -> V {
        self.write(&Location::OF, V::unknown());
        self.write(&Location::PF, V::unknown());
        self.write(&Location::AF, V::unknown());
        self.write(&Location::SF, value.sign());
        self.write(&Location::ZF, value.zero());
        let (value, carry) = value.parts();
        self.write(&Location::CF, carry);
        value
    }

    fn write_bitwise_result(&mut self, dest_op: Operand, value: ValueRes<V>) {
        let value = self.write_bitwise_flags(value);
        self.write_operand(&dest_op, value);
    }

    fn write_bitwise_flags(&mut self, value: ValueRes<V>) -> V {
        self.write(&Location::OF, V::from_const(0));
        self.write(&Location::CF, V::from_const(0));
        self.write(&Location::PF, V::unknown());
        self.write(&Location::AF, V::unknown());
        self.write(&Location::SF, value.sign());
        self.write(&Location::ZF, value.zero());
        let value = value.value();
        value
    }

    fn conditional_loc_write<
        DestFn: FnOnce() -> Location,
        R: FnOnce(&mut Self) -> V,
        AR: FnOnce(&mut Self) -> V
    >(&mut self, condition: V, dest: DestFn, result: R, antiresult: AR) {
        let res = match condition.as_bool() {
            Some(true) => {
                result(self)
            }
            Some(false) => {
                antiresult(self)
            }
            None => {
                V::from_set(&[result(self), antiresult(self)])
            }
        };
        self.write(&dest(), res);
    }

    #[allow(dead_code)]
    fn conditional_write<
        DestFn: FnOnce() -> Operand,
        R: FnOnce(&mut Self) -> V,
        AR: FnOnce(&mut Self) -> V
    >(&mut self, condition: V, dest: DestFn, result: R, antiresult: AR) {
        let res = match condition.as_bool() {
            Some(true) => {
                result(self)
            }
            Some(false) => {
                antiresult(self)
            }
            None => {
                V::from_set(&[result(self), antiresult(self)])
            }
        };
        self.write_operand(&dest(), res);
    }
}

impl<V: Value, D: DFGLocationQuery<V, amd64>> DFGAccessExt<V> for D { }
impl<V: Value, D: DFGLocationQueryMut<V, amd64>> DFGAccessExtMut<V> for D { }

pub fn evaluate<K: Copy, V: Value, D: DFG<V, amd64, K>>(when: K, instr: &<amd64 as Arch>::Instruction, dfg: &mut D) -> CompletionStatus {
    let dfg = &mut dfg.query_at_mut(when);
    match instr.opcode() {
        Opcode::Invalid => {
            // TODO: something??
        }
        Opcode::NOP => {},
        Opcode::LEA => {
            let ea = dfg.effective_address(&instr.operand(1));
            dfg.write_operand(&instr.operand(0), ea);
        }
        Opcode::SAR => {
            let amt = dfg.read_operand(&instr.operand(1));
            let value = dfg.read_operand(&instr.operand(0));
            let res = value.sar(&amt.modulo(&V::from_const(instr.operand(0).width() as u64 * 8)));
            dfg.write_operand(&instr.operand(0), res);
        }
        Opcode::SHR => {
            let amt = dfg.read_operand(&instr.operand(1));
            let value = dfg.read_operand(&instr.operand(0));
            let res = value.shr(&amt.modulo(&V::from_const(instr.operand(0).width() as u64 * 8)));
            dfg.write_operand(&instr.operand(0), res);
        }
        Opcode::SHL => {
            let amt = dfg.read_operand(&instr.operand(1));
            let value = dfg.read_operand(&instr.operand(0));
            let res = value.shl(&amt.modulo(&V::from_const(instr.operand(0).width() as u64 * 8)));
            dfg.write_operand(&instr.operand(0), res);
        }
        Opcode::CMP => {
            if instr.operand(0) == instr.operand(1) {
                dfg.write_alu_flags(ValueRes::from_zero());
            } else {
                let dest = dfg.read_operand(&instr.operand(0));
                let src = dfg.read_operand(&instr.operand(1));
                dfg.write_alu_flags(dest.sub(&src));
            }
        }
        Opcode::TEST => {
            if instr.operand(0) == instr.operand(1) {
                dfg.write_bitwise_flags(ValueRes::from_zero());
            } else {
                let dest = dfg.read_operand(&instr.operand(0));
                let src = dfg.read_operand(&instr.operand(1));
                dfg.write_bitwise_flags(dest.and(&src));
            }
        }
        Opcode::MOV => {
            let value = dfg.read_operand(&instr.operand(1));
            dfg.write_operand(&instr.operand(0), value);
        }
        Opcode::INC => {
            let src = dfg.read_operand(&instr.operand(0));
            let one = V::from_const(1);
            let value = src.add(&one);
            dfg.write(&Location::OF, V::unknown());
            dfg.write(&Location::PF, V::unknown());
            dfg.write(&Location::AF, V::unknown());
            dfg.write(&Location::SF, value.sign());
            dfg.write(&Location::ZF, value.zero());
            let value = value.value();
            dfg.write_operand(&instr.operand(0), value);
        }
        Opcode::DEC => {
            let src = dfg.read_operand(&instr.operand(0));
            let one = V::from_const(1);
            let value = src.sub(&one);
            dfg.write(&Location::OF, V::unknown());
            dfg.write(&Location::PF, V::unknown());
            dfg.write(&Location::AF, V::unknown());
            dfg.write(&Location::SF, value.sign());
            dfg.write(&Location::ZF, value.zero());
            let value = value.value();
            dfg.write_operand(&instr.operand(0), value);
        }
        Opcode::NEG => {
            let src = dfg.read_operand(&instr.operand(0));
            let zero = V::from_const(0);
            let value = zero.sub(&src);
            // from docs:
            // if (src == 0) CF = 0; else CF = 1;
            // but i think the current implementation upholds those semantics?
            dfg.write_alu_result(instr.operand(0), value);
        }
        Opcode::UD2 => {
            dfg.write(&Location::RIP, V::unknown());
            return CompletionStatus::Incomplete;
        }
        Opcode::SETO => {}
        Opcode::SETNO => {}
        Opcode::SETB => {}
        Opcode::SETAE => {}
        Opcode::SETZ => {}
        Opcode::SETNZ => {}
        Opcode::SETBE => {}
        Opcode::SETA => {}
        Opcode::SETS => {}
        Opcode::SETNS => {}
        Opcode::SETP => {}
        Opcode::SETNP => {}
        Opcode::SETL => {}
        Opcode::SETGE => {}
        Opcode::SETLE => {}
        Opcode::SETG => {}
        Opcode::CMOVO => {}
        Opcode::MUL => {}
        Opcode::SUB => {
            if instr.operand(0) == instr.operand(1) {
                dfg.write_alu_result(instr.operand(0), ValueRes::from_zero());
            } else {
                let dest = dfg.read_operand(&instr.operand(0));
                let src = dfg.read_operand(&instr.operand(1));
                dfg.write_alu_result(instr.operand(0), dest.sub(&src));
            }
        }
        Opcode::ADD => {
            let dest = dfg.read_operand(&instr.operand(0));
            let src = dfg.read_operand(&instr.operand(1));
            dfg.write_alu_result(instr.operand(0), src.add(&dest));
        }
        Opcode::XOR => {
            if instr.operand(0) == instr.operand(1) {
                dfg.write_bitwise_result(instr.operand(0), ValueRes::from_zero());
            } else {
                let dest = dfg.read_operand(&instr.operand(0));
                let src = dfg.read_operand(&instr.operand(1));
                dfg.write_bitwise_result(instr.operand(0), src.xor(&dest));
            }
        }
        Opcode::AND => {
            let dest = dfg.read_operand(&instr.operand(0));
            let src = dfg.read_operand(&instr.operand(1));
            dfg.write_bitwise_result(instr.operand(0), src.and(&dest));
        }
        Opcode::OR => {
            let dest = dfg.read_operand(&instr.operand(0));
            let src = dfg.read_operand(&instr.operand(1));
            dfg.write_bitwise_result(instr.operand(0), src.or(&dest));
        }
        Opcode::NOT => {
            let dest = dfg.read_operand(&instr.operand(0));
            dfg.write_operand(&instr.operand(0), dest.not());
        }
        Opcode::RETURN => {
            let ra = dfg.pop();
            dfg.write(&Location::RIP, ra);
        },
        Opcode::HLT => {
            dfg.write(&Location::RIP, V::unknown());
            return CompletionStatus::Incomplete;
        },
        Opcode::CALL => {
            let ra = dfg.read(&Location::RIP);
            dfg.push(ra);
            let jump_target = dfg.read_operand(&instr.operand(0));
            dfg.write(&Location::RIP, jump_target);
        },
        Opcode::CALLF => {
            dfg.write(&Location::RIP, V::unknown());
            return CompletionStatus::Incomplete;
        },
        Opcode::JMP => {
            let jump_target = dfg.read_operand(&instr.operand(0));
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
            dfg.conditional_loc_write(
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&dfg.read_jump_rel_operand(&instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JNO => {
            let of = dfg.read(&Location::OF);
            let condition = of.not();
            dfg.conditional_loc_write(
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&dfg.read_jump_rel_operand(&instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JB => {
            let cf = dfg.read(&Location::CF);
            let condition = cf;
            dfg.conditional_loc_write(
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&dfg.read_jump_rel_operand(&instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JNB => {
            let cf = dfg.read(&Location::CF);
            let condition = cf.not();
            dfg.conditional_loc_write(
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&dfg.read_jump_rel_operand(&instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JZ => {
            let zf = dfg.read(&Location::ZF);
            let condition = zf;
            dfg.conditional_loc_write(
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&dfg.read_jump_rel_operand(&instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JNZ => {
            let zf = dfg.read(&Location::ZF);
            let condition = zf.not();
            dfg.conditional_loc_write(
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&dfg.read_jump_rel_operand(&instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JA => {
            let cf = dfg.read(&Location::CF).eq(&V::from_const(0));
            let zf = dfg.read(&Location::ZF).eq(&V::from_const(0));
            let condition = zf.and(&cf).value();
            dfg.conditional_loc_write(
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&dfg.read_jump_rel_operand(&instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JNA => {
            let cf = dfg.read(&Location::CF);
            let zf = dfg.read(&Location::ZF);
            let condition = zf.or(&cf).value();
            dfg.conditional_loc_write(
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&dfg.read_jump_rel_operand(&instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JS => {
            let sf = dfg.read(&Location::SF);
            let condition = sf;
            dfg.conditional_loc_write(
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&dfg.read_jump_rel_operand(&instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JNS => {
            let sf = dfg.read(&Location::SF);
            let condition = sf.not();
            dfg.conditional_loc_write(
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&dfg.read_jump_rel_operand(&instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JP => {
            let pf = dfg.read(&Location::PF);
            let condition = pf;
            dfg.conditional_loc_write(
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&dfg.read_jump_rel_operand(&instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JNP => {
            let pf = dfg.read(&Location::PF);
            let condition = pf.not();
            dfg.conditional_loc_write(
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&dfg.read_jump_rel_operand(&instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JL => {
            let sf = dfg.read(&Location::SF);
            let of = dfg.read(&Location::OF);
            let condition = sf.ne(&of);
            dfg.conditional_loc_write(
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&dfg.read_jump_rel_operand(&instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JGE => {
            let sf = dfg.read(&Location::SF);
            let of = dfg.read(&Location::OF);
            let condition = sf.eq(&of);
            dfg.conditional_loc_write(
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&dfg.read_jump_rel_operand(&instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JLE => {
            let zf = dfg.read(&Location::ZF);
            let sf = dfg.read(&Location::SF);
            let of = dfg.read(&Location::OF);
            let condition = zf.not().and(&sf.eq(&of)).value();
            dfg.conditional_loc_write(
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&dfg.read_jump_rel_operand(&instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        Opcode::JG => {
            let zf = dfg.read(&Location::ZF);
            let sf = dfg.read(&Location::SF);
            let of = dfg.read(&Location::OF);
            let condition = zf.not().and(&sf.eq(&of)).value();
            dfg.conditional_loc_write(
                condition,
                || Location::RIP,
                |dfg| { dfg.read(&Location::RIP).add(&dfg.read_jump_rel_operand(&instr.operand(0))).value() },
                |dfg| { dfg.read(&Location::RIP) },
            );
        },
        _ => {
            return CompletionStatus::Incomplete;
        }
    };
    CompletionStatus::Incomplete
}
