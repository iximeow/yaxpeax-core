use yaxpeax_arch::Arch;
use yaxpeax_x86::{x86_64, Instruction, Operand, Opcode, RegSpec, RegisterBank};
use arch::x86_64::x86_64Data;
use arch::x86_64::analyses::data_flow::{Data, Location};
use analyses::evaluators::const_evaluator::{Domain, ConstEvaluator};
use analyses::static_single_assignment::SSA;

pub struct ConcreteDomain;

impl Domain for ConcreteDomain {
    type Value = u64;

    fn join(l: Option<u64>, r: Option<u64>) -> Option<Self::Value> {
        if l == r {
            l
        } else {
            None
        }
    }
}

fn effective_address(instr: &Instruction, mem_op: &Operand, addr: <x86_64 as Arch>::Address, dfg: &SSA<x86_64>, contexts: &x86_64Data) -> Option<u64> {
    match mem_op {
        Operand::DisplacementU32(disp) => Some(*disp as i32 as i64 as u64),
        Operand::DisplacementU64(disp) => Some(*disp),
        Operand::RegDisp(RegSpec { num: 0, bank: RegisterBank::RIP }, disp) => {
            Some((addr.wrapping_add(instr.length as u64)).wrapping_add(*disp as i64 as u64))
        }
        Operand::RegDeref(reg) => {
            match dfg.get_use(addr, Location::Register(*reg)).get_data() {
                Some(Data::Concrete(v, _)) => {
                    Some(v)
                },
                _ => None
            }
        },
        Operand::RegDisp(reg, disp) => {
            match dfg.get_use(addr, Location::Register(*reg)).get_data() {
                Some(Data::Concrete(v, _)) => {
                    Some(v.wrapping_add(*disp as i64 as u64))
                },
                _ => None
            }
        },
        Operand::RegScale(reg, scale) => {
            match dfg.get_use(addr, Location::Register(*reg)).get_data() {
                Some(Data::Concrete(v, _)) => {
                    Some(v.wrapping_mul(*scale as u64))
                },
                _ => None
            }
        },
        Operand::RegIndexBase(base, index) => {
            match (
                dfg.get_use(addr, Location::Register(*base)).get_data(),
                dfg.get_use(addr, Location::Register(*index)).get_data()
            ) {
                (Some(Data::Concrete(base, _)), Some(Data::Concrete(index, _))) => {
                    Some(base.wrapping_add(index))
                },
                _ => None
            }
        },
        Operand::RegIndexBaseDisp(base, index, disp) => {
            match (
                dfg.get_use(addr, Location::Register(*base)).get_data(),
                dfg.get_use(addr, Location::Register(*index)).get_data()
            ) {
                (Some(Data::Concrete(base, _)), Some(Data::Concrete(index, _))) => {
                    Some(base.wrapping_add(index).wrapping_add(*disp as i64 as u64))
                },
                _ => None
            }
        }
        Operand::RegScaleDisp(base, scale, disp) => {
            match dfg.get_use(addr, Location::Register(*base)).get_data() {
                Some(Data::Concrete(base, _)) => {
                    Some(base.wrapping_add((*disp as i64 as u64).wrapping_mul(*scale as i64 as u64)))
                },
                _ => None
            }
        }
        Operand::RegIndexBaseScale(index, base, scale) => {
            match (
                dfg.get_use(addr, Location::Register(*base)).get_data(),
                dfg.get_use(addr, Location::Register(*index)).get_data()
            ) {
                (Some(Data::Concrete(base, _)), Some(Data::Concrete(index, _))) => {
                    Some(index.wrapping_mul(*scale as u64).wrapping_add(base))
                },
                _ => None
            }
        }
        Operand::RegIndexBaseScaleDisp(index, base, scale, disp) => {
            match (
                dfg.get_use(addr, Location::Register(*base)).get_data(),
                dfg.get_use(addr, Location::Register(*index)).get_data()
            ) {
                (Some(Data::Concrete(base, _)), Some(Data::Concrete(index, _))) => {
                    Some(index.wrapping_mul(*scale as u64).wrapping_add(base).wrapping_add(*disp as i64 as u64))
                },
                _ => None
            }
        }
        _ => None
    }
}

impl ConstEvaluator<x86_64, x86_64Data, ConcreteDomain> for x86_64 {
    fn evaluate(instr: &<x86_64 as Arch>::Instruction, addr: <x86_64 as Arch>::Address, dfg: &SSA<x86_64>, _contexts: &x86_64Data) {
        //TODO: handle prefixes like at all
        match instr {
            Instruction { opcode: Opcode::XOR, operands: [Operand::Register(l), Operand::Register(r)], .. } => {
                if l == r {
                    dfg.get_def(addr, Location::Register(*r)).update(Data::Concrete(0, None));
                }
            }
            Instruction { opcode: Opcode::SUB, operands: [Operand::Register(l), Operand::Register(r)], .. } => {
                if l == r {
                    dfg.get_def(addr, Location::Register(*r)).update(Data::Concrete(0, None));
                }
            }
            Instruction { opcode: Opcode::MOV, operands: [Operand::Register(l), Operand::Register(r)], .. } => {
                dfg.get_def(addr, Location::Register(*l)).update(
                    Data::Alias(dfg.get_use(addr, Location::Register(*r)).as_rc())
                );
            }
            Instruction { opcode: Opcode::LEA, operands: [Operand::Register(l), mem_op], .. } => {
                effective_address(instr, mem_op, addr, dfg, _contexts).map(|ea| {
                    dfg.get_def(addr, Location::Register(*l)).update(Data::Concrete(ea, None));
                });
            },
            _ => { }
        }
    }
}
