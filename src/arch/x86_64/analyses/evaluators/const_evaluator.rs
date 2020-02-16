use yaxpeax_arch::Arch;
use yaxpeax_x86::long_mode::{Instruction, Operand, Opcode, RegSpec, RegisterBank};
use yaxpeax_x86::x86_64;
use data::modifier::ModifierExpression;
use arch::x86_64::analyses::data_flow::{Data, Location};
use analyses::evaluators::const_evaluator::{Domain, ConstEvaluator};
use analyses::static_single_assignment::SSA;
use data::ValueLocations;
use memory::MemoryRange;
use yaxpeax_arch::LengthedInstruction;

pub struct ConcreteDomain;

impl Domain for ConcreteDomain {
    type Modifier = ModifierExpression;
    type Value = u64;

    fn join(l: Option<u64>, r: Option<u64>) -> Option<Self::Value> {
        if l == r {
            l
        } else {
            None
        }
    }
}

fn effective_address(instr: &Instruction, mem_op: &Operand, addr: <x86_64 as Arch>::Address, dfg: &SSA<x86_64>, _contexts: &()) -> Option<u64> {
    match mem_op {
        Operand::DisplacementU32(disp) => Some(*disp as i32 as i64 as u64),
        Operand::DisplacementU64(disp) => Some(*disp),
        Operand::RegDisp(RegSpec { num: 0, bank: RegisterBank::RIP }, disp) => {
            Some((addr.wrapping_add(instr.len())).wrapping_add(*disp as i64 as u64))
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

impl ConstEvaluator<x86_64, (), ConcreteDomain> for x86_64 {
    fn apply_transient(from: <x86_64 as Arch>::Address, to: <x86_64 as Arch>::Address, location: Option<<x86_64 as ValueLocations>::Location>, exprs: &Vec<ModifierExpression>, dfg: &SSA<x86_64>, _contexts: &()) {
        for expr in exprs {
            match expr {
                ModifierExpression::IsNot(_) |
                ModifierExpression::Below(_) |
                ModifierExpression::Above(_) => { }
                ModifierExpression::Is(v) => {
                    if let Some(loc) = location {
                        println!("Applying bound {:?} to location {:?}", expr, loc);
                        println!("The corresponding def version is: {:?}", dfg.get_transient_def(from, to, loc).as_rc());
                        dfg.get_transient_def(from, to, loc).update(Data::Concrete(*v, None));
                    }
                }
            }
        }
    }

    fn evaluate_instruction<U: MemoryRange<<x86_64 as Arch>::Address>>(instr: &<x86_64 as Arch>::Instruction, addr: <x86_64 as Arch>::Address, dfg: &SSA<x86_64>, _contexts: &(), _data: &U) {
        //TODO: handle prefixes like at all
        match instr.opcode {
            Opcode::XOR => {
                if let (Operand::Register(l), Operand::Register(r)) = (instr.operand(0), instr.operand(1)) {
                    if l == r {
                        dfg.get_def(addr, Location::Register(r)).update(Data::Concrete(0, None));
                    }
                }
            },
            Opcode::SUB => {
                if let (Operand::Register(l), Operand::Register(r)) = (instr.operand(0), instr.operand(1)) {
                    if l == r {
                        dfg.get_def(addr, Location::Register(r)).update(Data::Concrete(0, None));
                    }
                }
            },
            Opcode::MOV => {
                if let (Operand::Register(l), Operand::Register(r)) = (instr.operand(0), instr.operand(1)) {
                    let def_site = if l.bank == RegisterBank::D {
                        // this assigns to Q instead of D
                        dfg.get_def(addr, Location::Register(RegSpec { num: l.num, bank: RegisterBank::Q }))
                    } else {
                        dfg.get_def(addr, Location::Register(l))
                    };

                    def_site.update(
                        Data::Alias(dfg.get_use(addr, Location::Register(r)).as_rc())
                    );
                }
            },
            Opcode::LEA => {
                if let (Operand::Register(l), mem_op) = (instr.operand(0), instr.operand(1)) {
                    effective_address(instr, &mem_op, addr, dfg, _contexts).map(|ea| {
                        dfg.get_def(addr, Location::Register(l)).update(Data::Concrete(ea, None));
                    });
                }
            },
            _ => {}
        }
    }
}
