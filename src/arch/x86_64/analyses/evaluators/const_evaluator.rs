use yaxpeax_arch::{AddressDiff, AddressBase, Arch};
use yaxpeax_x86::long_mode::{register_class, Instruction, Operand, Opcode, RegSpec};
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
        Operand::RegDisp(RegSpec::RIP, disp) => {
            Some((addr.wrapping_offset(instr.len())).wrapping_offset(AddressDiff::from_const(*disp as i64 as u64)))
        }
        Operand::RegDeref(reg) => {
            match dfg.get_use(addr, Location::Register(*reg)).get_data().as_ref() {
                Some(Data::Concrete(v, _)) => {
                    Some(*v)
                },
                _ => None
            }
        },
        Operand::RegDisp(reg, disp) => {
            match dfg.get_use(addr, Location::Register(*reg)).get_data().as_ref() {
                Some(Data::Concrete(v, _)) => {
                    Some(v.wrapping_add(*disp as i64 as u64))
                },
                _ => None
            }
        },
        Operand::RegScale(reg, scale) => {
            match dfg.get_use(addr, Location::Register(*reg)).get_data().as_ref() {
                Some(Data::Concrete(v, _)) => {
                    Some(v.wrapping_mul(*scale as u64))
                },
                _ => None
            }
        },
        Operand::RegIndexBase(base, index) => {
            match (
                dfg.get_use(addr, Location::Register(*base)).get_data().as_ref(),
                dfg.get_use(addr, Location::Register(*index)).get_data().as_ref()
            ) {
                (Some(Data::Concrete(base, _)), Some(Data::Concrete(index, _))) => {
                    Some(base.wrapping_add(*index))
                },
                _ => None
            }
        },
        Operand::RegIndexBaseDisp(base, index, disp) => {
            match (
                dfg.get_use(addr, Location::Register(*base)).get_data().as_ref(),
                dfg.get_use(addr, Location::Register(*index)).get_data().as_ref()
            ) {
                (Some(Data::Concrete(base, _)), Some(Data::Concrete(index, _))) => {
                    Some(base.wrapping_add(*index).wrapping_add(*disp as i64 as u64))
                },
                _ => None
            }
        }
        Operand::RegScaleDisp(base, scale, disp) => {
            match dfg.get_use(addr, Location::Register(*base)).get_data().as_ref() {
                Some(Data::Concrete(base, _)) => {
                    Some(base.wrapping_add((*disp as i64 as u64).wrapping_mul(*scale as i64 as u64)))
                },
                _ => None
            }
        }
        Operand::RegIndexBaseScale(index, base, scale) => {
            match (
                dfg.get_use(addr, Location::Register(*base)).get_data().as_ref(),
                dfg.get_use(addr, Location::Register(*index)).get_data().as_ref()
            ) {
                (Some(Data::Concrete(base, _)), Some(Data::Concrete(index, _))) => {
                    Some(index.wrapping_mul(*scale as u64).wrapping_add(*base))
                },
                _ => None
            }
        }
        Operand::RegIndexBaseScaleDisp(index, base, scale, disp) => {
            match (
                dfg.get_use(addr, Location::Register(*base)).get_data().as_ref(),
                dfg.get_use(addr, Location::Register(*index)).get_data().as_ref()
            ) {
                (Some(Data::Concrete(base, _)), Some(Data::Concrete(index, _))) => {
                    Some(index.wrapping_mul(*scale as u64).wrapping_add(*base).wrapping_add(*disp as i64 as u64))
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
                    if let Some(loc) = &location {
                        println!("Applying bound {:?} to location {:?}", expr, loc);
                        println!("The corresponding def version is: {:?}", dfg.get_transient_def(from, to, loc.clone()).as_rc());
                        dfg.get_transient_def(from, to, loc.clone()).update(Data::Concrete(*v, None));
                    }
                }
            }
        }
    }

    fn evaluate_instruction<U: MemoryRange<x86_64>>(instr: &<x86_64 as Arch>::Instruction, addr: <x86_64 as Arch>::Address, dfg: &SSA<x86_64>, _contexts: &(), _data: &U) {
        //TODO: handle prefixes like at all
        match instr.opcode() {
            Opcode::XOR => {
                if let (Operand::Register(l), Operand::Register(r)) = (instr.operand(0), instr.operand(1)) {
                    if l == r {
                        dfg.get_def(addr, Location::Register(r)).update(Data::Concrete(0, None));
                    }
                }
            },
            Opcode::ADD => {
                if let (Operand::Register(l), Operand::Register(r)) = (instr.operand(0), instr.operand(1)) {
                    let l_value = dfg.get_use(addr, Location::Register(l));
                    let r_value = dfg.get_use(addr, Location::Register(r));
                    match (l_value.get_data().as_ref(), r_value.get_data().as_ref()) {
                        (Some(Data::Concrete(l_v, _)), Some(Data::Concrete(r_v, _))) => {
                            let def_site = if l.class() == register_class::D {
                                // this assigns to Q instead of D
                                dfg.get_def(addr, Location::Register(RegSpec::q(l.num())))
                            } else {
                                dfg.get_def(addr, Location::Register(l))
                            };
                            def_site.update(
                                Data::Concrete(l_v.wrapping_add(*r_v), None)
                            );
                        }
                        _ => {}
                    };
                }
            }
            Opcode::SUB => {
                if let (Operand::Register(l), Operand::Register(r)) = (instr.operand(0), instr.operand(1)) {
                    if l == r {
                        dfg.get_def(addr, Location::Register(r)).update(Data::Concrete(0, None));
                    }
                }
            },
            Opcode::MOV => {
                // println!("DOING MOV for {}", instr);
                let def_site = if let Operand::Register(l) = instr.operand(0) {
                    if l.class() == register_class::D {
                        // this assigns to Q instead of D
                        dfg.get_def(addr, Location::Register(RegSpec::q(l.num())))
                    } else {
                        dfg.get_def(addr, Location::Register(l))
                    }
                } else {
                    // memory operand of some kind
                    if let Some(src) = crate::arch::x86_64::analyses::evaluators::symbolizer::referent(instr, &instr.operand(0), addr, dfg, _contexts) {
                        use analyses::memory_layout::MemoryAccessBaseInference;
                        use analyses::static_single_assignment::DFGLValue;
                        use crate::arch::x86_64::analyses::data_flow::ANY;
                        if let Some((base, addend)) = MemoryAccessBaseInference::infer_base_and_addend(&src) {
                            if let Some(def) = dfg.try_get_def(addr, Location::MemoryLocation(ANY, 8 /* l.width() */, Some((Data::Expression(base), Data::Expression(addend))))) {
                                DFGLValue { value: def }
                            } else {
                                return;
                            }
                        } else {
                            return;
                        }
                    } else {
                        return;
                    }
                };

                match instr.operand(1) {
                    Operand::Register(r) => {
                        def_site.update(
                            Data::Alias(dfg.get_use(addr, Location::Register(r)).as_rc())
                        );
                    },
                    Operand::ImmediateI8(imm) => {
                        def_site.update(Data::Concrete(imm as i64 as u64, None));
                    }
                    Operand::ImmediateU8(imm) => {
                        def_site.update(Data::Concrete(imm as u64, None));
                    }
                    Operand::ImmediateI16(imm) => {
                        def_site.update(Data::Concrete(imm as i64 as u64, None));
                    }
                    Operand::ImmediateU16(imm) => {
                        def_site.update(Data::Concrete(imm as u64, None));
                    }
                    Operand::ImmediateI32(imm) => {
                        def_site.update(Data::Concrete(imm as i64 as u64, None));
                    }
                    Operand::ImmediateU32(imm) => {
                        def_site.update(Data::Concrete(imm as u64, None));
                    }
                    Operand::ImmediateI64(imm) => {
                        def_site.update(Data::Concrete(imm as u64, None));
                    }
                    Operand::ImmediateU64(imm) => {
                        def_site.update(Data::Concrete(imm, None));
                    }
                    _ => {}
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
