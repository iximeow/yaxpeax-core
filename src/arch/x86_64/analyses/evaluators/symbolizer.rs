use yaxpeax_arch::Arch;
use yaxpeax_arch::AddressBase;
use yaxpeax_arch::AddressDiff;
use yaxpeax_x86::long_mode::{Instruction, Operand, Opcode, RegSpec, RegisterBank};
use yaxpeax_x86::x86_64;
use arch::x86_64::analyses::data_flow::{Data, Location, SymbolicExpression};
use analyses::evaluators::const_evaluator::{Domain, ConstEvaluator};
use analyses::static_single_assignment::SSA;
use data;
use data::modifier::ModifierExpression;
use data::ValueLocations;
use data::types::TypeSpec;
use memory::MemoryRange;
use yaxpeax_arch::LengthedInstruction;

pub struct SymbolicDomain;

impl Domain for SymbolicDomain {
    type Modifier = ModifierExpression;
    type Value = SymbolicExpression;

    fn join(l: Option<Self::Value>, r: Option<Self::Value>) -> Option<Self::Value> {
        if l == r {
            l
        } else {
            None
        }
    }
}

fn referent(instr: &Instruction, mem_op: &Operand, addr: <x86_64 as Arch>::Address, dfg: &SSA<x86_64>, _contexts: &()) -> Option<SymbolicExpression> {
    match mem_op {
        Operand::DisplacementU32(disp) => {
            if instr.prefixes.gs() {
                if *disp < 0x10000 {
                    Some(SymbolicExpression::Add(
                        Box::new(SymbolicExpression::Opaque(TypeSpec::PointerTo(Box::new(TypeSpec::LayoutId(data::types::KPCR))))),
                        *disp as u64
                    ))
                } else {
                // TODO
                    None
                }
            } else {
                // TODO
                None
            }
        }
        Operand::DisplacementU64(disp) => {
            if instr.prefixes.gs() {
                if *disp < 0x10000 {
                    Some(SymbolicExpression::Add(
                        Box::new(SymbolicExpression::Opaque(TypeSpec::PointerTo(Box::new(TypeSpec::LayoutId(data::types::KPCR))))),
                        *disp as u64
                        *disp
                    ))
                } else {
                // TODO
                    None
                }
            } else {
                // TODO
                None
            }
        }
        Operand::RegDisp(RegSpec { num: 0, bank: RegisterBank::RIP }, disp) => {
            let addr = (addr.wrapping_offset(instr.len())).wrapping_offset(AddressDiff::from_const(*disp as i64 as u64));
            if addr == 0x1402a3148 {
                // its that global struct referenced in ntoskrnl:0x1402a9370
                // ... it's actually the linear address of the KPCR...
                Some(SymbolicExpression::Opaque(
                    TypeSpec::PointerTo(Box::new(TypeSpec::PointerTo(Box::new(
                        TypeSpec::LayoutId(data::types::KPCR)
                    ))))
                ))
            } else if addr == 0x1402a3148 {
                // its that global struct referenced in ntoskrnl:0x1402a9387
                // stored in the KPCR, MAYBE??
                Some(SymbolicExpression::Opaque(
                    TypeSpec::PointerTo(Box::new(TypeSpec::PointerTo(Box::new(
                        TypeSpec::Unknown
                    ))))
                ))
            } else {
                None
            }
        }
        Operand::RegDeref(reg) => {
            match dfg.get_use(addr, Location::Register(*reg)).get_data() {
                Some(Data::Concrete(_v, None)) => {
                    // TODO: check const addr derefs for the same structures checked in disp
                    // eg gs:[rax] for rax = 0
                    None
                },
                Some(Data::Concrete(_v, Some(_ty))) => {
                    // TODO: check const addr derefs for the same structures checked in disp
                    // eg gs:[rax] for rax = 0
                    None
                },
                Some(Data::Expression(sym)) => Some(sym),
                _ => None
            }
        },
        Operand::RegDisp(reg, disp) => {
            match dfg.get_use(addr, Location::Register(*reg)).get_data() {
                Some(Data::Concrete(_v, None)) => {
//                    v.wrapping_add(*disp as i64 as u64).unwrap();
                    // TODO: check const addr derefs for the same structures checked in disp
                    // eg gs:[rax] for rax = 0
                    None
                },
                Some(Data::Concrete(_v, Some(_ty))) => {
//                    v.wrapping_add(*disp as i64 as u64).unwrap();
                    // TODO: check const addr derefs for the same structures checked in disp
                    // eg gs:[rax] for rax = 0
                    None
                },
                Some(Data::Expression(sym)) => Some(sym.offset(*disp as i64 as u64)),
                _ => None
            }
        },
        Operand::RegScale(_reg, _scale) => {
            None
        },
        Operand::RegIndexBase(_base, _index) => {
            None
        },
        Operand::RegIndexBaseDisp(_base, _index, _disp) => {
            None
        }
        Operand::RegScaleDisp(_base, _scale, _disp) => {
            None
        }
        Operand::RegIndexBaseScale(_index, _base, _scale) => {
            None
        }
        Operand::RegIndexBaseScaleDisp(_index, _base, _scale, _disp) => {
            None
        }
        _ => None
    }
}

impl ConstEvaluator<x86_64, (), SymbolicDomain> for x86_64 {
    fn apply_transient(_from: <x86_64 as Arch>::Address, _to: <x86_64 as Arch>::Address, _location: Option<<x86_64 as ValueLocations>::Location>, _exprs: &Vec<<SymbolicDomain as Domain>::Modifier>, _dfg: &SSA<x86_64>, _contexts: &()) {

    }
    fn evaluate_instruction<U: MemoryRange<<x86_64 as Arch>::Address>>(instr: &<x86_64 as Arch>::Instruction, addr: <x86_64 as Arch>::Address, dfg: &SSA<x86_64>, contexts: &(), _data: &U) {
        //TODO: handle prefixes like at all
        match instr.opcode {
            Opcode::MOV => {
                match (instr.operand(0), instr.operand(1)) {
                    (Operand::Register(l), op) => {
                        if op.is_memory() {
                            // might be a pointer deref or somesuch.
                            if let Some(src) = referent(instr, &op, addr, dfg, contexts) {
                                let def = dfg.get_def(addr, Location::Register(l));
                                if def.get_data().is_none() {
                                    def.update(
                                        Data::Expression(SymbolicExpression::Deref(Box::new(src)))
                                    )
                                }
                            }
                        }
                    },
                    _ => {
                        // don't handle others yet
                    }
                }
            }
            Opcode::ADD => {
                match instr.operand(0) {
                    Operand::Register(l) => {
                        let offset = match instr.operand(1) {
                            Operand::ImmediateI64(i) => i,
                            Operand::ImmediateI32(i) => i as i64,
                            Operand::ImmediateI8(i) => i as i64,
                            _ => {
                                // don't handle these yet
                                return;
                            }
                        };
                        let use_val = dfg.get_use(addr, Location::Register(l));
                        let def_val = dfg.get_def(addr, Location::Register(l));
        //                println!("Symbolizing use {:?} + {:#x} = ...", use_val.get_data(), i);
                        match use_val.get_data() {
                            Some(Data::Expression(expr)) => {
        //                        println!("  = {:?}", expr.clone().offset(*i as i64 as u64));
                                def_val.update(Data::Expression(expr.offset(offset as u64)));
                            }
                            _ => { }
                        }
                    }
                    _ => {
                        // don't handle others yet
                    }
                }
            }
            _ => { }
        }
    }
}
