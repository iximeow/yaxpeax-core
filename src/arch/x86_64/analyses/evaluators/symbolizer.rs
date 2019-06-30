use yaxpeax_arch::Arch;
use yaxpeax_x86::{x86_64, Instruction, Operand, Opcode, RegSpec, RegisterBank};
use arch::x86_64::x86_64Data;
use arch::x86_64::analyses::data_flow::{Data, Location, SymbolicExpression};
use analyses;
use analyses::evaluators::const_evaluator::{Domain, ConstEvaluator};
use analyses::static_single_assignment::cytron::SSA;
use data;
use data::types::TypeSpec;

pub struct SymbolicDomain;

impl Domain for SymbolicDomain {
    type Value = SymbolicExpression;

    fn join(l: Option<Self::Value>, r: Option<Self::Value>) -> Option<Self::Value> {
        if l == r {
            l
        } else {
            None
        }
    }
}

fn referent(instr: &Instruction, mem_op: &Operand, addr: <x86_64 as Arch>::Address, dfg: &SSA<x86_64>, contexts: &x86_64Data) -> Option<SymbolicExpression> {
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
            let addr = (addr.wrapping_add(instr.length as u64)).wrapping_add(*disp as i64 as u64);
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
                Some(Data::Concrete(v, None)) => {
                    // TODO: check const addr derefs for the same structures checked in disp
                    // eg gs:[rax] for rax = 0
                    None
                },
                Some(Data::Concrete(v, Some(ty))) => {
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
                Some(Data::Concrete(v, None)) => {
                    v.wrapping_add(*disp as i64 as u64);
                    // TODO: check const addr derefs for the same structures checked in disp
                    // eg gs:[rax] for rax = 0
                    None
                },
                Some(Data::Concrete(v, Some(ty))) => {
                    v.wrapping_add(*disp as i64 as u64);
                    // TODO: check const addr derefs for the same structures checked in disp
                    // eg gs:[rax] for rax = 0
                    None
                },
                Some(Data::Expression(sym)) => Some(sym.offset(*disp as i64 as u64)),
                _ => None
            }
        },
        Operand::RegScale(reg, scale) => {
            None
        },
        Operand::RegIndexBase(base, index) => {
            None
        },
        Operand::RegIndexBaseDisp(base, index, disp) => {
            None
        }
        Operand::RegScaleDisp(base, scale, disp) => {
            None
        }
        Operand::RegIndexBaseScale(index, base, scale) => {
            None
        }
        Operand::RegIndexBaseScaleDisp(index, base, scale, disp) => {
            None
        }
        _ => None
    }
}

impl ConstEvaluator<x86_64, x86_64Data, SymbolicDomain> for x86_64 {
    fn evaluate(instr: &<x86_64 as Arch>::Instruction, addr: <x86_64 as Arch>::Address, dfg: &SSA<x86_64>, contexts: &x86_64Data) {
        use yaxpeax_x86::Operand::{ImmediateI8, ImmediateI32, ImmediateI64};
        //TODO: handle prefixes like at all
        match instr {
            Instruction { opcode: Opcode::MOV, operands: [Operand::Register(l), op], .. } => {
                if op.is_memory() {
                    // might be a pointer deref or somesuch.
                    if let Some(src) = referent(instr, op, addr, dfg, contexts) {
                        let def = dfg.get_def(addr, Location::Register(*l));
                        if def.get_data().is_none() {
                            def.update(
                                Data::Expression(SymbolicExpression::Deref(Box::new(src)))
                            )
                        }
                    }
                }
            }
            Instruction { opcode: Opcode::ADD, operands: [Operand::Register(l), ImmediateI64(i)], .. } => {
                let use_val = dfg.get_use(addr, Location::Register(*l));
                let def_val = dfg.get_def(addr, Location::Register(*l));
//                println!("Symbolizing use {:?} + {:#x} = ...", use_val.get_data(), i);
                match use_val.get_data() {
                    Some(Data::Expression(expr)) => {
//                        println!("  = {:?}", expr.clone().offset(*i as i64 as u64));
                        def_val.update(Data::Expression(expr.offset(*i as i64 as u64)));
                    }
                    _ => { }
                }
            }
            Instruction { opcode: Opcode::ADD, operands: [Operand::Register(l), ImmediateI32(i)], .. } => {
                let use_val = dfg.get_use(addr, Location::Register(*l));
                let def_val = dfg.get_def(addr, Location::Register(*l));
                match use_val.get_data() {
                    Some(Data::Expression(expr)) => {
                        def_val.update(Data::Expression(expr.offset(*i as i64 as u64)));
                    }
                    _ => { }
                }
            }
            Instruction { opcode: Opcode::ADD, operands: [Operand::Register(l), ImmediateI8(i)], .. } => {
                let use_val = dfg.get_use(addr, Location::Register(*l));
                let def_val = dfg.get_def(addr, Location::Register(*l));
                match use_val.get_data() {
                    Some(Data::Expression(expr)) => {
                        def_val.update(Data::Expression(expr.offset(*i as i64 as u64)));
                    }
                    _ => { }
                }
            }
            _ => { }
        }
    }
}
