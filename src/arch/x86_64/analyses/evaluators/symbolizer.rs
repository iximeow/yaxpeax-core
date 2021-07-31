use yaxpeax_arch::Arch;
use yaxpeax_arch::AddressBase;
use yaxpeax_arch::AddressDiff;
use yaxpeax_x86::long_mode::{Instruction, Operand, Opcode, RegSpec};
use yaxpeax_x86::x86_64;
use arch::x86_64::analyses::data_flow::{Data, Location, SymbolicExpression};
use analyses::evaluators::const_evaluator::{Domain, ConstEvaluator};
use analyses::static_single_assignment::SSA;
use data;
use data::modifier::ModifierExpression;
use data::ValueLocations;
use data::types::TypeSpec;
use memory::MemoryRange;
use analyses::Item;
use analyses::ValueOrImmediate;
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

use std::rc::Rc;
pub(crate) fn referent(instr: &Instruction, mem_op: &Operand, addr: <x86_64 as Arch>::Address, dfg: &SSA<x86_64>, _contexts: &()) -> Option<Rc<Item<ValueOrImmediate<x86_64>>>> {
    match mem_op {
        Operand::DisplacementU32(disp) => {
            if instr.prefixes.gs() {
                if *disp < 0x10000 {
                    Some(
                        Item::opaque(TypeSpec::struct_pointer(data::types::KPCR)).add(&Item::immediate(*disp as u64))
                    )
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
                    Some(
                        Item::opaque(TypeSpec::struct_pointer(data::types::KPCR)).add(&Item::immediate(*disp))
                    )
                } else {
                // TODO
                    None
                }
            } else {
                // TODO
                None
            }
        }
        Operand::RegDisp(RegSpec::RIP, disp) => {
            let addr = (addr.wrapping_offset(instr.len())).wrapping_offset(AddressDiff::from_const(*disp as i64 as u64));
            if addr == 0x1402a3148 {
                // its that global struct referenced in ntoskrnl:0x1402a9370
                // ... it's actually the linear address of the KPCR...
                Some(Item::opaque(TypeSpec::struct_pointer(data::types::KPCR).pointer_to()))
            } else if addr == 0x1402a3148 {
                // its that global struct referenced in ntoskrnl:0x1402a9387
                // stored in the KPCR, MAYBE??
                Some(Item::opaque(TypeSpec::PointerTo(Rc::new(TypeSpec::Unknown)).pointer_to()))
            } else {
                None
            }
        }
        Operand::RegDeref(reg) => {
            match dfg.get_use(addr, Location::Register(*reg)).get_data().as_ref() {
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
                Some(Data::Expression(sym)) => Some(sym.to_owned()),
                _ => None
            }
        },
        Operand::RegDisp(reg, disp) => {
            let reg = dfg.get_use(addr, Location::Register(*reg));
            Some(Item::value(ValueOrImmediate::Value(reg.value)).add(&Item::immediate(*disp as i64 as u64)))
            /*
            match .get_data().as_ref() {
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
                Some(Data::Expression(sym)) => Some(sym.add(&Item::immediate(*disp as i64 as u64))),
                _ => None
            }
            */
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
    fn evaluate_instruction<U: MemoryRange<x86_64>>(instr: &<x86_64 as Arch>::Instruction, addr: <x86_64 as Arch>::Address, dfg: &SSA<x86_64>, contexts: &(), _data: &U) {
        //TODO: handle prefixes like at all
        match instr.opcode() {
            Opcode::MOV => {
                match (instr.operand(0), instr.operand(1)) {
                    (Operand::Register(l), op) => {
                        if op.is_memory() {
                            println!("DOING MOV for {}", instr);
                            // `referent` is also called `effective_address` in other circumstances
                            // - either we can find an access expression known to `dfg` (in which
                            // case we might be able to just alias an existing value), otherwise
                            // it's a load of some opaque value.
                            if let Some(src) = referent(instr, &op, addr, dfg, contexts) {
                                use analyses::memory_layout::MemoryAccessBaseInference;
                                use crate::arch::x86_64::analyses::data_flow::ANY;
                                let def = dfg.get_def(addr, Location::Register(l));

                                if let Some((base, addend)) = MemoryAccessBaseInference::infer_base_and_addend(&src) {
                                    println!("inferred base and addend {:?}, {:?}", base, addend);
                                    println!("  def is {:?}={:p}", def.value, def.value.as_ptr());
                                    if let Some(memory) = dfg.try_get_use(addr, Location::MemoryLocation(ANY, 8 /* l.width() */, Some((Data::Expression(base), Data::Expression(addend))))) {
                                        println!("    AND aliasing to {:?}, was {:?}", memory, def.get_data());
                                        def.update(Data::Alias(Rc::clone(&memory)));
                                        println!("    DONE aliasing to {:?}, is {:?}", memory, dfg.get_def(addr, Location::Register(l)).value);
                                        return;
                                    } else {
                                        if def.get_data().is_none() {
                                            def.update(
                                                Data::Expression(Item::load(&src, l.width()))
                                            )
                                        }
                                    }
                                } else {
                                    if def.get_data().is_none() {
                                        def.update(
                                            Data::Expression(Item::load(&src, l.width()))
                                        )
                                    }
                                }
                            }
                        }
                    },
                    (op, Operand::Register(r)) => {
                        if op.is_memory() {
                            // `referent` is also called `effective_address` in other circumstances
                            // - either we can find an access expression known to `dfg` (in which
                            // case we might be able to just alias an existing value), otherwise
                            // it's a load of some opaque value.
                            if let Some(src) = referent(instr, &op, addr, dfg, contexts) {
                                use analyses::memory_layout::MemoryAccessBaseInference;
                                use crate::arch::x86_64::analyses::data_flow::ANY;
                                let usage = dfg.get_use(addr, Location::Register(r));

                                if let Some((base, addend)) = MemoryAccessBaseInference::infer_base_and_addend(&src) {
                                    println!("inferred base and addend {:?}, {:?}", base, addend);
                                    if let Some(def) = dfg.try_get_def(addr, Location::MemoryLocation(ANY, 8 /* l.width() */, Some((Data::Expression(base), Data::Expression(addend))))) {
                                        def.borrow_mut().data.replace(Data::Alias(Rc::clone(&usage.value)));
                                    }
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
                        match use_val.get_data().as_ref() {
                            Some(Data::Expression(expr)) => {
        //                        println!("  = {:?}", expr.clone().offset(*i as i64 as u64));
                                def_val.update(Data::Expression(expr.add(&Item::immediate(offset as u64))));
                            }
                            _ => { }
                        };
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
