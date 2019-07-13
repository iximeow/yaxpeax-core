use yaxpeax_arch::{AddressDisplay, Arch};
use yaxpeax_x86::{x86_64, Instruction, Operand, Opcode, RegSpec, RegisterBank};
use arch::x86_64::x86_64Data;
use arch::x86_64::analyses::data_flow::{Data, Location, ValueRange};
use analyses::evaluators::const_evaluator::{Domain, ConstEvaluator};
use analyses::static_single_assignment::SSA;
use data::Direction;
use arch::x86_64::ModifierExpression;
use data::ValueLocations;
use memory::MemoryRange;

pub struct ValueSetDomain;

impl Domain for ValueSetDomain {
    type Modifier = ModifierExpression;
    type Value = Vec<ValueRange>;

    fn join(l: Option<Self::Value>, r: Option<Self::Value>) -> Option<Self::Value> {
        // TODO ... haha
        panic!("implement value set domain join");
        None
    }
}

fn referent(instr: &Instruction, mem_op: &Operand, addr: <x86_64 as Arch>::Address, dfg: &SSA<x86_64>, contexts: &x86_64Data) -> Option<Data> {
    match mem_op {
        Operand::RegDisp(RegSpec { num: 0, bank: RegisterBank::RIP }, _) |
        Operand::DisplacementU32(_) |
        Operand::DisplacementU64(_) => {
            None
        }
        Operand::RegDeref(reg) => {
            match dfg.get_use(addr, Location::Register(*reg)).get_data() {
                Some(Data::ValueSet(values)) => {
                    Some(Data::ValueSet(values.clone()))
                }
                _ => None
            }
        },
        Operand::RegDisp(reg, disp) => {
            match dfg.get_use(addr, Location::Register(*reg)).get_data() {
                Some(Data::ValueSet(values)) => {
                    Data::add(&Data::ValueSet(values.clone()), &Data::Concrete(*disp as i64 as u64, None))
                }
                _ => None
            }
        },
        Operand::RegScale(_reg, _scale) => {
            None
        },
        Operand::RegIndexBase(_base, _index) => {
            None
        },
        Operand::RegIndexBaseDisp(base, index, disp) => {
            if addr == 0x1404d5d88 {
                println!("mem operand: {:?}", mem_op);
            }
            let referent = match (dfg.get_use(addr, Location::Register(*base)).get_data(), dfg.get_use(addr, Location::Register(*index)).get_data()) {
                (Some(Data::ValueSet(base_values)), Some(Data::Concrete(i, _))) => {
                    Data::add(&Data::ValueSet(base_values.clone()), &Data::Concrete(i.wrapping_add(*disp as i64 as u64), None))
                }
                (Some(Data::Concrete(base_value, _)), Some(Data::ValueSet(index_values))) => {
                    Data::add(&Data::ValueSet(index_values.clone()), &Data::Concrete(base_value.wrapping_add(*disp as u64), None))
                }
                _ => None
            };
            if addr == 0x1404d5d88 {
                println!("Computed referent: {:?}", referent);
            }
            referent
        }
        Operand::RegScaleDisp(_base, _scale, _disp) => {
            None
        }
        Operand::RegIndexBaseScale(_index, _base, _scale) => {
            None
        }
        Operand::RegIndexBaseScaleDisp(base, index, scale, disp) => {
            if addr == 0x140486bc0 {
                println!("mem operand: {:?}", mem_op);
            }
            let referent = match (dfg.get_use(addr, Location::Register(*base)).get_data(), dfg.get_use(addr, Location::Register(*index)).get_data()) {
                (Some(Data::ValueSet(base_values)), Some(Data::Concrete(i, _))) => {
                    Data::add(&Data::ValueSet(base_values.clone()), &Data::Concrete(i.wrapping_mul(*scale as u64).wrapping_add(*disp as i64 as u64), None))
                }
                (Some(Data::Concrete(base_value, _)), Some(Data::ValueSet(index_values))) => {
                    let multiplied_values = Data::mul(&Data::ValueSet(index_values.clone()), &Data::Concrete(*scale as u64, None));
                    if addr == 0x140486bc0 {
                        println!("Multiplied indices: {:?}", multiplied_values);
                    }

                    multiplied_values
                        .as_ref().and_then(|d| Data::add(d, &Data::Concrete(base_value.wrapping_add(*disp as u64), None)))
                }
                _ => None
            };
            referent
        }
        _ => None
    }
}

fn valueset_deref<U: MemoryRange<<x86_64 as Arch>::Address>>(values: Vec<ValueRange>, data: &U, size: u8) -> Option<Vec<ValueRange>> {
    println!("Dereferencing the values: {:?}", values);
    let mut reads: Vec<ValueRange> = Vec::new();
    for value in values {
        match value {
            ValueRange::Between(Data::Concrete(low, _), Data::Concrete(high, _)) => {
                for v in low..=high {
                    let mut read_value = 0u64;
                    for i in 0..size {
                        if let Some(value) = data.read(v + i as u64) {
                            read_value |= (value as u64) << (8 * i);
                        } else {
                            // TODO: this is ... bad failure mode
                            println!("Read of {:#x} failed??", v + i as u64);
                            return None;
                        }
                    }
                    let new_value = ValueRange::Precisely(Data::Concrete(read_value, None));
                    if !reads.contains(&new_value) {
                        reads.push(new_value);
                    }
                }
            }
            ValueRange::Between(_, _) => {
                // TODO: figure out something to do here - something like Data::Indeterminate
                // return None;
            },
            ValueRange::Precisely(value) => {
                match value {
                    Data::Concrete(v, _) => {
                        let mut read_value = 0u64;
                        for i in 0..size {
                            if let Some(value) = data.read(v + i as u64) {
                                read_value |= (value as u64) << (8 * i);
                            } else {
                                // TODO: this is ... bad failure mode
                                println!("Read of {:#x} failed??", v + i as u64);
                                return None;
                            }
                        }
                        let new_value = ValueRange::Precisely(Data::Concrete(read_value, None));
                        if !reads.contains(&new_value) {
                            reads.push(new_value);
                        }
                    },
                    _ => {
                        // TODO: handle other values in the set
                        // return None;
                    }
                }
            }
        }
    }

    if reads.len() > 0 {
        Some(reads)
    } else {
        None
    }
}

impl ConstEvaluator<x86_64, x86_64Data, ValueSetDomain> for x86_64 {
    /// In a situation like
    /// v_1 <- Data::Concrete(10)
    /// v_2 = transient ModifierExpression::Above(20) . v_1
    ///
    /// TODO:
    /// v_2 should be computed to be impossible, and taint the entire branch.
    ///
    /// in reality, the best we can do is bail and set v_2 unknown for now.
    ///
    /// In a situation where ModifierExpression does not conflict with an existing Data entry, take
    /// the more specific of the two.
    /// eg
    /// v_1 <- Data::Concrete(10)
    /// v_2 = transient ModifierExpression::Below(20) . v_1
    /// -> v_2 == Data::Concrete(10)
    ///
    /// v_1 <- ValueSet(vec![ValueRange::Between(0, 1000)])
    /// v_2 = transient ModifierExpression::Below(20) .
    ///       transient ModifierExpression::Above(5) . v_1
    /// -> v_2 == Data::ValueSet(vec![ValueRange::Between(5, 20)])
    fn apply_transient(from: <x86_64 as Arch>::Address, to: <x86_64 as Arch>::Address, location: Option<<x86_64 as ValueLocations>::Location>, exprs: &Vec<<ValueSetDomain as Domain>::Modifier>, dfg: &SSA<x86_64>, contexts: &x86_64Data) {
        if let Some(loc) = location {
            for expr in exprs {
                let new_value = dfg.get_transient_value(from, to, loc, Direction::Read).and_then(|lvalue| {
                    // TODO
//                    lvalue.get_data().and_then(|data| data.merge_modifier(expr))
                    None
                }).unwrap_or_else(|| {
                    match expr {
                        ModifierExpression::Above(v) => {
                            Some(Data::ValueSet(
                                vec![ValueRange::Between(
                                    Data::Concrete(*v, None),
                                    Data::Concrete(0xffffffff_ffffffff, None)
                                )]
                            ))
                        }
                        ModifierExpression::Below(v) => {
                            Some(Data::ValueSet(
                                vec![ValueRange::Between(
                                    Data::Concrete(0, None),
                                    Data::Concrete(*v, None)
                                )]
                            ))
                        }
                        ModifierExpression::Is(v) => {
                            Some(Data::Concrete(*v, None))
                        }
                        ModifierExpression::IsNot(_) => {
                            None
                        }
                    }
                });

                if let Some(new_value) = new_value {
                    dfg.get_transient_def(from, to, loc).update(new_value);
                } else {
                    dfg.get_transient_def(from, to, loc).clear();
                }
            }
        } else {
            // TODO: modifier to some unknown location - not sure we can do much with this but
            // figure out something that is at least not lossy?
        }
    }

    fn evaluate_instruction<U: MemoryRange<<x86_64 as Arch>::Address>>(instr: &<x86_64 as Arch>::Instruction, addr: <x86_64 as Arch>::Address, dfg: &SSA<x86_64>, contexts: &x86_64Data, data: &U) {
        use yaxpeax_x86::Operand::{ImmediateI8, ImmediateI32, ImmediateI64};
        //TODO: handle prefixes like at all
        match instr {
            Instruction { opcode: Opcode::MOVSXD, operands: [Operand::Register(l), Operand::Register(r)], .. } => {
                // TODO: respect the sign extendy bits of movsxd
                dfg.get_def(addr, Location::Register(*l)).replace(
                    dfg.get_use(addr, Location::Register(*r)).get_data()
                );
            },
            Instruction { opcode: Opcode::MOVZX_b, operands: [Operand::Register(l), op], .. } => {
                if op.is_memory() {
                    // might be a pointer deref or somesuch.
                    if let Some(Data::ValueSet(values)) = referent(instr, op, addr, dfg, contexts) {
                        if let Some(read_values) = valueset_deref(values.clone(), data, 1) {
                            use arch::x86_64::display::DataDisplay;
                            println!(
                                "at {}, Derefed value set {} to read {}",
                                addr.stringy(),
                                DataDisplay { data: &Data::ValueSet(values), colors: None },
                                DataDisplay { data: &Data::ValueSet(read_values.clone()), colors: None }
                            );
                            dfg.get_def(addr, Location::Register(*l)).update(
                                Data::ValueSet(read_values)
                            );
                        } else {
                            // TODO: deref results in all values being invalid
                        }
                    } else {
                        // this was handled in some other evaluator
                    }
                } else {
                    // reg-reg mov is handled in another evalator
                }
            },
            Instruction { opcode: Opcode::MOVZX_w, operands: [Operand::Register(l), op], .. } => {
                if op.is_memory() {
                    // might be a pointer deref or somesuch.
                    if let Some(Data::ValueSet(values)) = referent(instr, op, addr, dfg, contexts) {
                        if let Some(read_values) = valueset_deref(values.clone(), data, 2) {
                            use arch::x86_64::display::DataDisplay;
                            println!(
                                "Derefed value set {} to read {}",
                                DataDisplay { data: &Data::ValueSet(values), colors: None },
                                DataDisplay { data: &Data::ValueSet(read_values.clone()), colors: None }
                            );
                            dfg.get_def(addr, Location::Register(*l)).update(
                                Data::ValueSet(read_values)
                            );
                        } else {
                            // TODO: deref results in all values being invalid
                        }
                    } else {
                        // this was handled in some other evaluator
                    }
                } else {
                    // reg-reg mov is handled in another evalator
                }
            },
            Instruction { opcode: Opcode::MOV, operands: [Operand::Register(l), op], .. } => {
                if op.is_memory() {
                    // might be a pointer deref or somesuch.
                    if let Some(Data::ValueSet(values)) = referent(instr, op, addr, dfg, contexts) {
                        let size = match l.bank {
                            RegisterBank::Q => 8,
                            RegisterBank::D => 4,
                            RegisterBank::W => 2,
                            RegisterBank::B |
                            RegisterBank::rB => 1,
                            _ => {
                                // TODO: handle movs to other registers
                                return;
                            }
                        };
                        if let Some(read_values) = valueset_deref(values.clone(), data, size) {
                            use arch::x86_64::display::DataDisplay;
                            println!(
                                "Derefed value set {} to read {}",
                                DataDisplay { data: &Data::ValueSet(values), colors: None },
                                DataDisplay { data: &Data::ValueSet(read_values.clone()), colors: None }
                            );
                            dfg.get_def(addr, Location::Register(*l)).update(
                                Data::ValueSet(read_values)
                            );
                        } else {
                            // TODO: deref results in all values being invalid
                        }
                    } else {
                        // this was handled in some other evaluator
                    }
                } else {
                    // reg-reg mov is handled in another evalator
                }
            }
            Instruction { opcode: Opcode::ADD, operands: [Operand::Register(l), Operand::Register(r)], .. } => {
                dfg.get_def(addr, Location::Register(*l)).replace(
                    dfg.get_use(addr, Location::Register(*l)).get_data().and_then(|ldata| {
                        dfg.get_use(addr, Location::Register(*r)).get_data().and_then(|rdata| {
                            Data::add(&ldata, &rdata)
                        })
                    })
                );
            }
            _ => { }
        }
    }
}
