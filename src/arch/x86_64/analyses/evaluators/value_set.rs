use yaxpeax_arch::{Arch, AddressDisplay};
use yaxpeax_x86::long_mode::{Instruction, Operand, Opcode, RegSpec, register_class};
use yaxpeax_x86::x86_64;
use arch::x86_64::analyses::data_flow::{Data, Location, ValueRange};
use analyses::evaluators::const_evaluator::{Domain, ConstEvaluator};
use analyses::static_single_assignment::SSA;
use data::Direction;
use data::modifier::ModifierExpression;
use data::ValueLocations;
use memory::MemoryRange;
use tracing::{event, Level};

pub struct ValueSetDomain;

impl Domain for ValueSetDomain {
    type Modifier = ModifierExpression;
    type Value = Vec<ValueRange>;

    fn join(_l: Option<Self::Value>, _r: Option<Self::Value>) -> Option<Self::Value> {
        unimplemented!("implement value set domain join");
    }
}

fn referent(_instr: &Instruction, mem_op: &Operand, addr: <x86_64 as Arch>::Address, dfg: &SSA<x86_64>, _contexts: &()) -> Option<Data> {
    match mem_op {
        Operand::DisplacementU32(_) |
        Operand::DisplacementU64(_) => {
            None
        }
        Operand::RegDeref(reg) => {
            match dfg.get_use(addr, Location::Register(*reg)).get_data().as_ref() {
                Some(Data::ValueSet(values)) => {
                    Some(Data::ValueSet(values.clone()))
                }
                _ => None
            }
        },
        Operand::RegDisp(RegSpec::RIP, _) => {
            None
        },
        Operand::RegDisp(reg, disp) => {
            match dfg.get_use(addr, Location::Register(*reg)).get_data().as_ref() {
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
            let referent = match (dfg.get_use(addr, Location::Register(*base)).get_data().as_ref(), dfg.get_use(addr, Location::Register(*index)).get_data().as_ref()) {
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
        Operand::RegScaleDisp(base, scale, disp) => {
//            let _data = dfg.get_use(addr, Location::Register(*base)).get_data().as_ref().and_then(|x| Data::underlying(x));
            let referent = match dfg.get_use(addr, Location::Register(*base)).get_data().as_ref().and_then(|x| Data::underlying(x)) {
                Some(Data::ValueSet(base_values)) => {
                    if let Some(scaled) = Data::mul(&Data::ValueSet(base_values.clone()), &Data::Concrete(*scale as u64, None)) {
                        Data::add(&scaled, &Data::Concrete(*disp as i64 as u64, None))
                    } else {
                        None
                    }
                }
                _ => None
            };
            referent
        }
        Operand::RegIndexBaseScale(_index, _base, _scale) => {
            None
        }
        Operand::RegIndexBaseScaleDisp(base, index, scale, disp) => {
            if addr == 0x140486bc0 {
                println!("mem operand: {:?}", mem_op);
            }
            let referent = match (dfg.get_use(addr, Location::Register(*base)).get_data().as_ref(), dfg.get_use(addr, Location::Register(*index)).get_data().as_ref()) {
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
    let mut reads: Vec<ValueRange> = Vec::new();
    for value in values {
        match value {
            ValueRange::Between(Data::Concrete(low, _), Data::Concrete(high, _)) => {
                if high - low > 0x10000 {
                    event!(Level::WARN, "suspiciously large valueset deref range");
                    return None;
                }
                for v in low..=high {
                    let mut read_value = 0u64;
                    for i in 0..size {
                        if let Some(value) = data.read(v + i as u64) {
                            read_value |= (value as u64) << (8 * i);
                        } else {
                            // TODO: this is a ... bad failure mode
                            tracing::error!("read of {:#x} failed?", v + i as u64);
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
                                tracing::error!("read of {:#x} failed?", v + i as u64);
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

impl ConstEvaluator<x86_64, (), ValueSetDomain> for x86_64 {
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
    fn apply_transient(from: <x86_64 as Arch>::Address, to: <x86_64 as Arch>::Address, location: Option<<x86_64 as ValueLocations>::Location>, exprs: &Vec<<ValueSetDomain as Domain>::Modifier>, dfg: &SSA<x86_64>, _contexts: &()) {
        if let Some(loc) = location {
            for expr in exprs {
                let new_value = dfg.get_transient_value(from, to, loc, Direction::Read).and_then(|_lvalue| {
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

    fn evaluate_instruction<U: MemoryRange<<x86_64 as Arch>::Address>>(instr: &<x86_64 as Arch>::Instruction, addr: <x86_64 as Arch>::Address, dfg: &SSA<x86_64>, contexts: &(), data: &U) {
        //TODO: handle prefixes at all
        match instr.opcode() {
            Opcode::MOVSXD => {
                match (instr.operand(0), instr.operand(1)) {
                    (Operand::Register(l), Operand::Register(r)) => {
                        // TODO: respect the sign extendy bits of movsxd
                        dfg.get_def(addr, Location::Register(l)).replace(
                            dfg.get_use(addr, Location::Register(r)).get_data().clone()
                        );
                    }
                    _ => {}
                }
            }
            Opcode::MOVZX_b => {
                match (instr.operand(0), instr.operand(1)) {
                    (Operand::Register(l), op) => {
                        if op.is_memory() {
                            // might be a pointer deref or somesuch.
                            if let Some(Data::ValueSet(values)) = referent(instr, &op, addr, dfg, contexts) {
                                if let Some(read_values) = valueset_deref(values.clone(), data, 1) {
                                    use arch::x86_64::display::DataDisplay;
                                    tracing::info!(
                                        "at {}, derefed value set {} to read {}",
                                        addr.show(),
                                        DataDisplay { data: &Data::ValueSet(values), colors: None },
                                        DataDisplay { data: &Data::ValueSet(read_values.clone()), colors: None }
                                    );
                                    dfg.get_def(addr, Location::Register(l)).update(
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
                    _ => {}
                }

            }
            Opcode::MOVZX_w => {
                match (instr.operand(0), instr.operand(1)) {
                    (Operand::Register(l), op) => {
                        if op.is_memory() {
                            // might be a pointer deref or somesuch.
                            if let Some(Data::ValueSet(values)) = referent(instr, &op, addr, dfg, contexts) {
                                if let Some(read_values) = valueset_deref(values.clone(), data, 2) {
                                    use arch::x86_64::display::DataDisplay;
                                    tracing::info!(
                                        "derefed value set {} to read {}",
                                        DataDisplay { data: &Data::ValueSet(values), colors: None },
                                        DataDisplay { data: &Data::ValueSet(read_values.clone()), colors: None }
                                    );
                                    dfg.get_def(addr, Location::Register(l)).update(
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
                    _ => {}
                }
            }
            Opcode::MOV => {
                match (instr.operand(0), instr.operand(1)) {
                    (Operand::Register(l), op) => {
                        if op.is_memory() {
                            // might be a pointer deref or somesuch.
                            if let Some(Data::ValueSet(values)) = referent(instr, &op, addr, dfg, contexts) {
                                let size = match l.class() {
                                    register_class::Q => 8,
                                    register_class::D => 4,
                                    register_class::W => 2,
                                    register_class::B |
                                    register_class::RB => 1,
                                    _ => {
                                        // TODO: handle movs to other registers
                                        return;
                                    }
                                };
                                if let Some(read_values) = valueset_deref(values.clone(), data, size) {
                                    use arch::x86_64::display::DataDisplay;
                                    tracing::info!(
                                        "derefed value set {} to read {}",
                                        DataDisplay { data: &Data::ValueSet(values), colors: None },
                                        DataDisplay { data: &Data::ValueSet(read_values.clone()), colors: None }
                                    );
                                    dfg.get_def(addr, Location::Register(l)).update(
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
                    _ => {}
                }
            }
            Opcode::JMP => {
                let jmpop = instr.operand(0);
                if jmpop.is_memory() {
                    if let Some(Data::ValueSet(values)) = referent(instr, &jmpop, addr, dfg, contexts) {
                        if let Some(read_values) = valueset_deref(values.clone(), data, 8) {
                            tracing::info!("    jump table with {} entries:", read_values.len());
                            for ent in read_values {
                                match ent {
                                    ValueRange::Precisely(Data::Concrete(ent, _)) => {
                                        tracing::info!("      {:#x}", ent);
                                    }
                                    entry => {
                                        tracing::warn!("can't process unknown value range entry: {:?}", entry);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Opcode::ADD => {
                match (instr.operand(0), instr.operand(1)) {
                    (Operand::Register(l), Operand::Register(r)) => {
                        dfg.get_def(addr, Location::Register(l)).replace(
                            dfg.get_use(addr, Location::Register(l)).get_data().as_ref().and_then(|ldata| {
                                dfg.get_use(addr, Location::Register(r)).get_data().as_ref().and_then(|rdata| {
                                    Data::add(&ldata, &rdata)
                                })
                            })
                        );
                    }
                    _ => {}
                }
            }
            _ => { }
        }
    }
}
