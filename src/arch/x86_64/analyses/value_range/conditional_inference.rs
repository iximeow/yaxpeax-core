use yaxpeax_arch::{Arch, LengthedInstruction};
use yaxpeax_x86::{x86_64, Opcode, Instruction, Operand, ConditionCode};
use arch::x86_64::analyses::data_flow::Location;
use arch::x86_64::{ModifierExpression, InstructionModifiers};
use data::{Direction, ValueLocations};
use analyses::control_flow::{ControlFlowGraph, Determinant, Target};
use analyses::static_single_assignment::{DefSource, SSA};
use analyses::value_range::ConditionalBoundInference;

pub struct ConditionalInference;

impl ConditionalBoundInference<x86_64, InstructionModifiers> for ConditionalInference {
    fn inferrable_conditional(conditional_instr: &<x86_64 as Arch>::Instruction) -> bool {
        if let Opcode::Jcc(_) = conditional_instr.opcode {
            true
        } else {
            false
        }
    }

    fn conditional_source_for(
        conditional_instr: &<x86_64 as Arch>::Instruction,
        addr: <x86_64 as Arch>::Address,
        dfg: &SSA<x86_64>
    ) -> Option<(<x86_64 as Arch>::Address, DefSource<<x86_64 as Arch>::Address>)> {
        let uses_vec = <x86_64 as ValueLocations>::decompose(conditional_instr);
        let mut uses = uses_vec.iter().filter_map(|(loc, dir)| {
            match (loc, dir) {
                (Some(loc), Direction::Read) => {
                    if crate::arch::x86_64::analyses::data_flow::FLAGS.contains(loc) {
                        Some(loc)
                    } else {
                        None
                    }
                },
                _ => None
            }
        });

        let def_site: Option<&(<x86_64 as Arch>::Address, DefSource<<x86_64 as Arch>::Address>)> = uses.next().and_then(|loc| {
            dfg.try_get_def_site(dfg.get_use(addr, *loc).value)
        });

        while let Some(loc) = uses.next() {
            if def_site != dfg.try_get_def_site(dfg.get_use(addr, *loc).value) {
                return None;
            }
        }

        return def_site.cloned();
    }

    fn infer_conditional_bounds(
        curr_block: <x86_64 as Arch>::Address,
        test_instr: &<x86_64 as Arch>::Instruction,
        test_addr: <x86_64 as Arch>::Address,
        conditional_instr: &<x86_64 as Arch>::Instruction,
        conditional_addr: <x86_64 as Arch>::Address,
        _cfg: &ControlFlowGraph<<x86_64 as Arch>::Address>,
        _dfg: &SSA<x86_64>,
        aux_data: &mut InstructionModifiers,
    ) -> bool {
        if test_addr == 0x140486bad {
            println!("inferring bounds for 0x140486bad");
        }
        // TODO: revisit how this handles sub-64-bit registers
        // TODO: only support z/nz, g/le/l/ge, a/be/b/ae
        if let Opcode::Jcc(cond) = conditional_instr.opcode {
            let next_addr = conditional_addr + conditional_instr.len();

                                                        // the option type doesn't actually matter
            let effect = conditional_instr.control_flow(Option::<&u8>::None);

            let conditional_dest = if let Some(Target::Relative(addr)) = effect.dest {
                next_addr.wrapping_add(addr)
            } else if let Some(Target::Absolute(addr)) = effect.dest {
                addr
            } else {
                unreachable!();
            };

            // Pair up conditions and their negations, swap destinations to phrase bounds in terms
            // of `bound applied` and `negated bound applied`
            let (bound_dest, negated_bound_dest) = match cond {
                ConditionCode::O |
                ConditionCode::B |
                ConditionCode::Z |
                ConditionCode::A |
                ConditionCode::S |
                ConditionCode::P |
                ConditionCode::L |
                ConditionCode::G => {
                    (conditional_dest, next_addr)
                },
                _ => {
                    (next_addr, conditional_dest)
                }
            };

            if test_addr == 0x140486bad {
                println!("test at 0x140486bad is {:?} with respect to condition {:?}", test_instr, cond);
            }

            match cond {
                ConditionCode::Z |
                ConditionCode::NZ => {
                    match test_instr {
                        Instruction { opcode: Opcode::CMP, operands: [Operand::Register(_l), Operand::Register(_r)], .. } => {
                            false
                        }
                        Instruction { opcode: Opcode::CMP, operands: [Operand::Register(l), Operand::ImmediateU8(imm)], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Is(*imm as u64));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::IsNot(*imm as u64));
                            true
                        },
                        Instruction { opcode: Opcode::CMP, operands: [Operand::Register(l), Operand::ImmediateU32(imm)], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Is(*imm as u64));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::IsNot(*imm as u64));
                            true
                        },
                        Instruction { opcode: Opcode::CMP, operands: [Operand::Register(l), Operand::ImmediateI32(imm)], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Is(*imm as i64 as u64));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::IsNot(*imm as i64 as u64));
                            true
                        },
                        Instruction { opcode: Opcode::TEST, operands: [Operand::Register(l), Operand::Register(r)], .. } => {
                            if l == r {
                                aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Is(0));
                                aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::IsNot(0));
                                true
                            } else {
                                false
                            }
                        },
                        Instruction { opcode: Opcode::TEST, operands: [Operand::Register(l), Operand::ImmediateU8(imm)], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Is(*imm as u64));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::IsNot(*imm as u64));
                            true
                        },
                        Instruction { opcode: Opcode::TEST, operands: [Operand::Register(l), Operand::ImmediateU32(imm)], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Is(*imm as u64));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::IsNot(*imm as u64));
                            true
                        },
                        Instruction { opcode: Opcode::ADD, operands: [Operand::Register(result), _], .. } |
                        Instruction { opcode: Opcode::SUB, operands: [Operand::Register(result), _], .. } |
                        Instruction { opcode: Opcode::AND, operands: [Operand::Register(result), _], .. } |
                        Instruction { opcode: Opcode::OR, operands: [Operand::Register(result), _], .. } |
                        Instruction { opcode: Opcode::XOR, operands: [Operand::Register(result), _], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*result)), ModifierExpression::Is(0));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*result)), ModifierExpression::IsNot(0));
                            true
                        }
                        _ => { false }
                    }
                },
                ConditionCode::G |
                ConditionCode::LE => {
                    match test_instr {
                        Instruction { opcode: Opcode::CMP, operands: [Operand::Register(_l), Operand::Register(_r)], .. } => {
                            false
                        },
                        Instruction { opcode: Opcode::CMP, operands: [Operand::Register(l), Operand::ImmediateU8(imm)], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Above(*imm as u64));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::Below((*imm as u64).wrapping_sub(1)));
                            true
                        },
                        Instruction { opcode: Opcode::CMP, operands: [Operand::Register(l), Operand::ImmediateI32(imm)], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Above(*imm as i64 as u64));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::Below((*imm as i64).wrapping_sub(1) as u64));
                            true
                        },
                        Instruction { opcode: Opcode::TEST, operands: [Operand::Register(_l), Operand::Register(_r)], .. } => {
                            false
                        },
                        Instruction { opcode: Opcode::TEST, operands: [Operand::Register(l), Operand::ImmediateU8(imm)], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Above(*imm as u64));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::Below((*imm as u64).wrapping_sub(1)));
                            true
                        },
                        _ => { false }
                    }
                }
                ConditionCode::L |
                ConditionCode::GE => {
                    match test_instr {
                        Instruction { opcode: Opcode::CMP, operands: [Operand::Register(_l), Operand::Register(_r)], .. } => {
                            false
                        },
                        Instruction { opcode: Opcode::CMP, operands: [Operand::Register(l), Operand::ImmediateU8(imm)], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Below(*imm as u64));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::Above((*imm as u64).wrapping_sub(1)));
                            true
                        },
                        Instruction { opcode: Opcode::CMP, operands: [Operand::Register(l), Operand::ImmediateI32(imm)], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Below(*imm as i64 as u64));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::Above((*imm as i64).wrapping_sub(1) as u64));
                            true
                        },
                        Instruction { opcode: Opcode::TEST, operands: [Operand::Register(_l), Operand::Register(_r)], .. } => {
                            false
                        },
                        Instruction { opcode: Opcode::TEST, operands: [Operand::Register(l), Operand::ImmediateU8(imm)], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Below(*imm as u64));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::Above((*imm as u64).wrapping_sub(1)));
                            true
                        },
                        _ => { false }
                    }
                }
                ConditionCode::A |
                ConditionCode::NA => {
                    match test_instr {
                        Instruction { opcode: Opcode::CMP, operands: [Operand::Register(_l), Operand::Register(_r)], .. } => {
                            false
                        },
                        Instruction { opcode: Opcode::CMP, operands: [Operand::Register(l), Operand::ImmediateU8(imm)], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Above((*imm as u64).wrapping_add(1)));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::Below(*imm as u64));
                            true
                        },
                        Instruction { opcode: Opcode::CMP, operands: [Operand::Register(l), Operand::ImmediateI32(imm)], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Above((*imm as i64 as u64).wrapping_add(1)));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::Below(*imm as i64 as u64));
                            true
                        },
                        Instruction { opcode: Opcode::TEST, operands: [Operand::Register(_l), Operand::Register(_r)], .. } => {
                            false
                        },
                        Instruction { opcode: Opcode::TEST, operands: [Operand::Register(l), Operand::ImmediateU8(imm)], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Above((*imm as u64).wrapping_add(1)));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::Below(*imm as u64));
                            true
                        },
                        _ => { false }
                    }
                }
                ConditionCode::B |
                ConditionCode::NB => {
                    match test_instr {
                        Instruction { opcode: Opcode::CMP, operands: [Operand::Register(_l), Operand::Register(_r)], .. } => {
                            false
                        },
                        Instruction { opcode: Opcode::CMP, operands: [Operand::Register(l), Operand::ImmediateU8(imm)], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Below(*imm as u64));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::Above((*imm as u64).wrapping_sub(1)));
                            true
                        },
                        Instruction { opcode: Opcode::CMP, operands: [Operand::Register(l), Operand::ImmediateI32(imm)], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Below(*imm as i64 as u64));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::Above((*imm as i64).wrapping_sub(1) as u64));
                            true
                        },
                        Instruction { opcode: Opcode::TEST, operands: [Operand::Register(_l), Operand::Register(_r)], .. } => {
                            false
                        },
                        Instruction { opcode: Opcode::TEST, operands: [Operand::Register(l), Operand::ImmediateU8(imm)], .. } => {
                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(*l)), ModifierExpression::Below(*imm as u64));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(*l)), ModifierExpression::Above((*imm as u64).wrapping_sub(1)));
                            true
                        },
                        _ => { false }
                    }
                }
                ConditionCode::S |
                ConditionCode::NS |
                ConditionCode::P |
                ConditionCode::NP |
                ConditionCode::O |
                ConditionCode::NO => { false },
            }
        } else {
            // do nothing
            false
        }
    }
}
