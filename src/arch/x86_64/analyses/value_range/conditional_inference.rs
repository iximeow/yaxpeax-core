use yaxpeax_arch::{Arch, AddressBase, LengthedInstruction};
use yaxpeax_x86::long_mode::{Opcode, Operand, ConditionCode};
use yaxpeax_x86::x86_64;
use arch::x86_64::analyses::data_flow::Location;
use data::modifier::{ModifierExpression, InstructionModifiers};
use data::{Direction, ValueLocations};
use analyses::control_flow::{ControlFlowGraph, Determinant, Target};
use analyses::static_single_assignment::{DefSource, SSA};
use analyses::value_range::ConditionalBoundInference;

pub struct ConditionalInference;

impl ConditionalBoundInference<x86_64, InstructionModifiers<x86_64>> for ConditionalInference {
    fn inferrable_conditional(conditional_instr: &<x86_64 as Arch>::Instruction) -> bool {
        if conditional_instr.opcode().condition().is_none() {
            return false;
        }

        match conditional_instr.control_flow(Option::<&u8>::None).dest {
            Some(Target::Relative(_)) |
            Some(Target::Absolute(_)) => {
                true
            },
            _ => {
                false
            }
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
        aux_data: &mut InstructionModifiers<x86_64>,
    ) -> bool {
        if test_addr == 0x140486bad {
            println!("inferring bounds for 0x140486bad");
        }
        let next_addr = conditional_addr + conditional_instr.len();
        let conditional_dest = match conditional_instr.control_flow(Option::<&u8>::None).dest {
            Some(Target::Relative(addr)) => {
                next_addr.wrapping_offset(addr)
            },
            Some(Target::Absolute(addr)) => {
                addr
            },
            Some(_) => {
                // this is some kind of jump to unknown destination
                // TODO: infer something about the known destination
                return false;
            }
            None => {
                // not a conditional branch, so there aren't arms to infer values over
                return false;
            }
        };

        // TODO: revisit how this handles sub-64-bit registers
        // TODO: only support z/nz, g/le/l/ge, a/be/b/ae
        if let Some(cond) = conditional_instr.opcode().condition() {
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
                    match test_instr.opcode() {
                        Opcode::CMP => {
                            // TODO: shouldn't care about dest operand, should be able to just query
                            // dfg for a value.
                            let dest_reg = match test_instr.operand(0) {
                                Operand::Register(r) => r,
                                _ => { return false; } // comparison with non-reg is not yet supported
                            };
                            let imm_src = match test_instr.operand(1) {
                                Operand::Register(_r) => { return false; } // TODO: support non-immediate sources
                                Operand::ImmediateU8(imm) => imm as u64,
                                Operand::ImmediateU16(imm) => imm as u64,
                                Operand::ImmediateU32(imm) => imm as u64,
                                Operand::ImmediateI8(imm) => imm as i64 as u64,
                                Operand::ImmediateI16(imm) => imm as i64 as u64,
                                Operand::ImmediateI32(imm) => imm as i64 as u64,
                                _ => { return false; } // TODO: support non-immediate sources
                            };

                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(dest_reg)), ModifierExpression::Is(imm_src));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(dest_reg)), ModifierExpression::IsNot(imm_src));
                            true
                        }
                        Opcode::TEST => {
                            // TODO: shouldn't care about dest operand, should be able to just query
                            // dfg for a value.
                            let dest_reg = match test_instr.operand(0) {
                                Operand::Register(r) => r,
                                _ => { return false; } // comparison with non-reg is not yet supported
                            };

                            match test_instr.operand(1) {
                                Operand::Register(l) => {
                                    if l == dest_reg {
                                        aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(l)), ModifierExpression::Is(0));
                                        aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(l)), ModifierExpression::IsNot(0));
                                        true
                                    } else {
                                        false
                                    }
                                }
                                _ => {
                                    // TODO: handle bit granularity
                                    false
                                }
                            }
                        }
                        Opcode::ADD |
                        Opcode::SUB |
                        Opcode::AND |
                        Opcode::OR |
                        Opcode::XOR => {
                            // if dest is a register
                            // TODO: shouldn't care about dest operand, should be able to just query
                            // dfg for a value.
                            let dest_reg = match test_instr.operand(0) {
                                Operand::Register(r) => r,
                                _ => { return false; }
                            };

                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(dest_reg)), ModifierExpression::Is(0));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(dest_reg)), ModifierExpression::IsNot(0));
                            true
                        }
                        _ => { false }
                    }
                },
                ConditionCode::G |
                ConditionCode::LE => {
                    match test_instr.opcode() {
                        Opcode::CMP => {
                            // TODO: shouldn't care about dest operand, should be able to just query
                            // dfg for a value.
                            let dest_reg = match test_instr.operand(0) {
                                Operand::Register(r) => r,
                                _ => { return false; } // comparison with non-reg is not yet supported
                            };
                            let imm_src = match test_instr.operand(1) {
                                Operand::Register(_r) => { return false; } // TODO: support non-immediate sources
                                Operand::ImmediateU8(imm) => imm as u64,
                                Operand::ImmediateU16(imm) => imm as u64,
                                Operand::ImmediateU32(imm) => imm as u64,
                                Operand::ImmediateI8(imm) => imm as i64 as u64,
                                Operand::ImmediateI16(imm) => imm as i64 as u64,
                                Operand::ImmediateI32(imm) => imm as i64 as u64,
                                _ => { return false; } // TODO: support non-immediate sources
                            };

                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(dest_reg)), ModifierExpression::Above(imm_src));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(dest_reg)), ModifierExpression::Below(imm_src));
                            true
                        }
                        Opcode::TEST => {
                            // TODO: shouldn't care about dest operand, should be able to just query
                            // dfg for a value.
                            let _dest_reg = match test_instr.operand(0) {
                                Operand::Register(r) => r,
                                _ => { return false; } // comparison with non-reg is not yet supported
                            };

                            match test_instr.operand(1) {
                                _ => {
                                    // TODO: handle bit granularity
                                    false
                                }
                            }
                        }
                        _ => { false }
                    }
                }
                ConditionCode::L |
                ConditionCode::GE => {
                    match test_instr.opcode() {
                        Opcode::CMP => {
                            // TODO: shouldn't care about dest operand, should be able to just query
                            // dfg for a value.
                            let dest_reg = match test_instr.operand(0) {
                                Operand::Register(r) => r,
                                _ => { return false; } // comparison with non-reg is not yet supported
                            };
                            let imm_src = match test_instr.operand(1) {
                                Operand::Register(_r) => { return false; } // TODO: support non-immediate sources
                                Operand::ImmediateU8(imm) => imm as u64,
                                Operand::ImmediateU16(imm) => imm as u64,
                                Operand::ImmediateU32(imm) => imm as u64,
                                Operand::ImmediateI8(imm) => imm as i64 as u64,
                                Operand::ImmediateI16(imm) => imm as i64 as u64,
                                Operand::ImmediateI32(imm) => imm as i64 as u64,
                                _ => { return false; } // TODO: support non-immediate sources
                            };

                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(dest_reg)), ModifierExpression::Below(imm_src));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(dest_reg)), ModifierExpression::Above(imm_src));
                            true
                        }
                        Opcode::TEST => {
                            // TODO: shouldn't care about dest operand, should be able to just query
                            // dfg for a value.
                            let _dest_reg = match test_instr.operand(0) {
                                Operand::Register(r) => r,
                                _ => { return false; } // comparison with non-reg is not yet supported
                            };

                            match test_instr.operand(1) {
                                _ => {
                                    // TODO: handle bit granularity
                                    false
                                }
                            }
                        }
                        _ => { false }
                    }
                }
                ConditionCode::A |
                ConditionCode::BE => { // A or NA
                    match test_instr.opcode() {
                        // TODO: add bounds as if this is an unsigned compare!
                        Opcode::CMP => {
                            // TODO: shouldn't care about dest operand, should be able to just query
                            // dfg for a value.
                            let dest_reg = match test_instr.operand(0) {
                                Operand::Register(r) => r,
                                _ => { return false; } // comparison with non-reg is not yet supported
                            };
                            let imm_src = match test_instr.operand(1) {
                                Operand::Register(_r) => { return false; } // TODO: support non-immediate sources
                                Operand::ImmediateU8(imm) => imm as u64,
                                Operand::ImmediateU16(imm) => imm as u64,
                                Operand::ImmediateU32(imm) => imm as u64,
                                Operand::ImmediateI8(imm) => imm as i64 as u64,
                                Operand::ImmediateI16(imm) => imm as i64 as u64,
                                Operand::ImmediateI32(imm) => imm as i64 as u64,
                                _ => { return false; } // TODO: support non-immediate sources
                            };

                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(dest_reg)), ModifierExpression::Above(imm_src));
                            eprintln!("adding negated bound dest, between blocks {:#x} and {:#x}, {} is {:?}", curr_block, negated_bound_dest, dest_reg, ModifierExpression::Below(imm_src));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(dest_reg)), ModifierExpression::Below(imm_src));
                            true
                        }
                        Opcode::TEST => {
                            // TODO: shouldn't care about dest operand, should be able to just query
                            // dfg for a value.
                            let _dest_reg = match test_instr.operand(0) {
                                Operand::Register(r) => r,
                                _ => { return false; } // comparison with non-reg is not yet supported
                            };

                            match test_instr.operand(1) {
                                _ => {
                                    // TODO: handle bit granularity
                                    false
                                }
                            }
                        }
                        _ => { false }
                    }
                }
                ConditionCode::B |
                ConditionCode::AE => { // B or NB
                    match test_instr.opcode() {
                        // TODO: add bounds as if this is an unsigned compare!
                        Opcode::CMP => {
                            // TODO: shouldn't care about dest operand, should be able to just query
                            // dfg for a value.
                            let dest_reg = match test_instr.operand(0) {
                                Operand::Register(r) => r,
                                _ => { return false; } // comparison with non-reg is not yet supported
                            };
                            let imm_src = match test_instr.operand(1) {
                                Operand::Register(_r) => { return false; } // TODO: support non-immediate sources
                                Operand::ImmediateU8(imm) => imm as u64,
                                Operand::ImmediateU16(imm) => imm as u64,
                                Operand::ImmediateU32(imm) => imm as u64,
                                Operand::ImmediateI8(imm) => imm as i64 as u64,
                                Operand::ImmediateI16(imm) => imm as i64 as u64,
                                Operand::ImmediateI32(imm) => imm as i64 as u64,
                                _ => { return false; } // TODO: support non-immediate sources
                            };

                            aux_data.add_edge_modifier(curr_block, bound_dest, Some(Location::Register(dest_reg)), ModifierExpression::Below(imm_src));
                            aux_data.add_edge_modifier(curr_block, negated_bound_dest, Some(Location::Register(dest_reg)), ModifierExpression::Above(imm_src));
                            true
                        }
                        Opcode::TEST => {
                            // TODO: shouldn't care about dest operand, should be able to just query
                            // dfg for a value.
                            let _dest_reg = match test_instr.operand(0) {
                                Operand::Register(r) => r,
                                _ => { return false; } // comparison with non-reg is not yet supported
                            };

                            match test_instr.operand(1) {
                                _ => {
                                    // TODO: handle bit granularity
                                    false
                                }
                            }
                        }
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
            // this instruction has multiple destinations but is not conditional? this could be the
            // case in sequences like
            // ```
            // mov rdx, 1
            // mov rcx, 2
            // cmp rax, 5
            // cmov rdx, rcx
            // jmp rdx
            // ```
            // where we've inferred exactly two possible concrete destinations, but the condition
            // is further back.
            false
        }
    }
}
