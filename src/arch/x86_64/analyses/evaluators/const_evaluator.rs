use yaxpeax_arch::Arch;
use yaxpeax_x86::{x86_64, Operand, Opcode};
use arch::x86_64::x86_64Data;
use analyses::evaluators::const_evaluator::{Domain, ConstEvaluator};
use analyses::static_single_assignment::cytron::SSA;

struct ConcreteDomain;

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

impl ConstEvaluator<x86_64, x86_64Data, ConcreteDomain> for x86_64 {
    fn evaluate(instr: <x86_64 as Arch>::Instruction, _addr: <x86_64 as Arch>::Address, _dfg: &SSA<x86_64>, _contexts: x86_64Data) {
        match instr.opcode {
            Opcode::XOR => {
                match instr.operands {
                    [Operand::Register(l), Operand::Register(r)] => {
                        if l == r {
//                            dfg[addr, Operand::Register(l)] = Some(0)
                        }
                    },
                    _ => { }
                }
            }
            _ => { }
        }
    }
}
