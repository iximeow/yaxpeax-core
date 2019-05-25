use yaxpeax_arch::Arch;
use yaxpeax_x86::{x86_64, Operand, Opcode};
use arch::x86_64::x86_64Data;
use arch::x86_64::analyses::data_flow::{Data, Location};
use analyses::evaluators::const_evaluator::{Domain, ConstEvaluator};
use analyses::static_single_assignment::cytron::SSA;

pub struct ConcreteDomain;

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
    fn evaluate(instr: &<x86_64 as Arch>::Instruction, addr: <x86_64 as Arch>::Address, dfg: &SSA<x86_64>, _contexts: &x86_64Data) {
        match instr.opcode {
            Opcode::XOR => {
                match instr.operands {
                    [Operand::Register(l), Operand::Register(r)] => {
                        if l == r {
                            dfg.get_def(addr, Location::Register(r)).update(Data::Concrete(0));
                        }
                    },
                    _ => { }
                }
            }
            Opcode::MOV => {
                match instr.operands {
                    [Operand::Register(l), Operand::Register(r)] => {
                        dfg.get_def(addr, Location::Register(l)).update(
                            Data::Alias(dfg.get_use(addr, Location::Register(r)).as_rc())
                        );
                    },
                    _ => { }
                }
            }
            _ => { }
        }
    }
}
