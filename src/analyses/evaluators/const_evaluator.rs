use yaxpeax_arch::Arch;
use analyses::static_single_assignment::cytron::{SSA, SSAValues};

pub trait Domain {
    type Value;

    fn join(l: Option<Self::Value>, r: Option<Self::Value>) -> Option<Self::Value>;
}

pub trait ConstEvaluator<A: Arch + SSAValues, Ctxs, D: Domain> {
    fn evaluate(instr: &A::Instruction, addr: A::Address, dfg: &SSA<A>, contexts: &Ctxs);
}
