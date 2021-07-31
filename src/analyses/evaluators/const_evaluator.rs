use yaxpeax_arch::Arch;
use analyses::static_single_assignment::{SSA, SSAValues};
use memory::MemoryRange;

pub trait Domain {
    /// arbitrary expressions that may constrain values in this domain
    type Modifier;
    type Value;

    fn join(l: Option<Self::Value>, r: Option<Self::Value>) -> Option<Self::Value>;
}

pub trait ConstEvaluator<A: Arch + SSAValues, Ctxs, D: Domain> {
    fn evaluate_instruction<U: MemoryRange<A>>(instr: &A::Instruction, addr: A::Address, dfg: &SSA<A>, contexts: &Ctxs, data: &U);
    fn apply_transient(from: A::Address, to: A::Address, location: Option<A::Location>, exprs: &Vec<D::Modifier>, dfg: &SSA<A>, contexts: &Ctxs);
}
