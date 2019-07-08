use yaxpeax_arch::{Arch, Decodable};
use memory::MemoryRange;
use analyses::control_flow::ControlFlowGraph;
use analyses::static_single_assignment::{DefSource, SSA, SSAValues};

trait ConditionalBoundInference<A: Arch + SSAValues> {
    /// Finds the instruction responsible for the condition that `conditional_instr` branches on.
    /// For some architectures, this may be a ways earlier in the program. For others, this may
    /// simply be "the conditional instruction".
    fn conditional_source_for(conditional_instr: &A::Instruction, conditional_addr: A::Address, dfg: &SSA<A>) -> Option<(A::Address, DefSource<A::Address>)>;
    /// This yadda yadda's the good stuff. Given a conditional instruction, and instruction that
    /// decides its conditionality (these may be the same!) apply bounds to the program for
    /// control-depndent edges.
    fn infer_conditional_bounds(test_instr: &A::Instruction, conditional_instr: &A::Instruction, cfg: &ControlFlowGraph<A::Address>, dfg: &SSA<A>) -> bool;
    /// Is the instruction in question one that an implementor might want to look at?
    fn inferrable_conditional(conditional_instr: &A::Instruction) -> bool;

    fn add_conditional_bounds<M: MemoryRange<A::Address>>(block_start: A::Address, conditional: A::Address, conditional_instr: &A::Instruction, cfg: &ControlFlowGraph<A::Address>, dfg: &SSA<A>, data: &M) -> bool {
        match Self::conditional_source_for(conditional_instr, conditional, dfg) {
            Some((src_addr, DefSource::Instruction)) => {
                if let Some(range) = data.range_from(src_addr) {
                    if let Some(test_instr) = A::Instruction::decode(range) {
                        // and now that we have the instruction...
                        Self::infer_conditional_bounds(&test_instr, conditional_instr, cfg, dfg)
                    } else {
                        // flags are defined at an instruction that does not decode or is not in
                        // the program's space?
                        unreachable!();
                    }
                } else {
                    // flags are defined at an address that's invalid?
                    unreachable!();
                }
            }
            Some((_, DefSource::Phi)) |
            Some((_, DefSource::Modifier(_))) |
            Some((_, DefSource::Between(_))) => {
                // Flags are defined by some non-instruction source. We can't do anything useful there,
                // because we would be bounding flags specifically - if a branch is taken or not we may
                // trivially know the state of flags upon reaching the branch.
                //
                // A conditional source being from not the conditional instruction will have some form
                // like:
                // * a specific modifier asserting the condition to some state (or, unknown)
                //   - here we will either agree or disagree. disagreement is interesting
                //     because that implies the condition will never be true - that should
                //     be eliminated as a possibility by conditional propagation before this
                //     is performed
                // * a phi node between condition variables
                //   - this is interesting, but as-yet unsupported. a bound introduced by a branch
                //     would be reflected on all phi'd nodes, and would be not-incorrectly written
                //     as a bound on the result of the phi operation.
                // * a between-block modifier
                //   - this is very similar to the modifier case above. either we agree with this
                //     and introduce no interesting information, or we disagree and are looking at
                //     a dead branch.
                false
            }
            None => {
                // Nothing we can do for flags that come from places we don't know
                // (really, this probably means the flags are defined by an input to the function)
                false
            }
        }
    }
}

/// A struct that existes purely to have a non-effecting implementation of conditional bound
/// inference.
struct NoConditionalInference { }

impl <A: Arch + SSAValues> ConditionalBoundInference<A> for NoConditionalInference {
    fn conditional_source_for(conditional_instr: &A::Instruction, conditional_addr: A::Address, dfg: &SSA<A>) -> Option<(A::Address, DefSource<A::Address>)> {
        None
    }

    fn infer_conditional_bounds(test_instr: &A::Instruction, conditional_instr: &A::Instruction, cfg: &ControlFlowGraph<A::Address>, dfg: &SSA<A>) -> bool {
        false
    }

    fn inferrable_conditional(conditional_instr: &A::Instruction) -> bool {
        false
    }
}