/*
/ *
 * This works as a common interface for architectures to present their functionality
 * /
struct Interface<A: Arch> {
    display: &DisplayInterface<A>,
    analysis: &AnalysisInterface<A>,
}

// for dynamically loaded modules we need to unmarshal
// something to call their types
// and do some WILDLY unsafe casts to force some function
// pointers into othe shape of populated types

trait DisplayInterface <A: Arch + BaseDisplay<F, U>, U, F, Contexts: ContextRead<A, U>> {
    show_block: Fn(&[u8], &Contexts, function_table: &HashMap<A::Address, F>, cfg: &ControlFlowGraph<A::Address>, block: &BasicBlock<A::Address>) -> ();
    show_linear: Fn(&[u8], &Contexts, start_addr: A::Address, end_addr: A::Address, function_table: &HashMap<A::Address, F>) -> ();
    ssa: Option<SSADisplay>
}

trait SSADisplay {
    show_function: ..
}

trait AnalysisInterface {
    control_flow: ControlFlowAnalyses,
    ssa_analyses: SSAAnalyses
}

/ *
 * And what about when A::Instruction does not implement Determinant...
 * /
trait ControlFlowAnalyses<A: Arch, F, Contexts: ContextRead<A, U> + ContextWrite<A, U>, InstrCallback> {
    explore_all: Fn(&[u8], &mut Contexts, &mut ControlFlowGraph<A::Address>, A::Address, &InstrCallback) -> Vec<A::Address>;
    explore_control_flow: Fn(&[u8], &mut Contexts, &mut ControlFlowGraph<A::Address>, A::Address, &InstrCallback) -> Vec<A::Address>;
}

/ *
 * this might not be implementable because in some cases A explicitly does NOT implement SSAValues
 * /
trait SSAAnalyses<A: Arch + SSAValues> {
    construct_ssa: Fn(&[u8], A::Address, &ControlFlowGraph<A::Address>, GraphMap<A::Address, (), petgraph::Directed> -> SSA<A>
}
*/

use yaxpeax_arch::Arch;
use analyses::control_flow::BasicBlock;

#[derive(Debug)]
pub enum Display<'a, A: Arch> {
    RenderInstruction(A::Address, &'a A::Instruction),
    RenderBlock(BasicBlock<A::Address>),
    RenderRange(A::Address, A::Address),
    RenderFunction(A::Address),
    RenderInstructionSSA(A::Address, &'a A::Instruction),
    RenderBlockSSA(BasicBlock<A::Address>),
    RenderFunctionSSA(A::Address)
}

#[derive(Debug)]
pub enum Operation<'a, A: Arch, T> {
    /* base ops */
    Display(Display<'a, A>),
    Analysis(Analysis<A>),
    Specific(T)
}

#[derive(Debug)]
pub enum Analysis<A: Arch> {
    ComputeSSAForm(A::Address),
    ControlFlowLinear(A::Address, A::Address),
    ControlFlowIncremental(Vec<A::Address>)
}

#[derive(Debug)]
pub enum OperationSuccess {
    Ok,
    OkWithOutput(Vec<String>)
}

#[derive(Debug)]
pub enum OperationError<'a, A: Arch, T> {
    Unsupported(Operation<'a, A, T>, String),
    Misc(String)
}

pub trait Operate<'a, T, A: Arch> {
//    type TargetArch: Arch;
//    fn evaluate(operation: Operation<'a, Self::TargetArch, T>) -> Result<OperationSuccess, OperationError<'a, Self::TargetArch, T>>;
    fn evaluate(&mut self, operation: Operation<'a, A, T>) -> Result<OperationSuccess, OperationError<'a, A, T>>;
}
