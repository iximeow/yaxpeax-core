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
use arch;
use arch::Symbol;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Summary<A: Arch> {
    ListBlocks,
    ListFunctions,
    ListFunctionBlocks(A::Address),
    HowMuchCode,
    ProgramInfo,
    SymbolInfo(Option<String>, String),
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum Display<A: Arch> {
    RenderInstruction(A::Address),
    RenderBlock(A::Address),
    RenderRange(A::Address, A::Address),
    RenderFunction(A::Address),
    RenderInstructionSSA(A::Address),
    RenderBlockSSA(A::Address),
    RenderFunctionSSA(A::Address)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Operation<A: Arch, T> {
    /* base ops */
    Display(Display<A>),
    Analysis(Analysis<A>),
    Debug(Debug),
    Summary(Summary<A>),
    Data(Data<A>),
    Specific(T)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Data<A: Arch> {
    DefineSymbol(A::Address, Symbol),
    CodeComment(A::Address, String),
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum Debug {
    ShowMaps,
    ShowModules,
    ShowThreads,
    ShowInfo
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Analysis<A: Arch> {
    ComputeSSAForm(A::Address),
    ControlFlowLinear(A::Address, A::Address),
    ControlFlowIncremental(Vec<A::Address>),
    DoEverything
}

#[derive(Debug, Serialize, Deserialize)]
pub enum OperationSuccess {
    Ok,
    OkWithOutput(Vec<String>),
    SwitchArch(arch::ISA)
}

#[derive(Debug, Serialize, Deserialize)]
pub enum OperationError {
    Unknown(String),
    Unsupported(String),
    Misc(String)
}

pub trait Operate<T, A: Arch> {
//    type TargetArch: Arch;
//    fn evaluate(operation: Operation<Self::TargetArch, T>) -> Result<OperationSuccess, OperationError<Self::TargetArch, T>>;
    fn evaluate(&mut self, operation: Operation<A, T>) -> Result<OperationSuccess, OperationError>;
}
