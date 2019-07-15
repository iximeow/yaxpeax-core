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

use yaxpeax_arch::{Arch, Address};
use arch;
use arch::Symbol;

use std::fmt;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Summary<A: Arch> {
    ListBlocks,
    ListFunctions,
    ListFunctionBlocks(A::Address),
    HowMuchCode,
    ProgramInfo,
    SymbolInfo(Option<String>, String),
}

pub struct SummaryHelp;

impl fmt::Display for SummaryHelp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        writeln!(fmt, "list_blocks")?;
        writeln!(fmt, "list_functions")?;
        writeln!(fmt, "list_function_blocks:<function_address>")?;
        writeln!(fmt, "howmuchcode")?;
        writeln!(fmt, "program_info")?;
        writeln!(fmt, "symbol_info:<library_name>:<symbol_name>")?;

        Ok(())
    }
}

pub struct ListHelp;

impl fmt::Display for ListHelp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        writeln!(fmt, "list_functions")?;
        writeln!(fmt, "list_function_blocks:<function_address>")?;

        Ok(())
    }
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

pub struct DisplayHelp;

impl fmt::Display for DisplayHelp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        writeln!(fmt, "render_instruction:<address>")?;
        writeln!(fmt, "render_block:<address>")?;
        writeln!(fmt, "render_range:<start_address>:<end_address>")?;
        writeln!(fmt, "render_function:<address>")?;
        writeln!(fmt, "render_instruction_ssa:<address>")?;
        writeln!(fmt, "render_block_ssa:<address>")?;
        writeln!(fmt, "render_function_ssa:<address>")?;

        Ok(())
    }
}

pub enum ParseResult<A: Arch, T> {
    Operation(Operation<A, T>),
    Help(&'static (fmt::Display)),
    Err(OperationError)
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

pub struct OperationHelp;

impl fmt::Display for OperationHelp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        writeln!(fmt, "display")?;
        writeln!(fmt, "  commands related to displaying data, instructions, functions, etc")?;
        writeln!(fmt, "analysis")?;
        writeln!(fmt, "  commands to run analyses of code")?;
        writeln!(fmt, "debug")?;
        writeln!(fmt, "  commands for debugging (attaching to a process, breakpoints, execution, ...")?;
        writeln!(fmt, "summary")?;
        writeln!(fmt, "  commands for summarizing information about the program in question, data that is known, ...")?;
        writeln!(fmt, "data")?;
        writeln!(fmt, "  commands for directly modifying low level data that is tracked - adding function hints, adding contextual overrides of instruction semantics, data overrides, etc")?;
        writeln!(fmt, "specific")?;
        writeln!(fmt, "  architecture-specific commands (none of these yet!)")?;

        Ok(())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Data<A: Arch> {
    AddFunctionHint(A::Address),
    DefineSymbol(A::Address, Symbol),
    CodeComment(A::Address, String),
}

pub struct DataHelp;

impl fmt::Display for DataHelp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        writeln!(fmt, "add_function_hint:<address>")?;
        writeln!(fmt, "define_symbol:<address>:<symbol name>")?;
        writeln!(fmt, "code_comment:<address>:<comment>")?;

        Ok(())
    }
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum Debug {
    ShowMaps,
    ShowModules,
    ShowThreads,
    ShowInfo
}

pub struct DebugHelp;

impl fmt::Display for DebugHelp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        writeln!(fmt, "show_maps")?;
        writeln!(fmt, "show_modules")?;
        writeln!(fmt, "show_threads")?;
        writeln!(fmt, "show_info")?;

        Ok(())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Analysis<A: Arch> {
    ComputeSSAForm(A::Address),
    ControlFlowLinear(A::Address, A::Address),
    ControlFlowIncremental(Vec<A::Address>),
    DoEverything
}

pub struct AnalysisHelp;

impl fmt::Display for AnalysisHelp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        writeln!(fmt, "compute_ssa_form:<at_address>")?;
        writeln!(fmt, "control_flow_linear:<start_address>:<end_address>")?;
        writeln!(fmt, "  computes control flow between `start` and `end`")?;
        writeln!(fmt, "  this is not what you want if a linear decode is invalid (might decode incorrect instructions, for example)")?;
        writeln!(fmt, "control_flow_incremental:<start_address>")?;
        writeln!(fmt, "  recursively builds control flow information from `start_address`, with no runtime/memory/size bounds")?;
        writeln!(fmt, "compute_ssa_form:<address>")?;
        writeln!(fmt, "  computes SSA numbering for the control flow graph started at `address`.")?;
        writeln!(fmt, "do_everything")?;
        writeln!(fmt, "  recursively finds control flow from hinted start points, then computes SSA form for reached code")?;

        Ok(())
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum OperationSuccess {
    Ok,
    OkWithOutput(Vec<(usize, Vec<String>)>),
    SwitchArch(arch::ISA)
}

#[derive(Debug, Serialize, Deserialize)]
pub enum ParseError {
    Address(String),
    Symbol(String),
    Command(String),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ParseError::Address(addr) => {
                write!(fmt, "Invalid address: \"{}\"", addr)
            },
            ParseError::Symbol(sym) => {
                write!(fmt, "Invalid symbol: \"{}\"", sym)
            },
            ParseError::Command(cmd) => {
                write!(fmt, "Invalid command: \"{}\"", cmd)
            },
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum OperationError {
    ParseError(ParseError),
    Invalid(String),
    Unknown(String),
    Unsupported(String),
    Misc(String)
}

pub trait Operate<T, A: Arch> {
//    type TargetArch: Arch;
//    fn evaluate(operation: Operation<Self::TargetArch, T>) -> Result<OperationSuccess, OperationError<Self::TargetArch, T>>;
    fn evaluate(&mut self, operation: Operation<A, T>) -> Result<OperationSuccess, OperationError>;
}
