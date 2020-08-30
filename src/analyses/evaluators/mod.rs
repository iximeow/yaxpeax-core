use yaxpeax_arch::Arch;

use petgraph::visit::Bfs;

use analyses::static_single_assignment::SSA;
use analyses::static_single_assignment::SSAValues;
use memory::MemoryRange;
use analyses::control_flow::ControlFlowGraph;
use arch::InstructionSpan;
use analyses::evaluators::const_evaluator::ConstEvaluator;
use arch::x86_64::analyses::evaluators::const_evaluator::ConcreteDomain;
use arch::x86_64::analyses::evaluators::symbolizer::SymbolicDomain;
use arch::x86_64::analyses::evaluators::value_set::ValueSetDomain;

pub mod const_evaluator;

pub struct Evaluator<'program, 'function, 'ssa, A: Arch + SSAValues, M: MemoryRange<A::Address>> {
    program: &'program M,
    fn_graph: &'function ControlFlowGraph<A::Address>,
    ssa: &'ssa SSA<A>,
}

impl<'program, 'function, 'ssa, A: Arch + SSAValues, M: MemoryRange<A::Address>> Evaluator<'program, 'function, 'ssa, A, M> where
    A: ConstEvaluator<A, (), ConcreteDomain>,
    A: ConstEvaluator<A, (), SymbolicDomain>,
    A: ConstEvaluator<A, (), ValueSetDomain>,
    A::Instruction: std::fmt::Display,
{
    pub fn new(program: &'program M, fn_graph: &'function ControlFlowGraph<A::Address>, ssa: &'ssa SSA<A>) -> Self {
        Evaluator {
            program,
            fn_graph,
            ssa,
        }
    }

    pub fn iterate_basic_block(&self, block: A::Address) {
        let block = self.fn_graph.get_block(block);
        let mut iter = self.program.instructions_spanning(A::Decoder::default(), block.start, block.end);
        while let Some((address, instr)) = iter.next() {
            use yaxpeax_arch::AddressDisplay;
            println!("evaluating {}: {}", address.show(), instr);
            <A as ConstEvaluator<A, (), ConcreteDomain>>::evaluate_instruction(&instr, address, self.ssa, &(), self.program);
            <A as ConstEvaluator<A, (), SymbolicDomain>>::evaluate_instruction(&instr, address, self.ssa, &(), self.program);
            <A as ConstEvaluator<A, (), ValueSetDomain>>::evaluate_instruction(&instr, address, self.ssa, &(), self.program);
            // `fn_query_ptr` from `program_info.rs`?
        }
    }

    pub fn full_function_iterate(&self) {
        use yaxpeax_arch::AddressDisplay;
        let mut bfs = Bfs::new(&self.fn_graph.graph, self.fn_graph.entrypoint);
        while let Some(k) = bfs.next(&self.fn_graph.graph) {
            self.iterate_basic_block(k);
        }
    }
}
