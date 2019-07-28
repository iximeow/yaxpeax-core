use yaxpeax_arch::Arch;
use yaxpeax_arch::LengthedInstruction;
use arch::InstructionSpan;
use arch::display::BaseDisplay;
use analyses::static_single_assignment::SSAValues;
use analyses::static_single_assignment::SSA;
use analyses::control_flow::ControlFlowGraph;
use memory::MemoryRepr;
use memory::MemoryRange;
use data::Direction;
use arch::FunctionQuery;
use arch::SymbolQuery;
use arch::FunctionRepr;
use yaxpeax_arch::ColorSettings;

use std::fmt;
use std::fmt::Write;
use std::marker::PhantomData;

use num_traits::WrappingAdd;
use num_traits::Zero;

/// This view in particular takes optionally-present data flow information, which ends up
/// restricting the usefulness of this struct to architectures that have SSA information defined.
/// a handy TODO: here is to either figure out how to have a default Location that specifies no
/// locations, and a corresponding data flow reference that never returns locations, OR to figure
/// out if there should be a different `FunctionView` for non-location-enabled architectures.
///
/// Anyway the question of highlighting operands is architecture-independent and only requires that
/// operands are known, which is true by virtue of `Arch` being implemented
pub struct FunctionView<
    'a, 'b, 'c, 'd, 'e,
    F: FunctionRepr,
    Context: FunctionQuery<A::Address>,
    A: Arch + BaseDisplay<F, Context> + SSAValues,
    M: MemoryRepr<A::Address> + MemoryRange<A::Address>
> {
    pub _function_type: PhantomData<F>,
    pub data: &'a M,
    pub ctx: &'b Context,
    pub fn_graph: &'c ControlFlowGraph<A::Address>,
    pub ssa: Option<&'d SSA<A>>,
    pub colors: Option<&'e ColorSettings>,
    pub highlight_instrs: Vec<A::Address>,
    pub highlight_locs: Vec<(A::Address, A::Location, Direction)>,
}

pub trait FunctionDisplay<A: Arch + SSAValues> {
    fn add_highlight_instr(&mut self, addr: A::Address);
    fn add_highlight_loc(&mut self, loc: (A::Address, A::Location, Direction));
    fn reset_highlight_instrs(&mut self);
    fn reset_highlight_locs(&mut self);
    fn view_between(&self, start: Option<A::Address>, end: Option<A::Address>) -> Vec<(A::Address, Vec<String>)>;
}

pub trait FunctionInstructionDisplay<A: Arch + SSAValues, Context: SymbolQuery<A::Address> + FunctionQuery<A::Address>> {
    fn display_instruction_in_function<W: fmt::Write>(
        dest: &mut W,
        instr: &A::Instruction,
        address: A::Address,
        context: &Context,
        ssa: Option<&SSA<A>>,
        colors: Option<&ColorSettings>,
    ) -> fmt::Result;
}


impl <
    'a, 'b, 'c, 'd, 'e,
    F: FunctionRepr,
    Context: FunctionQuery<A::Address> + SymbolQuery<A::Address>,
    A: Arch + BaseDisplay<F, Context> + SSAValues,
    M: MemoryRepr<A::Address> + MemoryRange<A::Address>
> FunctionDisplay<A> for FunctionView<'a, 'b, 'c, 'd, 'e, F, Context, A, M> where A: FunctionInstructionDisplay<A, Context>  {
    fn add_highlight_instr(&mut self, addr: A::Address) {
        self.highlight_instrs.push(addr);
    }
    fn add_highlight_loc(&mut self, loc: (A::Address, A::Location, Direction)) {
        self.highlight_locs.push(loc);
    }
    fn reset_highlight_instrs(&mut self) {
        self.highlight_instrs.clear();
    }
    fn reset_highlight_locs(&mut self) {
        self.highlight_locs.clear();
    }

    fn view_between(&self, start: Option<A::Address>, end: Option<A::Address>) -> Vec<(A::Address, Vec<String>)> {
        let mut text: Vec<(A::Address, Vec<String>)> = Vec::new();
        let mut blocks: Vec<A::Address> = self.fn_graph.blocks.iter().map(|x| x.start).collect();
        blocks.sort();

        for blockaddr in blocks.iter() {
            let block = self.fn_graph.get_block(*blockaddr);

            // hack to avoid looking at the "basic block" over [0, ... first real basic block)
            if block.start == A::Address::zero() { continue; }

            let mut iter = self.data.instructions_spanning::<A::Instruction>(block.start, block.end);

            let end = iter.end;
            while let Some((address, instr)) = iter.next() {
                let mut instr_string = String::new();
                A::render_frame(
                    &mut instr_string,
                    address,
                    instr,
                    &mut self.data.range(address..(address + instr.len())).unwrap(),
                    Some(self.ctx),
                ).unwrap();
                // ok come back to this
                write!(instr_string, " ").unwrap();
                A::display_instruction_in_function(&mut instr_string, &instr, address, self.ctx, self.ssa, self.colors).unwrap();
                if address.wrapping_add(&instr.len()) > end {
                    write!(instr_string ,"\n┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄").unwrap();
                }
                let strings: Vec<String> = instr_string.split("\n").map(|s| s.to_string()).collect();
                text.push((address, strings));
            }
        }
        text
    }
}

