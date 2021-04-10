use yaxpeax_arch::Arch;
use memory::MemoryRange;
use arch::FunctionQuery;
use arch::FunctionImpl;
use data::Disambiguator;
use data::LocIterator;
use data::modifier::ModifierCollection;
use data::modifier::NoModifiers;
use analyses::static_single_assignment::SSAValues;
use analyses::control_flow::ControlFlowGraph;
use analyses::static_single_assignment::SSA;
use analyses::static_single_assignment::cytron::{generate_ssa, generate_refined_ssa};
use arch::AbiDefaults;

use data::Direction;

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Use {
    Read, Write, ReadWrite
}

impl Use {
    pub fn first_use(&self) -> Direction {
        match self {
            Use::Read | Use::ReadWrite => {
                Direction::Read
            },
            Use::Write => {
                Direction::Write
            }
        }
    }
}

pub struct AnalysisBuilder<
    'memory,
    'cfg,
    'functions,
    'disambiguator,
    'modifiers,
    A: Arch,
    M,
    F,
    LocSpec,
    D,
    U,
> {
    memory: &'memory M,
    cfg: &'cfg ControlFlowGraph<A::Address>,
    functions: &'functions F,
    loc_spec: std::marker::PhantomData<LocSpec>,
    disambiguator: &'disambiguator mut D,
    modifiers: &'modifiers U,
}

impl<
    'memory,
    'cfg,
    'functions,
    'disambiguator,
    'modifiers,
    A: Arch + SSAValues,
    M: MemoryRange<A::Address>,
    F: FunctionQuery<A::Address, Function=FunctionImpl<A::Location>>,
    LocSpec,
    D: Disambiguator<A, LocSpec>,
> AnalysisBuilder<'memory, 'cfg, 'functions, 'disambiguator, 'modifiers, A, M, F, LocSpec, D, NoModifiers> where
    A::Location: 'static + AbiDefaults,
    for<'a> &'a <A as Arch>::Instruction: LocIterator<'disambiguator, 'functions, A, A::Location, D, F, Item=(Option<A::Location>, Direction), LocSpec=LocSpec>
{
    pub fn new(memory: &'memory M, cfg: &'cfg ControlFlowGraph<A::Address>, functions: &'functions F, disambiguator: &'disambiguator mut D) -> AnalysisBuilder<'memory, 'cfg, 'functions, 'disambiguator, 'static, A, M, F, LocSpec, D, NoModifiers> {
        AnalysisBuilder {
            memory,
            cfg,
            functions,
            loc_spec: std::marker::PhantomData,
            disambiguator,
            modifiers: &NoModifiers,
        }
    }
}

impl<
    'memory,
    'cfg,
    'functions,
    'disambiguator,
    'modifiers,
    A: Arch + SSAValues,
    M: MemoryRange<A::Address>,
    F: FunctionQuery<A::Address, Function=FunctionImpl<A::Location>>,
    LocSpec,
    D: Disambiguator<A, LocSpec>,
    U: ModifierCollection<A>,
> AnalysisBuilder<'memory, 'cfg, 'functions, 'disambiguator, 'modifiers, A, M, F, LocSpec, D, U> where
    A::Location: 'static + AbiDefaults,
    for<'a> &'a <A as Arch>::Instruction: LocIterator<'disambiguator, 'functions, A, A::Location, D, F, Item=(Option<A::Location>, Direction), LocSpec=LocSpec>
{
    pub fn with_modifiers<'new_modifiers, NewU: ModifierCollection<A>>(self, new_modifiers: &'new_modifiers NewU) -> AnalysisBuilder<'memory, 'cfg, 'functions, 'disambiguator, 'new_modifiers, A, M, F, LocSpec, D, NewU> {
        let Self {
            memory,
            cfg,
            functions,
            loc_spec,
            disambiguator,
            ..
        } = self;

        AnalysisBuilder {
            memory,
            cfg,
            functions,
            loc_spec,
            disambiguator,
            modifiers: new_modifiers,
        }
    }

    pub fn ssa_cytron(self) -> SSA<A> {
        let Self {
            memory,
            cfg,
            functions,
            disambiguator,
            modifiers,
            ..
        } = self;

        generate_ssa(memory, cfg.entrypoint, &cfg, &cfg.graph, modifiers, disambiguator, functions)
    }

    pub fn ssa_cytron_refining(self, prior_dfg: &SSA<A>) -> SSA<A> {
        let Self {
            memory,
            cfg,
            functions,
            disambiguator,
            modifiers,
            ..
        } = self;

        generate_refined_ssa(memory, cfg.entrypoint, &cfg, &cfg.graph, prior_dfg, modifiers, disambiguator, functions)
    }
}
