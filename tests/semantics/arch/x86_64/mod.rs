use yaxpeax_arch::Arch;
use yaxpeax_x86::long_mode::{Arch as x86_64, RegSpec};

use yaxpeax_core::arch::x86_64::semantic;
use yaxpeax_core::arch::x86_64::x86_64Data;
use yaxpeax_core::analyses::{control_flow, data_flow};
use yaxpeax_core::analyses::control_flow::ControlFlowGraph;
// use yaxpeax_core::analyses::static_single_assignment::Data;
use yaxpeax_core::analyses::static_single_assignment::SSA;
use yaxpeax_core::arch::x86_64::analyses::data_flow::{Data, Location};

fn do_analyses(data: &[u8]) -> (ControlFlowGraph<<x86_64 as Arch>::Address>, SSA<x86_64>) {
    // TODO: is this necessary? can this be removed from `AnalysisBuilder::new`?
    let mut x86_64_data = x86_64Data::default();

    let cfg = control_flow::AnalysisBuilder::new(&data.to_vec(), &mut x86_64_data.contexts)
        .evaluate();
    let dfg = data_flow::AnalysisBuilder::new(
        &data.to_vec(),
        &cfg,
        &*x86_64_data.contexts.functions.borrow(),
        &mut yaxpeax_core::arch::x86_64::analyses::data_flow::NoDisambiguation::default(),
    )
        .ssa_cytron();

    (cfg, dfg)
}

#[test]
fn test_arithmetic() {
    let instructions = &[
        0xb8, 0x04, 0x00, 0x00, 0x00, // mov eax, 4
        0xb9, 0x04, 0x00, 0x00, 0x00, // mov ecx, 4
        0x48, 0x01, 0xc1, // add rcx, rax
        0xc3,
    ];

    let (cfg, dfg) = do_analyses(instructions);

    let v = dfg.get_def(10, Location::Register(RegSpec::rcx()));
    // expect that rcx == 8
    assert_eq!(v.as_rc().borrow().data, Some(Data::Concrete(8, None)));
}
