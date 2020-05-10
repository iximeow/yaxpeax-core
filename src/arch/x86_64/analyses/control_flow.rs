use yaxpeax_x86::long_mode::{Instruction, Opcode};
use yaxpeax_x86::long_mode::{Arch as x86_64};
use yaxpeax_arch::Arch;
use analyses::control_flow;
use analyses::control_flow::Determinant;
use analyses::control_flow::ControlFlowAnalysis;

impl <T> control_flow::Determinant<T, <x86_64 as Arch>::Address> for Instruction {
    fn control_flow(&self, _ctx: Option<&T>) -> control_flow::Effect<<x86_64 as Arch>::Address> {
        let mut instr_control_flow = ControlFlowAnalysis::new();
        crate::arch::x86_64::semantic::evaluate(self, &mut instr_control_flow);
        let assume_calls_return = true;
        match self.opcode {
            Opcode::CALL |
            Opcode::SYSCALL => {
                if assume_calls_return {
                    return control_flow::Effect::cont();
                    // instr_control_flow.with_effect(control_flow::Effect::cont());
                }
            }
            _ => {}
        }
        instr_control_flow.into_effect()
    }
}

#[test]
fn test_x86_determinant() {
    use yaxpeax_arch::Decoder;
    use analyses::control_flow::Determinant;
    let decoder = <x86_64 as Arch>::Decoder::default();
    // call 0x1234567
    let call = decoder.decode([0xe8, 0x78, 0x56, 0x34, 0x12].iter().cloned()).unwrap();
    assert_eq!(call.control_flow(Option::<&()>::None), control_flow::Effect::cont());
    // jmp 0x1234567
    let jmp = decoder.decode([0xe9, 0x78, 0x56, 0x34, 0x12].iter().cloned()).unwrap();
    assert_eq!(jmp.control_flow(Option::<&()>::None), control_flow::Effect::stop_and(
        control_flow::Target::Relative(AddressDiff::from_const(0x12345678))
    ));
    // ret
    let ret = decoder.decode([0xc3].iter().cloned()).unwrap();
    assert_eq!(ret.control_flow(Option::<&()>::None), control_flow::Effect::stop());
    // jl 0x14
    let jl = decoder.decode([0x7c, 0x14].iter().cloned()).unwrap();
    assert_eq!(jl.control_flow(Option::<&()>::None), control_flow::Effect::cont_and(
        control_flow::Target::Relative(AddressDiff::from_const(0x14))
    ));
}
