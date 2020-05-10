use yaxpeax_x86::long_mode::{Instruction, Opcode};
use yaxpeax_x86::long_mode::{Arch as x86_64};
use yaxpeax_arch::{Address, Arch};
use std::fmt::Debug;
use analyses::control_flow;
use analyses::Value;
use analyses::control_flow::Determinant;
use analyses::control_flow::ControlFlowAnalysis;
use analyses::control_flow::ToAddrDiff;

use arch::x86_64::analyses::data_flow::Location;
use analyses::DFG;

impl<Addr: Address + Debug + ToAddrDiff> DFG<control_flow::Effect<Addr>> for ControlFlowAnalysis<Addr> {
    type Location = Location;

    fn read_loc(&self, loc: Self::Location) -> control_flow::Effect<Addr> {
        if loc == Location::RIP {
            self.effect.clone()
        } else if let Location::Memory(_) = loc {
            control_flow::Effect::unknown()
        } else {
            control_flow::Effect::unknown()
        }
    }

    fn write_loc(&mut self, loc: Self::Location, value: control_flow::Effect<Addr>) {
        if loc == Location::RIP {
            self.effect = value;
        } else {
            // do nothing, it's a location we ignore for control flow analysis
        }
    }
}

impl_control_flow!(
    crate::arch::x86_64::semantic::evaluate,
    yaxpeax_x86::long_mode::Arch,
    yaxpeax_x86::long_mode::Instruction,
    |inst| {
        let assume_calls_return = true;
        match inst.opcode {
            Opcode::CALL |
            Opcode::SYSCALL => {
                if assume_calls_return {
                    return Some(control_flow::Effect::cont());
                    // instr_control_flow.with_effect(control_flow::Effect::cont());
                }
            }
            _ => {}
        }
        None
    }
);

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
