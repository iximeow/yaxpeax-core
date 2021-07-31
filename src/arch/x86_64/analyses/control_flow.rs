use yaxpeax_x86::long_mode::Opcode;
use yaxpeax_x86::long_mode::{Arch as x86_64};
use yaxpeax_arch::Arch;
use analyses::control_flow;
use analyses::Value;
use data::ValueLocations;
use analyses::control_flow::ControlFlowAnalysis;
use analyses::OpaqueIndirection;

use arch::x86_64::analyses::data_flow::Location;
use analyses::DFG;

impl DFG<control_flow::Effect<<x86_64 as Arch>::Address>, x86_64, ()> for ControlFlowAnalysis<<x86_64 as Arch>::Address> {
    type Indirect = OpaqueIndirection<control_flow::Effect<<x86_64 as Arch>::Address>>;

    fn indirect_loc(&self, _when: (), _loc: <x86_64 as ValueLocations>::Location) -> OpaqueIndirection<control_flow::Effect<<x86_64 as Arch>::Address>> {
        OpaqueIndirection::inst()
    }
    fn read_loc(&self, _when: (), loc: <x86_64 as ValueLocations>::Location) -> control_flow::Effect<<x86_64 as Arch>::Address> {
        if loc == Location::RIP {
            self.effect.clone()
        } else if let Location::Memory(_) = loc {
            control_flow::Effect::unknown()
        } else {
            control_flow::Effect::unknown()
        }
    }

    fn write_loc(&mut self, _when: (), loc: <x86_64 as ValueLocations>::Location, value: control_flow::Effect<<x86_64 as Arch>::Address>) {
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
        match inst.opcode() {
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
    },
);

#[test]
fn test_x86_determinant() {
    use yaxpeax_arch::AddressDiff;
    use yaxpeax_arch::Decoder;
    use memory::MemoryRepr;
    use memory::MemoryRange;
    use memory::repr::ReadCursor;
    use analyses::control_flow::Determinant;
    fn decode(data: &[u8]) -> <x86_64 as Arch>::Instruction {
        let decoder = <x86_64 as Arch>::Decoder::default();
        let cursor: ReadCursor<x86_64, [u8]> = data.range(0..(data.len() as u64)).unwrap();
        decoder.decode(&mut cursor.to_reader()).unwrap()
    }
    // call 0x1234567
    let call = decode(&[0xe8, 0x78, 0x56, 0x34, 0x12][..]);
    assert_eq!(call.control_flow(Option::<&()>::None), control_flow::Effect::cont());
    // jmp 0x1234567
    let jmp = decode(&[0xe9, 0x78, 0x56, 0x34, 0x12][..]);
    assert_eq!(jmp.control_flow(Option::<&()>::None), control_flow::Effect::stop_and(
        control_flow::Target::Relative(AddressDiff::from_const(0x12345678))
    ));
    // ret
    let ret = decode(&[0xc3][..]);
    assert_eq!(ret.control_flow(Option::<&()>::None), control_flow::Effect::stop());
    // jl 0x14
    let jl = decode(&[0x7c, 0x14][..]);
    assert_eq!(jl.control_flow(Option::<&()>::None), control_flow::Effect::cont_and(
        control_flow::Target::Relative(AddressDiff::from_const(0x14))
    ));
}
