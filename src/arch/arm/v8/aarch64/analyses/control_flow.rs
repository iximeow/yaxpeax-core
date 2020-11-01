use yaxpeax_arm::armv8::a64::ARMv8;
use yaxpeax_arch::Arch;
use analyses::control_flow;
use analyses::Value;
use data::ValueLocations;
use analyses::control_flow::ControlFlowAnalysis;

use arch::arm::v8::aarch64::analyses::data_flow::Location;
use analyses::DFG;

opaque_indirection_dfg!(ARMv8, control_flow::Effect<<ARMv8 as Arch>::Address>, (), <ARMv8 as ValueLocations>::Location, ControlFlowAnalysis<<ARMv8 as Arch>::Address>);

impl DFG<control_flow::Effect<<ARMv8 as Arch>::Address>, ARMv8, ()> for ControlFlowAnalysis<<ARMv8 as Arch>::Address> {
    fn read_loc(&self, _when: (), loc: <ARMv8 as ValueLocations>::Location) -> control_flow::Effect<<ARMv8 as Arch>::Address> {
        if loc == Location::PC {
            self.effect.clone()
        } else {
            control_flow::Effect::unknown()
        }
    }

    fn write_loc(&mut self, _when: (), loc: <ARMv8 as ValueLocations>::Location, value: control_flow::Effect<<ARMv8 as Arch>::Address>) {
        if loc == Location::PC {
            self.effect = value;
        } else {
            // do nothing, it's a location we ignore for control flow analysis
        }
    }
}

impl_control_flow!(
    crate::arch::arm::v8::aarch64::semantic::evaluate,
    yaxpeax_arm::armv8::a64::ARMv8,
    yaxpeax_arm::armv8::a64::Instruction,
    |_inst| { None },
);
