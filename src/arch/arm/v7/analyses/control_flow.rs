use yaxpeax_arm::armv7::ARMv7;
use yaxpeax_arch::Arch;
use analyses::control_flow;
use analyses::Value;
use data::ValueLocations;
use analyses::control_flow::ControlFlowAnalysis;
use analyses::OpaqueIndirection;

use arch::arm::v7::analyses::data_flow::Location;
use analyses::DFG;

impl DFG<control_flow::Effect<<ARMv7 as Arch>::Address>, ARMv7, ()> for ControlFlowAnalysis<<ARMv7 as Arch>::Address> {
    type Indirect = OpaqueIndirection<control_flow::Effect<<ARMv7 as Arch>::Address>>;

    fn indirect_loc(&self, _when: (), _loc: <ARMv7 as ValueLocations>::Location) -> OpaqueIndirection<control_flow::Effect<<ARMv7 as Arch>::Address>> {
        OpaqueIndirection::inst()
    }
    fn read_loc(&self, _when: (), loc: <ARMv7 as ValueLocations>::Location) -> control_flow::Effect<<ARMv7 as Arch>::Address> {
        if loc == Location::pc() {
            self.effect.clone()
        } else {
            control_flow::Effect::unknown()
        }
    }

    fn write_loc(&mut self, _when: (), loc: <ARMv7 as ValueLocations>::Location, value: control_flow::Effect<<ARMv7 as Arch>::Address>) {
        if loc == Location::pc() {
            self.effect = value;
        } else {
            // do nothing, it's a location we ignore for control flow analysis
        }
    }
}

impl_control_flow!(
    crate::arch::arm::v7::semantic::evaluate,
    yaxpeax_arm::armv7::ARMv7,
    yaxpeax_arm::armv7::Instruction,
    |_inst| { None },
);
