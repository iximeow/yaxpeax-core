use yaxpeax_arm::armv8::a64::{ARMv8, Instruction, Opcode};
use yaxpeax_arch::{Address, Arch};
use std::fmt::Debug;
use analyses::control_flow;
use analyses::Value;
use data::ValueLocations;
use analyses::control_flow::Determinant;
use analyses::control_flow::ControlFlowAnalysis;
use analyses::control_flow::ToAddrDiff;

use arch::arm::v8::aarch64::analyses::data_flow::Location;
use analyses::DFG;

impl DFG<control_flow::Effect<<ARMv8 as Arch>::Address>, ARMv8> for ControlFlowAnalysis<<ARMv8 as Arch>::Address> {
    fn read_loc(&self, loc: <ARMv8 as ValueLocations>::Location) -> control_flow::Effect<<ARMv8 as Arch>::Address> {
        if loc == Location::PC {
            self.effect.clone()
        } else {
            control_flow::Effect::unknown()
        }
    }

    fn write_loc(&mut self, loc: <ARMv8 as ValueLocations>::Location, value: control_flow::Effect<<ARMv8 as Arch>::Address>) {
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
    |inst| { None },
);
