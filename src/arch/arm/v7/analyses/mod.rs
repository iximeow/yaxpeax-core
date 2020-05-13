use analyses::control_flow::Effect;
use analyses::xrefs;

use arch::arm::v7::Update;
use arch::arm;

use yaxpeax_arm::armv7::{ARMv7, Opcode, Operand};
use yaxpeax_arch::Arch;

use tracing::{Level, event};

pub mod control_flow;
pub mod data_flow;

pub fn all_instruction_analyses(
    instr: &<ARMv7 as Arch>::Instruction,
    address: <ARMv7 as Arch>::Address,
    effect: &Effect<<ARMv7 as Arch>::Address>,
    ctxs: &arm::v7::MergedContextTable
) -> Vec<(<ARMv7 as Arch>::Address, Update)> {
    let mut results = find_xrefs(instr, address, effect, ctxs);
//    results.extend(collect_function_hint(instr, address, effect, ctxs));
    results.extend(find_function_hints(instr, address, effect, ctxs));
    results.extend(compute_next_state(instr, address, effect, ctxs));
    results
}

pub fn compute_next_state(
    _instr: &<ARMv7 as Arch>::Instruction,
    _address: <ARMv7 as Arch>::Address,
    effect: &Effect<<ARMv7 as Arch>::Address>,
    _ctxs: &arm::v7::MergedContextTable
) -> Vec<(<ARMv7 as Arch>::Address, Update)> {
    if !effect.is_stop() {
//        let mut ctx = arm::v7::compute_state(&instr, &ctxs.at(&address));
//        vec![(address + instr.len(), arm::v7::Update::FullContext(ctx))]
        vec![]
    } else {
        vec![]
    }
}

pub fn find_function_hints(
    instr: &<ARMv7 as Arch>::Instruction,
    address: <ARMv7 as Arch>::Address,
    _effect: &Effect<<ARMv7 as Arch>::Address>,
    _ctxs: &arm::v7::MergedContextTable
) -> Vec<(<ARMv7 as Arch>::Address, Update)> {
    if instr.opcode == Opcode::BL {
        event!(Level::ERROR, instr = ?instr, opcode = ?instr.opcode, "missing call destination stuff for {}", instr);
        /*
                vec![
                    (dest, BaseUpdate::Specialized(x86Update::FunctionHint))
                ]
                */
        vec![]
    } else {
        vec![]
    }
}

// TODO: This does not distinguish between addresses in different segments!
// so ds:[0x8] is the same as gs:[0x8]
//
// worse, linear address of teb + 0x8 is not the same as gs:[0x8] in user code, f.ex!
pub fn find_xrefs(
    instr: &<ARMv7 as Arch>::Instruction,
    address: <ARMv7 as Arch>::Address,
    _effect: &Effect<<ARMv7 as Arch>::Address>,
    _ctxs: &arm::v7::MergedContextTable
) -> Vec<(<ARMv7 as Arch>::Address, Update)> {
    match instr.opcode {
        /*
        Opcode::JMPF => {
            let mut refs: Vec<(<ARMv7 as Arch>::Address, Update)> = vec![];
            match instr.operand(0) {
                Operand::ImmediateI8(offset) => {
                    refs.push((
                        address,
                        BaseUpdate::Specialized(x86Update::AddXRef(
                            RefType::Code,
                            RefAction::Referrer,
                            (address + instr.len()).wrapping_add(offset as u64)
                        ))
                    ));
                },
                Operand::ImmediateI32(offset) => {
                    refs.push((
                        address,
                        BaseUpdate::Specialized(x86Update::AddXRef(
                            RefType::Code,
                            RefAction::Referrer,
                            (address + instr.len()).wrapping_add(offset as u64)
                        ))
                    ));
                },
                Operand::DisplacementU32(disp) => {
                    refs.push((address, BaseUpdate::Specialized(x86Update::AddXRef(RefType::Data, RefAction::Read, disp as u64))));
                },
                Operand::DisplacementU64(disp) => {
                    refs.push((address, BaseUpdate::Specialized(x86Update::AddXRef(RefType::Data, RefAction::Read, disp as u64))));
                },
                _ => {}
            };
            refs
        }
        */

        o => {
            event!(Level::ERROR, opcode = ?o, "missing xref tests"); //, "missing control flow description", opcode = o);
            vec![]
        }
    }
}
