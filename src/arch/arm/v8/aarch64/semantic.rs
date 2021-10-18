use analyses::Value;
use arch::arm::v8::aarch64::analyses::data_flow::Location;
use yaxpeax_arm::armv8::a64::{ARMv8, Opcode, Operand};
use yaxpeax_arch::{AddressDiff, Arch};
use analyses::DFG;
use analyses::DFGLocationQuery;
use analyses::DFGLocationQueryMut;
use analyses::CompletionStatus;

fn apply_condition_code<
    V: Value + Clone + From<AddressDiff<<ARMv8 as Arch>::Address>>,
    D: DFGLocationQueryMut<V, ARMv8>
>(_code: u8, next_addr: V, dfg: &mut D) {
    let current_dest = dfg.read(&Location::PC);
    let joined = V::from_set(&[current_dest, next_addr.clone()]);

    let _apply = |_dfg: &mut D, condition: V| {
        match condition.as_bool() {
            Some(true) => {
                dfg.write(&Location::PC, next_addr);
            }
            Some(false) => {}
            None => {
                dfg.write(&Location::PC, joined);
            }
        }
    };

    unimplemented!("aarch64 condition codes");

    /*
    match code {
        ConditionCode::AL => {},
        ConditionCode::EQ => {
            let condition = dfg.read(&Location::ZF);
            apply(dfg, condition);
        },
        ConditionCode::NE => {
            let condition = dfg.read(&Location::ZF).not();
            apply(dfg, condition);
        },
        ConditionCode::HS => {
            let condition = dfg.read(&Location::CF);
            apply(dfg, condition);
        },
        ConditionCode::LO => {
            let condition = dfg.read(&Location::CF).not();
            apply(dfg, condition);
        },
        ConditionCode::MI => {
            let condition = dfg.read(&Location::NF);
            apply(dfg, condition);
        },
        ConditionCode::PL => {
            let condition = dfg.read(&Location::NF).not();
            apply(dfg, condition);
        },
        ConditionCode::VS => {
            let condition = dfg.read(&Location::VF);
            apply(dfg, condition);
        },
        ConditionCode::VC => {
            let condition = dfg.read(&Location::VF).not();
            apply(dfg, condition);
        },
        ConditionCode::HI => {
            let condition = dfg.read(&Location::CF).and(&dfg.read(&Location::ZF).not()).value();
            let condition = condition;
            apply(dfg, condition);
        },
        ConditionCode::LS => {
            let condition = dfg.read(&Location::CF).and(&dfg.read(&Location::ZF).not()).value();
            let condition = condition.not();
            apply(dfg, condition);
        },
        ConditionCode::GE => {
            let condition = dfg.read(&Location::NF).eq(&dfg.read(&Location::VF));
            let condition = condition;
            apply(dfg, condition);
        },
        ConditionCode::LT => {
            let condition = dfg.read(&Location::NF).eq(&dfg.read(&Location::VF));
            let condition = condition.not();
            apply(dfg, condition)
        },
        ConditionCode::GT => {
            let nv = dfg.read(&Location::NF).eq(&dfg.read(&Location::VF));
            let condition = dfg.read(&Location::ZF).not().and(&nv).value();
            let condition = condition;
            apply(dfg, condition);
        },
        ConditionCode::LE => {
            let nv = dfg.read(&Location::NF).eq(&dfg.read(&Location::VF)).not();
            let condition = dfg.read(&Location::ZF).and(&nv).value();
            let condition = condition;
            apply(dfg, condition);
        },
    }
    */
}

pub(crate) fn evaluate<
    K: Copy,
    V: Value + Clone + From<AddressDiff<<ARMv8 as Arch>::Address>>,
    D: DFG<V, ARMv8, K>
>(when: K, instr: &<ARMv8 as Arch>::Instruction, dfg: &mut D) -> CompletionStatus {
    let dfg = &mut dfg.query_at_mut(when);
    fn read_operand<V: Value, D: DFGLocationQuery<V, ARMv8>>(_dfg: &mut D, _operand: &Operand) -> V {
        V::unknown()
    }

    fn push<V: Value, D: DFGLocationQueryMut<V, ARMv8>>(dfg: &mut D, _value: V) {
        dfg.write(&Location::SP, V::unknown())
    }

    let next_instr = dfg.read(&Location::PC);

    let _complete = match instr.opcode {
        Opcode::B => {
            let dest = read_operand(dfg, &instr.operands[0]);
            dfg.write(&Location::PC, dest);
        },
        Opcode::BL => {
            let ra = dfg.read(&Location::PC);
            push(dfg, ra);
            let dest = read_operand(dfg, &instr.operands[0]);
            dfg.write(&Location::PC, dest);
        },
        Opcode::Bcc(cond) => {
            let dest = read_operand(dfg, &instr.operands[0]);
            dfg.write(&Location::PC, dest);

            apply_condition_code(cond, next_instr, dfg);
        }
        _ => {}
    };

    CompletionStatus::Incomplete
}
