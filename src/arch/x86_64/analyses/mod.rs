use yaxpeax_arch::{Arch, LengthedInstruction};
use yaxpeax_x86::long_mode::{Opcode, Operand, RegisterBank, RegSpec};
use yaxpeax_x86::{x86_64 as x86_64Arch};
use analyses::control_flow::Effect;
use arch::x86_64::{Update, x86Update};
use arch::x86_64;
use arch::BaseUpdate;
use analyses::xrefs::{RefType, RefAction};
use tracing::{event, span, Level};

pub mod data_flow;
pub mod evaluators;
pub mod value_range;

pub fn all_instruction_analyses(
    instr: &<x86_64Arch as Arch>::Instruction,
    address: <x86_64Arch as Arch>::Address,
    effect: &Effect<<x86_64Arch as Arch>::Address>,
    ctxs: &x86_64::MergedContextTable
) -> Vec<(<x86_64Arch as Arch>::Address, Update)> {
    let mut results = find_xrefs(instr, address, effect, ctxs);
//    results.extend(collect_function_hint(instr, address, effect, ctxs));
    results.extend(find_function_hints(instr, address, effect, ctxs));
    results.extend(compute_next_state(instr, address, effect, ctxs));
    results
}

pub fn compute_next_state(
    _instr: &<x86_64Arch as Arch>::Instruction,
    _address: <x86_64Arch as Arch>::Address,
    effect: &Effect<<x86_64Arch as Arch>::Address>,
    _ctxs: &x86_64::MergedContextTable
) -> Vec<(<x86_64Arch as Arch>::Address, Update)> {
    if !effect.is_stop() {
//        let mut ctx = x86_64::compute_state(&instr, &ctxs.at(&address));
//        vec![(address + instr.len(), x86_64::Update::FullContext(ctx))]
        vec![]
    } else {
        vec![]
    }
}

pub fn find_function_hints(
    instr: &<x86_64Arch as Arch>::Instruction,
    address: <x86_64Arch as Arch>::Address,
    _effect: &Effect<<x86_64Arch as Arch>::Address>,
    _ctxs: &x86_64::MergedContextTable
) -> Vec<(<x86_64Arch as Arch>::Address, Update)> {
    if instr.opcode == Opcode::CALL {
        match instr.operand(0) {
            Operand::ImmediateI32(disp) => {
                let dest = (disp as u64).wrapping_add(address).wrapping_add(instr.len());
                vec![
                    (dest, BaseUpdate::Specialized(x86Update::FunctionHint))
                ]
            },
            _ => {
                vec![]
                /* TODO: check other operand types and if we know registers */
            }
        }
    } else {
        vec![]
    }
}

// TODO: This does not distinguish between addresses in different segments!
// so ds:[0x8] is the same as gs:[0x8]
//
// worse, linear address of teb + 0x8 is not the same as gs:[0x8] in user code, f.ex!
pub fn find_xrefs(
    instr: &<x86_64Arch as Arch>::Instruction,
    address: <x86_64Arch as Arch>::Address,
    _effect: &Effect<<x86_64Arch as Arch>::Address>,
    _ctxs: &x86_64::MergedContextTable
) -> Vec<(<x86_64Arch as Arch>::Address, Update)> {
    // TODO: how to deal with peephole-rewritten code?
    // instruction reordering or replacement, where there's a user or
    // computed context taking precedence over the instruction?
    //
    // alright, let's start by splitting this up by number of
    // memory accesses this does NOT necessarily match the number
    // of operands! for example, push and pop are one operand, but
    // may access two memory locations
    match instr.opcode {
        // zero memory accesses
        // LEA is not included here because it often refers to memory
        // even if it doesn't access it then and there
        Opcode::XCHG |
        Opcode::HLT |
        Opcode::WAIT |
        Opcode::CBW |
        Opcode::CDQ |
        Opcode::INT |
        Opcode::INTO |
        Opcode::IRET | // TODO not sure i fully understand this
        Opcode::ENTER |
        Opcode::LEAVE |
        Opcode::INS |
        Opcode::OUTS |
        Opcode::LAHF |
        Opcode::SAHF |
        Opcode::CLC |
        Opcode::STC |
        Opcode::CLI |
        Opcode::STI |
        Opcode::CLD |
        Opcode::STD |
        Opcode::NOP => { vec! [] }

        // Control flow operations are special
        Opcode::JO |
        Opcode::JNO |
        Opcode::JZ |
        Opcode::JNZ |
        Opcode::JA |
        Opcode::JNA |
        Opcode::JB |
        Opcode::JNB |
        Opcode::JS |
        Opcode::JNS |
        Opcode::JP |
        Opcode::JNP |
        Opcode::JG |
        Opcode::JLE |
        Opcode::JGE |
        Opcode::JL |
        Opcode::CALL |
        Opcode::CALLF |
        Opcode::JMP |
        Opcode::JMPF => {
            let mut refs: Vec<(<x86_64Arch as Arch>::Address, Update)> = vec![];
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

        // one memory access (max)
        Opcode::XADD |
        Opcode::BT |
        Opcode::BTS |
        Opcode::BTR |
        Opcode::BTC |
        Opcode::BSR |
        Opcode::BSF |
        Opcode::LEA |
        Opcode::POPF |
        Opcode::PUSHF |
        Opcode::TEST |
        Opcode::CMP |
        Opcode::IMUL |
        Opcode::DIV |
        Opcode::IDIV |
        Opcode::MUL |
        Opcode::MOVSS |
        Opcode::SQRTSS |
        Opcode::ADDSS |
        Opcode::SUBSS |
        Opcode::MULSS |
        Opcode::DIVSS |
        Opcode::MINSS |
        Opcode::MAXSS |
        Opcode::MOVSLDUP |
        Opcode::CVTSI2SS |
        Opcode::CVTTSS2SI |
        Opcode::CVTSS2SI |
        Opcode::CVTSS2SD |
        Opcode::MOVSD |
        Opcode::SQRTSD |
        Opcode::ADDSD |
        Opcode::SUBSD |
        Opcode::MULSD |
        Opcode::DIVSD |
        Opcode::MINSD |
        Opcode::MAXSD |
        Opcode::MOVDDUP |
        Opcode::HADDPS |
        Opcode::HSUBPS |
        Opcode::ADDSUBPS |
        Opcode::CVTSI2SD |
        Opcode::CVTTSD2SI |
        Opcode::CVTSD2SI |
        Opcode::CVTSD2SS |
        Opcode::LDDQU |
        Opcode::SETO |
        Opcode::SETNO |
        Opcode::SETZ |
        Opcode::SETNZ |
        Opcode::SETA |
        Opcode::SETBE |
        Opcode::SETB |
        Opcode::SETAE |
        Opcode::SETP |
        Opcode::SETNP |
        Opcode::SETS |
        Opcode::SETNS |
        Opcode::SETG |
        Opcode::SETLE |
        Opcode::SETGE |
        Opcode::SETL |
        Opcode::CMOVO |
        Opcode::CMOVNO |
        Opcode::CMOVZ |
        Opcode::CMOVNZ |
        Opcode::CMOVA |
        Opcode::CMOVNA |
        Opcode::CMOVB |
        Opcode::CMOVNB |
        Opcode::CMOVP |
        Opcode::CMOVNP |
        Opcode::CMOVS |
        Opcode::CMOVNS |
        Opcode::CMOVG |
        Opcode::CMOVLE |
        Opcode::CMOVGE |
        Opcode::CMOVL |
        Opcode::MOV |
        Opcode::MOVAPS |
        Opcode::MOVDQA |
        Opcode::MOVSX_b |
        Opcode::MOVSX_w |
        Opcode::MOVZX_b |
        Opcode::MOVZX_w |
        Opcode::MOVSXD |
        Opcode::MOVSX => {
            // TODO: anything other than mov probably should be read+write
            let mut refs: Vec<(<x86_64Arch as Arch>::Address, Update)> = vec![];
            match instr.operand(0) {
                /* One day, consult ctx to figure out if we know effective addresses. */
                Operand::DisplacementU32(ref_addr) => {
                    refs.push((address, BaseUpdate::Specialized(x86Update::AddXRef(RefType::Data, RefAction::Write, ref_addr as u64))));
                },
                Operand::DisplacementU64(ref_addr) => {
                    refs.push((address, BaseUpdate::Specialized(x86Update::AddXRef(RefType::Data, RefAction::Write, ref_addr as u64))));
                }
                Operand::RegDisp(RegSpec { bank: RegisterBank::RIP, num: _ }, disp) => {
                    refs.push((address, BaseUpdate::Specialized(x86Update::AddXRef(
                        RefType::Data,
                        RefAction::Write,
                        (disp as i64 as u64)
                            .wrapping_add(address)
                            .wrapping_add(instr.len())
                    ))));
                }
                _ => {}
            };

            match instr.operand(1) {
                /* One day, consult ctx to figure out if we know effective addresses. */
                Operand::DisplacementU32(ref_addr) => {
                    refs.push((address, BaseUpdate::Specialized(x86Update::AddXRef(RefType::Data, RefAction::Read, ref_addr as u64))));
                },
                Operand::DisplacementU64(ref_addr) => {
                    refs.push((address, BaseUpdate::Specialized(x86Update::AddXRef(RefType::Data, RefAction::Read, ref_addr as u64))));
                }
                Operand::RegDisp(RegSpec { bank: RegisterBank::RIP, num: _ }, disp) => {
                    refs.push((address, BaseUpdate::Specialized(x86Update::AddXRef(
                        RefType::Data,
                        RefAction::Read,
                        (disp as i64 as u64)
                            .wrapping_add(address)
                            .wrapping_add(instr.len())
                    ))));
                }
                _ => {}
            };

            refs
        }
        // two memory accesses (max)
        Opcode::PUSH |
        Opcode::POP |
        Opcode::ADD |
        Opcode::ADC |
        Opcode::SUB |
        Opcode::SAR |
        Opcode::SAL |
        Opcode::SHR |
        Opcode::SHL |
        Opcode::RCR |
        Opcode::RCL |
        Opcode::ROR |
        Opcode::ROL |
        Opcode::INC |
        Opcode::DEC |
        Opcode::SBB |
        Opcode::AND |
        Opcode::XOR |
        Opcode::OR |
        Opcode::NEG |
        Opcode::NOT |
        Opcode::LODS |
        Opcode::STOS |
        Opcode::CMPS |
        Opcode::SCAS |
        Opcode::MOVS |
        Opcode::CMPXCHG => {
            vec![]
        },
        Opcode::WRMSR |
        Opcode::RDMSR |
        Opcode::RDTSC |
        Opcode::RDPMC |
        Opcode::FXSAVE |
        Opcode::FXRSTOR |
        Opcode::LDMXCSR |
        Opcode::STMXCSR |
        Opcode::XSAVE |
        Opcode::XRSTOR |
        Opcode::XSAVEOPT |
        Opcode::LFENCE |
        Opcode::MFENCE |
        Opcode::SFENCE |
        Opcode::CLFLUSH |
        Opcode::SGDT |
        Opcode::SIDT |
        Opcode::LGDT |
        Opcode::LIDT |
        Opcode::SMSW |
        Opcode::LMSW |
        Opcode::SWAPGS |
        Opcode::RDTSCP |
        Opcode::INVLPG |
        Opcode::CPUID |
        Opcode::UD2 |
        Opcode::WBINVD |
        Opcode::INVD |
        Opcode::SYSRET |
        Opcode::CLTS |
        Opcode::SYSCALL |
        Opcode::LSL |
        Opcode::LAR |
        Opcode::SLDT |
        Opcode::STR |
        Opcode::LLDT |
        Opcode::LTR |
        Opcode::VERR |
        Opcode::VERW |
        Opcode::JMPE |
        Opcode::RETF |
        Opcode::RETURN |
        Opcode::Invalid => {
            /* Honestly, i'm too tired to think about retf and ret right now.*/
            vec![]
        }
        o => {
            event!(Level::ERROR, opcode = ?o, "missing xref tests"); //, "missing control flow description", opcode = o);
            vec![]
        }
    }
}
