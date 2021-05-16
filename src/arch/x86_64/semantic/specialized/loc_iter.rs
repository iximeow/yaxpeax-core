use yaxpeax_x86::long_mode::{Instruction, Opcode, Operand, RegSpec};
use arch::x86_64::analyses::data_flow::{ANY, Location, cond_to_flags};
use analyses::data_flow::Use;
use data::{Direction, Disambiguator};
use arch::{FunctionQuery, FunctionImpl};
use tracing::{event, Level};

pub struct LocationIter<'a, 'b, 'c, D: Disambiguator<yaxpeax_x86::x86_64, (<yaxpeax_x86::x86_64 as yaxpeax_arch::Arch>::Address, u8, u8)> + ?Sized, F: FunctionQuery<<yaxpeax_x86::x86_64 as yaxpeax_arch::Arch>::Address> + ?Sized> {
    addr: <yaxpeax_x86::x86_64 as yaxpeax_arch::Arch>::Address,
    inst: &'a Instruction,
    op_count: u8,
    op_idx: u8,
    loc_count: u8,
    loc_idx: u8,
    curr_op: Option<Operand>,
    curr_use: Option<Use>,
    disambiguator: &'b D,
    _fn_query: &'c F,
}

fn operands_in(instr: &Instruction) -> u8 {
    match instr.opcode() {
        Opcode::SQRTSD |
        Opcode::SQRTSS |
        Opcode::MOVDDUP |
        Opcode::MOVSLDUP |
        Opcode::MOVSD |
        Opcode::MOVSS |
        Opcode::CVTSI2SS |
        Opcode::CVTTSS2SI |
        Opcode::CVTSS2SI |
        Opcode::CVTSS2SD |
        Opcode::CVTSI2SD |
        Opcode::CVTTSD2SI |
        Opcode::CVTSD2SI |
        Opcode::CVTSD2SS |
        Opcode::CVTPS2PD |
        Opcode::LDDQU |
        Opcode::LEA |
        Opcode::MOVSX_b |
        Opcode::MOVSX_w |
        Opcode::MOVZX_b |
        Opcode::MOVZX_w |
        Opcode::MOVSX |
        Opcode::MOVSXD |
        Opcode::MOVAPS |
        Opcode::MOVUPS |
        Opcode::MOVDQA |
        Opcode::MOVDQU |
        Opcode::MOVAPD |
        Opcode::MOVQ |
        Opcode::MOV => {
            3
        }
        Opcode::XADD |
        Opcode::XCHG => {
            3
        }
        Opcode::STI |
        Opcode::CLI |
        Opcode::STD |
        Opcode::CLD |
        Opcode::STC |
        Opcode::CLC => {
            1
        }
        Opcode::BT |
        Opcode::BTS |
        Opcode::BTR |
        Opcode::BTC |
        Opcode::BSR |
        Opcode::BSF |
        Opcode::TZCNT |
        Opcode::SAR |
        Opcode::SAL |
        Opcode::SHR |
        Opcode::SHL |
        Opcode::RCR |
        Opcode::RCL |
        Opcode::ROR |
        Opcode::ROL |
        Opcode::ADC |
        Opcode::SBB |
        Opcode::ADD |
        Opcode::SUB |
        Opcode::AND |
        Opcode::XOR |
        Opcode::MULPS |
        Opcode::ORPS |
        Opcode::PADDQ |
        Opcode::PCMPEQD |
        Opcode::PCMPGTD |
        Opcode::PCMPGTW |
        Opcode::PMULLW |
        Opcode::PSLLD |
        Opcode::PSLLDQ |
        Opcode::PSLLQ |
        Opcode::PSLLW |
        Opcode::PSRAD |
        Opcode::PSRLD |
        Opcode::PSRLDQ |
        Opcode::PSRLQ |
        Opcode::PSRLW |
        Opcode::PUNPCKHBW |
        Opcode::PUNPCKHDQ |
        Opcode::PUNPCKHQDQ |
        Opcode::PUNPCKHWD |
        Opcode::PUNPCKLBW |
        Opcode::PUNPCKLDQ |
        Opcode::PUNPCKLQDQ |
        Opcode::PUNPCKLWD |
        Opcode::PXOR |
        Opcode::SUBPS |
        Opcode::UNPCKHPS |
        Opcode::UNPCKLPS |
        Opcode::XORPS |
        Opcode::OR => {
            3
        }

        Opcode::IMUL => {
            // TODO: this.
            if !instr.operand_present(1) {
                2
            } else {
                3
            }
        },
        Opcode::IDIV |
        Opcode::DIV => {
            // TODO: this is lazy and assumes writes of all flags
            // this may not be true
            // TODO: this may not read *dx (if this is idiv r/m 8)
            //if let Operand::Nothing = instr.operand(1) {
                2
            //} else {
            //    3
            //}
        }
        Opcode::MUL => {
            2
        }

        Opcode::PUSH |
        Opcode::POP => {
            2
        },
        Opcode::INC |
        Opcode::DEC => {
            2
        }
        Opcode::ENTER |
        Opcode::LEAVE |
        Opcode::POPF |
        Opcode::PUSHF |
        Opcode::CDQ |
        Opcode::LAHF |
        Opcode::SAHF => {
            1
        }
        Opcode::TEST |
        Opcode::CMP => {
            3
        }
        Opcode::NEG |
        Opcode::NOT => {
            2
        }
        Opcode::CMPXCHG => {
            3
        },
        Opcode::CALLF | // TODO: this is wrong
        Opcode::CALL => {
            2
        }
        Opcode::JMP => {
            2
        },
        Opcode::JMPF => { // TODO: this is wrong.
            0
        },
        Opcode::IRET | // TODO: this is wrong
        Opcode::RETF | // TODO: this is wrong
        Opcode::RETURN => {
            1
        }
        Opcode::LFENCE |
        Opcode::MFENCE |
        Opcode::SFENCE |
        Opcode::NOP |
        Opcode::WAIT => { 0 }
        Opcode::CLFLUSH |
        Opcode::FXSAVE |
        Opcode::FXRSTOR |
        Opcode::XSAVE |
        Opcode::XRSTOR |
        Opcode::XSAVEOPT |
        Opcode::STMXCSR |
        Opcode::SGDT |
        Opcode::SIDT => {
            2
        }
        Opcode::LDMXCSR |
        Opcode::LGDT |
        Opcode::LIDT => {
            2
        }
        // TODO: this is wrong
        Opcode::SMSW => {
            2
        }
        // TODO: this is wrong
        Opcode::LMSW => {
            2
        }
        Opcode::SWAPGS |
        Opcode::RDTSCP |
        Opcode::WRMSR |
        Opcode::RDMSR |
        Opcode::RDTSC |
        Opcode::RDPMC => {
            1
        }
        Opcode::VERR |
        Opcode::VERW => {
            2
        }
        Opcode::SLDT |
        Opcode::STR |
        Opcode::INVLPG => {
            2
        }
        Opcode::LLDT |
        Opcode::LTR => {
            2
        }
        // these are immediate-only or have no operands
        Opcode::JMPE |
        Opcode::UD2 |
        Opcode::INT |
        Opcode::INTO |
        Opcode::HLT |
        Opcode::Invalid => {
            0
        },
        Opcode::JO |
        Opcode::JNO |
        Opcode::JZ |
        Opcode::JNZ |
        Opcode::JA |
        Opcode::JNA |
        Opcode::JNB |
        Opcode::JB |
        Opcode::JP |
        Opcode::JNP |
        Opcode::JS |
        Opcode::JNS |
        Opcode::JG |
        Opcode::JLE |
        Opcode::JGE |
        Opcode::JL => {
            2
        }
        Opcode::CMOVO |
        Opcode::CMOVNO |
        Opcode::CMOVZ |
        Opcode::CMOVNZ |
        Opcode::CMOVA |
        Opcode::CMOVNA |
        Opcode::CMOVNB |
        Opcode::CMOVB |
        Opcode::CMOVP |
        Opcode::CMOVNP |
        Opcode::CMOVS |
        Opcode::CMOVNS |
        Opcode::CMOVG |
        Opcode::CMOVLE |
        Opcode::CMOVGE |
        Opcode::CMOVL => {
            3
        }
        Opcode::SETO |
        Opcode::SETNO |
        Opcode::SETZ |
        Opcode::SETNZ |
        Opcode::SETA |
        Opcode::SETBE |
        Opcode::SETAE |
        Opcode::SETB |
        Opcode::SETP |
        Opcode::SETNP |
        Opcode::SETS |
        Opcode::SETNS |
        Opcode::SETG |
        Opcode::SETLE |
        Opcode::SETGE |
        Opcode::SETL => {
            3
        }
        Opcode::LSL => {
            3
        }
        Opcode::LAR => {
            3
        }
        Opcode::ADDSD |
        Opcode::SUBSD |
        Opcode::MULSD |
        Opcode::DIVSD |
        Opcode::MINSD |
        Opcode::MAXSD |
        Opcode::ADDSS |
        Opcode::SUBSS |
        Opcode::MULSS |
        Opcode::DIVSS |
        Opcode::MINSS |
        Opcode::MAXSS |
        Opcode::HADDPS |
        Opcode::HSUBPS |
        Opcode::ADDSUBPS => {
            3
        }
        Opcode::CMPS |
        Opcode::SCAS |
        Opcode::MOVS |
        Opcode::LODS |
        Opcode::STOS |
        Opcode::INS |
        Opcode::OUTS => {
            // TODO: incomplete
            2
        }
        Opcode::CBW |
        Opcode::CWDE |
        Opcode::CDQE => {
            2
        },
        o @ Opcode::CPUID |
        o @ Opcode::WBINVD |
        o @ Opcode::INVD |
        o @ Opcode::SYSRET |
        o @ Opcode::CLTS |
        o @ Opcode::SYSCALL |
        o => {
            event!(Level::ERROR, opcode = ?o, "missing implicit operand information");
            0
        }
    }
}

fn locations_in(op: &Operand, usage: Use) -> u8 {
    (if usage == Use::ReadWrite { 1 } else { 0 }) + match op {
        Operand::Register(_spec) => {
            // reg
            1
        },
        Operand::DisplacementU32(_) |
        Operand::DisplacementU64(_) => {
            // mem
            1
        },
        Operand::RegDeref(_spec) |
        Operand::RegDisp(_spec, _) |
        Operand::RegScale(_spec, _) |
        Operand::RegScaleDisp(_spec, _, _) => {
            // reg, mem
            2
        },
        Operand::RegIndexBase(_base, _index) |
        Operand::RegIndexBaseDisp(_base, _index, _) |
        Operand::RegIndexBaseScale(_base, _index, _) |
        Operand::RegIndexBaseScaleDisp(_base, _index, _, _) => {
            // reg, reg, mem
            3
        },
        _ => {
            0
        }
    }
}

#[allow(dead_code)]
impl <'a, 'b, 'c, D: Disambiguator<yaxpeax_x86::x86_64, (<yaxpeax_x86::x86_64 as yaxpeax_arch::Arch>::Address, u8, u8)> + ?Sized, F: FunctionQuery<<yaxpeax_x86::x86_64 as yaxpeax_arch::Arch>::Address>> LocationIter<'a, 'b, 'c, D, F> {
    pub fn new(addr: <yaxpeax_x86::x86_64 as yaxpeax_arch::Arch>::Address, inst: &'a Instruction, disambiguator: &'b D, _fn_query: &'c F) -> Self {
        LocationIter {
            addr,
            inst,
            op_count: operands_in(inst),
            op_idx: 0,
            loc_count: implicit_locs(inst.opcode()),
            loc_idx: 0,
            curr_op: None,
            curr_use: None,
            disambiguator,
            _fn_query,
        }
    }
    fn curr_loc(&self) -> (u8, u8) {
        (self.op_idx, self.loc_idx - 1)
    }
}

fn use_of(instr: &Instruction, idx: u8) -> Use {
    match instr.opcode() {
        Opcode::SQRTSD |
        Opcode::SQRTSS |
        Opcode::MOVDDUP |
        Opcode::MOVSLDUP |
        Opcode::MOVSD |
        Opcode::MOVSS |
        Opcode::CVTSI2SS |
        Opcode::CVTTSS2SI |
        Opcode::CVTSS2SI |
        Opcode::CVTSS2SD |
        Opcode::CVTSI2SD |
        Opcode::CVTTSD2SI |
        Opcode::CVTSD2SI |
        Opcode::CVTSD2SS |
        Opcode::CVTPS2PD |
        Opcode::LDDQU |
        Opcode::LEA |
        Opcode::MOVSX_b |
        Opcode::MOVSX_w |
        Opcode::MOVZX_b |
        Opcode::MOVZX_w |
        Opcode::MOVSX |
        Opcode::MOVSXD |
        Opcode::MOVAPS |
        Opcode::MOVUPS |
        Opcode::MOVDQA |
        Opcode::MOVDQU |
        Opcode::MOVAPD |
        Opcode::MOVQ |
        Opcode::MOV => {
            [Use::Write, Use::Read][idx as usize]
        }
        Opcode::XADD |
        Opcode::XCHG => {
            [Use::ReadWrite, Use::ReadWrite][idx as usize]
        }
        Opcode::STI |
        Opcode::CLI |
        Opcode::STD |
        Opcode::CLD |
        Opcode::STC |
        Opcode::CLC => {
            Use::Read
        }
        Opcode::BT |
        Opcode::BTS |
        Opcode::BTR |
        Opcode::BTC |
        Opcode::BSR |
        Opcode::BSF |
        Opcode::TZCNT => {
            Use::Read
        }
        Opcode::SAR |
        Opcode::SAL |
        Opcode::SHR |
        Opcode::SHL |
        Opcode::RCR |
        Opcode::RCL |
        Opcode::ROR |
        Opcode::ROL |
        Opcode::ADC |
        Opcode::SBB |
        Opcode::ADD |
        Opcode::SUB |
        Opcode::AND |
        Opcode::XOR |
        Opcode::MULPS |
        Opcode::ORPS |
        Opcode::PADDQ |
        Opcode::PCMPEQD |
        Opcode::PCMPGTD |
        Opcode::PCMPGTW |
        Opcode::PMULLW |
        Opcode::PSLLD |
        Opcode::PSLLDQ |
        Opcode::PSLLQ |
        Opcode::PSLLW |
        Opcode::PSRAD |
        Opcode::PSRLD |
        Opcode::PSRLDQ |
        Opcode::PSRLQ |
        Opcode::PSRLW |
        Opcode::PUNPCKHBW |
        Opcode::PUNPCKHDQ |
        Opcode::PUNPCKHQDQ |
        Opcode::PUNPCKHWD |
        Opcode::PUNPCKLBW |
        Opcode::PUNPCKLDQ |
        Opcode::PUNPCKLQDQ |
        Opcode::PUNPCKLWD |
        Opcode::PXOR |
        Opcode::SUBPS |
        Opcode::UNPCKHPS |
        Opcode::UNPCKLPS |
        Opcode::XORPS |
        Opcode::OR => {
            [Use::ReadWrite, Use::Read][idx as usize]
        }
        Opcode::IMUL => {
            // TODO: this.
            if !instr.operand_present(1) {
                Use::Read
            } else {
                [Use::ReadWrite, Use::Read][idx as usize]
            }
        },
        Opcode::IDIV |
        Opcode::DIV => {
            // TODO: this is lazy and assumes writes of all flags
            // this may not be true
            // TODO: this may not read *dx (if this is idiv r/m 8)
            Use::Read
        }
        Opcode::MUL => {
            Use::Read
        }

        Opcode::PUSH => {
            Use::Read
        },
        Opcode::POP => {
            Use::Write
        },
        Opcode::INC |
        Opcode::DEC => {
            Use::ReadWrite
        }
        Opcode::ENTER |
        Opcode::LEAVE |
        Opcode::POPF |
        Opcode::PUSHF |
        Opcode::CBW |
        Opcode::CDQ |
        Opcode::LAHF |
        Opcode::SAHF => {
            Use::Read
        }
        Opcode::TEST |
        Opcode::CMP => {
            Use::Read
        }
        Opcode::NEG => {
            Use::ReadWrite
        }
        Opcode::NOT => {
            Use::ReadWrite
        }
        Opcode::CMPXCHG => {
            [Use::ReadWrite, Use::Read][idx as usize]
        },
        Opcode::CALLF | // TODO: this is wrong
        Opcode::CALL => {
            Use::Read
        }
        Opcode::JMP => {
            Use::Read
        },
        Opcode::JMPF => { // TODO: this is wrong.
            Use::Read
        },
        Opcode::IRET | // TODO: this is wrong
        Opcode::RETF | // TODO: this is wrong
        Opcode::RETURN => {
            Use::Read
        }
        Opcode::LFENCE |
        Opcode::MFENCE |
        Opcode::SFENCE |
        Opcode::NOP |
        Opcode::WAIT => { Use::Read }
        Opcode::CLFLUSH |
        Opcode::FXSAVE |
        Opcode::FXRSTOR |
        Opcode::XSAVE |
        Opcode::XRSTOR |
        Opcode::XSAVEOPT |
        Opcode::STMXCSR |
        Opcode::SGDT |
        Opcode::SIDT => {
            Use::Write
        }
        Opcode::LDMXCSR |
        Opcode::LGDT |
        Opcode::LIDT => {
            Use::Read
        }
        // TODO: this is wrong
        Opcode::SMSW => {
            Use::Write
        }
        // TODO: this is wrong
        Opcode::LMSW => {
            Use::Read
        }
        Opcode::SWAPGS |
        Opcode::RDTSCP |
        Opcode::WRMSR |
        Opcode::RDMSR |
        Opcode::RDTSC |
        Opcode::RDPMC => {
            Use::Read
        }
        Opcode::VERR |
        Opcode::VERW => {
            Use::Read
        }
        Opcode::SLDT |
        Opcode::STR |
        Opcode::INVLPG => {
            Use::Write
        }
        Opcode::LLDT |
        Opcode::LTR => {
            Use::Read
        }
        // these are immediate-only or have no operands
        Opcode::JMPE |
        Opcode::UD2 |
        Opcode::INT |
        Opcode::INTO |
        Opcode::HLT |
        Opcode::Invalid => {
            Use::Read
        },
        Opcode::JO |
        Opcode::JNO |
        Opcode::JZ |
        Opcode::JNZ |
        Opcode::JA |
        Opcode::JNA |
        Opcode::JNB |
        Opcode::JB |
        Opcode::JP |
        Opcode::JNP |
        Opcode::JS |
        Opcode::JNS |
        Opcode::JG |
        Opcode::JLE |
        Opcode::JGE |
        Opcode::JL => {
            Use::Read
        }

        Opcode::CMOVO |
        Opcode::CMOVNO |
        Opcode::CMOVZ |
        Opcode::CMOVNZ |
        Opcode::CMOVA |
        Opcode::CMOVNA |
        Opcode::CMOVNB |
        Opcode::CMOVB |
        Opcode::CMOVP |
        Opcode::CMOVNP |
        Opcode::CMOVS |
        Opcode::CMOVNS |
        Opcode::CMOVG |
        Opcode::CMOVLE |
        Opcode::CMOVGE |
        Opcode::CMOVL => {
            [Use::Write, Use::Read][idx as usize]
        }
        Opcode::SETO |
        Opcode::SETNO |
        Opcode::SETZ |
        Opcode::SETNZ |
        Opcode::SETA |
        Opcode::SETBE |
        Opcode::SETAE |
        Opcode::SETB |
        Opcode::SETP |
        Opcode::SETNP |
        Opcode::SETS |
        Opcode::SETNS |
        Opcode::SETG |
        Opcode::SETLE |
        Opcode::SETGE |
        Opcode::SETL => {
            Use::Write
        }
        Opcode::LSL => {
            [Use::Write, Use::Read][idx as usize]
        }
        Opcode::LAR => {
            [Use::Write, Use::Read][idx as usize]
        }
        Opcode::ADDSD |
        Opcode::SUBSD |
        Opcode::MULSD |
        Opcode::DIVSD |
        Opcode::MINSD |
        Opcode::MAXSD |
        Opcode::ADDSS |
        Opcode::SUBSS |
        Opcode::MULSS |
        Opcode::DIVSS |
        Opcode::MINSS |
        Opcode::MAXSS |
        Opcode::HADDPS |
        Opcode::HSUBPS |
        Opcode::ADDSUBPS => {
            [Use::ReadWrite, Use::Read][idx as usize]
        }
        Opcode::MOVS => {
            Use::Read
        }
        Opcode::LODS => {
            // the only explicit operand is rax/eax/ax/al that is written to
            [Use::Write][idx as usize]
        }

        Opcode::STOS => {
            // the only explicit operand is rax/eax/ax/al that is read from
            [Use::Read][idx as usize]
        }
        Opcode::INS |
        Opcode::OUTS => {
            [Use::Write, Use::Read][idx as usize]
        }

        Opcode::CMPS |
        Opcode::SCAS => {
            Use::Read
        }
        o @ Opcode::CPUID |
        o @ Opcode::WBINVD |
        o @ Opcode::INVD |
        o @ Opcode::SYSRET |
        o @ Opcode::CLTS |
        o @ Opcode::SYSCALL |
        o => {
//            unimplemented!("yet-unsupported opcode {:?}", o);
            event!(Level::ERROR, opcode = ?o, "missing operand read/write information");
            Use::Write
        }
    }
}

fn implicit_loc(op: Opcode, i: u8) -> (Option<Location>, Direction) {
    match op {
        Opcode::SQRTSD |
        Opcode::SQRTSS |
        Opcode::MOVDDUP |
        Opcode::MOVSLDUP |
        Opcode::MOVSD |
        Opcode::MOVSS |
        Opcode::CVTSI2SS |
        Opcode::CVTTSS2SI |
        Opcode::CVTSS2SI |
        Opcode::CVTSS2SD |
        Opcode::CVTSI2SD |
        Opcode::CVTTSD2SI |
        Opcode::CVTSD2SI |
        Opcode::CVTSD2SS |
        Opcode::CVTPS2PD |
        Opcode::LDDQU |
        Opcode::LEA |
        Opcode::MOVSX_b |
        Opcode::MOVSX_w |
        Opcode::MOVZX_b |
        Opcode::MOVZX_w |
        Opcode::MOVSX |
        Opcode::MOVSXD |
        Opcode::MOVAPS |
        Opcode::MOVUPS |
        Opcode::MOVDQA |
        Opcode::MOVDQU |
        Opcode::MOVAPD |
        Opcode::MOVQ |
        Opcode::MOV |
        Opcode::XADD |
        Opcode::XCHG => {
            unreachable!();
        }
        Opcode::STI |
        Opcode::CLI => {
            (Some(Location::IF), Direction::Write)
        }
        Opcode::STD |
        Opcode::CLD => {
            (Some(Location::DF), Direction::Write)
        }
        Opcode::STC |
        Opcode::CLC => {
            (Some(Location::CF), Direction::Write)
        }
        Opcode::BT |
        Opcode::BTS |
        Opcode::BTR |
        Opcode::BTC => {
            (Some(Location::CF), Direction::Write)
        }
        Opcode::BSR |
        Opcode::BSF |
        Opcode::TZCNT => {
            (Some(Location::ZF), Direction::Write)
        }
        Opcode::SAR |
        Opcode::SAL |
        Opcode::SHR |
        Opcode::SHL => {
            // TODO: inspect imm8 and CL if context permits to decide
            // flags more granularly
            [
                (Some(Location::CF), Direction::Write),
                (Some(Location::SF), Direction::Write),
                (Some(Location::ZF), Direction::Write),
                (Some(Location::PF), Direction::Write),
                (Some(Location::AF), Direction::Write),
            ][i as usize].clone()
        }
        Opcode::RCR |
        Opcode::RCL |
        Opcode::ROR |
        Opcode::ROL => {
            // TODO: inspect imm8 and CL if context permits to decide
            // flags more granularly
            [
                (Some(Location::CF), Direction::Write),
                (Some(Location::OF), Direction::Write),
            ][i as usize].clone()
        }
        Opcode::CBW |
        Opcode::CDQ |
        Opcode::CWDE |
        Opcode::CDQE => {
            // TODO: overapproximates CBW, CWDE
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Read),
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
            ][i as usize].clone()
        }
        Opcode::ADC |
        Opcode::SBB => {
            // TODO: this is lazy and assumes writes of all flags
            // this may not be true
            [
                (Some(Location::CF), Direction::Read),
                (Some(Location::CF), Direction::Write),
                (Some(Location::OF), Direction::Write),
                (Some(Location::SF), Direction::Write),
                (Some(Location::ZF), Direction::Write),
                (Some(Location::PF), Direction::Write),
                (Some(Location::AF), Direction::Write),
            ][i as usize].clone()
        },
        Opcode::ADD |
        Opcode::SUB |
        Opcode::AND |
        Opcode::XOR |
        Opcode::OR => {
            // TODO: this is lazy and assumes writes of all flags
            // this may not be true
            [
                (Some(Location::CF), Direction::Write),
                (Some(Location::OF), Direction::Write),
                (Some(Location::SF), Direction::Write),
                (Some(Location::ZF), Direction::Write),
                (Some(Location::PF), Direction::Write),
                (Some(Location::AF), Direction::Write),
            ][i as usize].clone()
        }

        Opcode::IMUL => {
            // TODO: this.
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Read),
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Write),
                (Some(Location::CF), Direction::Write),
                (Some(Location::OF), Direction::Write),
                (Some(Location::SF), Direction::Write),
                (Some(Location::ZF), Direction::Write),
                (Some(Location::PF), Direction::Write),
                (Some(Location::AF), Direction::Write),
            ][i as usize].clone()
        },
        Opcode::IDIV |
        Opcode::DIV => {
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Read),
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Read),
                (Some(Location::CF), Direction::Write),
                (Some(Location::OF), Direction::Write),
                (Some(Location::SF), Direction::Write),
                (Some(Location::ZF), Direction::Write),
                (Some(Location::PF), Direction::Write),
                (Some(Location::AF), Direction::Write),
            ][i as usize].clone()
        }
        Opcode::MUL => {
            // TODO: this is lazy and assumes writes of all flags
            // this may not be true
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Read),
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Write),
                (Some(Location::CF), Direction::Write),
                (Some(Location::OF), Direction::Write),
                (Some(Location::SF), Direction::Write),
                (Some(Location::ZF), Direction::Write),
                (Some(Location::PF), Direction::Write),
                (Some(Location::AF), Direction::Write),
            ][i as usize].clone()
        }

        Opcode::PUSH => {
            [
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Memory(ANY)), Direction::Write),
            ][i as usize].clone()
        },
        Opcode::POP => {
            [
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Memory(ANY)), Direction::Read),
            ][i as usize].clone()
        },
        Opcode::INC |
        Opcode::DEC => {
            [
                (Some(Location::OF), Direction::Write),
                (Some(Location::SF), Direction::Write),
                (Some(Location::ZF), Direction::Write),
                (Some(Location::AF), Direction::Write),
                (Some(Location::PF), Direction::Write),
            ][i as usize].clone()
        }
        Opcode::ENTER => {
            [
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Register(RegSpec::rbp())), Direction::Read),
                (Some(Location::Register(RegSpec::rbp())), Direction::Write),
                (Some(Location::Memory(ANY)), Direction::Write)
            ][i as usize].clone()
        }
        Opcode::LEAVE => {
            [
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Register(RegSpec::rbp())), Direction::Write),
                (Some(Location::Memory(ANY)), Direction::Read)
            ][i as usize].clone()
        }
        Opcode::POPF => {
            [
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Memory(ANY)), Direction::Read),
                (Some(Location::Register(RegSpec::rflags())), Direction::Write)
            ][i as usize].clone()
        }
        Opcode::PUSHF => {
            [
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Memory(ANY)), Direction::Write),
                (Some(Location::Register(RegSpec::rflags())), Direction::Read)
            ][i as usize].clone()
        }
        Opcode::LAHF => {
            [
                (Some(Location::Register(RegSpec::ax())), Direction::Write),
                (Some(Location::Register(RegSpec::rflags())), Direction::Read)
            ][i as usize].clone()
        }
        Opcode::SAHF => {
            [
                (Some(Location::Register(RegSpec::ax())), Direction::Read),
                (Some(Location::Register(RegSpec::rflags())), Direction::Write)
            ][i as usize].clone()
        }
        Opcode::TEST |
        Opcode::CMP => {
            [
                (Some(Location::CF), Direction::Write),
                (Some(Location::OF), Direction::Write),
                (Some(Location::AF), Direction::Write),
                (Some(Location::SF), Direction::Write),
                (Some(Location::ZF), Direction::Write),
                (Some(Location::PF), Direction::Write),
            ][i as usize].clone()
        }
        Opcode::NEG => {
            [
                (Some(Location::CF), Direction::Write),
                (Some(Location::OF), Direction::Write),
                (Some(Location::AF), Direction::Write),
                (Some(Location::SF), Direction::Write),
                (Some(Location::ZF), Direction::Write),
                (Some(Location::PF), Direction::Write),
            ][i as usize].clone()
        }
        Opcode::NOT => {
            panic!();
        }
        Opcode::CMPXCHG => {
            [
                (Some(Location::ZF), Direction::Write),
                // this is over-general
                (Some(Location::Register(RegSpec::rax())), Direction::Read),
            ][i as usize].clone()
        },
        Opcode::CALLF | // TODO: this is wrong
        Opcode::CALL => {
            [
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Memory(ANY)), Direction::Write),
            ][i as usize].clone()
        }
        Opcode::JMP => {
            panic!();
        },
        Opcode::JMPF => { // TODO: this is wrong.
            panic!();
        },
        Opcode::IRET | // TODO: this is wrong
        Opcode::RETF | // TODO: this is wrong
        Opcode::RETURN => {
            [
                (Some(Location::Register(RegSpec::rsp())), Direction::Read),
                (Some(Location::Register(RegSpec::rsp())), Direction::Write),
                (Some(Location::Memory(ANY)), Direction::Read)
            ][i as usize].clone()
        }
        Opcode::LFENCE |
        Opcode::MFENCE |
        Opcode::SFENCE |
        Opcode::NOP |
        Opcode::WAIT => { panic!() }
        Opcode::CLFLUSH |
        Opcode::FXSAVE |
        Opcode::FXRSTOR |
        Opcode::XSAVE |
        Opcode::XRSTOR |
        Opcode::XSAVEOPT |
        Opcode::STMXCSR |
        Opcode::SGDT |
        Opcode::SIDT => {
            panic!()
        }
        Opcode::LDMXCSR |
        Opcode::LGDT |
        Opcode::LIDT => {
            panic!()
        }
        // TODO: this is wrong
        Opcode::SMSW => {
            panic!()
        }
        // TODO: this is wrong
        Opcode::LMSW => {
            panic!()
        }
        Opcode::SWAPGS => {
            [
                (Some(Location::Register(RegSpec::gs())), Direction::Read),
                (Some(Location::Register(RegSpec::gs())), Direction::Write)
            ][i as usize].clone()
        }
        Opcode::RDTSCP => {
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rcx())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Write)
            ][i as usize].clone()
        }
        Opcode::WRMSR => {
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Read),
                (Some(Location::Register(RegSpec::rdx())), Direction::Read)
            ][i as usize].clone()
        }
        Opcode::RDMSR => {
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Write),
                (Some(Location::Register(RegSpec::rcx())), Direction::Read)
            ][i as usize].clone()
        }
        Opcode::RDTSC => {
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Write)
            ][i as usize].clone()
        }
        Opcode::RDPMC => {
            [
                (Some(Location::Register(RegSpec::rax())), Direction::Write),
                (Some(Location::Register(RegSpec::rdx())), Direction::Write),
                (Some(Location::Register(RegSpec::rcx())), Direction::Read)
            ][i as usize].clone()
        }
        Opcode::VERR |
        Opcode::VERW => {
            (Some(Location::ZF), Direction::Write)
        }
        Opcode::SLDT |
        Opcode::STR |
        Opcode::INVLPG => {
            panic!()
        }
        Opcode::LLDT |
        Opcode::LTR => {
            panic!()
        }
        // these are immediate-only or have no operands
        Opcode::JMPE |
        Opcode::UD2 |
        Opcode::INT |
        Opcode::INTO |
        Opcode::HLT |
        Opcode::Invalid => {
            panic!()
        },
        Opcode::JO |
        Opcode::JNO |
        Opcode::JZ |
        Opcode::JNZ |
        Opcode::JA |
        Opcode::JNA |
        Opcode::JNB |
        Opcode::JB |
        Opcode::JP |
        Opcode::JNP |
        Opcode::JS |
        Opcode::JNS |
        Opcode::JG |
        Opcode::JLE |
        Opcode::JGE |
        Opcode::JL => {
            if i == 0 {
                (Some(Location::RIP), Direction::Read)
            } else {
                cond_to_flags(op.condition().unwrap())[i as usize - 1].clone()
            }
        }

        Opcode::CMOVO |
        Opcode::CMOVNO |
        Opcode::CMOVZ |
        Opcode::CMOVNZ |
        Opcode::CMOVA |
        Opcode::CMOVNA |
        Opcode::CMOVNB |
        Opcode::CMOVB |
        Opcode::CMOVP |
        Opcode::CMOVNP |
        Opcode::CMOVS |
        Opcode::CMOVNS |
        Opcode::CMOVG |
        Opcode::CMOVLE |
        Opcode::CMOVGE |
        Opcode::CMOVL => {
            cond_to_flags(op.condition().unwrap())[i as usize].clone()
        }
        Opcode::SETO |
        Opcode::SETNO |
        Opcode::SETZ |
        Opcode::SETNZ |
        Opcode::SETA |
        Opcode::SETBE |
        Opcode::SETAE |
        Opcode::SETB |
        Opcode::SETP |
        Opcode::SETNP |
        Opcode::SETS |
        Opcode::SETNS |
        Opcode::SETG |
        Opcode::SETLE |
        Opcode::SETGE |
        Opcode::SETL => {
            cond_to_flags(op.condition().unwrap())[i as usize].clone()
        }
        Opcode::LSL => {
            (Some(Location::ZF), Direction::Write)
        }
        Opcode::LAR => {
            (Some(Location::ZF), Direction::Read)
        }
        Opcode::ADDSD |
        Opcode::SUBSD |
        Opcode::MULSD |
        Opcode::DIVSD |
        Opcode::MINSD |
        Opcode::MAXSD |
        Opcode::ADDSS |
        Opcode::SUBSS |
        Opcode::MULSS |
        Opcode::DIVSS |
        Opcode::MINSS |
        Opcode::MAXSS |
        Opcode::HADDPS |
        Opcode::HSUBPS |
        Opcode::ADDSUBPS => {
            panic!()
        }
        Opcode::CMPS |
        Opcode::SCAS |
        Opcode::MOVS |
        Opcode::LODS |
        Opcode::STOS |
        Opcode::INS |
        Opcode::OUTS => {
            // TODO: incomplete
            (Some(Location::DF), Direction::Read)
        }
        o @ Opcode::CPUID |
        o @ Opcode::WBINVD |
        o @ Opcode::INVD |
        o @ Opcode::SYSRET |
        o @ Opcode::CLTS |
        o @ Opcode::SYSCALL |
        o => {
            event!(Level::ERROR, opcode = ?o, "missing implicit operand information");
            /* TODO: these. */
//            panic!()
//            assume the worst case, a machine-wide write
            (None, Direction::Write)
        }
    }
}

fn implicit_locs(op: Opcode) -> u8 {
    match op {
        Opcode::SQRTSD |
        Opcode::SQRTSS |
        Opcode::MOVDDUP |
        Opcode::MOVSLDUP |
        Opcode::MOVSD |
        Opcode::MOVSS |
        Opcode::CVTSI2SS |
        Opcode::CVTTSS2SI |
        Opcode::CVTSS2SI |
        Opcode::CVTSS2SD |
        Opcode::CVTSI2SD |
        Opcode::CVTTSD2SI |
        Opcode::CVTSD2SI |
        Opcode::CVTSD2SS |
        Opcode::CVTPS2PD |
        Opcode::LDDQU |
        Opcode::LEA |
        Opcode::MOVSX_b |
        Opcode::MOVSX_w |
        Opcode::MOVZX_b |
        Opcode::MOVZX_w |
        Opcode::MOVSX |
        Opcode::MOVSXD |
        Opcode::MOVAPS |
        Opcode::MOVUPS |
        Opcode::MOVDQA |
        Opcode::MOVDQU |
        Opcode::MOVAPD |
        Opcode::MOVQ |
        Opcode::MOV => {
            0
        }
        Opcode::XADD |
        Opcode::XCHG => {
            0
        }
        Opcode::STI |
        Opcode::CLI |
        Opcode::STD |
        Opcode::CLD |
        Opcode::STC |
        Opcode::CLC |
        Opcode::BT |
        Opcode::BTS |
        Opcode::BTR |
        Opcode::BTC |
        Opcode::BSR |
        Opcode::BSF |
        Opcode::TZCNT => {
            1
        }
        Opcode::SAR |
        Opcode::SAL |
        Opcode::SHR |
        Opcode::SHL => {
            5
        }
        Opcode::RCR |
        Opcode::RCL |
        Opcode::ROR |
        Opcode::ROL => {
            2
        }
        Opcode::ADC |
        Opcode::SBB => {
            7
        },
        Opcode::ADD |
        Opcode::SUB |
        Opcode::AND |
        Opcode::XOR |
        Opcode::OR => {
            6
        }

        Opcode::IMUL => {
            9
        },
        Opcode::IDIV |
        Opcode::DIV => {
            9
        }
        Opcode::MUL => {
            9
        }
        Opcode::PUSH => {
            3
        },
        Opcode::POP => {
            3
        },
        Opcode::INC |
        Opcode::DEC => {
            5
        }
        Opcode::ENTER => {
            5
        }
        Opcode::LEAVE => {
            4
        }
        Opcode::POPF => {
            4
        }
        Opcode::PUSHF => {
            4
        }
        Opcode::CBW |
        Opcode::CDQ => {
            2
        }
        Opcode::LAHF => {
            2
        }
        Opcode::SAHF => {
            2
        }
        Opcode::TEST |
        Opcode::CMP => {
            6
        }
        Opcode::NEG => {
            6
        }
        Opcode::NOT => {
            0
        }
        Opcode::CMPXCHG => { // TODO: this is wrong
            2
        },
        Opcode::CALLF | // TODO: this is wrong
        Opcode::CALL => {
            3
        }
        Opcode::JMP => {
            0
        },
        Opcode::JMPF => { // TODO: this is wrong.
            0
        },
        Opcode::IRET | // TODO: this is wrong
        Opcode::RETF | // TODO: this is wrong
        Opcode::RETURN => {
            3
        }
        Opcode::LFENCE |
        Opcode::MFENCE |
        Opcode::SFENCE |
        Opcode::NOP |
        Opcode::WAIT => { 0 }
        Opcode::CLFLUSH |
        Opcode::FXSAVE |
        Opcode::FXRSTOR |
        Opcode::XSAVE |
        Opcode::XRSTOR |
        Opcode::XSAVEOPT |
        Opcode::STMXCSR |
        Opcode::SGDT |
        Opcode::SIDT => {
            0
        }
        Opcode::LDMXCSR |
        Opcode::LGDT |
        Opcode::LIDT => {
            0
        }
        // TODO: this is wrong
        Opcode::SMSW => {
            0
        }
        // TODO: this is wrong
        Opcode::LMSW => {
            0
        }
        Opcode::SWAPGS => {
            2
        }
        Opcode::RDTSCP => {
            3
        }
        Opcode::WRMSR => {
            2
        }
        Opcode::RDMSR => {
            3
        }
        Opcode::RDTSC => {
            2
        }
        Opcode::RDPMC => {
            3
        }
        Opcode::VERR |
        Opcode::VERW => {
            1
        }
        Opcode::SLDT |
        Opcode::STR |
        Opcode::INVLPG |
        Opcode::LLDT |
        Opcode::LTR => {
            0
        }
        // these are immediate-only or have no operands
        Opcode::JMPE |
        Opcode::UD2 |
        Opcode::INT |
        Opcode::INTO |
        Opcode::HLT |
        Opcode::Invalid => {
            0
        },
        Opcode::JO |
        Opcode::JNO |
        Opcode::JZ |
        Opcode::JNZ |
        Opcode::JA |
        Opcode::JNA |
        Opcode::JNB |
        Opcode::JB |
        Opcode::JP |
        Opcode::JNP |
        Opcode::JS |
        Opcode::JNS |
        Opcode::JG |
        Opcode::JLE |
        Opcode::JGE |
        Opcode::JL => {
            // count rip
            1 + cond_to_flags(op.condition().unwrap()).len() as u8
        }

        Opcode::CMOVO |
        Opcode::CMOVNO |
        Opcode::CMOVZ |
        Opcode::CMOVNZ |
        Opcode::CMOVA |
        Opcode::CMOVNA |
        Opcode::CMOVNB |
        Opcode::CMOVB |
        Opcode::CMOVP |
        Opcode::CMOVNP |
        Opcode::CMOVS |
        Opcode::CMOVNS |
        Opcode::CMOVG |
        Opcode::CMOVLE |
        Opcode::CMOVGE |
        Opcode::CMOVL => {
            cond_to_flags(op.condition().unwrap()).len() as u8
        }
        Opcode::SETO |
        Opcode::SETNO |
        Opcode::SETZ |
        Opcode::SETNZ |
        Opcode::SETA |
        Opcode::SETBE |
        Opcode::SETAE |
        Opcode::SETB |
        Opcode::SETP |
        Opcode::SETNP |
        Opcode::SETS |
        Opcode::SETNS |
        Opcode::SETG |
        Opcode::SETLE |
        Opcode::SETGE |
        Opcode::SETL => {
            cond_to_flags(op.condition().unwrap()).len() as u8
        }
        Opcode::LSL |
        Opcode::LAR => {
            1
        }
        Opcode::CWDE |
        Opcode::CDQE => {
            1
        }
        Opcode::ADDSD |
        Opcode::SUBSD |
        Opcode::MULSD |
        Opcode::DIVSD |
        Opcode::MINSD |
        Opcode::MAXSD |
        Opcode::ADDSS |
        Opcode::SUBSS |
        Opcode::MULSS |
        Opcode::DIVSS |
        Opcode::MINSS |
        Opcode::MAXSS |
        Opcode::HADDPS |
        Opcode::HSUBPS |
        Opcode::ADDSUBPS => {
            0
        }
        Opcode::CMPS |
        Opcode::SCAS |
        Opcode::MOVS |
        Opcode::LODS |
        Opcode::STOS |
        Opcode::INS |
        Opcode::OUTS => {
            // TODO: incomplete
            1
        }
        o @ Opcode::CPUID |
        o @ Opcode::WBINVD |
        o @ Opcode::INVD |
        o @ Opcode::SYSRET |
        o @ Opcode::CLTS |
        o @ Opcode::SYSCALL |
        o => {
            event!(Level::ERROR, opcode = ?o, "missing operand information");
            0
        }
    }
}
#[test]
fn test_xor_locations() {
    use yaxpeax_arch::{Arch, Decoder};
    let inst = <yaxpeax_x86::x86_64 as Arch>::Decoder::default().decode([0x33u8, 0xc1].iter().map(|x| *x)).unwrap();
    use data::LocIterator;
//    let locs: Vec<(Option<Location>, Direction)> = inst.iter_locs(&mut NoDisambiguation::default()).collect();
//    panic!("{:?}", locs);
}

impl <'a, 'b, 'c, D: Disambiguator<yaxpeax_x86::x86_64, (<yaxpeax_x86::x86_64 as yaxpeax_arch::Arch>::Address, u8, u8)>, F: FunctionQuery<<yaxpeax_x86::x86_64 as yaxpeax_arch::Arch>::Address>> Iterator for LocationIter<'a, 'b, 'c, D, F> {
    type Item = (Option<Location>, Direction);
    fn next(&mut self) -> Option<Self::Item> {
        fn next_loc<'a, 'b, 'c, D: Disambiguator<yaxpeax_x86::x86_64, (<yaxpeax_x86::x86_64 as yaxpeax_arch::Arch>::Address, u8, u8)>, F: FunctionQuery<<yaxpeax_x86::x86_64 as yaxpeax_arch::Arch>::Address>>(iter: &mut LocationIter<'a, 'b, 'c, D, F>) -> Option<(Option<Location>, Direction)> {
            while iter.loc_count == iter.loc_idx {
                // advance op
                iter.op_idx += 1;

                if iter.op_idx >= iter.op_count {
                    // but we're at the last op, so we're actually done...
                    return None;
                }
    //            println!("opc: {}", iter.inst.opcode());

                let op = iter.inst.operand(iter.op_idx - 1);
                let op_use = use_of(iter.inst, iter.op_idx - 1);
                iter.loc_count = locations_in(&op, op_use);
                iter.loc_idx = 0;
                iter.curr_op = Some(op);
                iter.curr_use = Some(op_use);
            }

            if iter.op_idx == 0 {
                iter.loc_idx += 1;
                return Some(implicit_loc(iter.inst.opcode(), iter.loc_idx - 1));
            }

            if let Some(op) = &iter.curr_op {
                iter.loc_idx += 1;
                loc_by_id(iter.loc_idx - 1, iter.curr_use.unwrap(), op)
            } else {
                unreachable!()
            }
        }

        next_loc(self).map(|loc| {
            let loc_spec = (self.op_idx, self.loc_idx - 1);
            self.disambiguator.disambiguate(self.inst, loc.clone(), (self.addr, loc_spec.0, loc_spec.1)).map(|new_loc| (Some(new_loc), loc.1)).unwrap_or(loc)
        })
    }
}

fn loc_by_id(idx: u8, usage: Use, op: &Operand) -> Option<(Option<Location>, Direction)> {
    match op {
        Operand::Register(spec) => {
            if idx == 0 {
                Some((Some(Location::Register(*spec)), usage.first_use()))
            } else {
                Some((Some(Location::Register(*spec)), Direction::Write))
            }
        },
        Operand::DisplacementU32(_) |
        Operand::DisplacementU64(_) => {
            if idx == 0 {
                Some((Some(Location::Memory(ANY)), usage.first_use()))
            } else {
                Some((Some(Location::Memory(ANY)), Direction::Write))
            }
        },
        Operand::RegDeref(spec) |
        Operand::RegDisp(spec, _) |
        Operand::RegScale(spec, _) |
        Operand::RegScaleDisp(spec, _, _) => {
            match idx {
                0 => {
                    Some((Some(Location::Register(*spec)), Direction::Read))
                },
                1 => {
                    Some((Some(Location::Memory(ANY)), usage.first_use()))
                },
                2 => {
                    Some((Some(Location::Memory(ANY)), Direction::Write))
                }
                _ => {
                    unreachable!();
                }
            }
        },
        Operand::RegIndexBase(base, index) |
        Operand::RegIndexBaseDisp(base, index, _) |
        Operand::RegIndexBaseScale(base, index, _) |
        Operand::RegIndexBaseScaleDisp(base, index, _, _) => {
            match idx {
                0 => {
                    Some((Some(Location::Register(*base)), Direction::Read))
                },
                1 => {
                    Some((Some(Location::Register(*index)), Direction::Read))
                },
                2 => {
                    Some((Some(Location::Memory(ANY)), usage.first_use()))
                },
                3 => {
                    Some((Some(Location::Memory(ANY)), Direction::Write))
                }
                _ => {
                    unreachable!()
                }
            }
        },
        Operand::Nothing |
        Operand::ImmediateI8(_) |
        Operand::ImmediateI16(_) |
        Operand::ImmediateI32(_) |
        Operand::ImmediateI64(_) |
        Operand::ImmediateU8(_) |
        Operand::ImmediateU16(_) |
        Operand::ImmediateU32(_) |
        Operand::ImmediateU64(_) => {
            None
        }
    }
}

impl <
    'a,
    'disambiguator,
    'fns,
    D: 'disambiguator + Disambiguator<yaxpeax_x86::x86_64, (<yaxpeax_x86::x86_64 as yaxpeax_arch::Arch>::Address, u8, u8)>,
    F: 'fns + FunctionQuery<<yaxpeax_x86::x86_64 as yaxpeax_arch::Arch>::Address,
    Function=FunctionImpl<Location>>
> crate::data::LocIterator<'disambiguator, 'fns, yaxpeax_x86::x86_64, Location, D, F> for &'a Instruction {
    type Item = (Option<Location>, Direction);
    type LocSpec = (<yaxpeax_x86::x86_64 as yaxpeax_arch::Arch>::Address, u8, u8);
    type Iter = LocationIter<'a, 'disambiguator, 'fns, D, F>;
    fn iter_locs(self, addr: <yaxpeax_x86::x86_64 as yaxpeax_arch::Arch>::Address, disam: &'disambiguator D, functions: &'fns F) -> Self::Iter {
        LocationIter::new(addr, self, disam, functions)
    }
}
