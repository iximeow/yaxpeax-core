use nix::sys::ptrace;
use nix::libc::c_void;
// Need to ptrace directly because nix doesn't have nice wrappers for some bits i want
#[allow(deprecated)]
use nix::sys::ptrace::ptrace;
use nix::sys::ptrace::Request;
use nix::sys::wait::WaitStatus;
use proc_maps::{get_process_maps, MapRange};
use debug::{DebugTarget, RunResult, Peek};

use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use std::io;

use yaxpeax_arch::Arch;
use analyses::control_flow;
use analyses::static_single_assignment::SSA;
use analyses::xrefs;

use std::collections::HashMap;
use yaxpeax_x86::{x86_64, Opcode, Operand};
use std::rc::Rc;
use std::cell::RefCell;
use num_traits::Zero;

use arch::{BaseUpdate, Function, Symbol, SymbolQuery, Library};

use ContextRead;
use ContextWrite;

pub mod analyses;
pub mod cpu;
pub mod display;

#[derive(Serialize)]
#[allow(non_camel_case_types)]
pub struct x86_64Data {
    pub preferred_addr: <x86_64 as Arch>::Address,
    pub contexts: MergedContextTable,
    pub cfg: control_flow::ControlFlowGraph<<x86_64 as Arch>::Address>,
    pub functions: HashMap<<x86_64 as Arch>::Address, Function>
}

impl SymbolQuery<<x86_64 as Arch>::Address> for x86_64Data {
    fn symbol_for(&self, addr: <x86_64 as Arch>::Address) -> Option<&Symbol> {
        self.contexts.symbols.get(&addr)
    }
    fn symbol_addr(&self, sym: &Symbol) -> Option<<x86_64 as Arch>::Address> {
        for (k, v) in self.contexts.symbols.iter() {
            if v == sym {
                return Some(*k);
            }
        }

        None
    }
}

impl Default for x86_64Data {
    fn default() -> Self {
        x86_64Data {
            preferred_addr: <x86_64 as Arch>::Address::zero(),
            contexts: MergedContextTable::create_empty(),
            cfg: control_flow::ControlFlowGraph::new(),
            functions: HashMap::new()
        }
    }
}

#[derive(Serialize)]
pub struct MergedContextTable {
    pub user_contexts: HashMap<<x86_64 as Arch>::Address, Rc<()>>,
    pub computed_contexts: HashMap<<x86_64 as Arch>::Address, Rc<()>>,
    pub xrefs: xrefs::XRefCollection<<x86_64 as Arch>::Address>,
    pub symbols: HashMap<<x86_64 as Arch>::Address, Symbol>,
    #[serde(skip)]
    pub reverse_symbols: HashMap<Symbol, <x86_64 as Arch>::Address>,
    pub functions: HashMap<<x86_64 as Arch>::Address, Function>,
    pub function_hints: Vec<<x86_64 as Arch>::Address>,
    pub ssa: HashMap<
        <x86_64 as Arch>::Address,
        (control_flow::ControlFlowGraph<<x86_64 as Arch>::Address>, SSA<x86_64>)
    >
}

#[derive(Debug)]
pub struct MergedContext {
    pub computed: Option<Rc<()>>,
    pub user: Option<Rc<()>>
}

impl Default for MergedContextTable {
    fn default() -> Self {
        MergedContextTable::create_empty()
    }
}

impl MergedContextTable {
    pub fn create_empty() -> MergedContextTable {
        MergedContextTable {
            user_contexts: HashMap::new(),
            computed_contexts: HashMap::new(),
            xrefs: xrefs::XRefCollection::new(),
            functions: HashMap::new(),
            function_hints: Vec::new(),
            symbols: HashMap::new(),
            reverse_symbols: HashMap::new(),
            ssa: HashMap::new()
        }
    }
}

/*
pub enum Value {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u64, u64),
    U256(u64, u64, u64, u64),
    U512(u64, u64, u64, u64, u64, u64, u64, u64)
}

trait PartialInstructionContext {
    pub fn reg_value(reg: RegSpec) -> Option<Value>;
    pub fn mem_value(addr: u64, width: u8) -> Option<Value>;
}
*/

pub type Update = BaseUpdate<x86Update>;

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum x86Update {
    AddXRef(xrefs::RefType, xrefs::RefAction, <x86_64 as Arch>::Address),
    RemoveXRef(xrefs::RefType, xrefs::RefAction, <x86_64 as Arch>::Address),
    FunctionHint
}

impl ContextRead<x86_64, MergedContext> for MergedContextTable {
    fn at(&self, address: &<x86_64 as Arch>::Address) -> MergedContext {
        MergedContext {
            user: self.user_contexts.get(address).map(|v| Rc::clone(v)),
            computed: self.computed_contexts.get(address).map(|v| Rc::clone(v))
        }
    }
}

impl ContextWrite<x86_64, Update> for MergedContextTable {
    fn put(&mut self, address: <x86_64 as Arch>::Address, update: Update) {
        // println!("Applying update: {} -> {:?}", address.stringy(), update);
        match update {
            BaseUpdate::Specialized(x86Update::FunctionHint) => {
                if !self.function_hints.contains(&address) && !self.functions.contains_key(&address) {
//                    println!("Function hint: {}", address.stringy());
                    self.function_hints.push(address)
                }
            },
            BaseUpdate::Specialized(x86Update::AddXRef(tpe, action, dest)) => {
                // TODO: xrefs from non-code sources
                self.xrefs.insert_from_code(tpe, action, address, dest);
            },
            BaseUpdate::Specialized(x86Update::RemoveXRef(tpe, action, dest)) => {
                self.xrefs.delete_from_code(tpe, action, address, dest);
            }
            BaseUpdate::DefineSymbol(sym) => {
                //println!("address of {:?} recorded at {}", sym, address.stringy());
                match Symbol::to_function(&sym) {
                    Some(f) => {
                        self.functions.insert(address, f);
                    }
                    None => { }
                }
                self.symbols.insert(address, sym.clone());
                self.reverse_symbols.insert(sym, address);
            }
            BaseUpdate::DefineFunction(f) => {
                self.symbols.insert(address, Symbol(Library::This, f.name.clone()));
                self.reverse_symbols.insert(Symbol(Library::This, f.name.clone()), address);
                self.functions.insert(address, f.clone());
            }
            _ => { /* todo: the rest */ }
        }
    }
}

pub struct ProcessX86_64 {
    pid: nix::unistd::Pid
}

#[repr(C)]
#[derive(Debug)]
pub struct Regs {
    r15: u64,
    r14: u64,
    r13: u64,
    r12: u64,
    pub rbp: u64,
    pub rbx: u64,
    r11: u64,
    r10: u64,
    r9: u64,
    r8: u64,
    pub rax: u64,
    pub rcx: u64,
    pub rdx: u64,
    pub rsi: u64,
    pub rdi: u64,
    orig_rax: u64,
    pub rip: u64,
    cs: u64,
    eflags: u64,
    pub rsp: u64,
    ss: u64,
    fs_base: u64,
    gs_base: u64,
    ds: u64,
    es: u64,
    fs: u64,
    gs: u64
}

impl ProcessX86_64 {
    pub fn new(pid: nix::unistd::Pid) -> ProcessX86_64 {
        ProcessX86_64 {
            pid: pid
        }
    }

    /// Returns a list of process ids corresponding to threads under this debug target
    pub fn threads(&self) -> std::io::Result<Vec<i32>> {
        let mut thread_ids: Vec<i32> = vec![];
        let proc_dir = std::fs::read_dir(std::path::Path::new(&format!("/proc/{}/task", self.pid)))?;
        for dir in proc_dir {
            let thread_id = dir?.file_name();
            match thread_id.into_string() {
                Ok(s) => {
                    let parsed_thread_id = s.parse();
                    match parsed_thread_id {
                        Ok(tid) => {
                            thread_ids.push(tid);
                        },
                        Err(_) => {
                            panic!("Junk data in thread id {} for pid {}", s, self.pid);
                        }
                    }
                },
                Err(_) => {
                    panic!("Junk data in thread id path for pid {}", self.pid);
                }
            }
        }
        Ok(thread_ids)
    }

    pub fn mem_maps(&mut self) -> Vec<MapRange> {
        get_process_maps(i32::from(self.pid) as proc_maps::Pid).unwrap()
    }

    pub fn module_for(&mut self, address: usize) -> Option<String> {
        for map in self.mem_maps() {
            if address >= map.start() && address < (map.start() + map.size()) {
                return map.filename().to_owned();
            }
        }

        None
    }

    pub fn kill(&self, signal: nix::sys::signal::Signal) -> Result<(), String> {
        match nix::sys::signal::kill(self.pid, Some(signal)) {
            Ok(()) => Ok(()),
            Err(e) => Err(format!("{:?}", e))
        }
    }

    pub fn memory_view(&self) -> io::Result<ProcessMemory> {
        let f = File::open(format!("/proc/{}/mem", self.pid))?;
        Ok(ProcessMemory {
            pid: self.pid.clone(),
            mem_handle: f
        })
    }
}

pub struct ProcessMemory {
    pid: nix::unistd::Pid,
    mem_handle: File
}

impl ProcessMemory {
    pub fn find(&mut self, pattern: &[u8]) -> Vec<u64> {
        let mut result: Vec<u64> = Vec::new();
        for m in ProcessX86_64::new(self.pid.clone()).mem_maps() {
            println!("checking range between {:#016x} and {:#016x}",
                 m.start(),
                 m.start() +  m.size()
            );
            self.mem_handle.seek(
                SeekFrom::Start(m.start() as u64)
            ).unwrap();

            let mut data = vec![0; m.size()];

            self.mem_handle.read(&mut data).unwrap();

            for i in 0..data.len() {
                if m.size() - i < pattern.len() {
                    break;
                }
                if &data[i..(i + pattern.len())] == pattern {
                    result.push(i as u64 + m.start() as u64);
                }
            }
        }

        result
    }

    pub fn get_bytes(&mut self, at: u64, size: u64) -> Vec<u8> {
        self.mem_handle.seek(
            SeekFrom::Start(at)
        ).unwrap();

        let mut data = vec![0; size as usize];

        self.mem_handle.read(&mut data).unwrap();

        data
    }
}

#[derive(Debug)]
pub struct DebugeeX86_64 {
    pub pid: nix::unistd::Pid,
    pub pending_signal: Option<(bool, nix::sys::signal::Signal)>
}

impl Peek for Rc<RefCell<DebugeeX86_64>> {
    fn read<A: yaxpeax_arch::Address>(&self, addr: A) -> Option<u8> {
        Some(self.borrow_mut().read_qword(addr.to_linear()) as u8)
    }
    fn read16<A: yaxpeax_arch::Address>(&self, addr: A) -> Option<[u8; 2]> {
        let word: u64 = self.borrow_mut().read_qword(addr.to_linear());
        Some(
            [
                ((word >> 0) & 0xff) as u8,
                ((word >> 8) & 0xff) as u8,
            ]
        )
    }
    fn read32<A: yaxpeax_arch::Address>(&self, addr: A) -> Option<[u8; 4]> {
        let word: u64 = self.borrow_mut().read_qword(addr.to_linear());
        Some(
            [
                ((word >> 0) & 0xff) as u8,
                ((word >> 8) & 0xff) as u8,
                ((word >> 16) & 0xff) as u8,
                ((word >> 24) & 0xff) as u8,
            ]
        )
    }
    fn read64<A: yaxpeax_arch::Address>(&self, addr: A) -> Option<[u8; 8]> {
        let word: u64 = self.borrow_mut().read_qword(addr.to_linear());
        Some(
            [
                ((word >> 0) & 0xff) as u8,
                ((word >> 8) & 0xff) as u8,
                ((word >> 16) & 0xff) as u8,
                ((word >> 24) & 0xff) as u8,
                ((word >> 32) & 0xff) as u8,
                ((word >> 40) & 0xff) as u8,
                ((word >> 48) & 0xff) as u8,
                ((word >> 56) & 0xff) as u8
            ]
        )
    }
}

impl DebugeeX86_64 {
    pub fn regs(&mut self) -> Result<Regs, String> {
        unsafe {
            let mut regs: Regs = std::mem::uninitialized();
            // should just submit a PR to wrap PTRACE_GETREGS, really
            #[allow(deprecated)]
            match ptrace(
                Request::PTRACE_GETREGS,
                self.pid,
                std::ptr::null_mut(),
                &mut regs as *mut _ as *mut c_void
            ) {
                Ok(_) => {
                    Ok(regs)
                },
                Err(e) => { panic!("{:?}", e); }
            }
        }
    }

    pub fn read_qword(&self, ptr: usize) -> u64 {
        ptrace::read(self.pid, ptr as *mut c_void).unwrap() as u64
    }

    pub fn bytes_at_rip(&mut self) -> [u8; 16] {
        let rip = self.regs().unwrap().rip;
        let low: u64 = ptrace::read(self.pid, rip as *mut c_void).unwrap() as u64;
        let high: u64 = ptrace::read(self.pid, (rip + 8) as *mut c_void).unwrap() as u64;

        [
            (low & 0xff) as u8, ((low >> 8) & 0xff) as u8, ((low >> 16) & 0xff) as u8, ((low >> 24) & 0xff) as u8,
            ((low >> 32) & 0xff) as u8, ((low >> 40) & 0xff) as u8, ((low >> 48) & 0xff) as u8, ((low >> 56) & 0xff) as u8,
            (high & 0xff) as u8, ((high >> 8) & 0xff) as u8, ((high >> 16) & 0xff) as u8, ((high >> 24) & 0xff) as u8,
            ((high >> 32) & 0xff) as u8, ((high >> 40) & 0xff) as u8, ((high >> 48) & 0xff) as u8, ((high >> 56) & 0xff) as u8
        ]
    }
//}
//
//impl Drop for DebugeeX86_64 {
    pub fn detach(&mut self) {
        ptrace::detach(self.pid).expect("well this isn't good");
    }

    fn wait(&mut self) -> Result<(), String> {
        match nix::sys::wait::waitpid(self.pid, None) {
            Ok(WaitStatus::Signaled(_, signal, _core_dumped)) => {
                panic!("The thing stopped. :(\n{:?}", signal);
            },
            Ok(WaitStatus::Stopped(_, signal)) => {
                println!("Signalled: {:?}", signal);
                self.pending_signal = Some((false, signal));
                Ok(())
            },
            Ok(WaitStatus::StillAlive) => {
                // no changes, life goes on
                Ok(())
            }
            Ok(WaitStatus::Exited(_, code)) => {
                panic!("Debugee exited with code {:?}", code);
            },
            Err(nix::Error::Sys(err)) => {
                panic!("waitpid error: {:?}", err);
            },
            Err(nix::Error::InvalidPath)
                | Err(nix::Error::InvalidUtf8)
                | Err(nix::Error::UnsupportedOperation) => {
                unreachable!();
            },
            x => {
                // TODO: uhhh hh h h   hh h
                panic!("Unhandled waitpid result: {:?}", x);
            }
        }
    }
}

impl <'a> DebugTarget<'a, ProcessX86_64> for DebugeeX86_64 {
    type WatchTarget = ();
    type BreakCondition = ();

    fn attach(process: &'a mut ProcessX86_64) -> Self {
        match ptrace::attach(process.pid) {
            Ok(_) => {
                let status = nix::sys::wait::waitpid(process.pid, None).unwrap();
                println!("Attached and receieved status {:?}", status);
                DebugeeX86_64 {
                    pid: process.pid,
                    pending_signal: None
                }
            },
            Err(e) => {
                panic!("{}", e);
            }
        }
    }

    fn single_step(&mut self) -> Result<(), String> {
        let signal = match self.pending_signal {
            Some((true, signal)) => Some(signal),
            _ => None
        };
        match ptrace::step(self.pid, signal) {
            Ok(()) => {
                // this immediately SIGTRAPs from the instruction
                self.wait()
            }
            Err(e) => {
                panic!("{}", e);
            }
        }
    }

    fn run(&mut self) -> RunResult {
        let signal = match self.pending_signal {
            Some((true, signal)) => Some(signal),
            _ => None
        };
        match ptrace::step(self.pid, signal) {
            Ok(()) => { RunResult::Ok },
            Err(e) => {
                panic!("{}", e);
            }
        }
    }

    fn add_watch(&mut self, _target: Self::WatchTarget) -> Result<(), String> {
        panic!("uhhh");
    }

    fn add_break_condition(&mut self, _target: Self::BreakCondition) -> Result<(), String> {
        panic!("UHHH");
    }
}

impl <T> control_flow::Determinant<T, <x86_64 as Arch>::Address> for yaxpeax_x86::Instruction {
    // TODO: this assumes that instructions won't fault
    // we really don't know that, but also no T provided here gives
    // context such that we can make that determination
    fn control_flow(&self, _ctx: Option<&T>) -> control_flow::Effect<<x86_64 as Arch>::Address> {
        match self.opcode {
            Opcode::XADD |
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
            Opcode::CVTSI2SS |
            Opcode::CVTSI2SD |
            Opcode::CVTTSD2SI |
            Opcode::CVTSD2SI |
            Opcode::CVTSD2SS |
            Opcode::LDDQU |
            Opcode::MOVSX_b |
            Opcode::MOVSX_w |
            Opcode::MOVZX_b |
            Opcode::MOVZX_w |
            Opcode::MOVSX |
            Opcode::MOVSXD |
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
            Opcode::PUSH |
            Opcode::POP |
            Opcode::LEA |
            Opcode::NOP |
            Opcode::XCHG |
            Opcode::POPF |
            Opcode::ADD |
            Opcode::ADC |
            Opcode::SUB |
            Opcode::ENTER |
            Opcode::LEAVE |
            Opcode::MOV |
            Opcode::PUSHF |
            Opcode::WAIT |
            Opcode::CBW |
            Opcode::CDW |
            Opcode::LAHF |
            Opcode::SAHF |
            Opcode::TEST |
            Opcode::CMP |
            Opcode::INS |
            Opcode::OUTS |
            Opcode::IMUL |
            Opcode::DIV |
            Opcode::IDIV |
            Opcode::MUL |
            Opcode::NEG |
            Opcode::NOT |
            Opcode::SGDT |
            Opcode::SIDT |
            Opcode::SMSW |
            Opcode::LGDT |
            Opcode::LIDT |
            Opcode::LMSW |
            Opcode::SWAPGS |
            Opcode::RDTSCP |
            Opcode::INVLPG |
            Opcode::WBINVD |
            Opcode::INVD |
            Opcode::CPUID |
            Opcode::LSL |
            Opcode::LAR |
            Opcode::CLTS |
            Opcode::SYSCALL |
            Opcode::FXSAVE |
            Opcode::FXRSTOR |
            Opcode::LDMXCSR |
            Opcode::STMXCSR |
            Opcode::XSAVE |
            Opcode::XSTOR |
            Opcode::XSAVEOPT |
            Opcode::LFENCE |
            Opcode::MFENCE |
            Opcode::SFENCE |
            Opcode::CLFLUSH |
            Opcode::SLDT |
            Opcode::STR |
            Opcode::LLDT |
            Opcode::LTR |
            Opcode::VERR |
            Opcode::VERW |
            Opcode::JMPE |
            Opcode::RDMSR |
            Opcode::WRMSR |
            Opcode::RDTSC |
            Opcode::RDPMC |
            Opcode::CLI |
            Opcode::STI |
            Opcode::CLC |
            Opcode::STC |
            Opcode::CLD |
            Opcode::STD |
            Opcode::BT |
            Opcode::BTS |
            Opcode::BTR |
            Opcode::BTC |
            Opcode::BSR |
            Opcode::BSF |
            Opcode::CMPXCHG => {
                control_flow::Effect::cont()
            },
            Opcode::CALLF => {
                // TODO: honestly not sure how to model callf
                control_flow::Effect::stop()
            },
            Opcode::CALL => {
                // TODO: this is where i ought to reference context
                // to determine that the called address begins a well-formed
                // region that may or may not let the caller consider "call"
                // a single non-effectual instruction w.r.t control flow
                let dest = match self.operands[0] {
                    Operand::ImmediateI8(i) => {
                        Some(control_flow::Target::Relative(i as i64 as u64))
                    },
                    Operand::ImmediateI16(i) => {
                        Some(control_flow::Target::Relative(i as i64 as u64))
                    },
                    Operand::ImmediateI32(i) => {
                        Some(control_flow::Target::Relative(i as i64 as u64))
                    },
                    Operand::ImmediateI64(i) => {
                        Some(control_flow::Target::Relative(i as i64 as u64))
                    },
                    _ => {
                        // TODO one day when ctx can let this reach ... the current
                        // exeuction context ... this may be able to be smarter
                        // (f.ex, if this jumps to a jump table, 
                        None
                    }
                };

                match dest {
                    Some(_dest) => {
                    //    control_flow::Effect::cont_and(dest)
                        control_flow::Effect::cont()
                    },
                    None => control_flow::Effect::cont()
                }

            }
            Opcode::JMP => {
                let dest = match self.operands[0] {
                    Operand::ImmediateI8(i) => {
                        Some(control_flow::Target::Relative(i as i64 as u64))
                    },
                    Operand::ImmediateI16(i) => {
                        Some(control_flow::Target::Relative(i as i64 as u64))
                    },
                    Operand::ImmediateI32(i) => {
                        Some(control_flow::Target::Relative(i as i64 as u64))
                    },
                    Operand::ImmediateI64(i) => {
                        Some(control_flow::Target::Relative(i as i64 as u64))
                    },
                    _ => {
                        // TODO one day when ctx can let this reach ... the current
                        // exeuction context ... this may be able to be smarter
                        // (f.ex, if this jumps to a jump table, 
                        None
                    }
                };

                match dest {
                    Some(dest) => {
                        control_flow::Effect::stop_and(dest)
                    },
                    None => control_flow::Effect::stop()
                }
            },
            Opcode::JMPF => {
                // TODO: ...
                control_flow::Effect::stop()
            },
            Opcode::INT |
            Opcode::INTO |
            Opcode::IRET |
            Opcode::RETF |
            Opcode::SYSRET |
            Opcode::RETURN => {
                control_flow::Effect::stop()
            }
            Opcode::HLT => {
                control_flow::Effect::stop()
            },
            Opcode::JO |
            Opcode::JNO |
            Opcode::JB |
            Opcode::JNB |
            Opcode::JZ |
            Opcode::JNZ |
            Opcode::JA |
            Opcode::JNA |
            Opcode::JS |
            Opcode::JNS |
            Opcode::JP |
            Opcode::JNP |
            Opcode::JL |
            Opcode::JGE |
            Opcode::JLE |
            Opcode::JG => {
                match self.operands[0] {
                    Operand::ImmediateI8(i) => {
                        control_flow::Effect::cont_and(
                            control_flow::Target::Relative(i as i64 as u64)
                        )
                    },
                    Operand::ImmediateI16(i) => {
                        control_flow::Effect::cont_and(
                            control_flow::Target::Relative(i as i64 as u64)
                        )
                    },
                    Operand::ImmediateI32(i) => {
                        control_flow::Effect::cont_and(
                            control_flow::Target::Relative(i as i64 as u64)
                        )
                    },
                    Operand::ImmediateI64(i) => {
                        control_flow::Effect::cont_and(
                            control_flow::Target::Relative(i as i64 as u64)
                        )
                    },
                    _ => {
                        unreachable!()
                    }
                }
            },

            Opcode::UD2 |
            Opcode::Invalid => {
                control_flow::Effect::stop()
            },

            Opcode::CMOVA |
            Opcode::CMOVB |
            Opcode::CMOVG |
            Opcode::CMOVGE |
            Opcode::CMOVL |
            Opcode::CMOVLE |
            Opcode::CMOVNA |
            Opcode::CMOVNB |
            Opcode::CMOVNO |
            Opcode::CMOVNP |
            Opcode::CMOVNS |
            Opcode::CMOVNZ |
            Opcode::CMOVO |
            Opcode::CMOVP |
            Opcode::CMOVS |
            Opcode::CMOVZ |
            Opcode::SETO |
            Opcode::SETNO |
            Opcode::SETB |
            Opcode::SETAE |
            Opcode::SETZ |
            Opcode::SETNZ |
            Opcode::SETBE |
            Opcode::SETA |
            Opcode::SETS |
            Opcode::SETNS |
            Opcode::SETP |
            Opcode::SETNP |
            Opcode::SETL |
            Opcode::SETGE |
            Opcode::SETLE |
            Opcode::SETG => {
                control_flow::Effect::cont()
            }

            // these have control flow dependent on the rep prefix
            Opcode::CMPS |
            Opcode::SCAS |
            Opcode::MOVS |
            Opcode::LODS |
            Opcode::STOS => {
                control_flow::Effect::cont()
            }
        }
    }
}
