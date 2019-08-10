use nix::sys::ptrace;
use nix::libc::c_void;
// Need to ptrace directly because nix doesn't have nice wrappers for some bits i want
#[allow(deprecated)]
use nix::sys::ptrace::ptrace;
use nix::sys::ptrace::Request;
use nix::sys::wait::WaitStatus;
use proc_maps::{get_process_maps, MapRange};
use debug::{DebugTarget, RunResult, Peek};

use std::rc::Rc;
use std::cell::RefCell;

use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use std::io;

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
