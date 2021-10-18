use arch::{Decoder, MCU};
use memory::{MemoryRange, MemoryRepr};
use debug;
use debug::DebugTarget;
use arch::msp430;
use yaxpeax_msp430::{MSP430, Operand};
use yaxpeax_arch::Arch;

pub struct MSP430DebugTarget<'a> {
    pub target: &'a mut msp430::cpu::CPU,
    break_conditions: Vec<BreakCondition>,
    watch_targets: Vec<WatchTarget>
}

impl <'a> MSP430DebugTarget<'a> {
    fn check_breakpoints(&self) -> bool {
        for bp in &self.break_conditions {
            match bp {
                BreakCondition::IPValue(ip) => {
                    if &self.target.ip() == ip { return true; }
                },
                BreakCondition::Other(f) => {
                    if f(&self.target) { return true; }
                },
                BreakCondition::MemoryAccess(_dest) => {
                    unimplemented!("memory access breakpoints not yet supported for MSP430");
                },
                BreakCondition::IO => {
                    unimplemented!("IO breakpoints not yet supported for MSP430");
                }
            }
        }
        false
    }
    fn show_watches(&self) {
        for watch in &self.watch_targets {
            tracing::info!("WATCH: {}", watch.reify(&self.target));
        }
    }
}

impl WatchTarget {
    /// Builds a pointer from the target of the watch.
    #[allow(dead_code)]
    fn pointee(&self, _cpu: &msp430::cpu::CPU) -> Option<u16> {
        match self {
            WatchTarget::Pointee(_target) => {
                unimplemented!("MSP430 watches not yet supported");
            },
            WatchTarget::MemoryLocation(_addr) => {
                unimplemented!("MSP430 watches not yet supported");
            }
        }
    }
    #[allow(dead_code)]
    fn reify(&self, _cpu: &msp430::cpu::CPU) -> String {
        match self {
            WatchTarget::Pointee(_target) => {
                unimplemented!("MSP430 watches not yet supported");
            }
            WatchTarget::MemoryLocation(_addr) => {
                unimplemented!("MSP430 watches not yet supported");
            },
        }
    }
}

trait InstructionContext {
//    fn debank(&self, banked: u8) -> u16;
}

impl InstructionContext for msp430::cpu::CPU {
}

trait Contextual<T> {
    fn contextualize(&self, _ctx: &T) -> String;
}

impl <T> Contextual<T> for yaxpeax_msp430::Instruction
    where T: InstructionContext
{
    fn contextualize(&self, ctx: &T) -> String {
        fn contextualize_op<T: InstructionContext>(_op: yaxpeax_msp430::Operand, _ctx: &T) -> String {
            unimplemented!("unimplemented");
        }

        let mut result = format!("{}", self.opcode);
        match self.operands[0] {
            Operand::Nothing => { return result; },
            x @ _ => {
                result = format!("{} {}", result, contextualize_op(x, ctx));
            }
        }
        match self.operands[1] {
            Operand::Nothing => { return result; },
            x @ _ => {
                result = format!("{}, {}", result, contextualize_op(x, ctx));
            }
        }
        return result;
    }
}

#[derive(Debug)]
pub enum WatchTarget {
    Pointee(Box<WatchTarget>),
    MemoryLocation(u16),
}
pub enum BreakCondition {
    IPValue(u16),
    Other(fn(&msp430::cpu::CPU) -> bool),
    MemoryAccess(u16),
    IO
}

impl <'a> DebugTarget<'a, msp430::cpu::CPU> for MSP430DebugTarget<'a> {
    type WatchTarget = WatchTarget;
    type BreakCondition = BreakCondition;
    fn attach(cpu: &'a mut msp430::cpu::CPU) -> Self {
        MSP430DebugTarget {
            target: cpu,
            break_conditions: vec![],
            watch_targets: vec![]
        }
    }
    fn single_step(&mut self) -> Result<(), String> {
        self.show_watches();
        self.target.emulate()
    }
    fn run(&mut self) -> debug::RunResult {
        println!("Running...");
        match self.target.emulate() {
            Ok(()) => { },
            Err(msg) => {
                return debug::RunResult::ExecutionError(msg);
            }
        }
        loop {
            if self.check_breakpoints() {
                return debug::RunResult::HitBreakCondition;
            }
            match self.target.emulate() {
                Ok(()) => { },
                Err(msg) => {
                    return debug::RunResult::ExecutionError(msg);
                }
            }
        }
    }
    fn add_watch(&mut self, watch: WatchTarget) -> Result<(), String> {
        self.watch_targets.push(watch);
        Ok(())
    }
    fn add_break_condition(&mut self, break_cond: Self::BreakCondition) -> Result<(), String> {
        self.break_conditions.push(break_cond);
        Ok(())
    }
}

#[allow(dead_code)]
enum IOCause {
    UART,
    PORT
}

#[allow(non_snake_case)]
#[derive(Debug)]
pub struct CPU {
    pub registers: [u16; 16],
    pub memory: Vec<u8>,
    disable: bool
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            registers: [0u16; 16],
            memory: vec![0; 0x10000],
            disable: false
        }
    }
    pub fn ip(&self) -> u16 {
        self.registers[0]
    }
    pub fn set_ip(&mut self, newval: u16) {
        self.registers[0] = newval;
    }
    #[allow(dead_code)]
    fn push(&mut self, _value: u32) -> Result<(), String> {
        unimplemented!("msp430 push");
    }
    #[allow(dead_code)]
    fn pop(&mut self) -> Result<u32, String> {
        unimplemented!("msp430 pop");
    }

    /*
    #[allow(non_snake_case)]
    fn would_IO(&self) -> Option<IOCause> {
    }
    */
    pub fn get_byte(&mut self, addr: u16) -> Result<u8, String> {
        self.get_byte_noupdate(addr)
    }
    pub fn get_byte_noupdate(&self, _addr: u16) -> Result<u8, String> {
        unimplemented!("msp430 memory access");
    }
    pub fn set_byte_noupdate(&mut self, _addr: u16, _what: u8) -> Result<(), String> {
        unimplemented!("msp430 memory access");
    }
    pub fn set_byte(&mut self, addr: u16, what: u8) -> Result<(), String> {
        self.set_byte_noupdate(addr, what)
    }
    pub fn describe(&self) {
        println!("msp430: ");
        println!("ip=0x{:x}", self.ip());
        match self.decode() {
            Ok(instr) => println!("instruction: {}", instr.contextualize(self)),
            Err(e) => println!("[invalid: {}]", e)
        };
    }
    pub fn program<T: MemoryRepr<MSP430>>(&mut self, program: Option<T>) -> Result<(), String> {
        match program.and_then(|x| x.as_flat()) {
            Some(flat) => {
                let data = flat.data();
                if data.len() > self.memory.len() {
                    return Err(
                        format!(
                            "Data is larger than the chip: 0x{:x} bytes of memory but 0x{:x} available",
                            data.len(),
                            self.memory.len()
                        )
                    );
                }
                tracing::debug!("writing 0x{:x} bytes of program...", data.len());
                self.memory[0..data.len()].copy_from_slice(data);
            },
            None => {
                tracing::warn!("provided program includes no code.");
            }
        };

        let initial_ip = ((self.memory[0xffff] as u16) << 8) | (self.memory[0xfffe] as u16);
        if initial_ip == 0xffff {
            self.disable = true;
        } else {
            self.set_ip(initial_ip);
        }

        Ok(())
    }
}

impl MCU for CPU {
    type Addr = u16;
    type Instruction = <MSP430 as Arch>::Instruction;
    fn emulate(&mut self) -> Result<(), String> {
        if self.disable {
            return Ok(());
        }

        match self.decode() {
            Ok(_instr) => {
                unimplemented!("msp430 emulation not yet supported");
            },
            Err(msg) => { std::panic::panic_any(msg); }
        };
    }

    fn decode(&self) -> Result<Self::Instruction, String> {
        let cursor: crate::memory::repr::cursor::ReadCursor<MSP430, Vec<u8>> = self.memory.range_from(self.ip()).unwrap();
        <MSP430 as Arch>::Decoder::default().decode(&mut cursor.to_reader())
            .map_err(|_| {
                format!(
                    "Unable to decode bytes at 0x{:x}: {:x?}",
                    self.ip(),
                    self.memory[(self.ip() as usize)..((self.ip() + 4) as usize)].iter().collect::<Vec<&u8>>()
                )
            })
    }
}
