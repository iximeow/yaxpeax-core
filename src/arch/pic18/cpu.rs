use arch;
use arch::MCU;
use yaxpeax_arch::{Arch, AddressBase, AddressDiff, Decoder, LengthedInstruction};
use memory::{MemoryRepr, MemoryRange};
use memory::repr::FlatMemoryRepr;
use debug;
use debug::DebugTarget;
use arch::pic18;
use yaxpeax_pic18::consts::SFRS;
use yaxpeax_pic18::{PIC18, Operand, Opcode};

pub struct PIC18DebugTarget<'a> {
    pub target: &'a mut pic18::cpu::CPU,
    break_conditions: Vec<BreakCondition>,
    watch_targets: Vec<WatchTarget>
}

impl <'a> PIC18DebugTarget<'a> {
    fn check_breakpoints(&self) -> bool {
        for bp in &self.break_conditions {
            match bp {
                BreakCondition::IPValue(ip) => {
                    if &self.target.ip == ip { return true; }
                },
                BreakCondition::Other(f) => {
                    if f(&self.target) { return true; }
                }
            }
        }
        false
    }
    fn show_watches(&self) {
        for watch in &self.watch_targets {
            println!("WATCH: {}", watch.reify(&self.target));
        }
    }
}

impl WatchTarget {
    /// Builds a pointer from the target of the watch.
    fn pointee(&self, cpu: &arch::pic18::cpu::CPU) -> Option<u16> {
        match self {
            WatchTarget::Pointee(target) => {
                target.pointee(cpu).and_then(|value| {
                    if value > cpu.memory.len() as u16 - 2 {
                        return None
                    };

                    Some(cpu.get_byte_noupdate(value as u32).unwrap() as u16 |
                        ((cpu.get_byte_noupdate(value as u32 + 1).unwrap() as u16) << 8))
                })
            },
            WatchTarget::MemoryLocation(addr) => {
                cpu.get_byte_noupdate(*addr as u32).and_then(|low| {
                    cpu.get_byte_noupdate(*addr as u32 + 1).map(|high| {
                        ((high as u16) << 8) | (low as u16)
                    })
                }).ok()
            },
            WatchTarget::W => Some(((cpu.bank() as u16) << 8) | cpu.W as u16)
        }
    }
    fn reify(&self, cpu: &arch::pic18::cpu::CPU) -> String {
        match self {
            WatchTarget::Pointee(target) => {
                match target.pointee(cpu) {
                    Some(pointee) => {
                        format!("[{}]: 0x{:x}", target.reify(cpu), pointee)
                    }
                    None => {
                        format!("[{}]: invalid", target.reify(cpu))
                    }
                }
            }
            WatchTarget::MemoryLocation(addr) => {
                format!("[{}]: 0x{:x}", yaxpeax_pic18::consts::named_file(*addr), cpu.get_byte_noupdate(*addr as u32).unwrap())
            },
            WatchTarget::W => {
                format!("W: 0x{:x}", cpu.W)
            }
        }
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum WatchTarget {
    Pointee(Box<WatchTarget>),
    MemoryLocation(u16),
    W
}
pub enum BreakCondition {
    IPValue(u32),
    Other(fn(&arch::pic18::cpu::CPU) -> bool)
}

impl <'a> DebugTarget<'a, pic18::cpu::CPU> for PIC18DebugTarget<'a> {
    type WatchTarget = WatchTarget;
    type BreakCondition = BreakCondition;
    fn attach(cpu: &'a mut pic18::cpu::CPU) -> Self {
        PIC18DebugTarget {
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

#[allow(non_camel_case_types)]
#[derive(Debug)]
enum EECON_STATE {
    Default,
    Prep1,
    Ready
}

#[allow(non_snake_case)]
#[derive(Debug)]
pub struct CPU {
    W: u8,
    pub ip: u32, // might be limited to u18?
    psr_z: bool,
    psr_c: bool,
    psr_v: bool,
    psr_n: bool,
    stack: [u32; 31],
    holding_registers: [u8; 32],
    eecon_state: EECON_STATE,
    pub program: Vec<u8>,
    pub memory: Vec<u8>,
    sfr_start: u16,
    sfr_end: u16,
    sfrs: Vec<u8>
}

impl CPU {
    pub fn new(progsize: u32, memsize: u32) -> Self {
        CPU {
            W: 0,
            ip: 0,
            psr_z: false,
            psr_c: false,
            psr_v: false,
            psr_n: false,
            stack: [0u32; 31],
            holding_registers: [0u8; 32],
            eecon_state: EECON_STATE::Default,
            program: vec![0; progsize as usize],
            memory: vec![0; memsize as usize],
            sfr_start: 0xf60,
            sfr_end: 0x1000,
            sfrs: vec![0; 0x1000 - 0xf60]
        }
    }
    fn push(&mut self, value: u32) -> Result<(), String> {
        let ptr = self.sfrs[SFRS::STKPTR as usize];
        if ptr >= 31 {
            return Err("Stack overflow".to_owned());
        }

        self.stack[ptr as usize] = value;

        self.sfrs[SFRS::STKPTR as usize] = ptr + 1;
        Ok(())
    }
    fn pop(&mut self) -> Result<u32, String> {
        let mut ptr = self.sfrs[SFRS::STKPTR as usize];
        if ptr == 0 {
            return Err("Stack underflow".to_owned());
        }

        ptr -= 1;

        let result = self.stack[ptr as usize];

        self.sfrs[SFRS::STKPTR as usize] = ptr;
        Ok(result)
    }
    fn set_bank(&mut self, value: u8) {
        self.sfrs[(0xfe0 - self.sfr_start) as usize] = value; // TODO this should be configured somehow...
    }
    fn bank(&self) -> u8 {
        self.sfrs[(0xfe0 - self.sfr_start) as usize] // TODO this should be configured somehow...
    }
    fn get_fsr(&self, fsr: u8) -> u16 {
        match fsr {
            0 => {
                self.sfrs[SFRS::FSR0L as usize] as u16 |
                    ((self.sfrs[SFRS::FSR0H as usize] as u16) << 8)
            },
            1 => {
                self.sfrs[SFRS::FSR1L as usize] as u16 |
                    ((self.sfrs[SFRS::FSR1H as usize] as u16) << 8)
            },
            2 => {
                self.sfrs[SFRS::FSR2L as usize] as u16 |
                    ((self.sfrs[SFRS::FSR2H as usize] as u16) << 8)
            },
            _ => { unreachable!(); }
        }
    }
    fn set_fsr(&mut self, fsr: u8, value: u16) {
         match fsr {
            0 => {
                self.sfrs[SFRS::FSR0L as usize] = value as u8;
                self.sfrs[SFRS::FSR0H as usize] = (value >> 8) as u8;
            },
            1 => {
                self.sfrs[SFRS::FSR1L as usize] = value as u8;
                self.sfrs[SFRS::FSR1H as usize] = (value >> 8) as u8;
            },
            2 => {
                self.sfrs[SFRS::FSR2L as usize] = value as u8;
                self.sfrs[SFRS::FSR2H as usize] = (value >> 8) as u8;
            },
            _ => { unreachable!(); }
        }
    }
    fn debank(&self, f: u8, banked: bool) -> u16 {
        if banked {
            (f as u16) | ((self.bank() as u16) << 8)
        } else {
            if f < 0x80 {
                f as u16
            } else {
                (f as u16) | 0xf00u16
            }
        }
    }
    fn is_sfr_addr(&self, addr: u32) -> bool {
        if addr > 0xffff {
            return false;
        } else {
            return (addr as u16) >= self.sfr_start && (addr as u16) < self.sfr_end;
        }
    }
    pub fn get_byte(&mut self, addr: u32) -> Result<u8, String> {
        if self.is_sfr_addr(addr) {
            let sfr_addr = (addr as u16) - self.sfr_start;
            match sfr_addr {
                SFRS::PREINC2 => {
                    let real_addr = self.get_fsr(2).wrapping_add(1);
                    self.set_fsr(2, real_addr);
                    self.get_byte_noupdate(addr)
                },
                SFRS::POSTINC2 => {
                    let real_addr = self.get_fsr(2).wrapping_add(1);
                    let value = self.get_byte_noupdate(addr);
                    self.set_fsr(2, real_addr);
                    value
                },
                SFRS::POSTDEC2 => {
                    let real_addr = self.get_fsr(2).wrapping_sub(1);
                    let value = self.get_byte_noupdate(addr);
                    self.set_fsr(2, real_addr);
                    value
                },
                SFRS::PREINC1 => {
                    let real_addr = self.get_fsr(1).wrapping_add(1);
                    self.set_fsr(1, real_addr);
                    self.get_byte_noupdate(addr)
                },
                SFRS::POSTINC1 => {
                    let real_addr = self.get_fsr(1).wrapping_add(1);
                    let value = self.get_byte_noupdate(addr);
                    self.set_fsr(1, real_addr);
                    value
                },
                SFRS::POSTDEC1 => {
                    let real_addr = self.get_fsr(1).wrapping_sub(1);
                    let value = self.get_byte_noupdate(addr);
                    self.set_fsr(1, real_addr);
                    value
                },
                SFRS::PREINC0 => {
                    let real_addr = self.get_fsr(0).wrapping_add(1);
                    self.set_fsr(0, real_addr);
                    self.get_byte_noupdate(addr)
                },
                SFRS::POSTINC0 => {
                    let real_addr = self.get_fsr(0).wrapping_add(1);
                    let value = self.get_byte_noupdate(addr);
                    self.set_fsr(0, real_addr);
                    value
                },
                SFRS::POSTDEC0 => {
                    let real_addr = self.get_fsr(0).wrapping_sub(1);
                    let value = self.get_byte_noupdate(addr);
                    self.set_fsr(0, real_addr);
                    value
                },
                _ => {
                    self.get_byte_noupdate(addr)
                }
            }
        } else {
            self.get_byte_noupdate(addr)
        }
    }
    pub fn get_byte_noupdate(&self, addr: u32) -> Result<u8, String> {
        if self.is_sfr_addr(addr) {
            let sfr_addr = (addr as u16) - self.sfr_start;
            let value = match sfr_addr {
                /* indf0 */
                SFRS::PLUSW0 => {
                    let real_addr = self.get_fsr(0)
                        .wrapping_add(self.W as i8 as i16 as u16);
                    self.memory[real_addr as usize]
                },
                SFRS::PREINC0 |
                SFRS::POSTINC0 |
                SFRS::POSTDEC0 |
                SFRS::INDF0 => {
                    let real_addr = self.get_fsr(0);
                    self.memory[real_addr as usize]
                },
                SFRS::PLUSW1 => {
                    let real_addr = self.get_fsr(1)
                        .wrapping_add(self.W as i8 as i16 as u16);
                    self.memory[real_addr as usize]
                },
                SFRS::PREINC1 |
                SFRS::POSTINC1 |
                SFRS::POSTDEC1 |
                SFRS::INDF1 => {
                    let real_addr = self.get_fsr(1);
                    self.memory[real_addr as usize]
                }
                _ => {
                    self.sfrs[sfr_addr as usize]
                }
            };

            Ok(value)
        } else {
            if addr as usize > 0xffff {
                return Err("Invalid dest address".to_owned());
            }

            Ok(self.memory[addr as usize])
        }
    }
    pub fn set_byte_noupdate(&mut self, addr: u32, what: u8) -> Result<(), String> {
        if self.is_sfr_addr(addr) {
            let sfr_addr = (addr as u16) - self.sfr_start;
            match sfr_addr {
                /* indf0 */
                SFRS::PLUSW0 => {
                    let real_addr = self.get_fsr(0)
                        .wrapping_add(self.W as i8 as i16 as u16);
                    self.memory[real_addr as usize] = what;
                },
                SFRS::PREINC0 |
                SFRS::POSTINC0 |
                SFRS::POSTDEC0 |
                SFRS::INDF0 => {
                    let real_addr = self.get_fsr(0);
                    self.memory[real_addr as usize] = what;
                },
                SFRS::PLUSW1 => {
                    let real_addr = self.get_fsr(1)
                        .wrapping_add(self.W as i8 as i16 as u16);
                    self.memory[real_addr as usize] = what;
                },
                SFRS::PREINC1 |
                SFRS::POSTINC1 |
                SFRS::POSTDEC1 |
                SFRS::INDF1 => {
                    let real_addr = self.get_fsr(1);
                    self.memory[real_addr as usize] = what;
                }
                SFRS::EECON2 => {
                    match what {
                        0x55 => {
                            match self.eecon_state {
                                EECON_STATE::Default => {
                                    self.eecon_state = EECON_STATE::Prep1;
                                },
                                _ => {
                                    /* TODO: what happens?? */
                                }
                            }
                        },
                        0xaa => {
                            match self.eecon_state {
                                EECON_STATE::Prep1 => {
                                    self.eecon_state = EECON_STATE::Ready;
                                },
                                _ => {
                                    /* TODO: what happens?? */
                                }
                            }
                        },
                        _ => {
                            // TODO what does this do?
                        }
                    }
                },
                SFRS::EECON1 => {
                    self.sfrs[SFRS::EECON1 as usize] = what;
                    // TODO: more closely implement real behavior here..
                    if what & 0x02 == 0x02 {
                        match self.eecon_state {
                            EECON_STATE::Ready => {
                                if what & 0x80 == 0x80 &&
                                    what & 0x40 == 0x00 &&
                                    what & 0x04 == 0x04 {
                                    /* write latches to memory! */
                                    let start_addr = self.tblptr() & 0xffffe0;
                                    for i in 0..0x1f {
                                        self.program[(start_addr + i) as usize] &= self.holding_registers[i as usize];
                                        self.holding_registers[i as usize] = 0xff;
                                    }
                                } else {
                                    return Err(format!("Unsure how to handle write initiate with state 0x{:x}", what));
                                }
                            },
                            _ => { /* do nothing */ }
                        }
                    }
                }
                _ => {
                    self.sfrs[sfr_addr as usize] = what;
                }
            };
        } else {
            if addr as usize > 0xffff {
                return Err("Invalid dest address".to_owned());
            }

            self.memory[addr as usize] = what;
        }

        Ok(())
    }
    pub fn set_byte(&mut self, addr: u32, what: u8) -> Result<(), String> {
        if self.is_sfr_addr(addr) {
            let sfr_addr = (addr as u16) - self.sfr_start;
            match sfr_addr {
                SFRS::PREINC2 => {
                    let real_addr = self.get_fsr(2).wrapping_add(1);
                    self.set_fsr(2, real_addr);
                    self.set_byte_noupdate(addr, what)
                },
                SFRS::POSTINC2 => {
                    let real_addr = self.get_fsr(2).wrapping_add(1);
                    let value = self.set_byte_noupdate(addr, what);
                    self.set_fsr(2, real_addr);
                    value
                },
                SFRS::POSTDEC2 => {
                    let real_addr = self.get_fsr(2).wrapping_sub(1);
                    let value = self.set_byte_noupdate(addr, what);
                    self.set_fsr(2, real_addr);
                    value
                },
                SFRS::PREINC1 => {
                    let real_addr = self.get_fsr(1).wrapping_add(1);
                    self.set_fsr(1, real_addr);
                    self.set_byte_noupdate(addr, what)
                },
                SFRS::POSTINC1 => {
                    let real_addr = self.get_fsr(1).wrapping_add(1);
                    let value = self.set_byte_noupdate(addr, what);
                    self.set_fsr(1, real_addr);
                    value
                },
                SFRS::POSTDEC1 => {
                    let real_addr = self.get_fsr(1).wrapping_sub(1);
                    let value = self.set_byte_noupdate(addr, what);
                    self.set_fsr(1, real_addr);
                    value
                },
                SFRS::PREINC0 => {
                    let real_addr = self.get_fsr(0).wrapping_add(1);
                    self.set_fsr(0, real_addr);
                    self.set_byte_noupdate(addr, what)
                },
                SFRS::POSTINC0 => {
                    let real_addr = self.get_fsr(0).wrapping_add(1);
                    let value = self.set_byte_noupdate(addr, what);
                    self.set_fsr(0, real_addr);
                    value
                },
                SFRS::POSTDEC0 => {
                    let real_addr = self.get_fsr(0).wrapping_sub(1);
                    let value = self.set_byte_noupdate(addr, what);
                    self.set_fsr(0, real_addr);
                    value
                },
                _ => {
                    self.set_byte_noupdate(addr, what)
                }
            }
        } else {
            self.set_byte_noupdate(addr, what)
        }
    }
    pub fn set_word(&mut self, addr: u32, what: u32) -> Result<(), String> {
        if self.is_sfr_addr(addr) {
            let sfr_addr = (addr as u16) - self.sfr_start;

            let low = what as u8;
            let high = (what >> 8) as u8;
            self.sfrs[sfr_addr as usize] = low;
            self.sfrs[(sfr_addr + 1) as u16 as usize] = high;
        } else {
            if addr as usize > 0xffff {
                return Err("Invalid dest address".to_owned());
            }

            if what > 0xffff {
                return Err("Invalid data to write".to_owned());
            }

            let low = what as u8;
            let high = (what >> 8) as u8;
            self.memory[addr as usize] = low;
            self.memory[(addr + 1) as u16 as usize] = high;
        }

        Ok(())
    }
    pub fn tblptr(&self) -> u32 {
        ((self.sfrs[SFRS::TBLPTRL as usize] as u32)) |
        ((self.sfrs[SFRS::TBLPTRH as usize] as u32) << 8) |
        ((self.sfrs[SFRS::TBLPTRU as usize] as u32) << 16)
    }
    pub fn describe(&self) {
        println!("pic18: C: {}, Z: {}, N: {}, V: {}", self.psr_c, self.psr_z, self.psr_n, self.psr_v);
        println!("ip=0x{:x}, W=0x{:x}, stkptr=0x{:x}", self.ip, self.W, self.sfrs[SFRS::STKPTR as usize]);
        match self.decode() {
            Ok(instr) => println!("instruction: {}", instr),
            Err(e) => println!("[invalid: {}]", e)
        };
        println!("stack:");
        for i in 0..self.sfrs[SFRS::STKPTR as usize] {
            println!("  0x{:x}", self.stack[i as usize]);
        }
        println!("------");
        println!("tblptr: 0x{:x}", self.tblptr());
        println!("");
    }
    pub fn program<T: MemoryRange<<PIC18 as Arch>::Address>>(&mut self, program: Option<T>, config: Option<FlatMemoryRepr>) -> Result<(), String> {
        match program.and_then(|x| x.to_flat()) {
            Some(data) => {
                if data.len() > self.program.len() {
                    return Err(
                        format!(
                            "Data is larger than the chip: 0x{:x} bytes of memory but 0x{:x} available",
                            data.len(),
                            self.program.len()
                        )
                    );
                }
                println!("DEBUG: writing 0x{:x} bytes of program...", data.len());
                for i in 0..data.len() {
                    self.program[i] = data.read(i as <PIC18 as Arch>::Address).unwrap();
                }
            },
            None => {
                println!("WARN: Provided program includes no code.");
            }
        };

        match config {
            Some(_config) => {
                println!("WARN: ignoring config");
            },
            None => {
            }
        };

        Ok(())
    }
}

impl MCU for CPU {
    type Addr = u32;
    type Instruction = <PIC18 as Arch>::Instruction;
    type Decoder = <PIC18 as Arch>::Decoder;
    fn emulate(&mut self) -> Result<(), String> {
        let mut skip_next = false;
        let eval_result = match self.decode() {
            Ok(instr) => {
                match instr.opcode {
                    Opcode::TBLWT_S => {
                        let ptr = self.tblptr();
                        self.holding_registers[(ptr & 0x1f) as usize] = self.sfrs[SFRS::TABLAT as usize];
                    }
                    Opcode::TBLRD_S => {
                        let ptr = self.tblptr();
                        self.sfrs[SFRS::TABLAT as usize] = self.program[ptr as usize];
                    }
                    Opcode::TBLRD_S_I => {
                        let mut ptr = self.tblptr();
                        self.sfrs[SFRS::TABLAT as usize] = self.program[ptr as usize];
                        ptr += 1;
                        self.sfrs[SFRS::TBLPTRL as usize] = ptr as u8;
                        self.sfrs[SFRS::TBLPTRH as usize] = (ptr >> 8) as u8;
                        self.sfrs[SFRS::TBLPTRU as usize] = (ptr >> 16) as u8;
                    }
                    Opcode::SUBLW => {
                        match instr.operands[0] {
                            Operand::ImmediateU8(value) => {
                                self.W = self.W.wrapping_sub(value)
                            },
                            _ => { unreachable!(); }
                        }
                    }
                    Opcode::IORLW => {
                        match instr.operands[0] {
                            Operand::ImmediateU8(value) => {
                                self.W |= value
                            },
                            _ => { unreachable!(); }
                        }
                    }
                    Opcode::XORLW => {
                        match instr.operands[0] {
                            Operand::ImmediateU8(value) => {
                                self.W ^= value
                            },
                            _ => { unreachable!(); }
                        }
                    }
                    Opcode::ANDLW => {
                        match instr.operands[0] {
                            Operand::ImmediateU8(value) => {
                                self.W &= value
                            },
                            _ => { unreachable!(); }
                        }
                    }
                    Opcode::RETLW => {
                        match instr.operands[0] {
                            Operand::ImmediateU8(value) => {
                                self.W = value
                            },
                            _ => { unreachable!(); }
                        };
                        self.ip = self.pop().unwrap();
                        return Ok(());
                    }
                    Opcode::MULWF => {
                        let f = match instr.operands[0] {
                            Operand::File(value, a) => self.debank(value, a),
                            _ => { unreachable!(); }
                        };

                        let result = (self.get_byte(f as u32).unwrap() as u16) * (self.W as u16);
                        self.sfrs[SFRS::PRODH as usize] = (result >> 8) as u8;
                        self.sfrs[SFRS::PRODL as usize] = result as u8;
                    }
                    Opcode::MULLW => {
                        let literal = match instr.operands[0] {
                            Operand::ImmediateU8(value) => {
                                value as u16
                            },
                            _ => { unreachable!(); }
                        };

                        let result = literal * (self.W as u16);
                        self.sfrs[SFRS::PRODH as usize] = (result >> 8) as u8;
                        self.sfrs[SFRS::PRODL as usize] = result as u8;
                    }
                    Opcode::MOVLW => {
                        match instr.operands[0] {
                            Operand::ImmediateU8(value) => {
                                self.W = value
                            },
                            _ => {
                                unreachable!();
                            }
                        }
                    },
                    Opcode::ADDLW => {
                        match instr.operands[0] {
                            Operand::ImmediateU8(value) => {
                                self.W = self.W.wrapping_add(value)
                            },
                            _ => { unreachable!(); }
                        }
                    }
                    Opcode::MOVLB => {
                        match instr.operands[0] {
                            Operand::ImmediateU8(value) => {
                                self.set_bank(value);
                            },
                            _ => {
                                unreachable!();
                            }
                        }
                    },
                    Opcode::GOTO => {
                        match instr.operands[0] {
                            Operand::ImmediateU32(addr) => {
                                self.ip = addr;
                                return Ok(());
                            },
                            _ => {
                                unreachable!();
                            }
                        }
                    },
                    Opcode::CALL => {
                        match instr.operands[0] {
                            Operand::ImmediateU32(addr) => {
                                let return_address = self.ip + instr.len();
                                self.push(return_address).unwrap();
                                self.ip = addr;
                                return Ok(());
                            },
                            _ => {
                                unreachable!();
                            }
                        }
                    },
                    Opcode::RETURN => {
                        self.ip = self.pop().unwrap();
                        return Ok(());
                    },
                    Opcode::BRA => {
                        match instr.operands[0] {
                            Operand::ImmediateU32(rel) => {
                                self.ip = self.ip
                                    .wrapping_offset(instr.len())
                                    .wrapping_offset(AddressDiff::from_const((rel << 1) as i8 as i32 as u32));
                                return Ok(());
                            },
                            _ => {
                                unreachable!();
                            }
                        }
                    },
                    Opcode::RCALL => {
                        match instr.operands[0] {
                            Operand::ImmediateU32(rel) => {
                                let return_address = self.ip + instr.len();
                                self.push(return_address).unwrap();
                                self.ip = return_address
                                    .wrapping_add((rel << 1) as i8 as i32 as u32);
                                return Ok(());
                            },
                            _ => {
                                unreachable!();
                            }
                        }
                    },
                    Opcode::BZ => {
                        match instr.operands[0] {
                            Operand::ImmediateU8(rel) => {
                                if self.psr_z {
                                    self.ip = self.ip
                                        .wrapping_offset(instr.len())
                                        .wrapping_offset(AddressDiff::from_const((rel << 1) as i8 as i32 as u32));
                                    return Ok(());
                                }
                            },
                            _ => {
                                unreachable!();
                            }
                        }
                    },
                    Opcode::BNZ => {
                        match instr.operands[0] {
                            Operand::ImmediateU8(rel) => {
                                if ! self.psr_z {
                                    self.ip = self.ip
                                        .wrapping_offset(instr.len())
                                        .wrapping_offset(AddressDiff::from_const((rel << 1) as i8 as i32 as u32));
                                    return Ok(());
                                }
                            },
                            _ => {
                                unreachable!();
                            }
                        }
                    },
                    Opcode::BC => {
                        match instr.operands[0] {
                            Operand::ImmediateU8(rel) => {
                                if self.psr_c {
                                    self.ip = self.ip
                                        .wrapping_offset(instr.len())
                                        .wrapping_offset(AddressDiff::from_const((rel << 1) as i8 as i32 as u32));
                                    return Ok(());
                                }
                            },
                            _ => {
                                unreachable!();
                            }
                        }
                    },
                    Opcode::BNC => {
                        match instr.operands[0] {
                            Operand::ImmediateU8(rel) => {
                                if ! self.psr_c {
                                    self.ip = self.ip
                                        .wrapping_offset(instr.len())
                                        .wrapping_offset(AddressDiff::from_const((rel << 1) as i8 as i32 as u32));
                                    return Ok(());
                                }
                            },
                            _ => {
                                unreachable!();
                            }
                        }
                    },
                    Opcode::BOV => {
                        match instr.operands[0] {
                            Operand::ImmediateU8(rel) => {
                                if self.psr_v {
                                    self.ip = self.ip
                                        .wrapping_offset(instr.len())
                                        .wrapping_offset(AddressDiff::from_const((rel << 1) as i8 as i32 as u32));
                                    return Ok(());
                                }
                            },
                            _ => {
                                unreachable!();
                            }
                        }
                    },
                    Opcode::BNOV => {
                        match instr.operands[0] {
                            Operand::ImmediateU8(rel) => {
                                if ! self.psr_v {
                                    self.ip = self.ip
                                        .wrapping_offset(instr.len())
                                        .wrapping_offset(AddressDiff::from_const((rel << 1) as i8 as i32 as u32));
                                    return Ok(());
                                }
                            },
                            _ => {
                                unreachable!();
                            }
                        }
                    },
                    Opcode::BN => {
                        match instr.operands[0] {
                            Operand::ImmediateU8(rel) => {
                                if self.psr_n {
                                    self.ip = self.ip
                                        .wrapping_offset(instr.len())
                                        .wrapping_offset(AddressDiff::from_const((rel << 1) as i8 as i32 as u32));
                                    return Ok(());
                                }
                            },
                            _ => {
                                unreachable!();
                            }
                        }
                    },
                    Opcode::BNN => {
                        match instr.operands[0] {
                            Operand::ImmediateU8(rel) => {
                                if ! self.psr_n {
                                    self.ip = self.ip
                                        .wrapping_offset(instr.len())
                                        .wrapping_offset(AddressDiff::from_const((rel << 1) as i8 as i32 as u32));
                                    return Ok(());
                                }
                            },
                            _ => {
                                unreachable!();
                            }
                        }
                    },
                    Opcode::RRNCF => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => {
                                unreachable!();
                            }
                        };

                        let new_value = self.get_byte(dest_file as u32).unwrap()
                            .rotate_right(1);

                        if to_file {
                            self.set_byte_noupdate(dest_file as u32, new_value).unwrap();
                        } else {
                            self.W = new_value;
                        }
                    },
                    Opcode::RRCF => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => {
                                unreachable!();
                            }
                        };

                        let mut new_value = self.get_byte(dest_file as u32).unwrap();
                        let new_carry = (new_value >> 7) & 0x01 == 0x01;
                        new_value >>= 1;
                        if self.psr_c {
                            new_value |= 0x80;
                        }

                        self.psr_c = new_carry;

                        if to_file {
                            self.set_byte_noupdate(dest_file as u32, new_value).unwrap();
                        } else {
                            self.W = new_value;
                        }
                    },
                    Opcode::RLNCF => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => {
                                unreachable!();
                            }
                        };

                        let new_value = self.get_byte(dest_file as u32).unwrap()
                            .rotate_left(1);

                        if to_file {
                            self.set_byte_noupdate(dest_file as u32, new_value).unwrap();
                        } else {
                            self.W = new_value;
                        }
                    },
                    Opcode::RLCF => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => {
                                unreachable!();
                            }
                        };

                        let mut new_value = self.get_byte(dest_file as u32).unwrap();
                        let new_carry = new_value & 0x01 == 0x01;
                        new_value <<= 1;
                        if self.psr_c {
                            new_value |= 0x01;
                        }

                        self.psr_c = new_carry;

                        if to_file {
                            self.set_byte_noupdate(dest_file as u32, new_value).unwrap();
                        } else {
                            self.W = new_value;
                        }
                    },
                    Opcode::NOP => {

                    },
                    Opcode::LFSR => {
                        let dest_file = match instr.operands[0] {
                            Operand::FileFSR(f) => {
                                // math
                                match f {
                                    0 => 0xfe9,
                                    1 => 0xfe1,
                                    2 => 0xfd9,
                                    _ => panic!("Invalid FSRf")
                                }
                            },
                            _ => {
                                unreachable!();
                            }
                        };

                        let imm = match instr.operands[1] {
                            Operand::ImmediateU32(imm) => {
                                imm
                            },
                            _ => {
                                unreachable!();
                            }
                        };

                        self.set_word(dest_file, imm).unwrap();
                    },
                    Opcode::DCFSNZ => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => { unreachable!() }
                        };


                        if to_file {
                            // TODO: verify
                            let value = self.get_byte(dest_file as u32).unwrap().wrapping_sub(1);
                            skip_next = value != 0;
                            self.set_byte_noupdate(dest_file as u32, value).unwrap();
                        } else {
                            self.W = self.get_byte(dest_file as u32).unwrap();
                            self.W = self.W.wrapping_sub(1);
                            skip_next = self.W != 0;
                        }
                    },
                    Opcode::INCF => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => { unreachable!() }
                        };


                        if to_file {
                            // TODO: verify how this interacts with postinc/postdec,
                            let value = self.get_byte(dest_file as u32).unwrap().wrapping_add(1);
                            // what about C, OV, DC?
                            self.psr_z = value == 0;
                            self.psr_n = value > 0x7f;
                            self.psr_c = value == 0x00; // just rolled over.
                            self.set_byte_noupdate(dest_file as u32, value).unwrap();
                        } else {
                            self.W = self.get_byte(dest_file as u32).unwrap();
                            self.W = self.W.wrapping_add(1);
                            self.psr_z = self.W == 0;
                            self.psr_n = self.W > 0x7f;
                            self.psr_c = self.W == 0x00; // just rolled over.
                        }
                    },
                    Opcode::DECF => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => { unreachable!() }
                        };


                        let (value, carry) = self.get_byte(dest_file as u32).unwrap().overflowing_add(0xff);
                        self.psr_z = value == 0;
                        self.psr_n = value > 0x7f;
                        self.psr_c = carry;

                        if to_file {
                            // TODO: verify how this interacts with postinc/postdec,
                            self.set_byte_noupdate(dest_file as u32, value).unwrap();
                        } else {
                            self.W = value;
                        }
                    },
                    Opcode::DECFSZ => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => { unreachable!() }
                        };


                        if to_file {
                            // TODO: verify
                            let value = self.get_byte(dest_file as u32).unwrap().wrapping_sub(1);
                            skip_next = value == 0;
                            self.set_byte_noupdate(dest_file as u32, value).unwrap();
                        } else {
                            self.W = self.get_byte(dest_file as u32).unwrap();
                            self.W = self.W.wrapping_sub(1);
                            skip_next = self.W == 0;
                        }
                    },
                    Opcode::INFSNZ => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => { unreachable!() }
                        };


                        if to_file {
                            // TODO: verify
                            let value = self.get_byte(dest_file as u32).unwrap().wrapping_add(1);
                            skip_next = value != 0;
                            self.set_byte_noupdate(dest_file as u32, value).unwrap();
                        } else {
                            self.W = self.get_byte(dest_file as u32).unwrap();
                            self.W = self.W.wrapping_add(1);
                            skip_next = self.W != 0;
                        }
                    },
                    Opcode::INCFSZ => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => { unreachable!() }
                        };


                        if to_file {
                            // TODO: verify
                            let value = self.get_byte(dest_file as u32).unwrap().wrapping_add(1);
                            skip_next = value == 0;
                            self.set_byte_noupdate(dest_file as u32, value).unwrap();
                        } else {
                            self.W = self.get_byte(dest_file as u32).unwrap();
                            self.W = self.W.wrapping_add(1);
                            skip_next = self.W == 0;
                        }
                    },
                    Opcode::TSTFSZ => {
                        let dest_file = match instr.operands[0] {
                            Operand::File(f, banked) => self.debank(f, banked),
                            _ => { unreachable!() }
                        };


                        // TODO: verify
                        let value = self.get_byte(dest_file as u32).unwrap();
                        skip_next = value == 0;
                    },
                    Opcode::MOVF => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => { unreachable!() }
                        };


                        if to_file {
                            // TODO: verify
                            let value = self.get_byte(dest_file as u32).unwrap();
                            self.psr_n = value > 0x7f;
                            self.psr_z = value == 0x00;
                            self.set_byte_noupdate(dest_file as u32, value).unwrap();
                        } else {
                            self.W = self.get_byte(dest_file as u32).unwrap();
                            self.psr_n = self.W > 0x7f;
                            self.psr_z = self.W == 0x00;
                        }
                    },
                    Opcode::MOVFF => {
                        let src_file = match instr.operands[0] {
                            Operand::AbsoluteFile(f) => f,
                            _ => { unreachable!() }
                        };

                        let dest_file = match instr.operands[1] {
                            Operand::AbsoluteFile(f) => f,
                            _ => { unreachable!() }
                        };

                        let value = self.get_byte(src_file as u32).unwrap();
                        self.set_byte(dest_file as u32, value).unwrap();
                    },
                    Opcode::MOVWF => {
                        let dest_file = match instr.operands[0] {
                            Operand::File(f, banked) => self.debank(f, banked),
                            _ => { unreachable!() }
                        };

                        let value = self.W.clone();
                        self.set_byte(dest_file as u32, value).unwrap();
                    },
                    Opcode::CLRF => {
                        let dest_file = match instr.operands[0] {
                            Operand::File(f, banked) => self.debank(f, banked),
                            _ => { unreachable!() }
                        };

                        self.set_byte(dest_file as u32, 0).unwrap();
                    },
                    Opcode::BSF => {
                        let dest_file = match instr.operands[0] {
                            Operand::File(f, banked) => self.debank(f, banked),
                            _ => { unreachable!() }
                        };

                        let bit = match instr.operands[1] {
                            Operand::ImmediateU8(bit) => {
                                bit
                            },
                            _ => { unreachable!() }
                        };

                        let mut value = self.get_byte(dest_file as u32).unwrap();
                        value |= 1 << bit;
                        self.set_byte_noupdate(dest_file as u32, value).unwrap();
                    }
                    Opcode::BCF => {
                        let dest_file = match instr.operands[0] {
                            Operand::File(f, banked) => self.debank(f, banked),
                            _ => { unreachable!() }
                        };

                        let bit = match instr.operands[1] {
                            Operand::ImmediateU8(bit) => {
                                bit
                            },
                            _ => { unreachable!() }
                        };

                        let mut value = self.get_byte(dest_file as u32).unwrap();
                        value &= !(1 << bit);
                        self.set_byte_noupdate(dest_file as u32, value).unwrap();
                    }
                    Opcode::BTG => {
                        let dest_file = match instr.operands[0] {
                            Operand::File(f, banked) => self.debank(f, banked),
                            _ => { unreachable!() }
                        };

                        let bit = match instr.operands[1] {
                            Operand::ImmediateU8(bit) => {
                                bit
                            },
                            _ => { unreachable!() }
                        };

                        let mut value = self.get_byte(dest_file as u32).unwrap();
                        value ^= 1 << bit;
                        self.set_byte_noupdate(dest_file as u32, value).unwrap();
                    }
                    Opcode::BTFSS => {
                        let dest_file = match instr.operands[0] {
                            Operand::File(f, banked) => self.debank(f, banked),
                            _ => { unreachable!() }
                        };

                        let bit = match instr.operands[1] {
                            Operand::ImmediateU8(bit) => {
                                bit
                            },
                            _ => { unreachable!() }
                        };

                        let value = self.get_byte(dest_file as u32).unwrap();
                        let read = value & (1 << bit);

                        if read != 0 {
                            skip_next = true;
                        }
                    },
                    Opcode::BTFSC => {
                        let dest_file = match instr.operands[0] {
                            Operand::File(f, banked) => self.debank(f, banked),
                            _ => { unreachable!() }
                        };

                        let bit = match instr.operands[1] {
                            Operand::ImmediateU8(bit) => {
                                bit
                            },
                            _ => { unreachable!() }
                        };

                        let value = self.get_byte(dest_file as u32).unwrap();
                        let read = value & (1 << bit);

                        if read == 0 {
                            skip_next = true;
                        }
                    },
                    Opcode::ANDWF => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => { unreachable!() }
                        };

                        let value = self.get_byte(dest_file as u32).unwrap() & self.W;

                        self.psr_z = value == 0;
                        self.psr_n = value > 0x7f;

                        if to_file {
                            self.set_byte_noupdate(dest_file as u32, value).unwrap();
                        } else {
                            self.W = value;
                        }
                    },
                    Opcode::XORWF => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => { unreachable!() }
                        };

                        let value = self.get_byte(dest_file as u32).unwrap() ^ self.W;

                        self.psr_z = value == 0;
                        self.psr_n = value > 0x7f;

                        if to_file {
                            self.set_byte_noupdate(dest_file as u32, value).unwrap();
                        } else {
                            self.W = value;
                        }
                    },
                    Opcode::IORWF => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => { unreachable!() }
                        };

                        let value = self.get_byte(dest_file as u32).unwrap() | self.W;

                        self.psr_z = value == 0;
                        self.psr_n = value > 0x7f;

                        if to_file {
                            self.set_byte_noupdate(dest_file as u32, value).unwrap();
                        } else {
                            self.W = value;
                        }
                    },
                    Opcode::ADDWF => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => { unreachable!() }
                        };

                        if to_file {
                            // TODO: verify how this interacts with postinc/postdec,
                            let (value, carry) = self.get_byte(dest_file as u32).unwrap().overflowing_add(self.W);
                            self.psr_c = carry;
                            self.psr_z = value == 0;
                            self.psr_n = value > 0x7f;
                            self.set_byte_noupdate(dest_file as u32, value).unwrap();
                        } else {
                            let (value, carry) = self.get_byte(dest_file as u32).unwrap().overflowing_add(self.W);
                            self.psr_c = carry;
                            self.psr_z = value == 0;
                            self.psr_n = value > 0x7f;
                            self.W = value;
                        }
                    },
                    Opcode::ADDWFC => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => { unreachable!() }
                        };

                        let (intermediate, carry1) = self.W.overflowing_add(self.get_byte(dest_file as u32).unwrap());
                        let (value, carry2) = intermediate.overflowing_add(if self.psr_c { 1 } else { 0 });
                        let carry = carry1 | carry2;

                        self.psr_c = carry;
                        self.psr_z = value == 0;
                        self.psr_n = value > 0x7f;

                        if to_file {
                            // TODO: verify how this interacts with postinc/postdec,
                            self.set_byte_noupdate(dest_file as u32, value).unwrap();
                        } else {
                            self.W = value;
                        }

                    }
                    Opcode::SUBWF => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => { unreachable!() }
                        };

                        let (intermediate, carry1) = self.get_byte(dest_file as u32).unwrap().overflowing_add(!self.W);
                        let (value, carry2) = intermediate.overflowing_add(1);
                        let carry = carry1 | carry2;

                        self.psr_c = carry;
                        self.psr_z = value == 0;
                        self.psr_n = value > 0x7f;

                        if to_file {
                            // TODO: verify how this interacts with postinc/postdec,
                            self.set_byte_noupdate(dest_file as u32, value).unwrap();
                        } else {
                            self.W = value;
                        }
                    },
                    Opcode::SUBWFB => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => { unreachable!() }
                        };

                        let (intermediate, carry1) = self.get_byte(dest_file as u32).unwrap().overflowing_add(!self.W);
                        let (value, carry2) = intermediate.overflowing_add(if self.psr_c { 1 } else { 0 });
                        let carry = carry1 | carry2;

                        self.psr_c = carry;
                        self.psr_z = value == 0;
                        self.psr_n = value > 0x7f;

                        if to_file {
                            // TODO: verify how this interacts with postinc/postdec,
                            self.set_byte_noupdate(dest_file as u32, value).unwrap();
                        } else {
                            self.W = value;
                        }
                    },
                    Opcode::SUBFWB => {
                        let (dest_file, to_file) = match instr.operands[0] {
                            Operand::RedirectableFile(f, banked, direction) => (self.debank(f, banked), direction),
                            _ => { unreachable!() }
                        };

                        let (intermediate, carry1) = self.W.overflowing_add(!self.get_byte(dest_file as u32).unwrap());
                        let (value, carry2) = intermediate.overflowing_add(if self.psr_c { 1 } else { 0 });
                        let carry = carry1 | carry2;

                        self.psr_c = carry;
                        self.psr_z = value == 0;
                        self.psr_n = value > 0x7f;

                        if to_file {
                            // TODO: verify how this interacts with postinc/postdec,
                            self.set_byte_noupdate(dest_file as u32, value).unwrap();
                        } else {
                            self.W = value;
                        }
                    },
                    Opcode::RESET => {
                        /* TODO: reset ALL state */
                        self.psr_z = false;
                        self.psr_c = false;
                        self.psr_n = false;
                        self.psr_v = false;
                        self.W = 0;
                        self.stack = [0u32; 31];
                        self.holding_registers = [0u8; 32];
                        self.eecon_state = EECON_STATE::Default;
                        self.sfrs = vec![0u8; self.sfrs.len()];
                        self.ip = 0;
                        println!("Reset.");
                        return Ok(());
                    },
                    _ => {
                        return Err(format!("unhandled opcode: {:?}", instr.opcode));
                    }
                };
                self.ip += instr.len();
                Ok(())
            },
            Err(msg) => { std::panic::panic_any(msg); }
        };
        if skip_next {
            match self.decode() {
                Ok(next_instr) => {
                    self.ip += next_instr.len();
                    Ok(())
                },
                Err(msg) => { std::panic::panic_any(msg); }
            }
        } else {
            eval_result
        }
    }

    fn decode(&self) -> Result<Self::Instruction, String> {
        <PIC18 as Arch>::Decoder::default().decode(self.program.range_from(self.ip).unwrap())
            .map_err(|e| {
                format!(
                    "Unable to decode bytes at 0x{:x}: {:x?}, {}",
                    self.ip,
                    self.program[(self.ip as usize)..((self.ip + 4) as usize)].iter().collect::<Vec<&u8>>(),
                    e
                )
            })
    }
}
