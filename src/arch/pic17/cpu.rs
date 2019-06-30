use yaxpeax_arch::Decodable;
use arch::MCU;
use yaxpeax_arch::LengthedInstruction;
use yaxpeax_arch::Arch;
use yaxpeax_arch::ShowContextual;
use memory::{MemoryRepr, MemoryRange};
use memory::repr::FlatMemoryRepr;
use debug;
use debug::DebugTarget;
use arch::pic17;
use arch::pic17::{FullInstructionContext, PIC17, SFRS};
use yaxpeax_pic17::{Operand, Opcode};
use arch::pic17::deps::{updates_of, dependencies_of};
use arch::pic17::MergedContextTable;

pub struct PIC17DebugTarget<'a> {
    pub target: &'a mut pic17::cpu::CPU,
    break_conditions: Vec<BreakCondition>,
    watch_targets: Vec<WatchTarget>
}

impl <'a> PIC17DebugTarget<'a> {
    fn check_breakpoints(&self) -> bool {
        for bp in &self.break_conditions {
            match bp {
                BreakCondition::IPValue(ip) => {
                    if &self.target.ip == ip { return true; }
                },
                BreakCondition::Other(f) => {
                    if f(&self.target) { return true; }
                },
                BreakCondition::MemoryAccess(dest) => {
                    /*
                     * Needs to handle INDF0 and INDF1
                     * potentially referencing dest
                     */
                    let instr = self.target.decode();
                    match instr {
                        Ok(instr) => {
                            match instr.operands[0] {
                                Operand::File(f) => {
                                    if self.target.debank(f) == *dest {
                                        return true;
                                    }
                                },
                                _ => { }
                            };

                            match instr.operands[1] {
                                Operand::File(f) => {
                                    if self.target.debank(f) == *dest {
                                        return true;
                                    }
                                },
                                _ => { }
                            };
                        },
                        Err(_) => { }
                    };
                },
                BreakCondition::IO => {
                    if self.target.would_IO().is_some() { return true; }
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
    fn pointee(&self, cpu: &pic17::cpu::CPU) -> Option<u16> {
        match self {
            WatchTarget::Pointee(target) => {
                target.pointee(cpu).and_then(|value| {
                    if value > cpu.memory.len() as u16 - 2 {
                        return None
                    };

                    Some(cpu.get_byte_noupdate(value).unwrap() as u16 |
                        ((cpu.get_byte_noupdate(value + 1).unwrap() as u16) << 8))
                })
            },
            WatchTarget::MemoryLocation(addr) => {
                cpu.get_byte_noupdate(*addr).and_then(|low| {
                    cpu.get_byte_noupdate(*addr + 1).map(|high| {
                        ((high as u16) << 8) | (low as u16)
                    })
                }).ok()
            },
            WatchTarget::W => Some(cpu.W as u16)
        }
    }
    fn reify(&self, cpu: &pic17::cpu::CPU) -> String {
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
                format!("[{}]: 0x{:x}", pic17::named_file(*addr), cpu.get_byte_noupdate(*addr).unwrap())
            },
            WatchTarget::W => {
                format!("W: 0x{:x}", cpu.W)
            }
        }
    }
}

impl FullInstructionContext for pic17::cpu::CPU {
    fn memory(&self, addr: u16) -> u8 { self.memory[addr as usize] }
    fn bsr(&self) -> u8 {
        self.memory[SFRS::BSR as usize]
    }
    fn pclath(&self) -> u8 {
        self.memory[SFRS::PCLATH as usize]
    }
}

#[derive(Debug)]
pub enum WatchTarget {
    Pointee(Box<WatchTarget>),
    MemoryLocation(u16),
    W
}
pub enum BreakCondition {
    IPValue(u16),
    Other(fn(&pic17::cpu::CPU) -> bool),
    MemoryAccess(u16),
    IO
}

impl <'a> DebugTarget<'a, pic17::cpu::CPU> for PIC17DebugTarget<'a> {
    type WatchTarget = WatchTarget;
    type BreakCondition = BreakCondition;
    fn attach(cpu: &'a mut pic17::cpu::CPU) -> Self {
        PIC17DebugTarget {
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

enum IOCause {
    UART,
    PORT
}

use arch::pic17::PartialInstructionContext;

pub fn try_debank<T>(f: u8, ctx: Option<&T>) -> Option<u16> where T: PartialInstructionContext {
    if f < 0x10 {
        Some(f as u16)
    } else if f < 0x18 {
        ctx.and_then(|c| c.bsr_sfr()).map(|bank| ((bank as u16) << 8) | f as u16)
    } else {
        ctx.and_then(|c| c.bsr_gpr()).map(|bank| ((bank as u16) << 8) | f as u16)
    }
}

pub fn debank(f: u8, bsr: u8) -> u16 {
    if f < 0x10 {
        f as u16
    } else if f < 0x18 {
        f as u16 | ((bsr as u16 & 0x0f) << 8)
    } else {
        f as u16 | ((bsr as u16 & 0xf0) << 4)
    }
}

#[allow(non_snake_case)]
#[derive(Debug)]
pub struct CPU {
    pub W: u8,
    pub ip: u16, // might be u17?
    psr_z: bool,
    psr_c: bool,
    psr_v: bool,
    psr_dc: bool,
    stkptr: usize,
    stack: [u16; 16],
    tablat: [u8; 2],
    fsr0_post: u8,
    fsr1_post: u8,
    pub program: Vec<u8>,
    pub memory: Vec<u8>,
    pub memsize: usize,
    tsr: u8,
    tsr_len: u8
}

impl CPU {
    pub fn new(progsize: u32, memsize: u32) -> Self {
        let mut cpu = CPU {
            W: 0,
            ip: 0,
            psr_z: false,
            psr_c: false,
            psr_v: false,
            psr_dc: false,
            stkptr: 0,
            stack: [0u16; 16],
            tablat: [0u8; 2],
            fsr0_post: 0,
            fsr1_post: 0,
            program: vec![0; progsize as usize],
            memory: vec![0; 0x1000], // not `memsize` because this makes handling FSR easier
            memsize: memsize as usize,
            tsr: 0,
            tsr_len: 0
        };

        cpu.memory[SFRS::TXSTA1 as usize] |= 0b00000010;
        cpu.memory[SFRS::TXSTA2 as usize] |= 0b00000010;

        cpu
    }
    fn push(&mut self, value: u16) -> Result<(), String> {
        let ptr = self.stkptr;
        if ptr >= 16 {
            return Err("Stack overflow".to_owned());
        }

        self.stack[ptr as usize] = value;

        self.stkptr += 1;
        Ok(())
    }
    fn pop(&mut self) -> Result<u16, String> {
        let mut ptr = self.stkptr;
        if ptr == 0 {
            return Err("Stack underflow".to_owned());
        }

        ptr -= 1;

        let result = self.stack[ptr as usize];

        self.stkptr = ptr;
        Ok(result)
    }
    fn txen1(&self) -> bool { self.memory[SFRS::TXSTA1 as usize] & 0b00100000 != 0 }
    fn txen2(&self) -> bool { self.memory[SFRS::TXSTA2 as usize] & 0b00100000 != 0 }

    #[allow(non_snake_case)]
    fn would_IO(&self) -> Option<IOCause> {
        if self.tsr_len > 0 && (self.txen1() || self.txen2()) {
            return Some(IOCause::UART);
        }

        match self.decode() {
            Ok(instr) => {
                fn does_io(cpu: &CPU, op: Operand) -> Option<IOCause> {
                    match op {
                        Operand::File(f) => {
                            match cpu.debank(f) {
                                SFRS::PORTA |
                                SFRS::PORTB |
                                SFRS::PORTC |
                                SFRS::PORTD |
                                SFRS::PORTE |
                                SFRS::PORTF |
                                SFRS::PORTG => Some(IOCause::PORT),
                                _ => None
                            }
                        },
                        _ => None
                    }
                }

                does_io(self, instr.operands[0]).or(does_io(self, instr.operands[1]))
            }
            Err(_) => {
                return None;
            }
        }
    }
    fn set_sfr_bank(&mut self, value: u8) {
        self.memory[SFRS::BSR as usize] &= 0xf0;
        self.memory[SFRS::BSR as usize] |= value & 0x0f;
    }
    fn set_gpr_bank(&mut self, value: u8) {
        self.memory[SFRS::BSR as usize] &= 0x0f;
        self.memory[SFRS::BSR as usize] |= value & 0xf0;
    }
    /*
    fn bank(&self) -> u8 {
        self.memory[SFRS::BSR as usize] // TODO this should be configured somehow...
    }
    */
    fn get_fsr(&self, fsr: u8) -> u16 {
        // TODO
        match fsr {
            0 => {
                self.memory[SFRS::FSR0 as usize] as u16
            },
            1 => {
                self.memory[SFRS::FSR1 as usize] as u16
            },
            _ => { unreachable!(); }
        }
    }
    pub fn debank(&self, f: u8) -> u16 {
        pic17::cpu::debank(f, self.memory[SFRS::BSR as usize])
    }
    pub fn get_byte(&mut self, addr: u16) -> Result<u8, String> {
        self.get_byte_noupdate(addr)
    }
    pub fn get_byte_noupdate(&self, addr: u16) -> Result<u8, String> {
        let value = match addr {
            SFRS::PCL => { self.ip as u8 },
            SFRS::WREG => { self.W },
            SFRS::ALUSTA => {
                (self.fsr1_post << 6) |
                    (self.fsr0_post << 4) |
                    (if self.psr_v { 1 } else { 0 } << 3) |
                    (if self.psr_z { 1 } else { 0 } << 2) |
                    (if self.psr_dc { 1 } else { 0 } << 1) |
                    (if self.psr_c { 1 } else { 0 } << 0)
            },
            SFRS::INDF0 => {
                let real_addr = self.get_fsr(0);
                self.memory[real_addr as usize]
            },
            SFRS::INDF1 => {
                let real_addr = self.get_fsr(1);
                self.memory[real_addr as usize]
            }
            _ => {
                // pic17 is whack. i don't think the first 0x20 bytes
                // for pages are counted in "data size". so just add
                // those to the limit..
                let real_limit = self.memsize + 0x20 +
                    (self.memsize / 0x100) * 0x20;
                if addr as usize > real_limit && addr as u8 >= 0x20 {
                    return Err(format!("Invalid dest address: {:x}", addr));
                }
                self.memory[addr as usize]
            }
        };

        Ok(value)
    }
    pub fn set_byte_noupdate(&mut self, addr: u16, what: u8) -> Result<(), String> {
        match addr {
            SFRS::PCL => { /* actually a nop */ },
            SFRS::WREG => { self.W = what; },
            SFRS::ALUSTA => {
                self.memory[SFRS::ALUSTA as usize] = what;
                self.fsr1_post = (what & 0xc0) >> 6;
                self.fsr0_post = (what & 0x30) >> 6;
                self.psr_v = (what & 0x08) == 0x08;
                self.psr_z = (what & 0x04) == 0x04;
                self.psr_dc = (what & 0x02) == 0x02;
                self.psr_c = (what & 0x01) == 0x01;
            },
            SFRS::CPUSTA => {
                let value = (self.memory[SFRS::CPUSTA as usize] & 0b11101100) |
                    (what & 0b00010011);
                self.memory[SFRS::CPUSTA as usize] = value;
            },
            SFRS::INDF0 => {
                // TODO: debank real_addr
                let real_addr = self.get_fsr(0);
                self.memory[real_addr as usize] = what;
            },
            SFRS::INDF1 => {
                let real_addr = self.get_fsr(1);
                self.memory[real_addr as usize] = what;
            },
            SFRS::TXSTA1 => {
                let masked = what & 0b11110001;
                self.memory[SFRS::TXSTA1 as usize] = masked;
            },
            SFRS::TXSTA2 => {
                let masked = what & 0b11110001;
                self.memory[SFRS::TXSTA2 as usize] = masked;
            },
            _ => {
                // pic17 is whack. i don't think the first 0x20 bytes
                // for pages are counted in "data size". so just add
                // those to the limit..
                let real_limit = self.memsize + 0x20 +
                    (self.memsize / 0x100) * 0x20;
                if addr as usize > real_limit && addr as u8 >= 0x20 {
                    return Err(format!("Invalid dest address: {:x}", addr));
                }

                self.memory[addr as usize] = what;
            }
        };
        Ok(())
    }
    pub fn set_byte(&mut self, addr: u16, what: u8) -> Result<(), String> {
        self.set_byte_noupdate(addr, what)
    }
    pub fn tblptr(&self) -> u16 {
        ((self.memory[SFRS::TBLPTRL as usize] as u16)) |
        ((self.memory[SFRS::TBLPTRH as usize] as u16) << 8)
    }
    pub fn set_tblptr(&mut self, tblptr: u16) {
        self.memory[SFRS::TBLPTRL as usize] = tblptr as u8;
        self.memory[SFRS::TBLPTRH as usize] = (tblptr >> 8) as u8;
    }
    pub fn describe(&self) {
        println!("pic17: C: {}, Z: {}, DC: {}, V: {}", self.psr_c, self.psr_z, self.psr_dc, self.psr_v);
        println!("ip=0x{:x}, W=0x{:x}, stkptr=0x{:x}", self.ip, self.W, self.stkptr);
        match self.decode() {
            Ok(instr) => {
                let _ctx: Option<&pic17::cpu::CPU> = Some(self);
                let mut s = String::new();
                let ctx: Option<&MergedContextTable> = None;
                instr.contextualize(None, self.ip, /* TODO: ctx */ ctx, &mut s).unwrap();
                println!("instruction: {}", s);
            },
            Err(e) => println!("[invalid: {}]", e)
        };
        println!("stack:");
        for i in 0..self.stkptr {
            println!("  0x{:x}", self.stack[i as usize]);
        }
        println!("------");
        println!("tblptr: 0x{:x}", self.tblptr());
        println!("");
    }
    pub fn program<T: MemoryRange<<PIC17 as Arch>::Address>>(&mut self, program: Option<T>, config: Option<FlatMemoryRepr>) -> Result<(), String> {
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
                    self.program[i] = data.read(i as <PIC17 as Arch>::Address).unwrap();
                }
            },
            None => {
                println!("WARN: Provided program includes no code.");
            }
        };

        // TODO: where do config bits show up in pic17?
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

// TODO: this is wrong lol
use analyses::static_single_assignment::NoAliasing;
impl NoAliasing for Dependence { }

use data::types::{Typed, TypeAtlas, TypeSpec};
impl Typed for u8 {
    fn type_of(&self, _: &TypeAtlas) -> TypeSpec {
        TypeSpec::Unknown
    }
}

use analyses::static_single_assignment::{Direction, SSAValues};
use arch::pic17::{Dependence, Update};
impl SSAValues for PIC17 {
    type Location = Dependence;
    type Data = u8; // TODO: either some id of something interesting or a value?

    fn decompose(instr: &Self::Instruction) -> Vec<(Option<Self::Location>, Direction)> {
        use arch::pic17::MergedContext;
        let ctx = MergedContext {
            computed: None,
            user: None
        };
        let mut result: Vec<(Option<Self::Location>, Direction)> = Vec::new();
        for update in updates_of(instr, &ctx).into_iter() {
            let as_dep = match update {
                Update::W => Some(Dependence::W),
                Update::Memory(_) => None,
                Update::Program(_) => None,
//                Update::Stack(_) => None,
                Update::Carry => Some(Dependence::Carry),
                Update::BSR_SFR => Some(Dependence::BSR_SFR),
                Update::BSR_GPR => Some(Dependence::BSR_GPR),
                Update::Unknown => None
            };
            result.push((as_dep, Direction::Write));
        }
        for dependence in dependencies_of(instr, &ctx).into_iter() {
            let as_dep = match dependence {
                Dependence::W => Some(Dependence::W),
                Dependence::Memory(_) => None,
                Dependence::Program(_) => None,
                Dependence::Stack(_) => None,
                Dependence::Carry => Some(Dependence::Carry),
                Dependence::BSR_SFR => Some(Dependence::BSR_SFR),
                Dependence::BSR_GPR => Some(Dependence::BSR_GPR),
                Dependence::Unknown => None
            };
            result.push((as_dep, Direction::Read));
        }
        result
    }
}


impl MCU for CPU {
    type Addr = u16;
    type Instruction = yaxpeax_pic17::Instruction;
    fn emulate(&mut self) -> Result<(), String> {
        fn store_operand(cpu: &mut CPU, new_value: u8, dest: Operand) {
            match dest {
                Operand::File(f) => {
                    let dest_file = cpu.debank(f);
                    cpu.set_byte_noupdate(dest_file, new_value).unwrap();
                },
                Operand::W => {
                    cpu.W = new_value;
                }
                _ => {
                    unreachable!();
                }
            }
        }
        let mut skip_next = false;
        let eval_result = match self.decode() {
            Ok(instr) => {
                match instr.opcode {
                    Opcode::TLRDL => {
                        let dest_file = self.debank(instr.operands[0].file_value());
                        let value = self.tablat[0];
                        self.set_byte(dest_file, value).unwrap();
                    },
                    Opcode::TLRDH => {
                        let dest_file = self.debank(instr.operands[0].file_value());
                        let value = self.tablat[1];
                        self.set_byte(dest_file, value).unwrap();
                    },
                    Opcode::TLWTL => {
                        let source_file = self.debank(instr.operands[0].file_value());
                        self.tablat[0] = self.get_byte(source_file).unwrap();
                    },
                    Opcode::TLWTH => {
                        let source_file = self.debank(instr.operands[0].file_value());
                        self.tablat[1] = self.get_byte(source_file).unwrap();
                    },
                    Opcode::TABLRDL => {
                        self.tablat[0] = self.program[(self.tblptr() * 2) as usize];
                        self.tablat[1] = self.program[(self.tblptr() * 2 + 1) as usize];
                        let dest_file = self.debank(instr.operands[0].file_value());
                        let value = self.tablat[0];
                        self.set_byte(dest_file, value).unwrap();
                    },
                    Opcode::TABLRDLI => {
                        let tblptr = self.tblptr();
                        self.tablat[0] = self.program[(self.tblptr() * 2) as usize];
                        self.tablat[1] = self.program[(self.tblptr() * 2 + 1) as usize];
                        self.set_tblptr(tblptr + 1);
                        let dest_file = self.debank(instr.operands[0].file_value());
                        let value = self.tablat[0];
                        self.set_byte(dest_file, value).unwrap();
                    },
                    Opcode::TABLRDH => {
                        self.tablat[0] = self.program[(self.tblptr() * 2) as usize];
                        self.tablat[1] = self.program[(self.tblptr() * 2 + 1) as usize];
                        let dest_file = self.debank(instr.operands[0].file_value());
                        let value = self.tablat[1];
                        self.set_byte(dest_file, value).unwrap();
                    },
                    Opcode::TABLRDHI => {
                        let tblptr = self.tblptr();
                        self.tablat[0] = self.program[(self.tblptr() * 2) as usize];
                        self.tablat[1] = self.program[(self.tblptr() * 2 + 1) as usize];
                        self.set_tblptr(tblptr + 1);
                        let dest_file = self.debank(instr.operands[0].file_value());
                        let value = self.tablat[1];
                        self.set_byte(dest_file, value).unwrap();
                    },
                    Opcode::TABLWTL => {
                        let source_file = self.debank(instr.operands[0].file_value());
                        self.tablat[0] = self.get_byte(source_file).unwrap();
                        let mut tblptr = self.tblptr();
                        self.program[(tblptr * 2) as usize] = self.tablat[0];
                        self.program[(tblptr * 2 + 1) as usize] = self.tablat[1];
                    },
                    Opcode::TABLWTLI => {
                        let source_file = self.debank(instr.operands[0].file_value());
                        self.tablat[0] = self.get_byte(source_file).unwrap();
                        let mut tblptr = self.tblptr();
                        self.program[(tblptr * 2) as usize] = self.tablat[0];
                        self.program[(tblptr * 2 + 1) as usize] = self.tablat[1];
                        tblptr += 1;
                        self.set_tblptr(tblptr);
                    },
                    Opcode::TABLWTH => {
                        let source_file = self.debank(instr.operands[0].file_value());
                        self.tablat[1] = self.get_byte(source_file).unwrap();
                        let mut tblptr = self.tblptr();
                        self.program[(tblptr * 2) as usize] = self.tablat[0];
                        self.program[(tblptr * 2 + 1) as usize] = self.tablat[1];
                    },
                    Opcode::TABLWTHI => {
                        let source_file = self.debank(instr.operands[0].file_value());
                        self.tablat[1] = self.get_byte(source_file).unwrap();
                        let mut tblptr = self.tblptr();
                        self.program[(tblptr * 2) as usize] = self.tablat[0];
                        self.program[(tblptr * 2 + 1) as usize] = self.tablat[1];
                        tblptr += 1;
                        self.set_tblptr(tblptr);
                    },
                    Opcode::SUBLW => {
                        self.W = self.W.wrapping_sub(
                            instr.operands[0].imm8_value()
                        );
                    }
                    Opcode::IORLW => {
                        self.W |= instr.operands[0].imm8_value();
                    }
                    Opcode::XORLW => {
                        self.W ^= instr.operands[0].imm8_value();
                    }
                    Opcode::ANDLW => {
                        self.W &= instr.operands[0].imm8_value();
                    }
                    Opcode::RETLW => {
                        self.W = instr.operands[0].imm8_value();
                        self.ip = self.pop().unwrap();
                        return Ok(());
                    }
                    Opcode::MULWF => {
                        let f = self.debank(instr.operands[0].file_value());

                        let result = (self.get_byte(f).unwrap() as u16) * (self.W as u16);
                        self.memory[SFRS::PRODH as usize] = (result >> 8) as u8;
                        self.memory[SFRS::PRODL as usize] = result as u8;
                    }
                    Opcode::MULLW => {
                        let literal = instr.operands[0].imm8_value();

                        let result = (literal as u16) * (self.W as u16);
                        self.memory[SFRS::PRODH as usize] = (result >> 8) as u8;
                        self.memory[SFRS::PRODL as usize] = result as u8;
                    }
                    Opcode::MOVLW => {
                        self.W = instr.operands[0].imm8_value();
                    },
                    Opcode::ADDLW => {
                        self.W = self.W.wrapping_add(
                            instr.operands[0].imm8_value()
                        );
                    }
                    Opcode::MOVLB => {
                        self.set_sfr_bank(
                            instr.operands[0].imm8_value()
                        );
                    },
                    Opcode::MOVLR => {
                        self.set_gpr_bank(
                            instr.operands[0].imm8_value()
                        );
                    },
                    Opcode::GOTO => {
                        // TODO: hmmmmmmmm..... u16?
                        self.ip = instr.operands[0].imm32_value() as u16;
                        return Ok(());
                    },
                    Opcode::CALL => {
                        let return_address = self.ip + instr.len();
                        self.push(return_address).unwrap();
                        self.ip = instr.operands[0].imm32_value() as u16;
                        return Ok(());
                    },
                    Opcode::RETURN => {
                        self.ip = self.pop().unwrap();
                        return Ok(());
                    },
                    Opcode::RRNCF => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let new_value = self.get_byte(source_file).unwrap()
                            .rotate_right(1);

                        store_operand(self, new_value, instr.operands[1]);
                    },
                    Opcode::RRCF => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let mut new_value = self.get_byte(source_file).unwrap();
                        let new_carry = (new_value >> 7) & 0x01 == 0x01;
                        new_value >>= 1;
                        if self.psr_c {
                            new_value |= 0x80;
                        }

                        self.psr_c = new_carry;
                        store_operand(self, new_value, instr.operands[1]);
                    },
                    Opcode::RLNCF => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let new_value = self.get_byte(source_file).unwrap()
                            .rotate_left(1);
                        store_operand(self, new_value, instr.operands[1]);
                    },
                    Opcode::RLCF => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let mut new_value = self.get_byte(source_file).unwrap();
                        let new_carry = new_value & 0x01 == 0x01;
                        new_value <<= 1;
                        if self.psr_c {
                            new_value |= 0x01;
                        }

                        self.psr_c = new_carry;
                        store_operand(self, new_value, instr.operands[1]);
                    },
                    Opcode::NOP => {

                    },
                    Opcode::DCFSNZ => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let value = self.get_byte(source_file).unwrap().wrapping_sub(1);
                        skip_next = value != 0;
                        store_operand(self, value, instr.operands[1]);
                    },
                    Opcode::INCF => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let value = self.get_byte(source_file).unwrap().wrapping_add(1);
                        // what about C, OV, DC?
                        self.psr_z = value == 0;
                        self.psr_c = value == 0x00; // just rolled over.
                        store_operand(self, value, instr.operands[1]);
                    },
                    Opcode::DECF => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let (value, carry) = self.get_byte(source_file).unwrap().overflowing_add(0xff);
                        self.psr_z = value == 0;
                        self.psr_c = carry;

                        store_operand(self, value, instr.operands[1]);
                    },
                    Opcode::DECFSZ => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let value = self.get_byte(source_file).unwrap().wrapping_sub(1);
                        skip_next = value == 0;

                        store_operand(self, value, instr.operands[1]);
                    },
                    Opcode::INFSNZ => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let value = self.get_byte(source_file).unwrap().wrapping_add(1);
                        skip_next = value != 0;

                        store_operand(self, value, instr.operands[1]);
                    },
                    Opcode::INCFSZ => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let value = self.get_byte(source_file).unwrap().wrapping_add(1);
                        skip_next = value == 0;

                        store_operand(self, value, instr.operands[1]);
                    },
                    Opcode::TSTFSZ => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        // TODO: verify
                        let value = self.get_byte(source_file).unwrap();
                        skip_next = value == 0;
                    },
                    Opcode::MOVWF => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let value = self.W.clone();
                        self.set_byte(source_file, value).unwrap();
                    },
                    Opcode::MOVFP => {
                        let source_file = self.debank(instr.operands[0].file_value());
                        let dest_file = self.debank(instr.operands[1].file_value());

                        // TODO: verify?
                        let value = self.get_byte(source_file).unwrap();
                        self.set_byte(dest_file, value).unwrap();
                    },
                    Opcode::MOVPF => {
                        let source_file = self.debank(instr.operands[0].file_value());
                        let dest_file = self.debank(instr.operands[1].file_value());

                        // TODO: verify?
                        let value = self.get_byte(source_file).unwrap();
                        self.set_byte(dest_file, value).unwrap();
                    },
                    Opcode::SWAPF => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        // TODO: verify?
                        let value = self.get_byte(source_file).unwrap();
                        let new_value = (value & 0xf0 >> 4) | (value & 0x0f << 4);
                        self.set_byte(source_file, new_value).unwrap();
                    }
                    Opcode::CLRF => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        self.set_byte(source_file, 0).unwrap();

                        match instr.operands[1] {
                            Operand::W => {
                                self.W = 0;
                            },
                            _ => { }
                        }
                    },
                    Opcode::BSF => {
                        let source_file = self.debank(instr.operands[0].file_value());
                        let bit = self.debank(instr.operands[1].imm8_value());

                        let mut value = self.get_byte(source_file).unwrap();
                        value |= 1 << bit;
                        self.set_byte_noupdate(source_file, value).unwrap();
                    }
                    Opcode::BCF => {
                        let source_file = self.debank(instr.operands[0].file_value());
                        let bit = self.debank(instr.operands[1].imm8_value());

                        let mut value = self.get_byte(source_file).unwrap();
                        value &= !(1 << bit);
                        self.set_byte_noupdate(source_file, value).unwrap();
                    }
                    Opcode::BTG => {
                        let source_file = self.debank(instr.operands[0].file_value());
                        let bit = self.debank(instr.operands[1].imm8_value());

                        let mut value = self.get_byte(source_file).unwrap();
                        value ^= 1 << bit;
                        self.set_byte_noupdate(source_file, value).unwrap();
                    }
                    Opcode::BTFSS => {
                        let source_file = self.debank(instr.operands[0].file_value());
                        let bit = self.debank(instr.operands[1].imm8_value());

                        let mut value = self.get_byte(source_file).unwrap();
                        let read = value & (1 << bit);

                        if read != 0 {
                            skip_next = true;
                        }
                    },
                    Opcode::BTFSC => {
                        let source_file = self.debank(instr.operands[0].file_value());
                        let bit = self.debank(instr.operands[1].imm8_value());

                        let mut value = self.get_byte(source_file).unwrap();
                        let read = value & (1 << bit);

                        if read == 0 {
                            skip_next = true;
                        }
                    },
                    Opcode::ANDWF => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let value = self.get_byte(source_file).unwrap() & self.W;

                        self.psr_z = value == 0;

                        store_operand(self, value, instr.operands[1]);
                    },
                    Opcode::XORWF => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let value = self.get_byte(source_file).unwrap() ^ self.W;

                        self.psr_z = value == 0;

                        store_operand(self, value, instr.operands[1]);
                    },
                    Opcode::IORWF => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let value = self.get_byte(source_file).unwrap() | self.W;

                        self.psr_z = value == 0;

                        store_operand(self, value, instr.operands[1]);
                    },
                    Opcode::ADDWF => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let (value, carry) = self.get_byte(source_file).unwrap().overflowing_add(self.W);
                        self.psr_c = carry;
                        self.psr_z = value == 0;

                        store_operand(self, value, instr.operands[1]);
                    },
                    Opcode::ADDWFC => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let (intermediate, carry1) = self.W.overflowing_add(self.get_byte(source_file).unwrap());
                        let (value, carry2) = intermediate.overflowing_add(if self.psr_c { 1 } else { 0 });
                        let carry = carry1 | carry2;

                        self.psr_c = carry;
                        self.psr_z = value == 0;

                        store_operand(self, value, instr.operands[1]);
                    }
                    Opcode::SUBWF => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let (intermediate, carry1) = self.get_byte(source_file).unwrap().overflowing_add(!self.W);
                        let (value, carry2) = intermediate.overflowing_add(1);
                        let carry = carry1 | carry2;

                        self.psr_c = carry;
                        self.psr_z = value == 0;

                        store_operand(self, value, instr.operands[1]);
                    },
                    Opcode::SUBWFB => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        let (intermediate, carry1) = self.get_byte(source_file).unwrap().overflowing_add(!self.W);
                        let (value, carry2) = intermediate.overflowing_add(if self.psr_c { 1 } else { 0 });
                        let carry = carry1 | carry2;

                        self.psr_c = carry;
                        self.psr_z = value == 0;

                        store_operand(self, value, instr.operands[1]);
                    },
                    Opcode::CPFSGT => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        if source_file != SFRS::WREG {
                            let value = self.get_byte(source_file).unwrap().wrapping_add(1);
                            skip_next = value > self.W;
                        }
                    },
                    Opcode::CPFSEQ => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        if source_file != SFRS::WREG {
                            let value = self.get_byte(source_file).unwrap().wrapping_add(1);
                            skip_next = value == self.W;
                        }
                    },
                    Opcode::CPFSLT => {
                        let source_file = self.debank(instr.operands[0].file_value());

                        if source_file != SFRS::WREG {
                            let value = self.get_byte(source_file).unwrap().wrapping_add(1);
                            skip_next = value < self.W;
                        }
                    },
                    _ => {
                        return Err(format!("unhandled opcode: {:?}", instr.opcode));
                    }
                };
                self.ip += instr.len();
                Ok(())
            },
            Err(msg) => { panic!(msg); }
        };
        if skip_next {
            match self.decode() {
                Ok(next_instr) => {
                    self.ip += next_instr.len();
                    Ok(())
                },
                Err(msg) => { panic!(msg); }
            }
        } else {
            eval_result
        }
    }

    fn decode(&self) -> Result<Self::Instruction, String> {
        let mut result = yaxpeax_pic17::Instruction {
            opcode: Opcode::NOP,
            operands: [Operand::Nothing, Operand::Nothing]
        };
        match result.decode_into(self.program.range_from(self.ip).unwrap()) {
            Some(()) => Ok(result),
            None => {
                Err(
                    format!(
                        "Unable to decode bytes at 0x{:x}: {:x?}",
                        self.ip,
                        self.program[(self.ip as usize)..((self.ip + 4) as usize)].iter().collect::<Vec<&u8>>()
                    )
                )
            }
        }
    }
}
