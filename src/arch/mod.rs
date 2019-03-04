pub mod arm;
pub mod pic17;
pub mod pic18;
pub mod pic24;
pub mod msp430;

pub mod x86_64;

pub mod display;
pub mod interface;

use std::collections::VecDeque;

use std::fmt::Debug;

use yaxpeax_arch::{Address, Decodable, LengthedInstruction};

// The type param is only to thread PartialInstructionContext through.. not a huge fan.
pub trait OperandDefinitions<C> {
    type Update;
    type Dependence;

    fn updates(&self, ctx: &C) -> Vec<Self::Update>;
    fn dependencies(&self, ctx: &C) -> Vec<Self::Dependence>;
}

#[derive(Debug)]
pub enum Device {
    PIC24(pic24::CPU),
    PIC18(pic18::cpu::CPU),
    PIC17(pic17::cpu::CPU),
    MSP430(msp430::cpu::CPU),
    x86(x86_64::cpu::CPU)
}

pub enum ISA {
    PIC17,
    PIC18,
    PIC18e,
    PIC24,
    MSP430,
    x86
}

pub struct InstructionIteratorSpanned<'a, Addr, Instr> {
    data: &'a [u8],
    current: Addr,
    end: Addr,
    elem: Option<Instr>
}

pub trait InstructionSpan<'a, Addr> where Addr: Address {
    fn instructions_spanning<Instr: Decodable>(&'a self, start: Addr, end: Addr) -> InstructionIteratorSpanned<'a, Addr, Instr>;
}

impl <'a, Addr> InstructionSpan<'a, Addr> for [u8] where Addr: Address {
    fn instructions_spanning<Instr: Decodable>(&'a self, start: Addr, end: Addr) -> InstructionIteratorSpanned<'a, Addr, Instr> {
        InstructionIteratorSpanned {
            data: self,
            current: start,
            end: end,
            elem: None
        }
    }
}

pub enum ControlFlowEffect<Addr> {
    FollowingInstruction,
    Relative(Addr),
    Absolute(Addr),
    Multiple(Vec<Addr>),
    Indirect
}

pub trait ControlFlowDeterminant {
    fn control_flow<T, Addr>(&self, &T) -> ControlFlowEffect<Addr>;
}

trait MCU {
    type Addr;
    type Instruction: Decodable;
    fn emulate(&mut self) -> Result<(), String>;
    fn decode(&self) -> Result<Self::Instruction, String>;
}

// TODO: streaming_iterator crate
pub trait SimpleStreamingIterator {
    type Item;

    fn next<'b>(&mut self) -> Option<&'b Self::Item>;
}

impl <'a, Addr, Instr> InstructionIteratorSpanned<'a, Addr, Instr> where Addr: Address + Copy + Clone, Instr: Decodable + LengthedInstruction<Unit=Addr> {
    pub fn next<'b>(&mut self) -> Option<(Addr, &Instr)> {
        if self.elem.is_some() {
            let instr: &mut Instr = self.elem.as_mut().unwrap();
            //  TODO: check for wrappipng..
            match Some(self.current.add(instr.len())) {
                Some(next) => {
                    if next <= self.end {
                        self.current = next;
                        let decode_result = instr.decode_into(self.data[self.current.to_linear()..].iter());
                        match decode_result {
                            Some(_) => {
                                Some((self.current, instr))
                            },
                            None => None
                        }
                    } else {
                        None
                    }
                },
                None => None
            }
        } else {
            if self.current <= self.end {
                self.elem = Instr::decode(self.data[self.current.to_linear()..].iter());
                match self.elem {
                    Some(ref instr) => {
                        Some((self.current, &instr))
                    },
                    None => None
                }
            } else {
                None
            }
        }
        /*
            None => {
            }
        }*/
    }
}
