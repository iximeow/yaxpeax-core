use yaxpeax_pic24::Opcode;
use arch::MCU;

#[allow(non_snake_case)]
#[derive(Debug)]
pub struct CPU {
    W: [u16; 16],
    pub program: Vec<u8>,
    pub memory: Vec<u8>
}

impl CPU {
    pub fn new(progsize: u32, memsize: u32) -> Self {
        CPU {
            W: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            program: vec![0; progsize as usize],
            memory: vec![0; memsize as usize]
        }
    }
}

impl MCU for CPU {
    type Addr = u32;
    type Instruction = yaxpeax_pic24::Instruction;
    fn emulate(&mut self) -> Result<(), String> {
        panic!("not implemented yet")
    }
    fn decode(&self) -> Result<Self::Instruction, String> {
        Ok(yaxpeax_pic24::Instruction { opcode: Opcode::NOP })
    }
}

