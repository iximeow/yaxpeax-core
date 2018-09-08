use std::env;

mod arch;
mod debug;
mod memory;
mod parts;

use arch::pic17;
use arch::pic18;
use arch::pic24;
use arch::msp430;
use arch::Decodable;
use arch::ISA;

fn main() {
    let chipmap = parts::get_chip_map();

    let binary_path = env::args().nth(1).unwrap();

    let data = memory::load_from_path(binary_path).unwrap();

    let chip = if let Some(chip_str) = env::args().nth(2) {
        chip_str
    } else {
        "pic17c756a".to_string()
    };

    let mut address = 0;

    let isa = &chipmap.get(&chip).unwrap().isa;

    for _ in 0..1000 {
        let (next_address, instr) = agnostic_decode(&isa, &data.sections[&0], address);
        println!("0x{:04x}: {}", address, instr);
        address = next_address;
    }
}

fn agnostic_decode(isa: &ISA, data: &Vec<u8>, offset: usize) -> (usize, String) {
    match isa {
        ISA::PIC24 => { panic!("not handling pic24 yet"); },
        ISA::PIC18 => { panic!("pic18 support is WIP"); },
        ISA::PIC18e => {
            let mut instr = pic18::instruction::Instruction::blank();
            instr.decode_into(data[offset..].iter()).unwrap();
            (offset + instr.len() as usize, format!("{}", instr))
        },
        ISA::PIC17 => {
            let mut instr = pic17::instruction::Instruction::blank();
            instr.decode_into(data[(offset * 2)..].iter()).unwrap();
            (offset + instr.len() as usize, format!("{}", instr))
        },
        ISA::MSP430 => {
            let mut instr = msp430::instruction::Instruction::blank();
            instr.decode_into(data[offset..].iter()).unwrap();
            (offset + instr.len() as usize, format!("{}", instr))
        }
    }
}
