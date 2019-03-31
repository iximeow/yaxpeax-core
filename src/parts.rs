use std::collections::HashMap;

use arch::ISA;
use arch::Device;
use arch::pic17;
use arch::pic18;
use arch::pic24;
use arch::msp430;
use arch::x86_64;

pub struct PartConfig {
    pub isa: ISA,
    program_size: u32,
    data_size: u32,
    eeprom_size: u32
}

pub fn get_chip_map() -> HashMap<String, PartConfig> {
    let mut map = HashMap::new();
    map.insert("msp430".to_string(), PartConfig {
        isa: ISA::MSP430,
        program_size: 0,
        data_size: 0x10000,
        eeprom_size: 0
    });
    map.insert("pic17c752".to_string(), PartConfig {
        isa: ISA::PIC17,
        program_size: 2 * 8 * 1024,
        data_size: 678,
        eeprom_size: 0
    });
    map.insert("pic17c756a".to_string(), PartConfig {
        isa: ISA::PIC17,
        program_size: 2 * 16 * 1024,
        data_size: 902,
        eeprom_size: 0
    });
    map.insert("pic17c762".to_string(), PartConfig {
        isa: ISA::PIC17,
        program_size: 2 * 8 * 1024,
        data_size: 678,
        eeprom_size: 0
    });
    map.insert("pic17c766".to_string(), PartConfig {
        isa: ISA::PIC17,
        program_size: 2 * 16 * 1024,
        data_size: 902,
        eeprom_size: 0
    });
    map.insert("pic18f2455".to_string(), PartConfig {
        isa: ISA::PIC18e,
        program_size: 12288 * 2,
        data_size: 2048,
        eeprom_size: 256
    });
    map.insert("pic18f2550".to_string(), PartConfig {
        isa: ISA::PIC18e,
        program_size: 16384 * 2,
        data_size: 2048,
        eeprom_size: 256
    });
    map.insert("pic18f4550".to_string(), PartConfig {
        isa: ISA::PIC18e,
        program_size: 16384 * 2,
        data_size: 2048,
        eeprom_size: 256
    });
    map.insert("x86".to_string(), PartConfig {
        isa: ISA::x86,
        program_size: 0xffffffff,
        data_size: 0xffffffff,
        eeprom_size: 0
    });
    map.insert("x86_64".to_string(), PartConfig {
        isa: ISA::x86_64,
        program_size: 0xffffffff,
        data_size: 0xffffffff,
        eeprom_size: 0
    });
    map
}

pub fn get_cpu(part_config: &PartConfig) -> Result<Device, String> {
    match &part_config.isa {
        ISA::PIC24 => Ok(Device::PIC24(pic24::CPU::new(part_config.program_size, part_config.data_size))),
        ISA::PIC18e => Ok(Device::PIC18(pic18::cpu::CPU::new(part_config.program_size, part_config.data_size))),
        ISA::PIC18 => Err("Cannot construct PIC18 CPUs right now.".to_owned()),
        ISA::PIC17 => Ok(Device::PIC17(pic17::cpu::CPU::new(part_config.program_size, part_config.data_size))),
        ISA::MSP430 => Ok(Device::MSP430(msp430::cpu::CPU::new())),
        ISA::x86 => Ok(Device::x86(x86_64::cpu::CPU::new())),
        ISA::x86_64 => Ok(Device::x86(x86_64::cpu::CPU::new())),
        arch @ _ => {
            Err(format!("Unsupported ISA: {:?}", arch))
        }
    }
}
