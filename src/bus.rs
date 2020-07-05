use crate::cpu::Cpu;
use crate::parser::NesRom;
use std::boxed::Box;
use std::rc::Rc;
pub struct Bus {
    rom: Option<NesRom>,

    cpuRam: [u8; 0x0800],
    ppu: (),
    apu: (),
}

impl Bus {
    pub fn init() -> Self {
        Self {
            rom: None,
            cpuRam: [0x0; 0x0800],
            ppu: (),
            apu: (),
        }
    }

    pub fn installROM(&mut self, rom: NesRom) {
        self.rom = Some(rom);
    }

    // Reads from the CPU bus.
    pub fn cpu_read(&self, addr: &u16) -> u8 {
        if *addr < 0x2000 {
            self.cpuRam[(*addr & 0x07ff) as usize]
        } else if *addr < 0x3fff {
            // read from PPU registers
            unimplemented!();
        } else if *addr < 0x4018 {
            // read from APU or I/O
            unimplemented!();
        } else if *addr < 0x401f {
            // not allowed!
            unreachable!();
        } else if *addr < 0x8000 {
            // idk
            unimplemented!();
        } else if *addr < 0xbfff {
            // read the ROM through the mapper
            self.rom.as_ref().unwrap().prg_rom[(*addr & 0x3fff) as usize]
        } else if *addr < 0xffff {
            self.rom.as_ref().unwrap().prg_rom[((*addr - 0x4000) & 0x3fff) as usize]
        } else {
            panic!("address is out of bounds of CPU memory");
        }
    }

    // Writes to a location in the CPU memory map.
    pub fn cpu_write(&mut self, addr: &u16, val: u8) {
        println!("{:x}", addr);
        if *addr < 0x2000 {
            self.cpuRam[(*addr & 0x07ff) as usize] = val;
        } else if *addr < 0x3fff {
            // write to a PPU register
            unimplemented!();
        } else if *addr < 0x4018 {
            // write to APU or I/O
            unimplemented!();
        } else if *addr < 0x401f {
            // not allowed!
            unreachable!();
        } else if *addr < 0xffff {
            // write to ROM, is this allowed??
            // well part of this is the stack... i think?
            // no, stack lives in 0x0100 to 0x01ff
            unreachable!();
        } else {
            panic!("address is out of bounds of CPU memory");
        }
    }

    pub fn clock(&mut self) {}
}
