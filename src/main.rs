mod bus;
mod cpu;
mod isa;
mod parser;

use crate::bus::Bus;
use crate::cpu::Cpu;
use std::boxed::Box;
use std::cell::RefCell;
use std::rc::Rc;
fn main() {
    let rom = parser::read_nesrom(String::from("nestest.nes"));
    println!("{:?}", rom.prg_rom);
    let mut bus = Bus::init();

    bus.installROM(rom);
    let mut cpu = Cpu::init(bus);
    cpu.registers.pc = 0xc000;
    // let mut bus = bus::Bus::init(cpu);

    loop {
        cpu.clock();
    }
}
