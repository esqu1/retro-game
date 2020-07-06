mod bus;
mod cpu;
mod isa;
mod parser;

use crate::bus::Bus;
use crate::cpu::Cpu;
use std::boxed::Box;
use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;
fn main() {
    let rom = parser::read_nesrom(String::from("nestest.nes"));
    let mut bus = Bus::init();

    bus.installROM(rom);
    let mut cpu = Cpu::init(bus);
    cpu.registers.pc = 0xc000;

    let log = std::fs::File::create("neslog.log").unwrap();
    loop {
        cpu.clock();
    }
}
