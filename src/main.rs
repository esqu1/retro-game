mod bus;
mod cpu;
mod isa;
mod parser;

use crate::bus::Bus;
use crate::cpu::Cpu;
fn main() {
    let rom = parser::read_nesrom(String::from("nestest.nes"));
    let mut bus = Bus::init();

    bus.install_rom(rom);
    let mut cpu = Cpu::init(bus);
    cpu.registers.pc = 0xc000;

    loop {
        cpu.clock();
    }
}
