mod bus;
mod cpu;
mod isa;
mod mapper;
mod parser;
mod ppu;

use crate::bus::Bus;
use crate::cpu::Cpu;
use std::fs::File;
use std::io::Write;
fn main() {
    let rom = parser::read_nesrom(String::from("nestest.nes"));
    println!("{:?}", rom.header);
    let mut bus = Bus::init();

    bus.install_rom(&rom);
    let mut cpu = Cpu::init(bus);
    cpu.registers.pc = 0xc000;

    let pat_table = cpu.bus.ppu.pattern_tables_as_matrix();

    let mut file = File::create("pt.ppm").unwrap();
    write!(file, "P3\n128 256\n255\n").unwrap();
    for k in 0..2 {
        let p = if k == 0 { pat_table.0 } else { pat_table.1 };
        for i in 0..128 {
            for j in 0..128 {
                let rgb = match p[i][j] {
                    0 => (76, 154, 136),
                    1 => (160, 20, 100),
                    2 => (236, 88, 180),
                    3 => (0, 30, 116),
                    _ => unreachable!(),
                };
                write!(file, "{} {} {}\n", rgb.0, rgb.1, rgb.2).unwrap();
            }
        }
    }

    loop {
        cpu.clock();
    }
}
