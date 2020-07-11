mod bus;
mod cpu;
mod isa;
mod mapper;
mod ppu;
mod rom;

use crate::bus::Bus;
use crate::cpu::Cpu;
use sdl2;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::keyboard::Scancode;
use sdl2::pixels::Color;
use std::fs::File;
use std::io::Write;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

fn main() {
    let nes_name = std::env::args().nth(1).unwrap();
    let rom = rom::read_nesrom(nes_name);
    let mut bus = Bus::init();

    bus.install_rom(&rom);
    let mut cpu = Cpu::init(bus);

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

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window("demo", 256, 240)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();

    canvas.set_draw_color(Color::RGB(0, 255, 255));

    canvas.clear();
    canvas.present();
    cpu.bus.add_canvas(&mut canvas);
    let mut event_pump = sdl_context.event_pump().unwrap();
    'running: loop {
        let now = Instant::now();
        loop {
            cpu.clock();
            if cpu.bus.ppu.curr_scanline == 241 && cpu.bus.ppu.curr_col == 1 {
                break;
            }
        }
        let later = Instant::now();
        cpu.bus.input_controller[0] = 0;
        for event in event_pump.poll_iter() {
            match event {
                Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                _ => {}
            }
        }
        for event in event_pump.keyboard_state().pressed_scancodes() {
            match event {
                Scancode::F => cpu.bus.input_controller[0] |= 0x1, // A
                Scancode::D => cpu.bus.input_controller[0] |= 0x2, // B
                Scancode::U => cpu.bus.input_controller[0] |= 0x4, // Select
                Scancode::Return => cpu.bus.input_controller[0] |= 0x8, // Start
                Scancode::Up => cpu.bus.input_controller[0] |= 0x10,
                Scancode::Down => cpu.bus.input_controller[0] |= 0x20,
                Scancode::Left => cpu.bus.input_controller[0] |= 0x40,
                Scancode::Right => cpu.bus.input_controller[0] |= 0x80,
                _ => {} // },
            }
        }
        // let remaining = Duration::new(0, 16666666) - length;
        // std::thread::sleep(remaining);
    }
}
