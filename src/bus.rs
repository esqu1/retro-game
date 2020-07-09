use crate::ppu::Ppu;
use crate::rom::NesRom;
use sdl2::render::WindowCanvas;
pub struct Bus<'a, 'b> {
    rom: Option<&'b NesRom>,
    cpu_ram: [u8; 0x0800],
    pub ppu: Ppu<'a>,
    apu: (),
    pub nmi: bool,
}

impl<'a, 'b> Bus<'a, 'b> {
    pub fn init() -> Self {
        Self {
            rom: None,
            cpu_ram: [0x0; 0x0800],
            ppu: Ppu::new(),
            apu: (),
            nmi: false,
        }
    }

    pub fn install_rom(&mut self, rom: &'b NesRom)
    where
        'b: 'a,
    {
        self.rom = Some(rom);
        let rom_ref = self.rom.as_ref().unwrap();
        self.ppu.rom = Some(rom_ref);
    }

    pub fn add_canvas(&mut self, canvas: &'a mut WindowCanvas) {
        self.ppu.add_canvas(canvas);
    }

    // Reads from the CPU bus.
    pub fn cpu_read(&mut self, addr: &u16) -> u8 {
        if *addr < 0x2000 {
            self.cpu_ram[(*addr & 0x07ff) as usize]
        } else if *addr < 0x3fff {
            // read from PPU registers
            self.ppu.cpu_read(&(*addr & 0x7))
        } else if *addr < 0x4018 {
            // read from APU or I/O
            // unimplemented!();
            0
        } else if *addr < 0x401f {
            // not allowed!
            unreachable!();
        } else if *addr < 0x8000 {
            // idk
            unimplemented!();
        } else if *addr <= 0xffff {
            // read ROM through mapper
            self.rom.as_ref().unwrap().cpu_read(addr)
        } else {
            panic!(format!("address {} is out of bounds of CPU memory", *addr));
        }
    }

    // Writes to a location in the CPU memory map.
    pub fn cpu_write(&mut self, addr: &u16, val: u8) {
        if *addr < 0x2000 {
            self.cpu_ram[(*addr & 0x07ff) as usize] = val;
        } else if *addr < 0x3fff {
            // write to a PPU register
            self.ppu.cpu_write(&(*addr & 0x7), val);
        } else if *addr < 0x4018 {
            // println!("{:x}", *addr);
            if *addr == 0x4014 {
                // OAM DMA register
            } else {
                // write to APU or I/O
                // unimplemented!();
            }
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

    pub fn clock(&mut self) {
        // PPU runs 3 times faster than CPU
        for _ in 0..3 {
            self.ppu.clock();
            if self.ppu.nmi {
                self.nmi = true;
                self.ppu.nmi = false;
            }
        }
    }
}
