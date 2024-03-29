//! You can think of the bus as the central hub where all the components of the NES
//! sort of "dock" at. The bus serves as a read-write port for the registers of
//! the NES components, provides a unified mapping between them and also manages the
//! clocking of the overall system.

use crate::ppu::Ppu;
use crate::rom::NesRom;
use sdl2::render::WindowCanvas;
pub struct Bus<'a> {
    // rom: Option<&'b NesRom>,
    cpu_ram: [u8; 0x0800],
    pub ppu: Ppu<'a>,
    apu: (),
    pub nmi: bool,
    pub oam_cycles: u16,
    pub oam_addr_latch: u8, // lower byte pointer of OAM DMA writing
    oam_dma_data: u8,
    pub input_controller: [u8; 2],
    second_controller: u8,
    pub input_shift_reg: [u8; 2],
    pub strobe: bool,
    button: u8,
}

impl<'a, 'b> Bus<'a> {
    pub fn init() -> Self {
        Self {
            // rom: None,
            cpu_ram: [0x0; 0x0800],
            ppu: Ppu::new(),
            apu: (),
            nmi: false,
            oam_cycles: 0,
            oam_addr_latch: 0,
            oam_dma_data: 0,
            input_controller: [0, 0],
            input_shift_reg: [0, 0],
            strobe: false,
            button: 0,
            second_controller: 0,
        }
    }

    pub fn install_rom(&mut self, rom: &'b mut NesRom)
    where
        'b: 'a,
    {
        // self.rom = Some(&*rom);
        // let mut rom_ref = self.rom.unwrap();
        self.ppu.rom = Some(rom);
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
            if *addr >= 0x4016 || *addr <= 0x4017 {
                if *addr == 0x4017 {
                    // not implementing second controller now
                    (self.second_controller >= 8) as u8
                } else {
                    let button_val = if self.button < 8 {
                        (self.input_controller[(*addr & 1) as usize] & (1 << self.button) != 0)
                            as u8
                    } else {
                        1
                    };
                    if !self.strobe {
                        self.button += 1;
                    } else {
                        self.button = 0;
                    }
                    0x40 | button_val
                }
            } else {
                0
            }
        } else if *addr < 0x401f {
            // not allowed!
            unreachable!();
        } else if *addr < 0x8000 {
            // idk
            unimplemented!();
        } else {
            // read ROM through mapper
            self.ppu.rom.as_ref().unwrap().cpu_read(addr)
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
            if *addr == 0x4014 {
                self.ppu.registers.oam_dma = val;
                // OAM DMA register
                self.oam_cycles = 513;
            } else if *addr == 0x4016 {
                // write to APU or I/O
                self.strobe = (val & 1) == 1;
                if self.strobe {
                    self.button = 0;
                }
            }
        } else if *addr < 0x401f {
            // not allowed!
            unreachable!();
        } else if *addr < 0xffff {
            // write to ROM, is this allowed??
            unreachable!();
        } else {
            panic!("address is out of bounds of CPU memory");
        }
    }

    pub fn clock(&mut self) {
        if self.oam_cycles > 0 {
            if self.oam_cycles <= 512 {
                if self.oam_cycles % 2 == 0 {
                    // on even cycles, we read
                    let high_byte = self.ppu.registers.oam_dma;
                    let addr = ((high_byte as u16) << 8) | self.oam_addr_latch as u16;
                    self.oam_dma_data = self.cpu_read(&addr);
                    // subtract 1 from the y coordinate
                    if self.oam_cycles % 8 == 0 && self.oam_dma_data != 0 {
                        self.oam_dma_data -= 1;
                    }
                } else {
                    // on odd cycles, we write
                    self.ppu.oam_write(&self.oam_addr_latch, self.oam_dma_data);
                    if self.oam_addr_latch < 255 {
                        self.oam_addr_latch += 1;
                    }
                }
            }
            self.oam_cycles -= 1;
        } else {
            self.oam_addr_latch = 0;
        }
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
