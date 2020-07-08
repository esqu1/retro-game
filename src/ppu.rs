use crate::parser::NesRom;
use bitfield::*;
use std::rc::Rc;

pub struct Color(u8, u8, u8);

// pub const PALETTE_2C02: [Color; 0x10] = [
//     Color(84, 84, 84),
//     Color(0, 30, 116),
//     Color(8, 16, 144),
//     Color(48, 0, 136),
//     Color(68, 0, 100),
//     Color(92, 0, 48),
//     Color(84, 4, 0),
//     Color(60, 24, 0),
//     Color(32, 42, 0),
//     Color(8, 58, 0),
//     Color(0, 64, 0),
//     Color(0, 60, 0),
//     Color(0, 50, 60),
//     Color(0, 0, 0),
// ];

bitfield! {
    pub struct PpuCtrl(u8);
    impl Debug;
    base_nt_addr, set_base_nt_addr: 1, 0;
    vram_addr_inc, set_vram_addr_inc: 2;
    sprite_pt_addr, set_sprite_pt_addr: 3;
    bg_pt_addr, set_bg_pt_addr: 4;
    sprite_size, set_sprite_size: 5;
    ppu_ms_select, set_ppu_ms_select: 6;
    vblank_nmi, set_vblank_nmi: 7;
}

bitfield! {
    pub struct PpuMask(u8);
    impl Debug;
    greyscale, set_greyscale: 0;
    left_8_bg, set_left_8_bg: 1;
    left_8_sprites, set_left_8_sprites: 2;
    show_bg, set_show_bg: 3;
    show_sprites, set_show_sprites: 4;
    red_emph, set_red_emph: 5;
    green_emph, set_green_emph: 6;
    blue_emph, set_blue_emph: 7;
}

bitfield! {
    pub struct PpuStatus(u8);
    impl Debug;
    lsb_prev, set_lsb_prev: 4, 0;
    sprite_overflow, set_sprite_overflow: 5;
    sprite_0_hit, set_sprite_0_hit: 6;
    vblank, set_vblank: 7;
}

pub struct PpuRegs {
    ppu_ctrl: PpuCtrl,     // $2000
    ppu_mask: PpuMask,     // $2001
    ppu_status: PpuStatus, // $2002
    oam_addr: (),          // $2003
    oam_data: (),          // $2004
    ppu_scroll: u8,        // $2005
    ppu_addr: u8,          // $2006
    ppu_data: u8,          // $2007
    oam_dma: u8,           // $4014
}

// 2 pattern tables: 0x0000-0x1fff, 16 bytes for each 8x8 tile
// 4 nametables: 0x2000-2fff
// - only use two nametables, specify vertical or horizontal mirroring
// an 64B attribute table for each nametable: first one starts at 0x23c0
// 3000-3eff is mirror of 2000-2eff
// palettes from 3f00 to 3f0f for background, 3f10 to 3f1f for sprites

pub struct Ppu<'a> {
    regs: PpuRegs,
    // vram to store 2 nametables
    vram: [u8; 2048],
    oam_mem: [u8; 256],
    pub rom: Option<&'a NesRom>,
}

impl<'a> Ppu<'a> {
    pub fn new() -> Self {
        Self {
            regs: PpuRegs {
                ppu_ctrl: PpuCtrl(0),
                ppu_mask: PpuMask(0),
                ppu_status: PpuStatus(0),
                oam_addr: (),
                oam_data: (),
                ppu_scroll: 0,
                ppu_addr: 0,
                ppu_data: 0,
                oam_dma: 0,
            },
            vram: [0; 2048],
            oam_mem: [0; 256],
            rom: None,
        }
    }

    pub fn install_rom(&mut self, rom: &'a NesRom) {
        self.rom = Some(rom);
    }

    pub fn clock(&mut self) {}

    pub fn read(&self, addr: &u16) -> u8 {
        self.rom.as_ref().unwrap().ppu_read(addr)
    }

    pub fn get_pattern_tables(&self) -> [[u8; 0x1000]; 2] {
        // repeatedly read from ROM
        let mut tables = [[0; 0x1000]; 2];
        for i in 0..0x2000 {
            tables[(i >> 12) & 1][i & 0xfff] = self.read(&(i as u16));
        }
        tables
    }

    pub fn pattern_tables_as_matrix(&self) -> ([[u8; 128]; 128], [[u8; 128]; 128]) {
        let tables: [[u8; 0x1000]; 2] = self.get_pattern_tables();
        let mut matrix1 = [[0; 128]; 128];

        // println!("{:?}", &tables[0][2000..]);
        for r in 0..16 {
            for c in 0..16 {
                for byte in 0..8 as usize {
                    let low_value = tables[0][256 * r + 16 * c + byte] as u8;
                    let high_value = tables[0][256 * r + 16 * c + byte + 8] as u8;
                    for fine in 0..8 as u8 {
                        let val = ((low_value >> (7 - fine)) & 1)
                            | (((high_value >> (7 - fine)) & 1) << 1);
                        matrix1[8 * r + byte][8 * c + (fine as usize)] = val;
                    }
                }
            }
        }
        let mut matrix2 = [[0; 128]; 128];
        for r in 0..16 {
            for c in 0..16 {
                for byte in 0..8 as usize {
                    let low_value = tables[1][256 * r + 16 * c + byte] as u8;
                    let high_value = tables[1][256 * r + 16 * c + byte + 8] as u8;
                    for fine in 0..8 as u8 {
                        let val = ((low_value >> (7 - fine)) & 1)
                            | (((high_value >> (7 - fine)) & 1) << 1);
                        matrix2[8 * r + byte][8 * c + (fine as usize)] = val;
                    }
                }
            }
        }
        (matrix1, matrix2)
    }
}
