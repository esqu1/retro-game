use crate::rom::NesRom;
use bitfield::*;
use sdl2::pixels::Color;
use sdl2::render::WindowCanvas;

pub struct PaletteColors(Vec<Color>);

impl PaletteColors {
    pub fn from_pal(pal_file: &str) -> Self {
        let colors = std::fs::read(pal_file).expect("Couldn't open palette file.");
        Self(
            colors
                .chunks(3)
                .map(|l| Color::RGB(l[0], l[1], l[2]))
                .collect(),
        )
    }
}

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
    registers: PpuRegs,
    // vram to store 2 nametables
    pub vram: [u8; 2048],
    pub palette: [u8; 0x20],
    palette_colors: PaletteColors,
    oam_mem: [u8; 256],
    pub rom: Option<&'a NesRom>,
    pub canvas: Option<&'a mut WindowCanvas>,
    addr_latch: Option<u16>,
    pub curr_scanline: u16,
    pub curr_col: u16,
    nametable_latch: u8,
    attr_latch: u8,
    lo_latch: u8,
    hi_latch: u8,
    pt_lo_shift_reg: u16,
    pt_hi_shift_reg: u16,
    palette_lo_shift_reg: u8,
    palette_hi_shift_reg: u8,
    palette_latch: u8,
    vram_addr: u16,
    pub nmi: bool,
}

impl<'a> Ppu<'a> {
    pub fn new() -> Self {
        Self {
            registers: PpuRegs {
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
            palette: [0; 0x20],
            palette_colors: PaletteColors::from_pal("ntscpalette.pal"),
            oam_mem: [0; 256],
            rom: None,
            canvas: None,
            addr_latch: None,
            curr_col: 0,
            curr_scanline: 261,
            nametable_latch: 0,
            attr_latch: 0, // i don't think we need this anymore
            hi_latch: 0,
            lo_latch: 0,
            pt_lo_shift_reg: 0,
            pt_hi_shift_reg: 0,
            palette_lo_shift_reg: 0,
            palette_hi_shift_reg: 0,
            palette_latch: 0,
            vram_addr: 0x2000, // iterates through the nametable aka the 8x8 tiles
            nmi: false,
        }
    }

    pub fn install_rom(&mut self, rom: &'a NesRom) {
        self.rom = Some(rom);
    }

    pub fn add_canvas(&mut self, canvas: &'a mut WindowCanvas) {
        self.canvas = Some(canvas);
    }

    fn get_operating_pix(&self, offset: u8) -> (u16, u16) {
        let mut col = self.curr_col + offset as u16;
        let mut row = self.curr_scanline;
        if col > 340 {
            col -= 340;
            row += 1;
        }
        (row, col)
    }

    fn draw_pixel(&mut self, palette_index: u16, palette_addr: u16) {
        let color_index = self.read(&(palette_addr + palette_index));
        let canvas = self.canvas.as_deref_mut().unwrap();
        canvas.set_draw_color(self.palette_colors.0[color_index as usize]);
        canvas
            .draw_point(sdl2::rect::Point::new(
                self.curr_col as i32,
                self.curr_scanline as i32,
            ))
            .expect("Could not write to canvas.");
    }

    pub fn clock(&mut self) {
        // draw, shift, then replenish
        // draw
        // it makes more sense to put the newer one in the lower bits, no?
        let palette_info =
            ((self.palette_hi_shift_reg & 0x80) >> 6) | ((self.palette_lo_shift_reg & 0x80) >> 7);
        let palette_addr = 0x3f00 | ((palette_info as u16) << 2);
        let pattern_data =
            ((self.pt_hi_shift_reg & 0x80) >> 6) | ((self.pt_lo_shift_reg & 0x80) >> 7);
        self.draw_pixel(pattern_data, palette_addr);

        let col = self.curr_col;
        if col < 337 {
            // shift
            self.palette_hi_shift_reg <<= 1;
            self.palette_lo_shift_reg <<= 1;
            self.pt_hi_shift_reg <<= 1;
            self.pt_lo_shift_reg <<= 1;
        }

        // this is where the real meat is
        if self.curr_scanline >= 240 && self.curr_scanline != 261 {
            // do nothing, unless we're at (1, 241) at which
            // we set the VBlank flag
            if self.curr_scanline == 241 && col == 1 {
                self.registers.ppu_status.set_vblank(true);
                if self.registers.ppu_ctrl.vblank_nmi() {
                    self.nmi = true;
                }
            }
        } else {
            // oh also, at (1, 261) we should say that the vblank is ended
            if self.curr_scanline == 261 && self.curr_col == 1 {
                self.registers.ppu_status.set_vblank(false);
            }
            // between 257 - 320 and 337-340 this scanline does nothing
            if (col >= 257 && col <= 320) || (col >= 337 && col <= 340) {
            } else {
                let end_offset = if col >= 321 && col <= 336 { 5 } else { 0 };
                // on odd columns, we actually do the dirty work
                match self.curr_col % 8 {
                    0 => {
                        if col != 336 {
                            // load the shift registers
                            self.palette_hi_shift_reg =
                                if (self.attr_latch >> 1) == 1 { 0xff } else { 0 };
                            self.palette_lo_shift_reg =
                                if (self.attr_latch & 1) == 1 { 0xff } else { 0 };
                            self.pt_hi_shift_reg &= 0xff;
                            self.pt_hi_shift_reg |= self.hi_latch as u16;
                            self.pt_lo_shift_reg &= 0xff;
                            self.pt_lo_shift_reg |= self.lo_latch as u16;
                        }
                    }
                    1 => {
                        // fetch nametable byte
                        let (px_row, px_col) = self.get_operating_pix(8 + end_offset);
                        let vram_addr = 0x2000 | (px_col >> 3) | ((px_row >> 3) << 5);
                        self.nametable_latch = self.read(&vram_addr);
                    }
                    3 => {
                        // fetch attr table byte
                        let (px_row, px_col) = self.get_operating_pix(6 + end_offset);
                        let vram_addr = 0x2000 | (px_col >> 3) | ((px_row >> 3) << 5);
                        let mut row = (vram_addr >> 2) & 0x7;
                        let mut col = (vram_addr >> 7) & 0x7;
                        let temp = row;
                        row = col;
                        col = temp;
                        let addr: u16 = (row << 3) as u16 | col as u16 | 0x23c0;
                        // this currently gives the whole byte for a 4x4 tile area
                        let attr_byte = self.read(&addr);
                        self.attr_latch = match (px_row % 32 < 16, px_col % 32 < 16) {
                            (true, true) => attr_byte & 0x3,
                            (true, false) => (attr_byte >> 2) & 0x3,
                            (false, true) => (attr_byte >> 4) & 0x3,
                            (false, false) => (attr_byte >> 6) & 0x3,
                        };
                    }
                    5 => {
                        // fetch low bg tile byte from pattern table
                        let (row, _) = self.get_operating_pix(4 + end_offset);
                        let addr = ((self.registers.ppu_ctrl.bg_pt_addr() as u16) << 12)
                            | ((self.nametable_latch as u16) << 4)
                            | ((row as u16) % 8);
                        self.lo_latch = self.read(&addr);
                    }
                    7 => {
                        // fetch high bg tile byte
                        let (row, _) = self.get_operating_pix(2 + end_offset);
                        let addr = ((self.registers.ppu_ctrl.bg_pt_addr() as u16) << 12)
                            | ((self.nametable_latch as u16) << 4)
                            | ((row as u16) % 8);
                        self.hi_latch = self.read(&(addr + 8));
                    }
                    _ => {}
                }
            }
            // even columns we rest
        }
        if self.curr_scanline == 241 && col == 0 {
            self.canvas
                .as_deref_mut()
                .unwrap()
                .set_draw_color(sdl2::pixels::Color::RGB(255, 0, 0));
            self.canvas
                .as_deref_mut()
                .unwrap()
                .draw_rect(sdl2::rect::Rect::new(0, 0, 8, 8));
            self.canvas
                .as_deref_mut()
                .unwrap()
                .draw_rect(sdl2::rect::Rect::new(64, 32, 32, 32));
            self.canvas
                .as_deref_mut()
                .unwrap()
                .draw_rect(sdl2::rect::Rect::new(32, 32, 32, 32));
            self.canvas.as_deref_mut().unwrap().present();
        }

        self.curr_col += 1;
        if self.curr_col == 341 {
            self.curr_scanline = (self.curr_scanline + 1) % 262;
            self.curr_col = 0;
        }
    }

    pub fn cpu_read(&mut self, addr: &u16) -> u8 {
        match *addr {
            0x0 => self.registers.ppu_ctrl.0,
            0x1 => self.registers.ppu_mask.0,
            0x2 => {
                self.addr_latch = None;
                self.registers.ppu_status.0
            }
            0x3 | 0x4 => unreachable!(),
            0x5 => self.registers.ppu_scroll,
            0x6 => self.registers.ppu_addr,
            0x7 => self.registers.ppu_data,
            _ => unreachable!(),
        }
    }

    pub fn cpu_write(&mut self, addr: &u16, val: u8) {
        match *addr {
            0x0 => self.registers.ppu_ctrl.0 = val,
            0x1 => self.registers.ppu_mask.0 = val,
            0x2 => self.registers.ppu_status.0 = val,
            0x3 | 0x4 => {} // for now assume unreachability
            0x5 => self.registers.ppu_scroll = val,
            0x6 => {
                if let Some(x) = self.addr_latch {
                    self.addr_latch = Some((x << 8) | (val as u16));
                } else {
                    self.addr_latch = Some(val as u16);
                }
                self.registers.ppu_addr = val;
            }
            0x7 => {
                if let Some(x) = self.addr_latch {
                    self.write(&x, val);
                    let inc = self.registers.ppu_ctrl.vram_addr_inc();
                    let inc2 = if inc { 32 } else { 1 };
                    self.addr_latch = Some(x + inc2);
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn read(&self, addr: &u16) -> u8 {
        if *addr <= 0x1fff {
            self.rom.as_ref().unwrap().ppu_read(addr)
        } else if *addr <= 0x3eff {
            self.vram[(*addr & 0xfff) as usize]
        } else if *addr <= 0x3fff {
            self.palette[(*addr & 0xff) as usize]
        } else {
            panic!("Out of range of PPU memory map");
        }
    }

    pub fn write(&mut self, addr: &u16, val: u8) {
        if *addr <= 0x1fff {
            // pattern memory, assume for now it's a rom and is unwritable
            unreachable!();
        } else if *addr <= 0x3eff {
            self.vram[(*addr & 0xfff) as usize] = val;
        } else if *addr <= 0x3fff {
            // palette tables
            self.palette[(*addr & 0xff) as usize] = val;
        }
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
