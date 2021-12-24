//! See https://wiki.nesdev.org/w/images/d/d1/Ntsc_timing.png
//! for an image of the frame timings.

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

bitfield! {
    pub struct VRamAddr(u16);
    impl Debug;
    coarse_x, set_coarse_x: 4, 0;
    coarse_y, set_coarse_y: 9, 5;
    nametable, set_nametable: 11,10;
    fine_y, set_fine_y: 14, 12;
}

impl VRamAddr {
    fn new() -> Self {
        Self(0)
    }
}

pub struct ScrollRegisters {
    vram_addr: VRamAddr,
    temp_vram_addr: VRamAddr,
    fine_x: u8,
    w: bool,
}

impl ScrollRegisters {
    fn new() -> Self {
        Self {
            vram_addr: VRamAddr::new(),
            temp_vram_addr: VRamAddr::new(),
            fine_x: 0,
            w: false,
        }
    }

    // From http://wiki.nesdev.com/w/index.php/PPU_scrolling
    fn inc_coarse_x(&mut self) {
        if (self.vram_addr.0 & 0x1f) == 31 {
            self.vram_addr.0 &= !0x001f;
            self.vram_addr.0 ^= 0x0400;
        } else {
            self.vram_addr.0 += 1;
        }
    }

    // From http://wiki.nesdev.com/w/index.php/PPU_scrolling
    fn inc_fine_y(&mut self) {
        if (self.vram_addr.0 & 0x7000) != 0x7000 {
            self.vram_addr.0 += 0x1000;
        } else {
            self.vram_addr.0 &= !0x7000;
            let mut y = (self.vram_addr.0 & 0x3e0) >> 5;
            if y == 29 {
                y = 0;
                self.vram_addr.0 ^= 0x0800;
            } else if y == 31 {
                y = 0;
            } else {
                y += 1;
            }
            self.vram_addr.0 = (self.vram_addr.0 & !0x03e0) | (y << 5);
        }
    }
}

pub struct PpuRegs {
    ppu_ctrl: PpuCtrl,     // $2000
    pub ppu_mask: PpuMask, // $2001
    ppu_status: PpuStatus, // $2002
    oam_addr: (),          // $2003
    oam_data: (),          // $2004
    pub oam_dma: u8,       // $4014
}

// 2 pattern tables: 0x0000-0x1fff, 16 bytes for each 8x8 tile
// 4 nametables: 0x2000-2fff
// - only use two nametables, specify vertical or horizontal mirroring
// an 64B attribute table for each nametable: first one starts at 0x23c0
// 3000-3eff is mirror of 2000-2eff
// palettes from 3f00 to 3f0f for background, 3f10 to 3f1f for sprites

#[derive(Default)]
struct BackgroundLatch {
    nametable_latch: u8, // saves a pattern table address from nametable, for bg rendering
    attr_latch: u8,      // stores attribute table latch
    lo_latch: u8,        // low byte latch for bg rendering
    hi_latch: u8,        // high byte latch for bg rendering
}

#[derive(Default)]
struct BackgroundShiftRegs {
    pt_lo_shift_reg: u16, // for drawing from pt, contains data for next two tiles
    pt_hi_shift_reg: u16, // same here
    palette_lo_shift_reg: u16, // contains the palette info for drawing bg
    palette_hi_shift_reg: u16, // same
}

impl BackgroundShiftRegs {
    fn shift(&mut self) {
        self.palette_hi_shift_reg <<= 1;
        self.palette_lo_shift_reg <<= 1;
        self.pt_hi_shift_reg <<= 1;
        self.pt_lo_shift_reg <<= 1;
    }
}

#[derive(Default)]
struct SpriteEvaluationVars {
    scan_n: u8,            // used for scanning through the sprites
    scan_m: u8,            // used for scanning through each sprite byte
    secondary_oam_ptr: u8, // used for pointing to which secondary oam byte to write to
    cycles: i8,            // a ctr for how many more cycles to wait for writing to secondary oam
    alt_latch: u8,         // for writing across odd and even cycles during secondary OAM writing
    has_sprite_zero: bool, // detects the presence of sprite 0 in secondary OAM, for sprite 0 hit
}

impl SpriteEvaluationVars {
    fn reset(&mut self) {
        self.scan_m = 0;
        self.scan_n = 0;
        self.secondary_oam_ptr = 0;
        self.has_sprite_zero = false;
    }
}

#[derive(Default)]
struct SpriteRenderFields {
    sprite_hi_shift_reg: [u8; 8], // contains the sprites to write next scanline
    sprite_lo_shift_reg: [u8; 8], // same
    sprite_attr_latch: [u8; 8],   // contains the palette info
    sprite_x_ctr: [i16; 8],       // ctrs for when to draw each sprite
}

impl SpriteRenderFields {
    fn reset(&mut self) {
        self.sprite_hi_shift_reg = [0; 8];
        self.sprite_lo_shift_reg = [0; 8];
        self.sprite_attr_latch = [0; 8];
        self.sprite_x_ctr = [0xff; 8];
    }

    fn get_palette_index(&self, i: usize) -> u8 {
        let attr = self.sprite_attr_latch[i];
        let mut new_x = (self.sprite_x_ctr[i] * -1) as u8;
        // is the sprite flipped horizontally?
        if ((attr >> 6) & 0x1) == 1 {
            new_x = 7 - new_x;
        }
        let lo = (self.sprite_lo_shift_reg[i] >> (7 - new_x)) & 0x1;
        let hi = (self.sprite_hi_shift_reg[i] >> (7 - new_x)) & 0x1;
        (hi << 1) | lo
    }

    fn get_first_opaque_sprite(&self) -> Option<usize> {
        // returns the index
        (0..8)
            .map(|i| {
                let ctr = self.sprite_x_ctr[i];
                let in_range = -7 <= ctr && ctr <= 0;
                let palette_index = self.get_palette_index(i);
                in_range && (palette_index != 0)
            })
            .position(|x| x)
    }
}
pub struct Ppu<'a> {
    pub registers: PpuRegs,
    // vram to store 2 nametables
    pub vram: [u8; 2048],
    pub palette: [u8; 0x20],
    palette_colors: PaletteColors,
    oam_mem: [u8; 256],
    secondary_oam: [u8; 32],
    pub rom: Option<&'a mut NesRom>,
    pub canvas: Option<&'a mut WindowCanvas>,
    ppu_data_buffer: u8,    // ppu reads from $2007 are delayed one cycle
    pub curr_scanline: u16, // current scanline
    pub curr_col: u16,      // column
    pub nmi: bool,          // represents if we want to trigger a nmi
    addr_latch: Option<u16>,

    scroll_regs: ScrollRegisters,
    bg_latch: BackgroundLatch,
    bg_shift_regs: BackgroundShiftRegs,
    sprite_eval: SpriteEvaluationVars,
    sprite_render: SpriteRenderFields,
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
                oam_dma: 0,
            },
            vram: [0; 2048],
            palette: [0; 0x20],
            palette_colors: PaletteColors::from_pal("ntscpalette.pal"),
            oam_mem: [0x0f; 256],
            secondary_oam: [0xff; 32],
            rom: None,
            canvas: None,
            ppu_data_buffer: 0,
            curr_col: 0,
            curr_scanline: 261,
            addr_latch: None,
            bg_latch: BackgroundLatch::default(),
            bg_shift_regs: BackgroundShiftRegs::default(),
            nmi: false,
            scroll_regs: ScrollRegisters::new(),
            sprite_eval: SpriteEvaluationVars::default(),
            sprite_render: SpriteRenderFields::default(),
        }
    }

    pub fn install_rom(&mut self, rom: &'a mut NesRom) {
        self.rom = Some(rom);
    }

    pub fn add_canvas(&mut self, canvas: &'a mut WindowCanvas) {
        self.canvas = Some(canvas);
    }

    fn draw_pixel(&mut self, palette_index: u16, palette_addr: u16) {
        let mut new_palette_addr = palette_addr + palette_index;
        if new_palette_addr % 4 == 0 {
            new_palette_addr = 0x3f00;
        }
        let color_index = self.read(&new_palette_addr);
        let canvas = self.canvas.as_deref_mut().unwrap();
        canvas.set_draw_color(self.palette_colors.0[color_index as usize]);
        canvas
            .draw_rect(sdl2::rect::Rect::new(
                2 * self.curr_col as i32,
                2 * self.curr_scanline as i32,
                2,
                2,
            ))
            .expect("Could not write to canvas.");
    }

    pub fn clock(&mut self) {
        let col = self.curr_col;
        let scanline = self.curr_scanline;

        if col == 257 {
            // reset the registers
            self.sprite_eval.reset();
            self.sprite_render.reset();
        }

        // check if we're allowed to render sprites now
        if self.registers.ppu_mask.show_sprites() {
            if scanline <= 239 {
                if col >= 1 && col <= 64 && (col % 2 == 1) {
                    // at the beginning of each scanline, clear secondary OAM
                    self.secondary_oam_write(&((col as u8) >> 1), 0xff);
                } else if col >= 65 && col <= 256 {
                    // sprite evaluation
                    if self.sprite_eval.scan_n == 64 {
                    } else if self.sprite_eval.secondary_oam_ptr == 32 {
                        // if we have enough sprites, check to see if there are any more
                        let potential_overflow = self
                            .oam_read(&((self.sprite_eval.scan_n << 2) | self.sprite_eval.scan_m));
                        let height = if self.registers.ppu_ctrl.sprite_size() {
                            16
                        } else {
                            8
                        };
                        if scanline - (potential_overflow as u16) < height {
                            // we're in range, set the overflow flag
                            self.registers.ppu_status.set_sprite_overflow(true);
                            self.sprite_eval.scan_n = 64;
                        } else {
                            self.sprite_eval.scan_m += 1;
                            if self.sprite_eval.scan_m == 4 {
                                self.sprite_eval.scan_n += 1;
                                self.sprite_eval.scan_m = 0;
                            }
                        }
                    } else if self.sprite_eval.cycles == 0 {
                        let eval_sprite_y = self.oam_read(&(self.sprite_eval.scan_n << 2)) + 1;
                        let height = if self.registers.ppu_ctrl.sprite_size() {
                            16
                        } else {
                            8
                        };
                        if scanline >= (eval_sprite_y as u16)
                            && scanline - (eval_sprite_y as u16) < height
                        {
                            self.sprite_eval.cycles += 7;
                            self.sprite_eval.alt_latch = eval_sprite_y;
                            if self.sprite_eval.scan_n == 0 {
                                // if this is sprite 0, then it will appear first
                                self.sprite_eval.has_sprite_zero = true;
                            }
                        } else {
                            self.sprite_eval.cycles = -1;
                        }
                    } else if self.sprite_eval.cycles == -1 {
                        self.sprite_eval.cycles = 0;
                        self.sprite_eval.scan_n += 1;
                    } else {
                        if col % 2 == 1 {
                            // read
                            self.sprite_eval.alt_latch = self.oam_read(
                                &((self.sprite_eval.scan_n << 2) | self.sprite_eval.scan_m),
                            );
                        } else {
                            // write
                            let addr = self.sprite_eval.secondary_oam_ptr.clone();
                            self.secondary_oam_write(&addr, self.sprite_eval.alt_latch);
                            self.sprite_eval.secondary_oam_ptr += 1;
                            self.sprite_eval.scan_m += 1;
                            if self.sprite_eval.scan_m == 4 {
                                self.sprite_eval.scan_n += 1;
                                self.sprite_eval.scan_m = 0;
                            }
                        }
                        self.sprite_eval.cycles -= 1;
                    }
                } else if col >= 257 && col <= 320 {
                    let n = (((col - 1) % 64) >> 3) as u8;
                    let sec_oam_addr = n << 2;
                    // load the shift registers with the sprite palette info
                    match col % 8 {
                        1 => {
                            // load y into the pattern table register for now
                            self.sprite_render.sprite_hi_shift_reg[n as usize] =
                                self.secondary_oam_read(&sec_oam_addr);
                        }
                        5 => {
                            // load the pattern table data into the shift registers
                            let y = self.sprite_render.sprite_hi_shift_reg[n as usize] as u16;
                            if y != 0xff {
                                let pt_byte = self.secondary_oam_read(&(sec_oam_addr + 1));
                                let attr = self.sprite_render.sprite_attr_latch[n as usize];
                                let offset = if (attr >> 7) == 1 {
                                    7 - (scanline - y)
                                } else {
                                    scanline - y
                                };
                                let addr = ((pt_byte as u16) << 4) + offset;
                                let lo_byte = self.read(&addr);
                                let hi_byte = self.read(&(addr + 8));
                                self.sprite_render.sprite_lo_shift_reg[n as usize] = lo_byte;
                                self.sprite_render.sprite_hi_shift_reg[n as usize] = hi_byte;
                            }
                        }
                        3 => {
                            // load attribute data
                            self.sprite_render.sprite_attr_latch[n as usize] =
                                self.secondary_oam_read(&(sec_oam_addr + 2));
                        }
                        7 => {
                            // load the X coordinate counter to be decremented
                            self.sprite_render.sprite_x_ctr[n as usize] =
                                self.secondary_oam_read(&(sec_oam_addr + 3)) as i16 + 1;
                        }
                        _ => {}
                    }
                }
            }
        }

        ////////////
        // DRAWING
        ////////////
        if col <= 255 && scanline <= 239 {
            let fine_x = self.scroll_regs.fine_x;
            let palette_info = ((self.bg_shift_regs.palette_hi_shift_reg >> (14 - fine_x)) & 0x2)
                | ((self.bg_shift_regs.palette_lo_shift_reg >> (15 - fine_x)) & 1);
            let palette_addr = 0x3f00 | ((palette_info as u16) << 2);

            // select bit of pattern table based on fine_x scroll
            let pattern_data = ((self.bg_shift_regs.pt_hi_shift_reg >> (14 - fine_x)) & 0x2)
                | ((self.bg_shift_regs.pt_lo_shift_reg >> (15 - fine_x)) & 1);
            if !self.registers.ppu_mask.show_sprites() {
                self.draw_pixel(pattern_data, palette_addr);
            } else {
                if let Some(top_most_sprite) = self.sprite_render.get_first_opaque_sprite() {
                    let attr = self.sprite_render.sprite_attr_latch[top_most_sprite];
                    let palette_index = self.sprite_render.get_palette_index(top_most_sprite);
                    // if we were originally going to draw a transparent background,
                    // then priority doesn't matter here
                    if (palette_addr | pattern_data) % 4 == 0 || (attr & 0x20) == 0 {
                        if palette_index == 0 {
                            // if the sprite is transparent at this pixel
                            self.draw_pixel(pattern_data, palette_addr);
                        } else {
                            let palette = 0x3f10 | ((attr & 0x3) << 2) as u16;
                            self.draw_pixel(palette_index as u16, palette);
                        }
                    } else {
                        self.draw_pixel(pattern_data, palette_addr);
                    }
                    // sprite pixel is opaque, check to see if background is also opaque;
                    // if this is sprite 0, then we have a sprite 0 hit
                    if self.sprite_eval.has_sprite_zero
                        && top_most_sprite == 0
                        && palette_index != 0
                        && (palette_addr | pattern_data) % 4 != 0
                    {
                        self.registers.ppu_status.set_sprite_0_hit(true);
                    }
                } else {
                    self.draw_pixel(pattern_data, palette_addr);
                }
            }
            for i in 0..8 {
                self.sprite_render.sprite_x_ctr[i] -= 1;
            }
        }
        // only scan if we're in the visible area
        if scanline >= 240 && scanline != 261 {
            // do nothing, unless we're at (1, 241) at which
            // we set the VBlank flag
            if scanline == 241 && col == 1 {
                self.registers.ppu_status.set_vblank(true);
                if self.registers.ppu_ctrl.vblank_nmi() {
                    self.nmi = true;
                }
            }
        } else {
            // oh also, at (1, 261) we should say that the vblank is ended
            if scanline == 261 && self.curr_col == 1 {
                self.registers.ppu_status.set_vblank(false);
                // clear sprite 0 hit
                self.registers.ppu_status.set_sprite_0_hit(false);
                self.sprite_eval.has_sprite_zero = false;
            }

            if self.registers.ppu_mask.show_bg() || self.registers.ppu_mask.show_sprites() {
                // between 257 - 320 and 337-340 this scanline does nothing
                if (col >= 257 && col <= 320) || (col >= 337 && col <= 340) {
                } else {
                    // on odd columns, we actually do the dirty work
                    match self.curr_col % 8 {
                        // every 8 pixels, we reload the shift registers with whatever
                        // is stored in the latches
                        0 => {
                            if col != 0 {
                                // load the shift registers
                                self.bg_shift_regs.palette_hi_shift_reg |=
                                    if (self.bg_latch.attr_latch >> 1) == 1 {
                                        0xff
                                    } else {
                                        0
                                    };
                                self.bg_shift_regs.palette_lo_shift_reg |=
                                    if (self.bg_latch.attr_latch & 1) == 1 {
                                        0xff
                                    } else {
                                        0
                                    };
                                self.bg_shift_regs.pt_hi_shift_reg |= self.bg_latch.hi_latch as u16;
                                self.bg_shift_regs.pt_lo_shift_reg |= self.bg_latch.lo_latch as u16;
                                self.scroll_regs.inc_coarse_x();
                            }
                        }
                        1 => {
                            // fetch nametable byte
                            let vram_addr = 0x2000 | (self.scroll_regs.vram_addr.0 & 0xfff);
                            self.bg_latch.nametable_latch = self.read(&vram_addr);
                        }
                        3 => {
                            // fetch attr table byte
                            // let (px_row, px_col) = self.get_operating_pix(14 + end_offset);
                            let v = self.scroll_regs.vram_addr.0;
                            let addr: u16 =
                                (v & 0x0c00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07) | 0x23c0;
                            let attr_byte = self.read(&addr);
                            let coarse_x = self.scroll_regs.vram_addr.coarse_x();
                            let coarse_y = self.scroll_regs.vram_addr.coarse_y();
                            self.bg_latch.attr_latch =
                                match ((coarse_y & 0b11) < 2, (coarse_x & 0b11) < 2) {
                                    (true, true) => attr_byte & 0x3,
                                    (true, false) => (attr_byte >> 2) & 0x3,
                                    (false, true) => (attr_byte >> 4) & 0x3,
                                    (false, false) => (attr_byte >> 6) & 0x3,
                                };
                        }
                        5 => {
                            // fetch low bg tile byte from pattern table
                            let addr = ((self.registers.ppu_ctrl.bg_pt_addr() as u16) << 12)
                                | ((self.bg_latch.nametable_latch as u16) << 4)
                                | self.scroll_regs.vram_addr.fine_y();
                            self.bg_latch.lo_latch = self.read(&addr);
                        }
                        7 => {
                            // fetch high bg tile byte
                            let addr = ((self.registers.ppu_ctrl.bg_pt_addr() as u16) << 12)
                                | ((self.bg_latch.nametable_latch as u16) << 4)
                                | self.scroll_regs.vram_addr.fine_y();
                            self.bg_latch.hi_latch = self.read(&(addr + 8));
                        }
                        _ => {}
                    }
                }
                // we increment fine y when we finish a scanline
                if col == 256 {
                    self.scroll_regs.inc_fine_y();
                }

                // transfer the x position over to vram
                if col == 257 {
                    self.scroll_regs
                        .vram_addr
                        .set_coarse_x(self.scroll_regs.temp_vram_addr.coarse_x());
                    self.scroll_regs
                        .vram_addr
                        .set_bit(10, (self.scroll_regs.temp_vram_addr.nametable() & 1) == 1);
                }

                if scanline == 261 && col >= 280 && col <= 304 {
                    self.scroll_regs
                        .vram_addr
                        .set_fine_y(self.scroll_regs.temp_vram_addr.fine_y());
                    self.scroll_regs
                        .vram_addr
                        .set_coarse_y(self.scroll_regs.temp_vram_addr.coarse_y());
                    self.scroll_regs
                        .vram_addr
                        .set_bit(11, (self.scroll_regs.temp_vram_addr.nametable() >> 1) == 1);
                }
            }
        }

        if scanline == 241 && col == 0 {
            self.canvas
                .as_deref_mut()
                .unwrap()
                .set_draw_color(Color::RGB(255, 0, 0));
            self.canvas.as_deref_mut().unwrap().present();
        }

        // shift
        if col < 336 {
            self.bg_shift_regs.shift();
        }

        self.curr_col += 1;
        if self.curr_col == 341 {
            self.curr_scanline = (scanline + 1) % 262;
            self.curr_col = 0;
        }
    }

    pub fn cpu_read(&mut self, addr: &u16) -> u8 {
        match *addr {
            0x0 => self.registers.ppu_ctrl.0,
            0x1 => self.registers.ppu_mask.0,
            0x2 => {
                self.addr_latch = None;
                self.scroll_regs.w = false;
                self.registers.ppu_status.0
            }
            0x3 | 0x4 => unreachable!(),
            0x7 => {
                let x2 = self.scroll_regs.vram_addr.0;
                let mut val = self.ppu_data_buffer;
                self.ppu_data_buffer = self.read(&x2);
                if x2 >= 0x3f00 {
                    val = self.ppu_data_buffer;
                }
                if (self.registers.ppu_mask.show_bg() || self.registers.ppu_mask.show_sprites())
                    && (self.curr_scanline == 261 || self.curr_scanline <= 239)
                {
                    self.scroll_regs.inc_coarse_x();
                    self.scroll_regs.inc_fine_y();
                } else {
                    let inc2_ = self.registers.ppu_ctrl.vram_addr_inc();
                    let inc22 = if inc2_ { 32 } else { 1 };
                    self.scroll_regs.vram_addr.0 = x2 + inc22;
                }
                val
            }
            _ => unreachable!(),
        }
    }

    pub fn cpu_write(&mut self, addr: &u16, val: u8) {
        match *addr {
            0x0 => {
                self.registers.ppu_ctrl.0 = val;
                self.scroll_regs
                    .temp_vram_addr
                    .set_nametable((val & 0x3) as u16);
            }
            0x1 => self.registers.ppu_mask.0 = val,
            0x2 => self.registers.ppu_status.0 = val,
            0x3 | 0x4 => {} // for now assume unreachability
            0x5 => {
                if !self.scroll_regs.w {
                    // first write
                    self.scroll_regs
                        .temp_vram_addr
                        .set_coarse_x((val >> 3) as u16);
                    self.scroll_regs.fine_x = val & 0x7;
                    self.scroll_regs.w = true;
                } else {
                    self.scroll_regs
                        .temp_vram_addr
                        .set_fine_y((val & 0x7) as u16);
                    self.scroll_regs
                        .temp_vram_addr
                        .set_coarse_y((val >> 3) as u16);
                    self.scroll_regs.w = false;
                }
            }
            0x6 => {
                if !self.scroll_regs.w {
                    self.scroll_regs.temp_vram_addr.0 =
                        ((val as u16) << 8) | (self.scroll_regs.temp_vram_addr.0 & 0xff);
                    self.scroll_regs.w = true;
                } else {
                    self.scroll_regs.temp_vram_addr.0 =
                        (self.scroll_regs.temp_vram_addr.0 & 0xff00) | (val as u16);
                    self.scroll_regs.w = false;
                    self.scroll_regs.vram_addr.0 = self.scroll_regs.temp_vram_addr.0;
                }
            }
            0x7 => {
                let x2 = self.scroll_regs.vram_addr.0;
                self.write(&x2, val);
                if (self.registers.ppu_mask.show_bg() || self.registers.ppu_mask.show_sprites())
                    && (self.curr_scanline == 261 || self.curr_scanline <= 239)
                {
                    self.scroll_regs.inc_coarse_x();
                    self.scroll_regs.inc_fine_y();
                } else {
                    let inc2_ = self.registers.ppu_ctrl.vram_addr_inc();
                    let inc22 = if inc2_ { 32 } else { 1 };
                    self.scroll_regs.vram_addr.0 = x2 + inc22;
                }
            }
            _ => unreachable!(),
        }
    }

    /// Reads from the PPU's internal VRAM space. The VRAM of a PPU takes up 16KB
    /// (until $3FFF).
    /// - $0000 to $1FFF store pattern tables, which are mapped to in the ROM.
    /// - $2000 to $2FFF store nametables, which is where the PPU reads from to render.
    /// - $3000 to $3EFF mirror the previous region.
    /// - $3F00 to $3F1F represents the palette RAM.
    /// - $3F20 to $3FFF mirror the previous region.
    pub fn read(&self, addr: &u16) -> u8 {
        if *addr <= 0x1fff {
            // Pattern tables
            // TODO: make this not read directly from ROM
            self.rom.as_ref().unwrap().ppu_read(addr)
        } else if *addr <= 0x3eff {
            // Name tables
            let vert_mirroring = self.rom.as_ref().unwrap().header.mirroring();
            if vert_mirroring {
                // 2800 -> 2000, 2c00 -> 2400
                self.vram[(*addr & 0x7ff) as usize]
            } else {
                // TODO: properly implement vertical mirroring
                // 2400 -> 2000, 2c00 -> 2800
                // let mut a = *addr;
                // if a >= 0x3000 {
                //     a -= 0x1000;
                // }
                // if a >= 0x2400 && a < 0x2c00 {
                //     a -= 0x400;
                // } else if a >= 0x2c00 {
                //     a -= 0x800;
                // }
                // self.vram[(a - 0x2000) as usize]
                self.vram[(*addr & 0x7ff) as usize]
            }
        } else if *addr <= 0x3fff {
            // Palette RAM
            let mut palette_index = (*addr & 0x1f) as usize;
            if *addr % 4 == 0 && *addr >= 0x3f10 {
                palette_index -= 0x10;
            }
            self.palette[palette_index]
        } else {
            panic!("Out of range of PPU memory map");
        }
    }

    pub fn write(&mut self, addr: &u16, val: u8) {
        if *addr <= 0x1fff {
            // pattern memory, assume for now it's a rom and is unwritable
            self.rom.as_deref_mut().unwrap().ppu_write(addr, val);
        } else if *addr <= 0x3eff {
            let vert_mirroring = self.rom.as_ref().unwrap().header.mirroring();
            if vert_mirroring {
                self.vram[(*addr & 0x7ff) as usize] = val;
            } else {
                let mut a = *addr;
                // if a >= 0x3000 {
                //     a -= 0x1000;
                // }
                // if a >= 0x2400 && a < 0x2c00 {
                //     a -= 0x400;
                // } else if a >= 0x2c00 {
                //     a -= 0x800;
                // }
                // self.vram[(a - 0x2000) as usize] = val;
                self.vram[(*addr & 0x7ff) as usize] = val;
            }
        } else if *addr <= 0x3fff {
            // palette tables
            let mut palette_index = (*addr & 0x1f) as usize;
            if palette_index % 4 == 0 && palette_index >= 0x10 {
                palette_index -= 0x10;
            }
            self.palette[palette_index] = val;
        }
    }

    fn oam_read(&self, addr: &u8) -> u8 {
        self.oam_mem[*addr as usize]
    }

    pub fn oam_write(&mut self, addr: &u8, val: u8) {
        self.oam_mem[*addr as usize] = val;
    }

    fn secondary_oam_read(&self, addr: &u8) -> u8 {
        self.secondary_oam[(*addr & 0x1f) as usize]
    }

    pub fn secondary_oam_write(&mut self, addr: &u8, val: u8) {
        self.secondary_oam[(*addr & 0x1f) as usize] = val;
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
