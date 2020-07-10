use std::fmt::Debug;

pub trait Mapper: Debug {
    fn cpu_map_addr(&self, addr: &u16) -> u16;
    fn ppu_map_addr(&self, addr: &u16) -> u16;
}

#[derive(Debug)]
pub struct Mapper000 {
    num_prg: u8,
}

impl Mapper000 {
    pub fn new(num_prg: u8) -> Self {
        Self { num_prg }
    }
}

impl Mapper for Mapper000 {
    fn cpu_map_addr(&self, addr: &u16) -> u16 {
        *addr & (if self.num_prg == 1 { 0x3fff } else { 0x7fff })
    }

    fn ppu_map_addr(&self, addr: &u16) -> u16 {
        if *addr <= 0x1fff {
            addr.clone()
        } else {
            addr.clone()
        }
    }
}
