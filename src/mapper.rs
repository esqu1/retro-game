use std::fmt::Debug;

pub trait Mapper: Debug {
    fn cpu_map_addr(&self, addr: &u16) -> u16;
    fn ppu_map_addr(&self, addr: &u16) -> u16;
}

#[derive(Debug)]
pub struct Mapper000 {}

impl Mapper000 {
    pub fn new() -> Self {
        Self {}
    }
}

impl Mapper for Mapper000 {
    fn cpu_map_addr(&self, addr: &u16) -> u16 {
        *addr & 0x3fff
    }

    fn ppu_map_addr(&self, addr: &u16) -> u16 {
        if *addr <= 0x1fff {
            addr.clone()
        } else {
            addr.clone()
        }
    }
}
