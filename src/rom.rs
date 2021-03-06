use crate::mapper::*;
use bitfield::*;

const NES_HEADER: [u8; 4] = [0x4e, 0x45, 0x53, 0x1a];

bitfield! {
    pub struct Header([u8]);
    impl Debug;
    u8;
    // in 16kb units
    prg_rom_size, _: 7, 0;
    // in 8kb units
    chr_rom_size, _: 15, 8;
    pub mirroring, _: 16;
    battery_backed, _: 17;
    trainer_presence, _: 18;
    ignore_mirroring, _: 19;
    mapper_id_lower_nibble, _: 23, 20;
    vs_unisystem, _: 24;
    playchoice10, _: 25;
    nes20, _: 27, 26;
    mapper_id_upper_nibble, _: 31, 28;
    flags8, _: 39, 32;
    flags9, _: 47, 40;
    flags10, _: 55, 48;
}

impl<T: AsRef<[u8]> + AsMut<[u8]>> Header<T> {
    pub fn get_mapper_id(&self) -> u8 {
        self.mapper_id_upper_nibble() << 4 | self.mapper_id_lower_nibble()
    }
}

#[derive(Debug)]
pub struct NesRom {
    pub header: Header<[u8; 12]>,
    pub trainer: Option<Vec<u8>>,
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub mapper: Option<std::rc::Rc<dyn Mapper>>,
}

impl NesRom {
    pub fn new() -> Self {
        NesRom {
            header: Header([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]),
            trainer: None,
            prg_rom: vec![0; 0x4000],
            chr_rom: vec![0; 0x2000],
            mapper: None,
        }
    }

    pub fn cpu_read(&self, addr: &u16) -> u8 {
        self.prg_rom[self.mapper.as_ref().unwrap().cpu_map_addr(&addr) as usize]
    }

    pub fn cpu_write(&mut self, addr: &u16, val: u8) {
        self.prg_rom[self.mapper.as_ref().unwrap().cpu_map_addr(&addr) as usize] = val;
    }

    pub fn ppu_read(&self, addr: &u16) -> u8 {
        self.chr_rom[self.mapper.as_ref().unwrap().ppu_map_addr(&addr) as usize]
    }

    pub fn ppu_write(&mut self, addr: &u16, val: u8) {
        self.chr_rom[self.mapper.as_ref().unwrap().ppu_map_addr(&addr) as usize] = val;
    }
}

fn read_header<I>(file_stream: &mut I, rom: &mut NesRom)
where
    I: Iterator<Item = u8>,
{
    let nes_head = file_stream.take(4).collect::<Vec<u8>>();
    if *nes_head != NES_HEADER {
        panic!("not an nes file");
    }
    let mut header: [u8; 12] = [0; 12];
    let header_from_file: Vec<u8> = file_stream.take(12).collect();
    header.copy_from_slice(&*header_from_file);
    rom.header = Header(header);

    // also load the correct mapper
    rom.mapper = match rom.header.get_mapper_id() {
        0 => Some(std::rc::Rc::new(Mapper000::new(rom.header.prg_rom_size()))),
        _ => unimplemented!(),
    };
}

fn read_trainer<I>(file_stream: &mut I, rom: &mut NesRom)
where
    I: Iterator<Item = u8>,
{
    if rom.header.trainer_presence() {
        let mut trainer: [u8; 512] = [0; 512];
        let trainer_from_file: Vec<u8> = file_stream.take(512).collect();
        trainer.copy_from_slice(&*trainer_from_file);
        rom.trainer = Some(trainer_from_file);
    } else {
        rom.trainer = None;
    }
}

macro_rules! read_rom_data {
    ($func_name: ident, $prop: ident, $size: ident, $unit_size: expr) => {
        pub fn $func_name<I>(file_stream: &mut I, rom: &mut NesRom)
        where
            I: Iterator<Item = u8>,
        {
            if rom.header.$size() > 0 {
                let rom_data = file_stream
                    .take($unit_size * rom.header.$size() as usize)
                    .collect();
                rom.$prop = rom_data;
            }
        }
    };
}

read_rom_data!(read_prg_rom, prg_rom, prg_rom_size, 0x4000);
read_rom_data!(read_chr_rom, chr_rom, chr_rom_size, 0x2000);

pub fn read_nesrom<'a>(filename: String) -> NesRom {
    let mut nesrom = NesRom::new();
    let mut rom_bytes_iter = std::fs::read(filename)
        .expect("Could not read ROM file.")
        .into_iter();
    read_header(&mut rom_bytes_iter, &mut nesrom);
    read_trainer(&mut rom_bytes_iter, &mut nesrom);
    read_prg_rom(&mut rom_bytes_iter, &mut nesrom);
    read_chr_rom(&mut rom_bytes_iter, &mut nesrom);
    nesrom
}
