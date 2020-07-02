use crate::isa::*;

pub struct CpuRegs {
    pub acc: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub s: u8,
    pub p: u8,
}

union Operand {
    byte: u8,
    two_bytes: u16,
}

impl CpuRegs {
    pub fn init() -> Self {
        Self {
            acc: 0x0,
            x: 0x0,
            y: 0x0,
            pc: 0x8000,
            s: 0xfd,
            p: 0x34,
        }
    }
}

pub struct Cpu {
    pub registers: CpuRegs,
    ram: [u8; 0x0800],
    cycles_left: u8,
    curr_instruction: &'static Option<(Opcode, AddressingMode, u8)>,
    bus: (),
}

impl Cpu {
    pub fn init() -> Self {
        Self {
            registers: CpuRegs::init(),
            ram: [0x0; 0x0800],
            cycles_left: 0,
            curr_instruction: &None,
            bus: (),
        }
    }
    pub fn read(&self, addr: &u16) -> u8 {
        if *addr < 0x2000 {
            self.ram[(*addr & 0x07ff) as usize]
        } else if *addr < 0x3fff {
            0
        // read from PPU registers
        } else if *addr < 0x4018 {
            0
        // read from APU or I/O
        } else if *addr < 0x401f {
            unreachable!();
        // not allowed!
        } else if *addr < 0xffff {
            0
        // read the ROM through the mapper
        } else {
            panic!("address is out of bounds of CPU memory");
        }
    }

    fn get_operand_as_val(&self, low_byte: u8, high_byte: u8, addr_mode: &AddressingMode) -> u8 {
        use crate::isa::AddressingMode::*;
        match *addr_mode {
            // Literally just the value.
            Imm => low_byte, 
            // We only need the first byte, and we know it resides in the
            // page. So just fetch the value at that address.
            Zpg => {
                let addr = low_byte as u16;
                self.read(&addr)
            },
            ZpgX => {
                let addr = ((low_byte + self.registers.x) & 0xff) as u16;
                self.read(&addr)
            },
            ZpgY => {
                let addr = ((low_byte + self.registers.y) & 0xff) as u16;
                self.read(&addr)
            },
            Abs => {
                let addr = ((high_byte << 4) | low_byte) as u16;
                self.read(&addr)
            }
            _ => 0,
        }
    }

    fn execute_instruction(&mut self) {
        use crate::isa::Opcode::*;
        if let Some((opc, addr_mode, _)) = self.curr_instruction {
            match *opc {
                LDA => {
                    let (byte1, byte2) = (self.read(&(self.registers.pc + 1)), self.read(&(self.registers.pc + 2)));
                    // TODO big endian or little endian??
                    let val = self.get_operand_as_val(byte1, byte2, addr_mode);
                    self.registers.acc = val;
                }
                LDX => {

                }
                _ => unimplemented!(),
            }
        } else {
            panic!("Invalid opcode.")
        }
    }

    pub fn clock(&mut self) {
        if self.cycles_left == 0 {
            // fetch a new instruction, wait appropriate number of cycles
            let opcode = self.read(&self.registers.pc);
            self.curr_instruction = get_instruction(opcode);
            if let Some((_, _, num_cycles)) = self.curr_instruction {
                self.cycles_left = *num_cycles;
            }
        }
        if self.cycles_left == 1 {
            self.execute_instruction();
            self.curr_instruction = &None;
        }
        self.cycles_left -= 1;
    }
}

fn get_instruction(byte: u8) -> &'static Option<(Opcode, AddressingMode, u8)> {
    let high_nibble = byte >> 4;
    let lower_nibble = byte & 0xf;
    let row = &OPCODE_MATRIX[high_nibble as usize];
    &row[lower_nibble as usize]
}
