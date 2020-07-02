use crate::isa::*;
use bitfield::*;

bitfield! {
    pub struct StatusFlags(u8);
    impl Debug;
    carry, set_carry: 0;
    zero, set_zero: 1;
    interrupt_disable, set_interrupt: 2;
    decimal, set_decimal: 3;
    bflag, set_bflag: 5, 4;
    overflow, set_overflow: 6;
    negative, set_negative: 7;
}

pub struct CpuRegs {
    pub acc: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub s: StatusFlags,
    pub p: u8,
}

impl CpuRegs {
    pub fn init() -> Self {
        Self {
            acc: 0x0,
            x: 0x0,
            y: 0x0,
            pc: 0x8000,
            s: StatusFlags(0xfd),
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
            // Any implied operands should not need to call this function.
            Imp => unreachable!(),
            // Literally just the value.
            Imm => low_byte, 
            // The accumulator register value.
            Acc => self.registers.acc,
            // We only need the first byte, and we know it resides in the
            // page. So just fetch the value at that address.
            Zpg => {
                let addr = low_byte as u16;
                self.read(&addr)
            },
            // Add the X register to the zero page address, and discard any overflow.
            ZpgX => {
                let addr = ((low_byte + self.registers.x) & 0xff) as u16;
                self.read(&addr)
            },
            // Same here, except for the Y register.
            ZpgY => {
                let addr = ((low_byte + self.registers.y) & 0xff) as u16;
                self.read(&addr)
            },
            // Just look at the actual address.
            Abs => {
                let addr = ((high_byte as u16) << 4) | low_byte as u16;
                self.read(&addr)
            },
            // Absolute address with an X offset.
            AbsX => {
                let addr = ((((high_byte as u16) << 4) | (low_byte as u16)) + self.registers.x as u16) & 0xffff;
                self.read(&addr)
            }
            // Absolute address with a Y offset.
            AbsY => {
                let addr = ((((high_byte as u16) << 4) | (low_byte as u16)) + self.registers.y as u16) & 0xffff;
                self.read(&addr)
            }
            // Relative addressing to the program counter.
            Rel => {
                // TODO: add extra cycle for page transition
                // TODO: check if this is for pc or pc + 1?
                let addr = ((self.registers.pc as i16) + (low_byte as i16)) as u16;
                self.read(&addr)
            }
            // Has two layers of indirection; read the address first and then read that address.
            Ind => {
                let addr = ((high_byte as u16) << 4) | low_byte as u16;
                let (ind_addr_low, ind_addr_high) = (self.read(&addr), self.read(&(addr + 1)));
                let ind_addr = ((ind_addr_high as u16) << 4) | ind_addr_low as u16;
                self.read(&ind_addr)
            }
            // Indexes the initial indirection by the X register.
            IndX => {
                let addr = ((((high_byte as u16) << 4) | low_byte as u16) + self.registers.x as u16) & 0xffff;
                let (ind_addr_low, ind_addr_high) = (self.read(&addr), self.read(&(addr + 1)));
                let ind_addr = ((ind_addr_high as u16) << 4) | ind_addr_low as u16;
                self.read(&ind_addr)
            }
            // Indexes the second indirection by the Y register.
            IndY => {
                let addr = ((high_byte as u16) << 4) | low_byte as u16;
                let (ind_addr_low, ind_addr_high) = (self.read(&addr), self.read(&(addr + 1)));
                let ind_addr = ((((ind_addr_high as u16) << 4) | ind_addr_low as u16) + self.registers.y as u16) & 0xffff;
                self.read(&ind_addr)
            }
        }
    }

    fn execute_instruction(&mut self) {
        use crate::isa::Opcode::*;
        if let Some((opc, addr_mode, _)) = self.curr_instruction {
            let (byte1, byte2) = (self.read(&(self.registers.pc + 1)), self.read(&(self.registers.pc + 2)));
            match *opc {
                LDA => {
                    // TODO big endian or little endian??
                    let val = self.get_operand_as_val(byte1, byte2, addr_mode);
                    self.registers.acc = val;
                    self.registers.s.set_negative(val > 0x7f);
                    self.registers.s.set_zero(val == 0x00);
                },
                LDX => {
                    let val = self.get_operand_as_val(byte1, byte2, addr_mode);
                    self.registers.x = val;
                    self.registers.s.set_negative((val >> 7) == 1);
                    self.registers.s.set_zero(val == 0x00);
                },
                LDY => {
                    let val = self.get_operand_as_val(byte1, byte2, addr_mode);
                    self.registers.y = val;
                    self.registers.s.set_negative(val > 0x7f);
                    self.registers.s.set_zero(val == 0x00);
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
