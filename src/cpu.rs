use crate::bus::Bus;
use crate::isa::*;
use bitfield::*;
use std::rc::Rc;

#[allow(dead_code)]

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
    pub s: u8,
    pub p: StatusFlags,
}

impl CpuRegs {
    pub fn init() -> Self {
        Self {
            acc: 0x0,
            x: 0x0,
            y: 0x0,
            pc: 0x8000,
            s: 0xfd,
            p: StatusFlags(0x34),
        }
    }
}

fn bytes_as_16bit_addr(low_byte: u8, high_byte: u8) -> u16 {
    ((high_byte as u16) << 8) | low_byte as u16
}

// TODO: write a function that determines number of bytes to advance the program counter by
fn addr_mode_num_bytes(addr: AddressingMode) -> u8 {
    use crate::isa::AddressingMode::*;
    match addr {
        Imp | Acc => 0,
        Rel | Imm | Zpg | ZpgX | ZpgY | IndX | IndY => 1,
        Abs | AbsX | AbsY | Ind => 2,
    }
}

pub struct Cpu {
    pub registers: CpuRegs,
    cycles_left: u8,
    curr_instruction: &'static Option<(Opcode, AddressingMode, u8)>,
    pub bus: Bus,
    num_cycles: u64,
}

impl Cpu {
    pub fn init(bus: Bus) -> Self {
        Self {
            registers: CpuRegs::init(),
            cycles_left: 0,
            curr_instruction: &None,
            bus,
            num_cycles: 0,
        }
    }
    pub fn read(&self, addr: &u16) -> u8 {
        self.bus.cpu_read(addr)
    }

    pub fn write(&mut self, addr: &u16, val: u8) {
        self.bus.cpu_write(addr, val);
    }

    fn push(&mut self, val: u8) {
        self.write(&(self.registers.pc | 0x0100), val);
        self.registers.pc -= 1;
    }

    fn pull(&mut self) -> u8 {
        self.registers.pc += 1;
        let addr = self.registers.pc | 0x0100;
        let val = self.read(&addr);
        self.write(&addr, 0);
        val
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
            }
            // Add the X register to the zero page address, and discard any overflow.
            ZpgX => {
                let addr = ((low_byte + self.registers.x) & 0xff) as u16;
                self.read(&addr)
            }
            // Same here, except for the Y register.
            ZpgY => {
                let addr = ((low_byte + self.registers.y) & 0xff) as u16;
                self.read(&addr)
            }
            // Just look at the actual address.
            Abs => {
                let addr = bytes_as_16bit_addr(low_byte, high_byte);
                self.read(&addr)
            }
            // Absolute address with an X offset.
            AbsX => {
                let addr =
                    (bytes_as_16bit_addr(low_byte, high_byte) + self.registers.x as u16) & 0xffff;
                self.read(&addr)
            }
            // Absolute address with a Y offset.
            AbsY => {
                let addr =
                    (bytes_as_16bit_addr(low_byte, high_byte) + self.registers.y as u16) & 0xffff;
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
                let addr = bytes_as_16bit_addr(low_byte, high_byte);
                let (ind_addr_low, ind_addr_high) = (self.read(&addr), self.read(&(addr + 1)));
                let ind_addr = bytes_as_16bit_addr(ind_addr_low, ind_addr_high);
                self.read(&ind_addr)
            }
            // Indexes the initial indirection by the X register.
            IndX => {
                let addr =
                    (bytes_as_16bit_addr(low_byte, high_byte) + self.registers.x as u16) & 0xffff;
                let (ind_addr_low, ind_addr_high) = (self.read(&addr), self.read(&(addr + 1)));
                let ind_addr = bytes_as_16bit_addr(ind_addr_low, ind_addr_high);
                self.read(&ind_addr)
            }
            // Indexes the second indirection by the Y register.
            IndY => {
                let addr = bytes_as_16bit_addr(low_byte, high_byte);
                let (ind_addr_low, ind_addr_high) = (self.read(&addr), self.read(&(addr + 1)));
                let ind_addr = (bytes_as_16bit_addr(ind_addr_low, ind_addr_high)
                    + self.registers.y as u16)
                    & 0xffff;
                self.read(&ind_addr)
            }
        }
    }

    fn get_operand_as_dest(&self, low_byte: u8, high_byte: u8, addr_mode: &AddressingMode) -> u16 {
        use crate::isa::AddressingMode::*;
        match *addr_mode {
            // An immediate value cannot be used as an address (only an absolute value).
            Imm | Imp | Acc => unreachable!(),
            Rel => (self.registers.pc as i16 + low_byte as i16) as u16,
            Abs => bytes_as_16bit_addr(low_byte, high_byte),
            AbsX => bytes_as_16bit_addr(low_byte, high_byte) + self.registers.x as u16,
            AbsY => bytes_as_16bit_addr(low_byte, high_byte) + self.registers.y as u16,
            Ind => {
                let addr = bytes_as_16bit_addr(low_byte, high_byte);
                bytes_as_16bit_addr(self.read(&addr), self.read(&(addr + 1)))
            }
            IndX => {
                let addr = bytes_as_16bit_addr(low_byte, high_byte) + self.registers.x as u16;
                bytes_as_16bit_addr(self.read(&addr), self.read(&(addr + 1)))
            }
            IndY => {
                let addr = bytes_as_16bit_addr(low_byte, high_byte);
                bytes_as_16bit_addr(self.read(&addr), self.read(&addr)) + self.registers.y as u16
            }
            Zpg => low_byte as u16,
            ZpgX => (low_byte + self.registers.x) as u16,
            ZpgY => (low_byte + self.registers.y) as u16,
        }
    }

    fn execute_instruction(&mut self, skip_bytes: u8) {
        use crate::isa::Opcode::*;
        if let Some((opc, addr_mode, _)) = self.curr_instruction {
            let (byte1, byte2) = (
                self.read(&(self.registers.pc)),
                self.read(&(self.registers.pc + 1)),
            );
            println!("{:?}", *opc);
            match *opc {
                // loading from memory into registers
                LDA | LDX | LDY => {
                    // TODO: big endian or little endian?
                    let val = self.get_operand_as_val(byte1, byte2, addr_mode);
                    self.registers.p.set_zero(val == 0x00);
                    match *opc {
                        LDA => self.registers.acc = val,
                        LDX => self.registers.x = val,
                        LDY => self.registers.y = val,
                        _ => unreachable!(),
                    };
                    self.registers.p.set_negative(match *opc {
                        LDX => (val >> 7) == 1,
                        LDA | LDY => val > 0x7f,
                        _ => unreachable!(),
                    });
                    self.registers.pc += skip_bytes as u16;
                }
                // storing into memory
                STA | STX | STY => {
                    let addr = self.get_operand_as_dest(byte1, byte2, addr_mode);
                    self.write(
                        &addr,
                        match *opc {
                            STA => self.registers.acc,
                            STX => self.registers.x,
                            STY => self.registers.y,
                            _ => unreachable!(),
                        },
                    );
                    self.registers.pc += skip_bytes as u16;
                }
                // transferring between registers
                TAX | TAY | TXA | TYA | TSX | TXS => {
                    let val = match *opc {
                        TAX | TAY => self.registers.acc,
                        TXA | TXS => self.registers.x,
                        TYA => self.registers.y,
                        TSX => self.registers.s,
                        _ => unreachable!(),
                    };
                    match *opc {
                        TXA | TYA => self.registers.acc = val,
                        TAX | TSX => self.registers.x = val,
                        TAY => self.registers.y = val,
                        TXS => self.registers.s = val,
                        _ => unreachable!(),
                    };
                    self.registers.p.set_negative((val >> 7) == 1);
                    self.registers.p.set_zero(val == 0);
                    self.registers.pc += skip_bytes as u16;
                }
                ADC | SBC => {
                    let val = self.get_operand_as_val(byte1, byte2, addr_mode) as u8;
                    let (unsigned_result, unsigned_overflow) = match *opc {
                        ADC => val.overflowing_add(self.registers.acc),
                        SBC => val.overflowing_sub(self.registers.acc),
                        _ => unreachable!(),
                    };
                    let carry_bit = if *opc == ADC {
                        self.registers.p.carry()
                    } else {
                        !self.registers.p.carry()
                    } as u8;
                    self.registers.acc = unsigned_result + carry_bit;
                    let (signed_result, signed_overflow) = match *opc {
                        ADC => (val as i8).overflowing_add(self.registers.acc as i8),
                        SBC => (val as i8).overflowing_sub(self.registers.acc as i8),
                        _ => unreachable!(),
                    };
                    self.registers.p.set_carry(unsigned_overflow);
                    self.registers.p.set_overflow(signed_overflow);
                    self.registers.p.set_negative(signed_result < 0);
                    self.registers.p.set_zero(signed_result == 0);
                    self.registers.pc += skip_bytes as u16;
                }
                INC | DEC => {
                    let val = self.get_operand_as_val(byte1, byte2, addr_mode);
                    let addr = self.get_operand_as_dest(byte1, byte2, addr_mode);
                    let result = match *opc {
                        INC => val.overflowing_add(1).0,
                        DEC => val.overflowing_sub(1).0,
                        _ => unreachable!(),
                    };
                    self.write(&addr, result);
                    self.registers.p.set_negative((result >> 7) == 1);
                    self.registers.p.set_zero(result == 0);
                    self.registers.pc += skip_bytes as u16;
                }
                INX | INY | DEX | DEY => {
                    let reg_ptr = match *opc {
                        INX | DEX => &mut self.registers.x,
                        INY | DEY => &mut self.registers.y,
                        _ => unreachable!(),
                    };
                    let result = match *opc {
                        INX | INY => *reg_ptr + 1,
                        DEX | DEY => *reg_ptr - 1,
                        _ => unreachable!(),
                    };
                    *reg_ptr = result;
                    self.registers.p.set_negative((result >> 7) == 1);
                    self.registers.p.set_zero(result == 0);
                    self.registers.pc += skip_bytes as u16;
                }
                AND | ORA | EOR => {
                    let val = self.get_operand_as_val(byte1, byte2, addr_mode);
                    let result = match *opc {
                        AND => val & self.registers.acc,
                        ORA => val | self.registers.acc,
                        EOR => val ^ self.registers.acc,
                        _ => unreachable!(),
                    };
                    self.registers.p.set_negative(result <= 0x7f);
                    self.registers.p.set_zero(result == 0);
                    self.registers.pc += skip_bytes as u16;
                }
                ASL | LSR | ROL | ROR => {
                    let val = self.get_operand_as_val(byte1, byte2, addr_mode);
                    let (result, carry) = match *opc {
                        ASL => (val << 1, val >> 7),
                        LSR => (val >> 1, val & 0x1),
                        ROL => ((val << 1) | self.registers.p.carry() as u8, val >> 7),
                        ROR => (
                            (val >> 1) | ((self.registers.p.carry() as u8) << 7),
                            val & 0x1,
                        ),
                        _ => unreachable!(),
                    };
                    self.registers.p.set_negative((result >> 7) == 1);
                    self.registers.p.set_zero(result == 0);
                    self.registers.p.set_carry(carry == 1);
                    if *addr_mode == crate::isa::AddressingMode::Acc {
                        self.registers.acc = result;
                    } else {
                        let addr = self.get_operand_as_dest(byte1, byte2, addr_mode);
                        self.write(&addr, result);
                    }
                    self.registers.pc += skip_bytes as u16;
                }
                BIT => {
                    let val = self.get_operand_as_val(byte1, byte2, addr_mode);
                    self.registers.p.set_negative((val >> 7) == 1);
                    self.registers.p.set_overflow(((val & 0b1111111) >> 6) == 1);
                    self.registers.p.set_zero((self.registers.acc & val) == 0);
                    self.registers.pc += skip_bytes as u16;
                }
                // BRANCHING INSTRUCTIONS
                BCS | BCC | BEQ | BMI | BNE | BPL | BVS | BVC => {
                    let new_addr = self.get_operand_as_dest(byte1, byte2, addr_mode);
                    let cond_met = match *opc {
                        BCS => self.registers.p.carry(),
                        BCC => !self.registers.p.carry(),
                        BEQ => self.registers.p.zero(),
                        BNE => !self.registers.p.zero(),
                        BMI => self.registers.p.negative(),
                        BPL => !self.registers.p.negative(),
                        BVS => self.registers.p.overflow(),
                        BVC => !self.registers.p.overflow(),
                        _ => unreachable!(),
                    };
                    if cond_met {
                        self.registers.pc = new_addr;
                    } else {
                        self.registers.pc += skip_bytes as u16;
                    }
                }
                JMP => {
                    let new_addr = self.get_operand_as_dest(byte1, byte2, addr_mode);
                    println!("new addr: {}", new_addr);
                    self.registers.pc = new_addr;
                }
                JSR => {
                    self.registers.pc -= 1;
                    self.push((self.registers.pc >> 8) as u8); // high byte pushed first
                    self.push((self.registers.pc & 0xff) as u8);
                    let new_addr = self.get_operand_as_dest(byte1, byte2, addr_mode);
                    self.registers.pc = new_addr;
                }
                RTI => {
                    self.registers.p.0 = self.pull();
                    let low_byte = self.pull();
                    let high_byte = self.pull();
                    self.registers.pc = self.get_operand_as_dest(
                        low_byte,
                        high_byte,
                        &crate::isa::AddressingMode::Abs,
                    );
                }
                RTS => {
                    let low_byte = self.pull();
                    let high_byte = self.pull();
                    self.registers.pc = self.get_operand_as_dest(
                        low_byte,
                        high_byte,
                        &crate::isa::AddressingMode::Abs,
                    ) + 1;
                }
                CLC => {
                    self.registers.p.set_carry(false);
                    self.registers.pc += skip_bytes as u16;
                }
                CLD => {
                    self.registers.p.set_decimal(false);
                    self.registers.pc += skip_bytes as u16;
                }
                CLI => {
                    self.registers.p.set_interrupt(false);
                    self.registers.pc += skip_bytes as u16;
                }
                CLV => {
                    self.registers.p.set_overflow(false);
                    self.registers.pc += skip_bytes as u16;
                }
                SEC => {
                    self.registers.p.set_carry(true);
                    self.registers.pc += skip_bytes as u16;
                }
                SED => {
                    self.registers.p.set_decimal(true);
                    self.registers.pc += skip_bytes as u16;
                }
                SEI => {
                    self.registers.p.set_interrupt(true);
                    self.registers.pc += skip_bytes as u16;
                }
                CMP | CPX | CPY => {
                    let orig = match *opc {
                        CMP => self.registers.acc,
                        CPX => self.registers.x,
                        _ => self.registers.y,
                    };
                    let val = self.get_operand_as_val(byte1, byte2, addr_mode);
                    let (result, overflow) = orig.overflowing_sub(val);
                    self.registers.p.set_negative((result >> 7) == 1);
                    self.registers.p.set_zero(result == 0);
                    self.registers.p.set_carry(overflow);
                    self.registers.pc += skip_bytes as u16;
                }
                PHA => {
                    self.push(self.registers.acc);
                    self.registers.pc += skip_bytes as u16;
                }
                PHP => {
                    self.push(self.registers.s);
                    self.registers.pc += skip_bytes as u16;
                }
                PLA => {
                    self.registers.acc = self.pull();
                    self.registers.pc += skip_bytes as u16;
                }
                PLP => {
                    self.registers.s = self.pull();
                    self.registers.pc += skip_bytes as u16;
                }
                BRK => {
                    self.registers.pc += 1;
                    // push pc
                    self.push((self.registers.pc >> 8) as u8); // high byte pushed first
                    self.push((self.registers.pc & 0xff) as u8);
                    // push p register
                    self.push(self.registers.p.0); // TODO: is this actually mutated?
                                                   // set interrupt flag
                    self.registers.p.set_interrupt(true);
                    // load from 0xfffe-0xffff
                    let new_pc = (self.read(&0xfffe) as u16) | ((self.read(&0xffff) as u16) << 8);
                    self.registers.pc = new_pc;
                }
                NOP => self.registers.pc += skip_bytes as u16,
            };
        } else {
            panic!("Invalid opcode.")
        }
    }

    pub fn log(&self) {
        // format: addr, instr bytes, formatted instr, regs, ppu, cycles
    }

    pub fn clock(&mut self) {
        println!("{}", self.registers.pc);
        if self.cycles_left == 0 {
            // fetch a new instruction, wait appropriate number of cycles
            let opcode = self.read(&self.registers.pc);
            println!("opcode: {:?}", opcode);

            // increment program counter
            self.registers.pc += 1;

            self.curr_instruction = get_instruction(opcode);
            if let Some((_, _, num_cycles)) = self.curr_instruction {
                self.cycles_left = *num_cycles;
            }
        }
        if self.cycles_left == 1 {
            let addr_mode = self.curr_instruction.as_ref().unwrap().1;
            self.execute_instruction(addr_mode_num_bytes(addr_mode));
            self.curr_instruction = &None;
        }
        self.cycles_left -= 1;
        self.num_cycles += 1;
    }
}

fn get_instruction(byte: u8) -> &'static Option<(Opcode, AddressingMode, u8)> {
    let high_nibble = byte >> 4;
    let lower_nibble = byte & 0xf;
    let row = &OPCODE_MATRIX[high_nibble as usize];
    &row[lower_nibble as usize]
}
