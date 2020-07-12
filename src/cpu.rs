use crate::bus::Bus;
use crate::isa::*;
use bitfield::*;

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

impl std::fmt::Display for CpuRegs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
            self.acc, self.x, self.y, self.p.0, self.s
        )
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

pub struct Cpu<'a> {
    pub registers: CpuRegs,
    cycles_left: u8,
    curr_instruction: &'static Option<(Opcode, AddressingMode, u8)>,
    pub bus: Bus<'a>,
    num_cycles: u64,
}

impl<'a, 'b> Cpu<'a> {
    pub fn init(bus: Bus<'a>) -> Self {
        let mut cpu = Self {
            registers: CpuRegs::init(),
            cycles_left: 0,
            curr_instruction: &None,
            bus,
            num_cycles: 7,
        };
        let lo = cpu.read(&0xfffc);
        let hi = cpu.read(&0xfffd);
        cpu.registers.pc = bytes_as_16bit_addr(lo, hi);
        cpu
    }
    pub fn read(&mut self, addr: &u16) -> u8 {
        self.bus.cpu_read(addr)
    }

    pub fn write(&mut self, addr: &u16, val: u8) {
        self.bus.cpu_write(addr, val);
    }

    fn push(&mut self, val: u8) {
        self.write(&(self.registers.s as u16 | 0x0100), val);
        self.registers.s -= 1;
    }

    fn pull(&mut self) -> u8 {
        self.registers.s += 1;
        let addr = (self.registers.s as u16) | 0x0100;
        let val = self.read(&addr);
        val
    }

    fn format_to_instr(&self, bytes: &Vec<u8>) -> String {
        use crate::isa::AddressingMode::*;
        let opcode = get_instruction(bytes[0]);
        let operand = match opcode.as_ref().unwrap().1 {
            Imm => format!("#${:02X}", bytes[1]),
            Abs => format!("${:02X}{:02X}", bytes[2], bytes[1]),
            Imp => format!("    "),
            Rel => format!("    "),
            _ => format!("    "),
        };
        format!("{} {}", opcode.as_ref().unwrap().0.to_string(), operand)
    }

    fn get_operand_as_val(
        &mut self,
        low_byte: u8,
        high_byte: u8,
        addr_mode: &AddressingMode,
    ) -> u8 {
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
                let addr = ((low_byte.overflowing_add(self.registers.x).0) & 0xff) as u16;
                self.read(&addr)
            }
            // Same here, except for the Y register.
            ZpgY => {
                let addr = ((low_byte.overflowing_add(self.registers.y).0) & 0xff) as u16;
                self.read(&addr)
            }
            // Just look at the actual address.
            Abs => {
                let addr = bytes_as_16bit_addr(low_byte, high_byte);
                self.read(&addr)
            }
            // Absolute address with an X offset.
            AbsX => {
                let addr = (bytes_as_16bit_addr(low_byte, high_byte)
                    .overflowing_add(self.registers.x as u16))
                .0 & 0xffff;
                self.read(&addr)
            }
            // Absolute address with a Y offset.
            AbsY => {
                let addr = (bytes_as_16bit_addr(low_byte, high_byte)
                    .overflowing_add(self.registers.y as u16)
                    .0)
                    & 0xffff;
                self.read(&addr)
            }
            // Relative addressing to the program counter.
            Rel => {
                // TODO: add extra cycle for page transition
                // TODO: check if this is for pc or pc + 1?
                let addr = ((self.registers.pc as i16) + (low_byte as i8 as i16)) as u16;
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
                let addr = (bytes_as_16bit_addr(low_byte, 0) + self.registers.x as u16) & 0xffff;
                let (ind_addr_low, ind_addr_high) =
                    (self.read(&(addr & 0xff)), self.read(&((addr + 1) & 0xff)));
                let ind_addr = bytes_as_16bit_addr(ind_addr_low, ind_addr_high);
                self.read(&ind_addr)
            }
            // Indexes the second indirection by the Y register.
            IndY => {
                let addr = bytes_as_16bit_addr(low_byte, 0);
                let (ind_addr_low, ind_addr_high) =
                    (self.read(&addr), self.read(&((addr + 1) & 0xff)));
                let ind_addr = (bytes_as_16bit_addr(ind_addr_low, ind_addr_high)
                    .overflowing_add(self.registers.y as u16))
                .0 & 0xffff;
                self.read(&ind_addr)
            }
        }
    }

    fn get_operand_as_dest(
        &mut self,
        low_byte: u8,
        high_byte: u8,
        addr_mode: &AddressingMode,
    ) -> u16 {
        use crate::isa::AddressingMode::*;
        match *addr_mode {
            // An immediate value cannot be used as an address (only an absolute value).
            Imm | Imp | Acc => unreachable!(),
            Rel => (self.registers.pc as i16 + low_byte as i8 as i16) as u16,
            Abs => bytes_as_16bit_addr(low_byte, high_byte),
            AbsX => bytes_as_16bit_addr(low_byte, high_byte) + self.registers.x as u16,
            AbsY => bytes_as_16bit_addr(low_byte, high_byte) + self.registers.y as u16,
            Ind => {
                let addr = bytes_as_16bit_addr(low_byte, high_byte);
                bytes_as_16bit_addr(self.read(&addr), self.read(&(addr + 1)))
            }
            IndX => {
                let addr = bytes_as_16bit_addr(low_byte, 0) + self.registers.x as u16;
                bytes_as_16bit_addr(self.read(&(addr & 0xff)), self.read(&((addr + 1) & 0xff)))
            }
            IndY => {
                let addr = bytes_as_16bit_addr(low_byte, 0);
                bytes_as_16bit_addr(self.read(&addr), self.read(&((addr + 1) & 0xff)))
                    .overflowing_add(self.registers.y as u16)
                    .0
            }
            Zpg => low_byte as u16,
            ZpgX => (low_byte.overflowing_add(self.registers.x).0) as u16,
            ZpgY => (low_byte.overflowing_add(self.registers.y).0) as u16,
        }
    }

    fn get_jmp_operand(&mut self, low_byte: u8, high_byte: u8, addr_mode: &AddressingMode) -> u16 {
        match *addr_mode {
            AddressingMode::Abs => bytes_as_16bit_addr(low_byte, high_byte),
            AddressingMode::Ind => {
                let addr = bytes_as_16bit_addr(low_byte, high_byte);
                let high = (addr & 0xff00) | (addr + 1) & 0xff;
                bytes_as_16bit_addr(self.read(&addr), self.read(&high))
            }
            _ => unreachable!(),
        }
    }

    fn execute_instruction(&mut self, skip_bytes: u8) {
        use crate::isa::Opcode::*;
        let pc = self.registers.pc.clone();
        if let Some((opc, addr_mode, _)) = self.curr_instruction {
            let (byte1, byte2) = (self.read(&pc), self.read(&(pc + 1)));
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
                    if *opc != TXS {
                        self.registers.p.set_negative((val >> 7) == 1);
                        self.registers.p.set_zero(val == 0);
                    }
                    self.registers.pc += skip_bytes as u16;
                }
                ADC | SBC => {
                    let mut val = self.get_operand_as_val(byte1, byte2, addr_mode) as u8;
                    if *opc == SBC {
                        val = !val;
                    }
                    let result = (self.registers.acc as u16)
                        + (val as u16)
                        + (self.registers.p.carry() as u16);
                    self.registers.p.set_zero((result & 0x00ff) == 0);
                    self.registers.p.set_carry(result > 0xff);
                    self.registers.p.set_overflow(
                        ((result ^ (self.registers.acc as u16)) & (result ^ (val as u16)) & 0x80)
                            > 0,
                    );
                    self.registers.p.set_negative((result & 0x0080) > 0);
                    self.registers.acc = (result & 0x00ff) as u8;
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
                        INX | INY => (*reg_ptr).overflowing_add(1).0,
                        DEX | DEY => (*reg_ptr).overflowing_sub(1).0,
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
                    self.registers.acc = result;
                    self.registers.p.set_negative(result > 0x7f);
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
                        let orig_pc = self.registers.pc;
                        self.registers.pc = new_addr + skip_bytes as u16;
                        if (orig_pc & 0xff00) == (self.registers.pc & 0xff00) {
                            // page boundary has not been crossed
                            self.cycles_left += 1;
                        } else {
                            self.cycles_left += 2;
                        }
                    } else {
                        self.registers.pc += skip_bytes as u16;
                    }
                }
                JMP => {
                    let new_addr = self.get_jmp_operand(byte1, byte2, addr_mode);
                    self.registers.pc = new_addr;
                }
                JSR => {
                    self.registers.pc -= 1;
                    self.registers.pc += skip_bytes as u16;
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
                    let result = orig.overflowing_sub(val).0;
                    self.registers.p.set_negative((result >> 7) == 1);
                    self.registers.p.set_zero(result == 0);
                    self.registers.p.set_carry(orig >= val);

                    self.registers.pc += skip_bytes as u16;
                }
                PHA => {
                    self.push(self.registers.acc);
                    self.registers.pc += skip_bytes as u16;
                }
                PHP => {
                    self.push(self.registers.p.0);
                    self.registers.p.set_bflag(0b11);
                    self.registers.pc += skip_bytes as u16;
                }
                PLA => {
                    self.registers.acc = self.pull();
                    self.registers
                        .p
                        .set_negative((self.registers.acc >> 7) == 1);
                    self.registers.p.set_zero(self.registers.acc == 0);
                    self.registers.pc += skip_bytes as u16;
                }
                PLP => {
                    self.registers.p.0 = self.pull();
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

    pub fn log(&mut self, num_bytes: u8) {
        // format: addr, instr bytes, formatted instr, regs, ppu, cycles
        let mut bytes = vec![];
        let mut p = self.registers.pc.clone();
        for _ in 0..=num_bytes {
            bytes.push(self.read(&p));
            p += 1;
        }
        println!(
            "{:X}  {}\t{}\t\t\t{} PPU: {},{} CYC: {}",
            self.registers.pc,
            bytes
                .iter()
                .map(|x| format!("{:02X}", x))
                .collect::<Vec<String>>()
                .join(" "),
            self.format_to_instr(&bytes),
            self.registers,
            3 * self.num_cycles / 341,
            3 * self.num_cycles % 341,
            self.num_cycles,
        );
    }

    pub fn clock(&mut self) {
        if self.bus.oam_cycles > 0 {
            self.num_cycles += 1;
            self.bus.clock();
        } else {
            if self.cycles_left == 0 {
                if self.bus.nmi {
                    let pc = self.registers.pc.clone();
                    self.push((pc >> 8) as u8);
                    self.push((pc & 0xff) as u8);
                    self.push(self.registers.p.0);
                    let addr = bytes_as_16bit_addr(self.read(&0xfffa), self.read(&0xfffb));
                    self.registers.pc = addr;
                    self.bus.nmi = false;
                } else {
                    self.registers.pc = self.registers.pc.clone();
                }
                // fetch a new instruction, wait appropriate number of cycles
                let pc = self.registers.pc.clone();
                let opcode = self.read(&pc);

                self.curr_instruction = get_instruction(opcode);
                if let Some((_, _, num_cycles)) = self.curr_instruction {
                    self.cycles_left = *num_cycles;
                } else {
                    panic!("Invalid instruction reached.");
                }
                // self.log(addr_mode_num_bytes(
                //     self.curr_instruction.as_ref().unwrap().1,
                // ));
                // increment program counter
                self.registers.pc += 1;
            }
            if self.cycles_left == 1 && *self.curr_instruction != None {
                let addr_mode = self.curr_instruction.as_ref().unwrap().1;
                self.execute_instruction(addr_mode_num_bytes(addr_mode));
                self.curr_instruction = &None;
            }
            self.cycles_left -= 1;
            self.num_cycles += 1;
            self.bus.clock();
        }
    }
}

fn get_instruction(byte: u8) -> &'static Option<(Opcode, AddressingMode, u8)> {
    let high_nibble = byte >> 4;
    let lower_nibble = byte & 0xf;
    let row = &OPCODE_MATRIX[high_nibble as usize];
    &row[lower_nibble as usize]
}
