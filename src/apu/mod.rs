mod divider;
mod envelope;
mod registers;

use envelope::EnvelopeGenerator;
use registers::{APURegisters, PulseRegs};
use std::cell::RefCell;
use std::rc::Rc;

struct PulseChannel {
    envelope_gen: EnvelopeGenerator,
}

impl PulseChannel {
    fn new(regs: Rc<RefCell<PulseRegs>>) -> Self {
        Self {
            envelope_gen: EnvelopeGenerator::new(regs),
        }
    }

    fn clock(&mut self) -> u8 {
        0
    }
}

pub struct APU {
    registers: APURegisters,
    pulse1_channel: PulseChannel,
    pulse2_channel: PulseChannel,
}

impl APU {
    pub fn new() -> Self {
        let registers = APURegisters::new();
        Self {
            pulse1_channel: PulseChannel::new(registers.pulse_1.clone()),
            pulse2_channel: PulseChannel::new(registers.pulse_2.clone()),
            registers,
        }
    }
    pub fn clock(&mut self) {
        // self.registers.borrow_mut().something();
        let pulse1_signal = self.pulse1_channel.clock();
        let pulse2_signal = self.pulse2_channel.clock();
    }
}
