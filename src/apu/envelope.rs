use super::divider::Divider;
use super::registers::PulseRegs;
use super::APURegisters;
use std::cell::RefCell;
use std::rc::Rc;

pub struct EnvelopeGenerator {
    decay: u8,
    divider: Divider,
    start_flag: bool,
    apu_regs: Rc<RefCell<PulseRegs>>,
}

impl EnvelopeGenerator {
    pub fn new(regs: Rc<RefCell<PulseRegs>>) -> Self {
        Self {
            decay: 0,
            divider: Divider::new(0),
            start_flag: false,
            apu_regs: regs,
        }
    }
    pub fn clock(&mut self) -> u8 {
        // regardless, the divider must be clocked
        // false, only when the start flag is set
        if self.start_flag {
            // if start flag is set,
            self.start_flag = false;
            self.decay = 15;
            self.divider.period = self.apu_regs.borrow().0.env_or_vol();
            // self.apu_regs.borrow().0.env_or_vol()
        } else {
            self.divider.clock();
        }

        if self.apu_regs.borrow().0.const_vol() {
            // constant volume, just return the env parameter

            // self.apu_regs.borrow().0.env_or_vol()
        } else {
            // Returns the volume to output.
        }
        0
    }
}
