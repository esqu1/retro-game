use bitfield::*;
use std::cell::RefCell;
use std::rc::Rc;

// DDLC NNNN
bitfield! {
    pub struct PulseReg0(u8);
    impl Debug;
    pub env_or_vol, set_env_or_vol: 3,0;
    pub const_vol, set_const_vol: 4;
    pub loop_or_len_disable, set_loop_disable: 5;
    pub duty, set_duty: 7, 6;
}

// EPPP NSSS
bitfield! {
    pub struct PulseSweep(u8);
    impl Debug;
    pub shift_count, set_shift: 2, 0;
    pub negative, set_negative: 3;
    pub period, set_period: 6, 4;
    pub enable_sweep, set_enable_sweep: 7;
}

#[derive(Debug)]
pub struct PulseTimerLow(u8);

// LLLL LHHH
bitfield! {
    pub struct PulseReg3(u8);
    impl Debug;
    pub timer_high, set_timer_high: 2, 0;
    pub length_ctr, set_length_ctr: 7, 3;
}

pub struct PulseRegs(
    pub PulseReg0,
    pub PulseSweep,
    pub PulseTimerLow,
    pub PulseReg3,
);

pub struct APURegisters {
    pub pulse_1: Rc<RefCell<PulseRegs>>,
    pub pulse_2: Rc<RefCell<PulseRegs>>,
    triangle: (),
    noise: (),
    dmc: (),
}

impl APURegisters {
    pub fn new() -> Self {
        Self {
            pulse_1: Rc::new(RefCell::new(PulseRegs(
                PulseReg0(0),
                PulseSweep(0),
                PulseTimerLow(0),
                PulseReg3(0),
            ))),
            pulse_2: Rc::new(RefCell::new(PulseRegs(
                PulseReg0(0),
                PulseSweep(0),
                PulseTimerLow(0),
                PulseReg3(0),
            ))),
            triangle: (),
            noise: (),
            dmc: (),
        }
    }
    pub fn something(&mut self) {
        println!("test");
    }
}
