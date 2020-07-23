pub struct Divider {
    pub period: u8,
    state: u8,
}

impl Divider {
    pub fn new(period: u8) -> Self {
        Self { period, state: 0 }
    }

    pub fn clock(&mut self) -> u8 {
        if self.period > 0 {
            self.state = (self.state + 1) % self.period;
            (self.state == 0) as u8
        } else {
            0
        }
    }
}
