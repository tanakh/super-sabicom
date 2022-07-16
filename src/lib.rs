pub mod bus;
pub mod consts;
pub mod context;
pub mod cpu;
pub mod rom;
pub mod spc;

pub use rom::Rom;

use context::Context;

pub struct Snes {
    ctx: Context,
}

impl Snes {
    pub fn new(rom: Rom) -> Self {
        Self {
            ctx: Context::from_rom(rom),
        }
    }

    pub fn exec_frame(&mut self) {
        use context::*;

        loop {
            self.ctx.exec_one();
            self.ctx.tick_spc();
        }
    }
}
