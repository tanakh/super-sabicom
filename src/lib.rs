pub mod bus;
pub mod consts;
pub mod context;
pub mod cpu;
pub mod ppu;
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

        let start_frame = self.ctx.ppu().frame();

        while start_frame == self.ctx.ppu().frame() {
            self.ctx.exec_one();
            self.ctx.ppu_tick();
            self.ctx.spc_tick();
        }
    }
}
