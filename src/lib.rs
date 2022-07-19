pub mod bus;
pub mod consts;
pub mod context;
pub mod cpu;
pub mod ppu;
pub mod rom;
pub mod spc;

use crate::context::Context;
pub use crate::rom::Rom;

use meru_interface::{ConfigUi, EmulatorCore};
use serde::{Deserialize, Serialize};

pub struct Snes {
    ctx: Context,
}

impl Snes {
    pub fn new(rom: Rom) -> Self {
        Self {
            ctx: Context::from_rom(rom),
        }
    }
}

#[derive(Default, Serialize, Deserialize)]
pub struct SnesConfig {}

impl ConfigUi for SnesConfig {
    fn ui(&mut self, ui: &mut egui::Ui) {
        todo!()
    }
}

impl EmulatorCore for Snes {
    type Config = SnesConfig;

    fn core_info() -> &'static meru_interface::CoreInfo {
        const CORE_INFO: meru_interface::CoreInfo = meru_interface::CoreInfo {
            system_name: "Super Famicom",
            abbrev: "sfc",
            file_extensions: &["sfc", "smc", "swc", "fig"],
        };
        &CORE_INFO
    }

    fn try_from_file(
        data: &[u8],
        backup: Option<&[u8]>,
        config: &Self::Config,
    ) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let rom = rom::Rom::from_bytes(&data)?;
        Ok(Snes::new(rom))
    }

    fn game_info(&self) -> Vec<(String, String)> {
        use context::Rom;
        let rom = self.ctx.rom();
        vec![
            ("Title".into(), String::from_utf8_lossy(&rom.title).into()),
            ("Speed".into(), format!("{:?}", rom.speed)),
            ("MapMode".into(), format!("{:?}", rom.map_mode)),
            ("Chipset".into(), format!("{}", rom.chipset)),
            ("RomSize".into(), format!("{}", rom.rom_size)),
            ("RamSize".into(), format!("{}", rom.ram_size)),
            ("Country".into(), format!("{}", rom.country)),
            ("Developer ID".into(), format!("{}", rom.developer_id)),
            ("Game Code".into(), format!("{:?}", rom.game_code)),
            ("Rom Version".into(), format!("{}", rom.rom_version)),
        ]
    }

    fn set_config(&mut self, _config: &Self::Config) {}

    fn exec_frame(&mut self, render_graphics: bool) {
        use context::*;

        let start_frame = self.ctx.ppu().frame();

        while start_frame == self.ctx.ppu().frame() {
            self.ctx.exec_one();
            self.ctx.bus_tick();
            self.ctx.ppu_tick();
            self.ctx.spc_tick();
        }
    }

    fn reset(&mut self) {
        todo!()
    }

    fn frame_buffer(&self) -> &meru_interface::FrameBuffer {
        use context::Ppu;
        self.ctx.ppu().frame_buffer()
    }

    fn audio_buffer(&self) -> &meru_interface::AudioBuffer {
        use context::Spc;
        self.ctx.spc().audio_buffer()
    }

    fn default_key_config() -> meru_interface::KeyConfig {
        todo!()
    }

    fn set_input(&mut self, input: &meru_interface::InputData) {
        // TODO
    }

    fn backup(&self) -> Option<Vec<u8>> {
        None
    }

    fn save_state(&self) -> Vec<u8> {
        todo!()
    }

    fn load_state(&mut self, data: &[u8]) -> anyhow::Result<()> {
        todo!()
    }
}
