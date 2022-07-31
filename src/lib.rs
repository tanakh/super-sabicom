pub mod bus;
pub mod cartridge;
pub mod consts;
pub mod context;
pub mod cpu;
pub mod dsp;
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
            abbrev: "snes",
            file_extensions: &["sfc", "smc", "swc", "fig"],
        };
        &CORE_INFO
    }

    fn try_from_file(
        data: &[u8],
        backup: Option<&[u8]>,
        config: &Self::Config,
    ) -> anyhow::Result<Self> {
        Ok(Snes::new(rom::Rom::from_bytes(&data)?))
    }

    fn game_info(&self) -> Vec<(String, String)> {
        use context::Cartridge;
        let rom = self.ctx.cartridge().rom();
        let game_code = rom.game_code.map_or_else(
            || "N/A".to_string(),
            |b| String::from_utf8_lossy(&b).to_string(),
        );
        vec![
            ("Title".into(), String::from_utf8_lossy(&rom.title).into()),
            ("Game Code".into(), game_code),
            ("Speed".into(), format!("{:?}", rom.speed)),
            ("Map Mode".into(), format!("{:?}", rom.map_mode)),
            ("Chipset".into(), format!("{}", rom.chipset)),
            ("ROM Size".into(), format!("{}", rom.rom_size)),
            ("File Size".into(), format!("{}", rom.rom.len())),
            ("SRAM Size".into(), format!("{}", rom.sram_size)),
            ("Country".into(), format!("{}", rom.country)),
            ("Developer ID".into(), format!("{}", rom.developer_id)),
            ("Rom Version".into(), format!("{}", rom.rom_version)),
        ]
    }

    fn set_config(&mut self, _config: &Self::Config) {}

    fn exec_frame(&mut self, render_graphics: bool) {
        use context::*;

        self.ctx.spc_mut().clear_audio_buffer();

        let start_frame = self.ctx.ppu().frame();

        while start_frame == self.ctx.ppu().frame() {
            self.ctx.exec_one();
            self.ctx.ppu_tick(render_graphics);
            self.ctx.spc_tick();
            self.ctx.bus_tick();
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
        use meru_interface::key_assign::*;

        let controller = vec![
            ("A".to_string(), keycode!(X)),
            ("B".to_string(), keycode!(Z)),
            ("X".to_string(), keycode!(S)),
            ("Y".to_string(), keycode!(A)),
            ("L".to_string(), keycode!(Q)),
            ("R".to_string(), keycode!(W)),
            ("Start".to_string(), keycode!(Return)),
            ("Select".to_string(), keycode!(LShift)),
            ("Up".to_string(), keycode!(Up)),
            ("Down".to_string(), keycode!(Down)),
            ("Left".to_string(), keycode!(Left)),
            ("Right".to_string(), keycode!(Right)),
        ];

        meru_interface::KeyConfig {
            controllers: vec![controller],
        }
    }

    fn set_input(&mut self, input: &meru_interface::InputData) {
        use context::Bus;
        self.ctx.bus_mut().set_input(input);
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
