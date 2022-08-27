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

use meru_interface::EmulatorCore;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use thiserror::Error;

pub struct Snes {
    pub ctx: Context,
}

impl Snes {
    pub fn new(rom: Rom, backup: Option<&[u8]>) -> Self {
        Self {
            ctx: Context::from_rom(rom, backup),
        }
    }
}

#[derive(Default, JsonSchema, Serialize, Deserialize)]
pub struct Config {}

#[derive(Error, Debug)]
pub enum Error {
    #[error("{0}")]
    RomError(#[from] rom::RomError),
    #[error("deserialize failed: {0}")]
    DeserializeFailed(#[from] bincode::Error),
}

const CORE_INFO: meru_interface::CoreInfo = meru_interface::CoreInfo {
    system_name: "SNES (Super Sabicom)",
    abbrev: "snes",
    file_extensions: &["sfc", "smc", "swc", "fig"],
};

impl EmulatorCore for Snes {
    type Config = Config;
    type Error = Error;

    fn core_info() -> &'static meru_interface::CoreInfo {
        &CORE_INFO
    }

    fn try_from_file(
        data: &[u8],
        backup: Option<&[u8]>,
        _config: &Self::Config,
    ) -> Result<Self, Self::Error> {
        Ok(Snes::new(rom::Rom::from_bytes(&data)?, backup))
    }

    fn game_info(&self) -> Vec<(String, String)> {
        use context::Cartridge;
        let rom = self.ctx.cartridge().rom();
        let header = &rom.header;
        let game_code = header.game_code.map_or_else(
            || "N/A".to_string(),
            |b| String::from_utf8_lossy(&b).to_string(),
        );
        vec![
            ("Title".into(), header.title.clone()),
            ("Game Code".into(), game_code),
            ("Speed".into(), format!("{:?}", header.speed)),
            ("Map Mode".into(), format!("{:?}", header.map_mode)),
            ("Chipset".into(), format!("{}", header.chipset)),
            ("ROM Size".into(), format!("{}", header.rom_size)),
            ("File Size".into(), format!("{}", rom.rom.len())),
            ("SRAM Size".into(), format!("{}", header.sram_size)),
            ("Country".into(), format!("{}", header.country)),
            ("Developer ID".into(), format!("{}", header.developer_id)),
            ("Rom Version".into(), format!("{}", header.rom_version)),
        ]
    }

    fn set_config(&mut self, _config: &Self::Config) {}

    fn exec_frame(&mut self, render_graphics: bool) {
        use context::*;

        self.ctx.spc_mut().clear_audio_buffer();
        self.ctx.ppu_mut().set_render_graphics(render_graphics);

        let start_frame = self.ctx.ppu().frame();

        while start_frame == self.ctx.ppu().frame() {
            self.ctx.exec_one();
            self.ctx.ppu_tick();
            self.ctx.spc_tick();
            self.ctx.bus_tick();
        }
    }

    fn reset(&mut self) {
        use context::Cartridge;

        let rom = self.ctx.cartridge().rom().clone();
        let backup = self.backup();

        self.ctx = Context::from_rom(rom, backup.as_deref());
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
            ("A", any!(keycode!(X), pad_button!(0, East))),
            ("B", any!(keycode!(Z), pad_button!(0, South))),
            ("X", any!(keycode!(S), pad_button!(0, North))),
            ("Y", any!(keycode!(A), pad_button!(0, West))),
            ("L", any!(keycode!(Q), pad_button!(0, LeftTrigger))),
            ("R", any!(keycode!(W), pad_button!(0, RightTrigger))),
            ("Start", any!(keycode!(Return), pad_button!(0, Start))),
            ("Select", any!(keycode!(LShift), pad_button!(0, Select))),
            ("Up", any!(keycode!(Up), pad_button!(0, DPadUp))),
            ("Down", any!(keycode!(Down), pad_button!(0, DPadDown))),
            ("Left", any!(keycode!(Left), pad_button!(0, DPadLeft))),
            ("Right", any!(keycode!(Right), pad_button!(0, DPadRight))),
        ];

        let empty = vec![
            ("A", KeyAssign::default()),
            ("B", KeyAssign::default()),
            ("X", KeyAssign::default()),
            ("Y", KeyAssign::default()),
            ("L", KeyAssign::default()),
            ("R", KeyAssign::default()),
            ("Start", KeyAssign::default()),
            ("Select", KeyAssign::default()),
            ("Up", KeyAssign::default()),
            ("Down", KeyAssign::default()),
            ("Left", KeyAssign::default()),
            ("Right", KeyAssign::default()),
        ];

        meru_interface::KeyConfig {
            controllers: vec![controller, empty.clone(), empty.clone(), empty]
                .into_iter()
                .map(|kvs| {
                    kvs.into_iter()
                        .map(|(key, assign)| (key.to_string(), assign))
                        .collect()
                })
                .collect(),
        }
    }

    fn set_input(&mut self, input: &meru_interface::InputData) {
        use context::Bus;
        self.ctx.bus_mut().set_input(input);
    }

    fn backup(&self) -> Option<Vec<u8>> {
        use context::Cartridge;
        self.ctx.cartridge().backup()
    }

    fn save_state(&self) -> Vec<u8> {
        bincode::serialize(&self.ctx).unwrap()
    }

    fn load_state(&mut self, data: &[u8]) -> Result<(), Self::Error> {
        use context::Cartridge;

        let mut ctx: Context = bincode::deserialize(data)?;

        // Restore ROM and memory mapping
        ctx.cartridge_mut().restore(self.ctx.cartridge_mut());

        self.ctx = ctx;

        Ok(())
    }
}
