use std::{
    fs::{self, File},
    path::{Path, PathBuf},
    time::Duration,
};

use anyhow::{anyhow, bail, Result};
use compress_tools::{list_archive_files, uncompress_archive_file};
use log::{error, info};
use meru_interface::{
    key_assign::{InputState, KeyCode, SingleKey},
    EmulatorCore,
};
use sdl2::{
    audio::{AudioQueue, AudioSpecDesired},
    controller::{Button, GameController},
    event::Event,
    keyboard::{Keycode, Scancode},
    pixels::Color,
    surface::Surface,
    EventPump,
};

use super_sabicom::Snes;

const EMULATOR_NAME: &str = "super-sabicom";

#[argopt::cmd]
fn main(rom: PathBuf) -> Result<()> {
    use std::io::Write;
    env_logger::builder()
        .format(|buf, record| {
            writeln!(
                buf,
                "[{:5} {}] {}",
                record.level(),
                record
                    .module_path()
                    .map_or("", |path| path.trim_start_matches("super_sabicom::")),
                record.args()
            )
        })
        .init();

    run(&rom)
}

const ARCHIVE_EXTS: &[&str] = &["zip", "7z", "rar"];
const ROM_EXTS: &[&str] = &["sfc", "smc"];

fn load_rom_file(path: &Path) -> Result<Vec<u8>> {
    let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");

    if ARCHIVE_EXTS.contains(&ext) {
        let rom_path = list_archive_files(File::open(path)?)?
            .into_iter()
            .find(|path| {
                let path = Path::new(path);
                path.extension()
                    .and_then(|e| e.to_str())
                    .and_then(|e| ROM_EXTS.iter().find(|f| &&e == f))
                    .is_some()
            })
            .ok_or(anyhow!("Does not contain a rom file"))?;

        let mut data = vec![];
        uncompress_archive_file(File::open(&path)?, &mut data, &rom_path)?;
        Ok(data)
    } else {
        Ok(fs::read(&path)?)
    }
}

fn backup_dir() -> Result<PathBuf> {
    let data_dir = dirs::data_dir()
        .ok_or_else(|| anyhow!("Could not find data directory"))?
        .join(EMULATOR_NAME);

    if data_dir.exists() {
        if !data_dir.is_dir() {
            bail!("Data directory is not a directory");
        }
    } else {
        std::fs::create_dir_all(&data_dir)?;
    }

    Ok(data_dir)
}

fn load_backup(rom_path: &Path) -> Result<Option<Vec<u8>>> {
    let backup_path = backup_dir()?.join(rom_path.with_extension("sav").file_name().unwrap());

    if backup_path.exists() {
        info!("Loading backup from: `{}`", backup_path.display());
        let ret = std::fs::read(backup_path)?;
        Ok(Some(ret))
    } else {
        Ok(None)
    }
}

fn save_backup(rom_path: &Path, backup: Vec<u8>) -> Result<()> {
    let backup_path = backup_dir()?.join(rom_path.with_extension("sav").file_name().unwrap());

    info!("Saving backup to: `{}`", backup_path.display());
    std::fs::write(backup_path, backup)?;
    Ok(())
}

fn state_filename(rom_path: &Path, slot: u32) -> Result<PathBuf> {
    Ok(backup_dir()?.join(
        rom_path
            .with_extension(format!("{slot}.state"))
            .file_name()
            .unwrap(),
    ))
}

fn load_state(snes: &mut Snes, rom_path: &Path, slot: u32) -> Result<()> {
    let state_path = state_filename(rom_path, slot)?;

    if !state_path.exists() {
        bail!("State {slot} does not exist");
    }

    let data = std::fs::read(state_path)?;
    snes.load_state(&data)?;

    Ok(())
}

fn save_state(snes: &Snes, rom_path: &Path, slot: u32) -> Result<()> {
    let state_path = state_filename(rom_path, slot)?;
    std::fs::write(state_path, snes.save_state())?;
    Ok(())
}

fn print_game_info(snes: &Snes) {
    use super_sabicom::context::Cartridge;

    let game_info = snes.game_info();

    let field_len = game_info.iter().map(|r| r.0.len()).max().unwrap();

    for (desc, value) in snes.game_info() {
        println!("{desc:field_len$}: {value}");
    }

    let cart = snes.ctx.cartridge();
    let errors = &cart.rom().header.errors;

    if !errors.is_empty() {
        println!("Errors:");
        for e in errors {
            println!("  * {}", e);
        }
    }
}

const SCREEN_WIDTH: u32 = 512;
const SCREEN_HEIGHT: u32 = 448;
const SCALING: u32 = 1;

fn run(rom_path: &Path) -> Result<()> {
    let bytes = load_rom_file(&rom_path)?;
    let backup = load_backup(&rom_path)?;
    let mut snes = Snes::try_from_file(&bytes, backup.as_deref(), &Default::default())?;

    print_game_info(&snes);

    let key_config = Snes::default_key_config();

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem
        .window(
            "Super Sabicom",
            SCREEN_WIDTH * SCALING,
            SCREEN_HEIGHT * SCALING,
        )
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();

    canvas.set_draw_color(Color::RGB(0, 0, 0));
    let mut event_pump = sdl_context.event_pump().unwrap();

    let texture_creator = canvas.texture_creator();
    let mut surface = Surface::new(
        SCREEN_WIDTH,
        SCREEN_HEIGHT,
        sdl2::pixels::PixelFormatEnum::RGB24,
    )
    .unwrap();

    const SOUND_BUF_LEN: u16 = 1024;

    let audio_subsystem = sdl_context.audio().map_err(|e| anyhow!("{e}"))?;
    let desired_spec = AudioSpecDesired {
        freq: Some(32000),
        channels: Some(2),
        samples: Some(SOUND_BUF_LEN),
    };
    let audio_queue: AudioQueue<i16> = audio_subsystem
        .open_queue(None, &desired_spec)
        .map_err(|e| anyhow!("{e}"))?;
    audio_queue.queue_audio(&vec![0; 2048]).unwrap();
    audio_queue.resume();

    let game_controller_subsystem = sdl_context.game_controller().unwrap();

    let game_controller = game_controller_subsystem.open(0).ok().into_iter().collect();

    let mut im = InputManager::new(game_controller);

    let mut frames = 0;
    let mut cur_slot = 0_u32;

    while process_events(&mut event_pump) {
        im.update(&event_pump);

        if im.hotkey(HotKey::SaveState).just_pressed(&im) {
            if let Err(err) = save_state(&snes, &rom_path, cur_slot) {
                error!("Failed to save state to slot {cur_slot}: {err}");
            } else {
                info!("State saved to slot {cur_slot}");
            }
        }
        if im.hotkey(HotKey::LoadState).just_pressed(&im) {
            if let Err(err) = load_state(&mut snes, &rom_path, cur_slot) {
                error!("Failed to load state from slot {cur_slot}: {err}");
            } else {
                info!("State loaded from slot {cur_slot}");
            }
        }

        if im.hotkey(HotKey::NextSlot).just_pressed(&im) {
            cur_slot += 1;
            info!("State save slot changed: {cur_slot}");
        }
        if im.hotkey(HotKey::PrevSlot).just_pressed(&im) {
            cur_slot = cur_slot.saturating_sub(1);
            info!("State save slot changed: {cur_slot}");
        }

        let key_input = key_config.input(&im);
        snes.set_input(&key_input);
        snes.exec_frame(true);

        let frame_buf = snes.frame_buffer();

        surface.with_lock_mut(|buf| {
            for y in 0..SCREEN_HEIGHT {
                for x in 0..SCREEN_WIDTH {
                    let p = frame_buf.pixel(x as usize, y as usize);
                    let ix = (y * SCREEN_WIDTH + x) as usize * 3;
                    buf[ix] = p.r;
                    buf[ix + 1] = p.g;
                    buf[ix + 2] = p.b;
                }
            }
        });

        let texture = surface.as_texture(&texture_creator).unwrap();
        canvas.copy(&texture, None, None).unwrap();

        canvas.present();

        let audio_buf = snes.audio_buffer();

        // Sync to audio
        while audio_queue.size() > SOUND_BUF_LEN as u32 * 4 {
            std::thread::sleep(Duration::from_millis(1));
        }

        audio_queue
            .queue_audio(
                &audio_buf
                    .samples
                    .iter()
                    .flat_map(|s| [s.left, s.right])
                    .collect::<Vec<i16>>(),
            )
            .unwrap();

        frames += 1;

        if frames % (60 * 60) == 0 {
            if let Some(backup) = snes.backup() {
                save_backup(&rom_path, backup)?;
            }
        }
    }

    if let Some(backup) = snes.backup() {
        save_backup(&rom_path, backup)?;
    }

    Ok(())
}

fn process_events(event_pump: &mut EventPump) -> bool {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. } => return false,
            _ => {}
        }
    }
    true
}

use meru_interface::key_assign::*;

struct InputManager {
    cur_state: State,
    prev_state: State,
    controllers: Vec<GameController>,
    hotkey: Vec<(HotKey, KeyAssign)>,
}

#[derive(Default)]
struct State {
    pressed_scancodes: Vec<Scancode>,
}

#[derive(PartialEq, Eq)]
enum HotKey {
    SaveState,
    LoadState,
    NextSlot,
    PrevSlot,
}

macro_rules! def_key_bind {
    ($key:path => $e:expr, $($rest:tt)*) => {
        def_key_bind!($($rest)* ($key, $e))
    };
    ($(($key:path, $e:expr))*) => {
        vec![ $( ($key, $e) ,)* ]
    };
}

fn default_hotkey() -> Vec<(HotKey, KeyAssign)> {
    use HotKey::*;
    def_key_bind! {
        SaveState => keycode!(F5),
        LoadState => keycode!(F7),
        NextSlot => keycode!(F3),
        PrevSlot => keycode!(F2),
    }
}

impl InputManager {
    fn new(controllers: Vec<GameController>) -> Self {
        let hotkey = default_hotkey();

        Self {
            cur_state: State::default(),
            prev_state: State::default(),
            controllers,
            hotkey,
        }
    }

    fn update(&mut self, e: &EventPump) {
        let ks = e.keyboard_state();

        std::mem::swap(&mut self.prev_state, &mut self.cur_state);

        self.cur_state.pressed_scancodes = ks.pressed_scancodes().collect();
    }

    fn hotkey(&self, hotkey: HotKey) -> &KeyAssign {
        &self.hotkey.iter().find(|r| r.0 == hotkey).unwrap().1
    }
}

impl InputState for InputManager {
    fn pressed(&self, key: &SingleKey) -> bool {
        self.cur_state.pressed(key)
    }

    fn just_pressed(&self, key: &SingleKey) -> bool {
        !self.prev_state.pressed(key) && self.cur_state.pressed(key)
    }
}

impl State {
    fn pressed(&self, key: &SingleKey) -> bool {
        match key {
            SingleKey::KeyCode(code) => self
                .pressed_scancodes
                .iter()
                .any(|sc| eq_scancode(sc, code)),
            SingleKey::GamepadButton(_) => todo!(),
            SingleKey::GamepadAxis(_, _) => todo!(),
        }
    }
}

fn eq_scancode(scancode: &Scancode, keycode: &KeyCode) -> bool {
    let rsc = keycode_to_scancode(keycode);
    scancode == &rsc
}

fn keycode_to_scancode(keycode: &KeyCode) -> Scancode {
    match keycode {
        KeyCode::Return => Scancode::Return,
        KeyCode::LShift => Scancode::RShift,
        KeyCode::X => Scancode::X,
        KeyCode::Z => Scancode::Z,
        KeyCode::S => Scancode::S,
        KeyCode::A => Scancode::A,
        KeyCode::Q => Scancode::Q,
        KeyCode::W => Scancode::W,
        KeyCode::Up => Scancode::Up,
        KeyCode::Down => Scancode::Down,
        KeyCode::Left => Scancode::Left,
        KeyCode::Right => Scancode::Right,
        KeyCode::Space => Scancode::Space,
        _ => todo!(),
    }
}
