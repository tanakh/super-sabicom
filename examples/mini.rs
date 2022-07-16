use std::{
    fs::{self, File},
    path::{Path, PathBuf},
};

use anyhow::{anyhow, Result};

use compress_tools::{list_archive_files, uncompress_archive_file};
use super_sabicom::{Rom, Snes};

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

    let data = load_rom_file(&rom)?;
    let rom = Rom::from_bytes(&data)?;
    print_rom_info(&rom);

    let mut snes = Snes::new(rom);

    loop {
        snes.exec_frame();
    }
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

fn print_rom_info(rom: &Rom) {
    println!("Title:        {}", String::from_utf8_lossy(&rom.title));
    println!("HiROM:        {}", if rom.is_hirom { "Yes" } else { "No" });
    println!("Speed:        {:?}", rom.speed);
    println!("Map mode:     {:?}", rom.map_mode);
    println!("Chipset:      {}", rom.chipset);
    println!("ROM size:     {}", rom.rom_size);
    println!("RAM size:     {}", rom.ram_size);
    println!("Country:      {:?}", rom.country);
    println!("Developer ID: {:?}", rom.developer_id);
    println!("Game code:    {:?}", rom.game_code);
    println!("ROM version:  {:?}", rom.rom_version);
    println!("Image size:   {}", rom.rom.len());
}
