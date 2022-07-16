use std::fmt::Display;

use log::warn;
use thiserror::Error;

#[derive(Debug)]
pub struct Rom {
    pub title: Vec<u8>,
    pub is_hirom: bool,
    pub speed: Speed,
    pub map_mode: MapMode,
    pub chipset: Chipset,
    pub rom_size: usize,
    pub ram_size: usize,
    pub country: u8,
    pub developer_id: u8,
    pub game_code: Option<[u8; 4]>,
    pub rom_version: u8,
    pub rom: Vec<u8>,
}

#[derive(Debug)]
pub enum Speed {
    Slow,
    Fast,
}

impl From<u8> for Speed {
    fn from(value: u8) -> Self {
        match value {
            0 => Speed::Slow,
            1 => Speed::Fast,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum MapMode {
    LoRom,
    HiRom,
    Sdd1,
    Sa1,
    ExHiRom,
    Spc7110,
    Unknown(u8),
}

impl From<u8> for MapMode {
    fn from(value: u8) -> Self {
        match value {
            0 => MapMode::LoRom,
            1 => MapMode::HiRom,
            2 => MapMode::Sdd1,
            3 => MapMode::Sa1,
            5 => MapMode::ExHiRom,
            0xA => MapMode::Spc7110,
            _ => MapMode::Unknown(value),
        }
    }
}

#[derive(Default, Debug)]
pub struct Chipset {
    code: u8,
    subclass: u8,
    is_valid: bool,
    has_rom: bool,
    has_ram: bool,
    has_battery: bool,
    has_rtc: bool,
    coprocessor: Option<Coprocessor>,
}

impl Display for Chipset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.is_valid {
            write!(f, "Unknown({:#04X}, {:#04X})", self.code, self.subclass)?;
            return Ok(());
        }

        assert!(self.has_rom);

        write!(f, "ROM")?;

        if let Some(cop) = &self.coprocessor {
            write!(f, "+{:?}", cop)?;
        }
        if self.has_ram {
            write!(f, "+RAM")?;
        }
        if self.has_battery {
            write!(f, "+Battery")?;
        }
        if self.has_rtc {
            write!(f, "+RTC-4513")?;
        }

        Ok(())
    }
}

impl Chipset {
    fn new(code: u8, subclass: u8) -> Self {
        Self {
            code,
            subclass,
            is_valid: true,
            ..Default::default()
        }
    }

    fn with_rom(mut self) -> Self {
        self.has_rom = true;
        self
    }
    fn with_ram(mut self) -> Self {
        self.has_ram = true;
        self
    }
    fn with_battery(mut self) -> Self {
        self.has_battery = true;
        self
    }
    fn with_rtc(mut self) -> Self {
        self.has_rtc = true;
        self
    }
    fn with_coprocessor(mut self, coprocessor: Coprocessor) -> Self {
        self.coprocessor = Some(coprocessor);
        self
    }
}

#[derive(Debug)]
pub enum Coprocessor {
    Dsp,     // DSP1, DSP1A, DSP1B, DSP2, DSP3, DSP4
    Gsu,     // MarioChip1, GSU1, GSU2, GSU2-SP1
    Obc1,    // OBC1
    Sa1,     // SA-1
    Sdd1,    // S-DD1
    Srtc,    // S-RTC
    Other,   // Super Gameboy / Satellaview
    Spc7110, // SPC7110
    St010,   // ST010/ST011
    St018,   // ST018
    Cx4,     // CX4
    Unknown,
}

fn parse_chipset(code: u8, subclass: u8) -> Chipset {
    let chipset = Chipset::new(code, subclass);

    if code == 0x00 {
        chipset.with_rom()
    } else if code == 0x01 {
        chipset.with_rom().with_ram()
    } else if code == 0x02 {
        chipset.with_rom().with_ram().with_battery()
    } else if code & 0xF == 0x3 {
        chipset
            .with_rom()
            .with_coprocessor(parse_coprocessor(code, subclass))
    } else if code & 0xF == 0x4 {
        chipset
            .with_rom()
            .with_coprocessor(parse_coprocessor(code, subclass))
            .with_ram()
    } else if code & 0xF == 0x5 {
        chipset
            .with_rom()
            .with_coprocessor(parse_coprocessor(code, subclass))
            .with_ram()
            .with_battery()
    } else if code & 0xF == 0x6 {
        chipset
            .with_rom()
            .with_coprocessor(parse_coprocessor(code, subclass))
            .with_battery()
    } else if code & 0xF == 0x9 {
        chipset
            .with_rom()
            .with_coprocessor(parse_coprocessor(code, subclass))
            .with_ram()
            .with_battery()
            .with_rtc()
    } else {
        Chipset {
            is_valid: false,
            ..chipset
        }
    }
}

fn parse_coprocessor(c: u8, subclass: u8) -> Coprocessor {
    match c >> 4 {
        0x0 => Coprocessor::Dsp,
        0x1 => Coprocessor::Gsu,
        0x2 => Coprocessor::Obc1,
        0x3 => Coprocessor::Sa1,
        0x4 => Coprocessor::Sdd1,
        0x5 => Coprocessor::Srtc,
        0xE => Coprocessor::Other,
        0xF => match subclass {
            0x00 => Coprocessor::Spc7110,
            0x01 => Coprocessor::St010,
            0x02 => Coprocessor::St018,
            0x10 => Coprocessor::Cx4,
            _ => Coprocessor::Unknown,
        },
        _ => Coprocessor::Unknown,
    }
}

#[derive(Error, Debug)]
pub enum RomError {
    #[error("invalid rom size")]
    InvalidRomSize,
    #[error("unknown rom type")]
    UnknownRomType,
}

impl Rom {
    pub fn from_bytes(bytes: &[u8]) -> Result<Rom, RomError> {
        let bytes = if bytes.len() & 0x3FF == 0 {
            bytes
        } else if bytes.len() & 0x3FF == 0x200 {
            &bytes[0x200..]
        } else {
            Err(RomError::InvalidRomSize)?
        };

        let is_hirom = if is_lorom(bytes) {
            false
        } else if is_hirom(bytes) {
            true
        } else {
            warn!("Invalid checksum");
            if bytes.len() == 0x8000 {
                false
            } else {
                Err(RomError::UnknownRomType)?
            }
        };

        let header = if !is_hirom {
            &bytes[0x7F00..=0x7FFF]
        } else {
            &bytes[0xFF00..=0xFFFF]
        };

        let title = header[0xC0..=0xD4].to_vec();

        let v = header[0xD5];

        if v & 0xE0 != 0x20 {
            warn!("Invalid data in header at 0xFFD5: {v:#04X}");
        }

        let speed = Speed::from((v >> 4) & 1);
        let map_mode = MapMode::from(v & 0xF);

        let chipset = parse_chipset(header[0xD6], header[0xBF]);

        if !chipset.is_valid {
            warn!(
                "Invalid chipset: code={:#04X}, subclass={:#04X}",
                header[0xD6], header[0xBF]
            );
        }

        let rom_size_code = header[0xD7];
        if rom_size_code == 0 || rom_size_code >= 0xD {
            todo!("Too large rom size in header: {rom_size_code}");
        }

        // Number of banks (bank = 32KB)
        const ROM_SIZE_TABLE: &[usize] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 32, 64, 128];

        let rom_size = ROM_SIZE_TABLE[rom_size_code as usize] * 32 * 1024;
        if !(rom_size / 2 + 1..=rom_size).contains(&bytes.len()) {
            warn!(
                "ROM size does not match with headers info: expected: {rom_size}, actual: {}",
                bytes.len()
            );
        }

        if rom_size != bytes.len() {
            todo!("Support odd sized ROM");
        }

        let ram_size_code = header[0xD8];
        if ram_size_code > 5 {
            warn!("Too large ram size in header: {ram_size_code:#04X}");
        }

        let ram_size = if ram_size_code == 0 {
            0
        } else {
            1 << (10 + ram_size_code as u32)
        };

        let country = header[0xD9];

        let developer_id = header[0xDA];

        let game_code = if developer_id == 0x33 {
            Some(header[0xB2..0xB6].try_into().unwrap())
        } else {
            None
        };

        let rom_version = header[0xD8];

        Ok(Self {
            title,
            is_hirom,
            speed,
            map_mode,
            chipset,
            rom_size,
            ram_size,
            country,
            developer_id,
            game_code,
            rom_version,
            rom: bytes.to_vec(),
        })
    }
}

fn is_lorom(bytes: &[u8]) -> bool {
    if bytes.len() < 0x8000 {
        return false;
    }
    test_checksum(bytes, 0x7FDC)
}

fn is_hirom(bytes: &[u8]) -> bool {
    if bytes.len() < 0x10000 {
        return false;
    }
    test_checksum(bytes, 0xFFDC)
}

fn test_checksum(bytes: &[u8], checksum_offset: usize) -> bool {
    let checksum_comp = u16::from_le_bytes(
        bytes[checksum_offset..checksum_offset + 2]
            .try_into()
            .unwrap(),
    );
    let checksum = u16::from_le_bytes(
        bytes[checksum_offset + 2..checksum_offset + 4]
            .try_into()
            .unwrap(),
    );

    if checksum != checksum_comp ^ 0xFFFF {
        return false;
    }

    let mut sum = 0_u16;
    for i in 0..bytes.len() {
        let b = if (checksum_offset..checksum_offset + 2).contains(&i) {
            0xFF
        } else if (checksum_offset + 2..checksum_offset + 4).contains(&i) {
            0
        } else {
            bytes[i]
        };
        sum = sum.wrapping_add(b as u16);
    }

    checksum == sum
}
