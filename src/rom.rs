use std::fmt::Display;

use educe::Educe;
use thiserror::Error;

#[derive(Default, Clone)]
pub struct Rom {
    pub header: Header,
    pub rom: Vec<u8>,
}

#[derive(Educe, Clone)]
#[educe(Default)]
pub struct Header {
    pub title: String,
    #[educe(Default(expression = "Speed::Slow"))]
    pub speed: Speed,
    #[educe(Default(expression = "MapMode::LoRom"))]
    pub map_mode: MapMode,
    #[educe(Default(expression = "Chipset::new(0, 0)"))]
    pub chipset: Chipset,
    pub rom_size: usize,
    pub sram_size: usize,
    pub country: u8,
    pub developer_id: u8,
    pub game_code: Option<[u8; 4]>,
    pub rom_version: u8,
    pub checksum: u16,
    pub checksum_correct: bool,
    pub errors: Vec<RomError>,
}

impl Header {
    fn score(&self) -> u32 {
        self.errors.iter().map(|e| e.score()).sum()
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Default, Clone, Debug)]
pub struct Chipset {
    pub code: u8,
    pub subclass: u8,
    pub is_valid: bool,
    pub has_rom: bool,
    pub has_ram: bool,
    pub has_battery: bool,
    pub has_rtc: bool,
    pub coprocessor: Option<Coprocessor>,
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Error, Debug)]
pub enum RomError {
    #[error("invalid rom size")]
    InvalidRomSize,
    #[error("invalid title string")]
    InvalidTitleString,
    #[error("invalid speed: {0:02X}")]
    InvalidSpeed(u8),
    #[error("invalid map mode: {0:02X}")]
    InvalidMapMode(u8),
    #[error("invalid chipset code: {0:02X} (sub: {1:02X})")]
    InvalidChipset(u8, u8),
    #[error("invalid rom size code: {0}")]
    InvalidRomSizeCode(u8),
    #[error("rom size does not match")]
    RomSizeDoesNotMatch,
    #[error("invalid sram size code: {0}")]
    InvalidSramSizeCode(u8),
    #[error("invalid checksum: {0:04X} (complement: {1:04X})")]
    InvalidChecksumComplement(u16, u16),
    #[error("invalid checksum: {0:04X} (actual: {1:04X})")]
    InvalidChecksum(u16, u16),
    #[error("unknown rom type")]
    UnknownRomType,
}

impl RomError {
    fn score(&self) -> u32 {
        match self {
            RomError::InvalidRomSize => 10000,
            RomError::InvalidTitleString => 1,
            RomError::InvalidSpeed(_) => 100,
            RomError::InvalidMapMode(_) => 10,
            RomError::InvalidChipset(_, _) => 100,
            RomError::InvalidRomSizeCode(_) => 1000,
            RomError::RomSizeDoesNotMatch => 10,
            RomError::InvalidSramSizeCode(_) => 1000,
            RomError::InvalidChecksumComplement(_, _) => 10,
            RomError::InvalidChecksum(_, _) => 1,
            RomError::UnknownRomType => 10000,
        }
    }
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

        let mut candidates = vec![];

        if let Ok(header) = try_parse_header(bytes, 0x7F00) {
            candidates.push((true, header));
        }
        if let Ok(header) = try_parse_header(bytes, 0xFF00) {
            candidates.push((false, header));
        }
        if let Ok(header) = try_parse_header(bytes, 0x40FF00) {
            candidates.push((false, header));
        }

        if candidates.is_empty() {
            Err(RomError::UnknownRomType)?
        }

        for (_, c) in &candidates {
            log::debug!("Cand: score: {:?}, errors: {:?}", c.score(), c.errors);
        }

        let (header_pos_first, header) = candidates
            .into_iter()
            .min_by_key(|(_, r)| r.score())
            .unwrap();

        let interleaved = if header_pos_first {
            match header.map_mode {
                MapMode::LoRom => false,
                _ => true,
            }
        } else {
            match header.map_mode {
                MapMode::HiRom | MapMode::ExHiRom => false,
                _ => true,
            }
        };

        if interleaved {
            // FIXME: More accurate detection
            // Maybe interleaved
            todo!("Interleaved ROM image suport");
        }

        Ok(Rom {
            header,
            rom: bytes.to_vec(),
        })
    }
}

fn try_parse_header(bytes: &[u8], header_pos: usize) -> Result<Header, RomError> {
    if header_pos + 0x100 > bytes.len() {
        Err(RomError::UnknownRomType)?
    }

    let mut errors = vec![];

    let header = &bytes[header_pos..header_pos + 0x100];

    let title = header[0xC0..=0xD4].to_vec();

    let (title, _, invalid) = encoding_rs::SHIFT_JIS.decode(&title);
    if invalid {
        errors.push(RomError::InvalidTitleString);
    }

    let v = header[0xD5];

    if v & 0xE0 != 0x20 {
        errors.push(RomError::InvalidSpeed(v));
    }

    let speed = Speed::from((v >> 4) & 1);
    let map_mode = MapMode::from(v & 0xF);

    if matches!(map_mode, MapMode::Unknown(_)) {
        errors.push(RomError::InvalidMapMode(v));
    }

    let chipset_code = header[0xD6];
    let chipset_code_sub = header[0xBF];
    let chipset = parse_chipset(chipset_code, chipset_code_sub);

    if !chipset.is_valid {
        errors.push(RomError::InvalidChipset(chipset_code, chipset_code_sub));
    }

    let rom_size_code = header[0xD7];
    if rom_size_code > 0xD {
        errors.push(RomError::InvalidRomSizeCode(rom_size_code));
        Err(RomError::InvalidRomSizeCode(rom_size_code))?;
    }

    let rom_size = (1 << rom_size_code) * 1024;
    if rom_size != bytes.len().next_power_of_two() {
        errors.push(RomError::RomSizeDoesNotMatch);
    }

    let ram_size_code = header[0xD8];
    if ram_size_code > 9 {
        errors.push(RomError::InvalidSramSizeCode(ram_size_code));
        Err(RomError::InvalidSramSizeCode(ram_size_code))?;
    }

    let sram_size = if ram_size_code == 0 {
        0
    } else {
        if !chipset.has_ram {
            errors.push(RomError::InvalidSramSizeCode(ram_size_code));
        }
        (1 << ram_size_code as u32) * 1024
    };

    let country = header[0xD9];

    let developer_id = header[0xDA];

    let game_code = if developer_id == 0x33 {
        Some(header[0xB2..0xB6].try_into().unwrap())
    } else {
        None
    };

    let rom_version = header[0xD8];

    let checksum_comp = u16::from_le_bytes(header[0xDC..0xDE].try_into().unwrap());
    let checksum = u16::from_le_bytes(header[0xDE..0xE0].try_into().unwrap());

    if checksum_comp != !checksum {
        errors.push(RomError::InvalidChecksumComplement(checksum, checksum_comp));
    }

    let mut actual_sum = 0_u16;
    for i in 0..bytes.len() {
        let b = if (header_pos + 0xDC..header_pos + 0xDE).contains(&i) {
            0xFF
        } else if (header_pos + 0xDE..header_pos + 0xE0).contains(&i) {
            0
        } else {
            bytes[i]
        };
        actual_sum = actual_sum.wrapping_add(b as u16);
    }

    let checksum_correct = actual_sum == checksum;

    if !checksum_correct {
        errors.push(RomError::InvalidChecksum(checksum, actual_sum));
    }

    Ok(Header {
        title: title.to_string(),
        speed,
        map_mode,
        chipset,
        rom_size,
        sram_size,
        country,
        developer_id,
        game_code,
        rom_version,
        checksum,
        checksum_correct,
        errors,
    })
}
