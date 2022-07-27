use crate::rom::{MapMode, Rom};

pub struct Cartridge {
    rom: Rom,
    sram: Vec<u8>,
}

impl Cartridge {
    pub fn new(rom: Rom) -> Self {
        if let Some(coprocessor) = &rom.chipset.coprocessor {
            todo!("Coprocessor {coprocessor:?} support");
        }

        let sram_size = rom.sram_size;

        Self {
            rom,
            sram: vec![0; sram_size],
        }
    }

    pub fn rom(&self) -> &Rom {
        &self.rom
    }

    pub fn read(&self, addr: u32) -> u8 {
        match self.rom.map_mode {
            MapMode::LoRom => {
                let bank = addr >> 16;

                if self.rom.rom.len() > 0x40 * 0x8000 {
                    // The older boards map SRAM to the whole 64K areas at banks 70h-7Dh/F0-FFh.
                    // The newer boards map SRAM to the lower 32K areas at banks 70h-7Dh/F0-FFh (this allows "BigLoROM" games to use the upper 32K of that banks as additional LoROM banks, which is required for games with more than 3MB LoROM).
                    todo!("Support LoROM larger than 2MB");
                }

                if !self.rom.chipset.has_ram || bank & 0x7F < 0x70 {
                    let rom_addr = (bank & 0x3F) << 15 | addr & 0x7FFF;
                    self.rom.rom[rom_addr as usize]
                } else {
                    let sram_addr = (bank & 0xF) << 15 | addr & 0x7FFF;
                    self.sram[sram_addr as usize]
                }
            }
            MapMode::HiRom => {
                let bank = addr >> 16;

                if bank & 0x40 != 0 || addr & 0x8000 != 0 {
                    let rom_addr = (bank & 0x3F) << 16 | addr & 0xFFFF;
                    self.rom.rom[rom_addr as usize]
                } else if bank & 0x20 == 0x20 && addr & 0x6000 == 0x6000 {
                    let sram_addr =
                        ((bank & 0x1F) << 13 | addr & 0x1FFF) as usize % self.sram.len();
                    self.sram[sram_addr]
                } else {
                    panic!("Unmapped HiROM region {:02X}:{:04X}", bank, addr & 0xFFFF);
                }
            }
            _ => todo!("Map mode: {:?}", self.rom.map_mode),
        }
    }

    pub fn write(&mut self, addr: u32, data: u8) {
        match self.rom.map_mode {
            MapMode::LoRom => {
                let bank = addr >> 16;

                if !self.rom.chipset.has_ram && bank & 0x7F < 0x70 {
                    panic!("Write to SRAM with no SRAM cartridge");
                } else {
                    let sram_addr = (bank & 0xF) << 15 | addr & 0x7FFF;
                    self.sram[sram_addr as usize] = data;
                }
            }
            MapMode::HiRom => {
                let bank = addr >> 16;

                if bank & 0x40 != 0 || addr & 0x8000 != 0 {
                    let rom_addr = (bank & 0x3F) << 16 | addr & 0xFFFF;
                    self.rom.rom[rom_addr as usize] = data;
                } else if bank & 0x20 == 0x20 && addr & 0x6000 == 0x6000 {
                    let sram_addr =
                        ((bank & 0x1F) << 13 | addr & 0x1FFF) as usize % self.sram.len();
                    self.sram[sram_addr] = data;
                } else {
                    panic!("Unmapped HiROM region {:02X}:{:04X}", bank, addr & 0xFFFF);
                }
            }
            _ => todo!("Map mode: {:?}", self.rom.map_mode),
        }
    }
}
