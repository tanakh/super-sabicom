use crate::rom::{MapMode, Rom};

pub struct Cartridge {
    rom: Rom,
    sram: Vec<u8>,
}

impl Cartridge {
    pub fn new(rom: Rom, backup: Option<&[u8]>) -> Self {
        if let Some(coprocessor) = &rom.chipset.coprocessor {
            todo!("Coprocessor {coprocessor:?} support");
        }

        let sram_size = rom.sram_size;
        assert!(
            sram_size == 0 || sram_size.is_power_of_two(),
            "SRAM size is not power of two: {sram_size:X}"
        );

        let sram = if let Some(backup) = backup {
            if !rom.chipset.has_battery {
                panic!("Cartridge has no battery backup, but backup data is provided");
            }
            if !rom.chipset.has_ram {
                panic!("Cartridge has no SRAM, but backup data is provided");
            }
            if rom.sram_size != backup.len() {
                panic!(
                    "Backup's SRAM size mismatch: actual: {}, expected: {}",
                    backup.len(),
                    rom.sram_size
                );
            }

            backup.to_vec()
        } else {
            vec![0; sram_size]
        };

        Self { rom, sram }
    }

    pub fn backup(&self) -> Option<Vec<u8>> {
        if self.rom.chipset.has_battery {
            Some(self.sram.clone())
        } else {
            None
        }
    }

    pub fn rom(&self) -> &Rom {
        &self.rom
    }

    pub fn read(&self, addr: u32) -> u8 {
        match self.rom.map_mode {
            MapMode::LoRom => {
                let bank = addr >> 16;

                if self.rom.rom.len() > 4 << 20 {
                    todo!("Support LoROM larger than 4MB");
                }

                match bank as u8 {
                    0x00..=0x3F | 0x80..=0xBF => {
                        if addr & 0x8000 != 0 {
                            let rom_addr = (bank & 0x3F) << 15 | addr & 0x7FFF;
                            self.rom.rom[rom_addr as usize]
                        } else {
                            panic!("Unmapped LoROM area: {addr:06X}")
                        }
                    }
                    0x40..=0x6F | 0xC0..=0xEF => {
                        // FIXME:
                        // May be mapped as the higher bank ($8000 - $FFFF) if chip is not MAD-1. Otherwise this area is unused
                        let rom_addr = (bank & 0x7F) << 15 | addr & 0x7FFF;
                        self.rom.rom[rom_addr as usize]
                    }
                    0x70..=0x7D | 0xF0..=0xFF => {
                        if addr & 0x8000 == 0 {
                            let sram_addr = ((bank & 0xF) << 15 | addr & 0x7FFF) as usize;
                            let sram_len = self.sram.len();
                            self.sram[sram_addr % sram_len]
                        } else {
                            let rom_addr = ((bank & 0x7F) << 15 | addr & 0x7FFF) as usize;
                            if rom_addr < self.rom.rom.len() {
                                self.rom.rom[rom_addr as usize]
                            } else {
                                // FIXME: Open-bus?
                                0
                            }
                        }
                    }
                    0x7E..=0x7F => unreachable!(),
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

                match bank as u8 {
                    0x70..=0x7D | 0xF0..=0xFF => {
                        if addr & 0x8000 == 0 {
                            let sram_addr = ((bank & 0xF) << 15 | addr & 0x7FFF) as usize;
                            let sram_len = self.sram.len();
                            self.sram[sram_addr % sram_len] = data;
                        }
                    }
                    _ => {
                        log::warn!("Write to unmapped LoROM area: {addr:06X} = {data:02X}");
                    }
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
