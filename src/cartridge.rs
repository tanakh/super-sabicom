use log::info;
use serde::{Deserialize, Serialize};

use crate::rom::{MapMode, Rom};

#[derive(Serialize, Deserialize)]
pub struct Cartridge {
    #[serde(skip)]
    rom: Rom,
    sram: Vec<u8>,
    #[serde(skip)]
    mapping: Vec<Mapping>,
}

#[derive(Clone)]
enum Mapping {
    Rom(u32),
    Ram(u32),
    None,
}

impl Cartridge {
    pub fn new(rom: Rom, backup: Option<&[u8]>) -> Self {
        if let Some(coprocessor) = &rom.header.chipset.coprocessor {
            todo!("Coprocessor {coprocessor:?} support");
        }

        let sram_size = rom.header.sram_size;
        assert!(
            sram_size == 0 || sram_size.is_power_of_two(),
            "SRAM size is not power of two: {sram_size:X}"
        );

        let sram = if let Some(backup) = backup {
            if !rom.header.chipset.has_battery {
                panic!("Cartridge has no battery backup, but backup data is provided");
            }
            if !rom.header.chipset.has_ram {
                panic!("Cartridge has no SRAM, but backup data is provided");
            }
            if rom.header.sram_size != backup.len() {
                panic!(
                    "Backup's SRAM size mismatch: actual: {}, expected: {}",
                    backup.len(),
                    rom.header.sram_size
                );
            }

            backup.to_vec()
        } else {
            vec![0; sram_size]
        };

        let mapping = make_mapping(&rom);

        Self { rom, sram, mapping }
    }

    pub fn backup(&self) -> Option<Vec<u8>> {
        if self.rom.header.chipset.has_battery {
            Some(self.sram.clone())
        } else {
            None
        }
    }

    pub fn restore(&mut self, origin: &mut Cartridge) {
        std::mem::swap(&mut self.rom, &mut origin.rom);
        self.mapping = make_mapping(&self.rom);
    }

    pub fn rom(&self) -> &Rom {
        &self.rom
    }

    pub fn read(&self, addr: u32) -> Option<u8> {
        Some(match self.mapping[(addr >> 11) as usize] {
            Mapping::Rom(offset) => self.rom.rom[(offset + (addr & 0x07ff)) as usize],
            Mapping::Ram(offset) => self.sram[(offset + (addr & 0x7ff)) as usize],
            Mapping::None => {
                info!("Reading from unmapped region: {addr:06X}");
                None?
            }
        })
    }

    pub fn write(&mut self, addr: u32, data: u8) {
        match self.mapping[(addr >> 11) as usize] {
            Mapping::Rom(_) => {
                info!("Writing to ROM region: {addr:06X} = {data:02X}")
            }
            Mapping::Ram(offset) => self.sram[(offset + (addr & 0x7ff)) as usize] = data,
            Mapping::None => {
                info!("Writing to unmapped region: {addr:06X} = {data:02X}")
            }
        }
    }
}

fn make_mapping(rom: &Rom) -> Vec<Mapping> {
    // 2KB page x 0x2000 entries
    let mut mapping = vec![Mapping::None; 0x2000];
    let rom_len = rom.rom.len() as u32;
    let ram_len = rom.header.sram_size as u32;

    for bank in 0..=0xFF {
        for page in 0..=0x1F {
            // FIXME: odd sized ROM
            let entry = match rom.header.map_mode {
                MapMode::LoRom => {
                    let rom_offset = (bank & 0x7F) << 15 | (page & 0xF) << 11;
                    let ram_offset = (bank & 0xF) << 15 | (page & 0xF) << 11;

                    match bank as u8 {
                        0x00..=0x3F | 0x80..=0xBF => match (page << 11) as u16 {
                            0x0000..=0x7FFF => {
                                // System region
                                // FIXME: 0x3000..=0x3FFF for DSP and SuperFX?
                                Mapping::None
                            }
                            0x8000..=0xFFFF => Mapping::Rom(rom_addr_mask(rom_offset, rom_len)),
                        },
                        0x40..=0x6F | 0xC0..=0xEF => match (page << 11) as u16 {
                            0x0000..=0x7FFF => {
                                // FIXME:
                                // May be mapped as the higher bank ($8000 - $FFFF) if chip is not MAD-1. Otherwise this area is unused
                                Mapping::Rom(rom_addr_mask(rom_offset, rom_len))
                            }
                            0x8000..=0xFFFF => Mapping::Rom(rom_addr_mask(rom_offset, rom_len)),
                        },
                        0x70..=0x7D | 0xF0..=0xFF => match (page << 11) as u16 {
                            0x0000..=0x7FFF => {
                                if rom.header.sram_size != 0 {
                                    Mapping::Ram(ram_addr_mask(ram_offset, ram_len))
                                } else {
                                    Mapping::None
                                }
                            }
                            0x8000..=0xFFFF => Mapping::Rom(rom_addr_mask(rom_offset, rom_len)),
                        },
                        0x7E..=0x7F => Mapping::None,
                    }
                }
                MapMode::HiRom | MapMode::ExHiRom => {
                    let rom_offset = if bank & 0x80 == 0 { 0x40_0000 } else { 0 }
                        | (bank & 0x3F) << 16
                        | (page & 0x1F) << 11;
                    let ram_offset = (bank & 0x1F) << 13 | (page & 3) << 11;

                    match bank as u8 {
                        0x00..=0x1F | 0x80..=0x9F => match (page << 11) as u16 {
                            0x0000..=0x7FFF => {
                                // FIXME: 0x3000..=0x3FFF for DSP and SuperFX?
                                Mapping::None
                            }
                            0x8000..=0xFFFF => Mapping::Rom(rom_addr_mask(rom_offset, rom_len)),
                        },
                        0x20..=0x3F | 0xA0..=0xBF => match (page << 11) as u16 {
                            0x0000..=0x5FFF => {
                                // FIXME: 0x3000..=0x3FFF for DSP and SuperFX?
                                Mapping::None
                            }
                            0x6000..=0x7FFF => {
                                if rom.header.sram_size != 0 {
                                    Mapping::Ram(ram_addr_mask(ram_offset, ram_len))
                                } else {
                                    Mapping::None
                                }
                            }
                            0x8000..=0xFFFF => Mapping::Rom(rom_addr_mask(rom_offset, rom_len)),
                        },
                        0x40..=0x7D => Mapping::Rom(rom_addr_mask(rom_offset, rom_len)),
                        0xC0..=0xFF => Mapping::Rom(rom_addr_mask(rom_offset, rom_len)),
                        0x7E..=0x7F => Mapping::None,
                    }
                }
                MapMode::Sdd1 => todo!("S-DD1 memory mapping"),
                MapMode::Sa1 => todo!("SA-1 memory mapping"),
                MapMode::Spc7110 => todo!("SPC7110 memory mapping"),
                MapMode::Unknown(_) => panic!("Unknown memory mapping"),
            };

            mapping[((bank << 5) | page) as usize] = entry;
        }
    }

    mapping
}

fn rom_addr_mask(addr: u32, len: u32) -> u32 {
    let addr = addr & (len.next_power_of_two() - 1);

    assert!(len.count_ones() <= 2, "Unsupported ROM size: {len:#X}");

    let msb = 1 << (31 - len.leading_zeros());
    if addr < msb {
        return addr;
    }

    let offset = msb;
    let len = len & !msb;
    let addr = addr & !msb;

    let msb = 1 << (31 - len.leading_zeros());
    addr & (msb - 1) | offset
}

fn ram_addr_mask(addr: u32, len: u32) -> u32 {
    assert!(len.count_ones() <= 1, "Unsupported SRAM size: {len:#X}");
    addr & (len - 1)
}
