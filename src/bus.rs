use log::{info, trace};

use crate::context;

pub trait Context: context::Rom + context::Timing {}
impl<T: context::Rom + context::Timing> Context for T {}

pub struct Bus {
    wram: Vec<u8>,
}

impl Default for Bus {
    fn default() -> Self {
        Self {
            wram: vec![0; 0x20000], // 128KB
        }
    }
}

const CYCLES_SLOW: u64 = 8;
const CYCLES_FAST: u64 = 6;
const CYCLES_JOY: u64 = 12;

impl Bus {
    pub fn read(&mut self, ctx: &mut impl Context, addr: u32) -> u8 {
        let bank = addr >> 16;
        let offset = addr as u16;

        let data = match bank {
            0x00..=0x3F => match offset {
                0x0000..=0x1FFF => {
                    ctx.elapse(CYCLES_SLOW);
                    self.wram[offset as usize]
                }
                0x2000..=0x20FF => {
                    panic!("Read unused region: {bank:02X}:{offset:04X}")
                }
                0x2100..=0x21FF => {
                    ctx.elapse(CYCLES_FAST);
                    info!("Read I/O: {bank:02X}:{offset:04X}");
                    0
                }
                0x2200..=0x3FFF => {
                    panic!("Read unused region: {bank:02X}:{offset:04X}")
                }
                0x4000..=0x41FF => {
                    ctx.elapse(CYCLES_JOY);
                    info!("Read Joypad I/O: {bank:02X}:{offset:04X}");
                    0
                }
                0x4200..=0x5FFF => {
                    ctx.elapse(CYCLES_FAST);
                    info!("Read I/O: {bank:02X}:{offset:04X}");
                    0
                }
                0x6000..=0x7FFF => {
                    panic!("Read expantion region: {bank:02X}:{offset:04X}")
                }
                0x8000..=0xFFFF => {
                    // WS1 LoROM
                    ctx.elapse(CYCLES_SLOW);
                    let rom_offset = (bank << 15) | addr & 0x7FFF;
                    ctx.rom().rom[rom_offset as usize]
                }
            },

            0x7E..=0x7F => {
                ctx.elapse(CYCLES_SLOW);
                self.wram[(addr & 0x1FFFF) as usize]
            }
            _ => todo!("Read:  {bank:02X}:{offset:04X}"),
        };
        trace!("Read:  {bank:02X}:{offset:04X} = {data:#04X}");
        data
    }

    pub fn read_pure(&self, ctx: &impl Context, addr: u32) -> Option<u8> {
        let bank = addr >> 16;
        let offset = addr as u16;

        Some(match bank {
            0x00..=0x3F => match offset {
                0x0000..=0x1FFF => self.wram[offset as usize],
                0x8000..=0xFFFF => {
                    // WS1 LoROM
                    let rom_offset = (bank << 15) | addr & 0x7FFF;
                    ctx.rom().rom[rom_offset as usize]
                }
                _ => None?,
            },
            0x7E..=0x7F => self.wram[(addr & 0x1FFFF) as usize],
            _ => todo!("Read:  {bank:02X}:{offset:04X}"),
        })
    }

    pub fn write(&mut self, ctx: &mut impl Context, addr: u32, data: u8) {
        let bank = addr >> 16;
        let offset = addr as u16;

        trace!("Write:  {bank:02X}:{offset:04X} = {data:#04X}");

        match bank {
            0x00..=0x3F => match offset {
                0x0000..=0x1FFF => {
                    ctx.elapse(CYCLES_SLOW);
                    self.wram[offset as usize] = data;
                }
                0x2000..=0x20FF => {
                    panic!("Write unused region: {bank:02X}:{offset:04X}");
                }
                0x2100..=0x21FF => {
                    ctx.elapse(CYCLES_FAST);
                    info!("Write I/O: {offset:#06X} = {data:#04X}");
                }
                0x2200..=0x3FFF => {
                    panic!("Write unused region: {bank:02X}:{offset:04X}")
                }
                0x4000..=0x41FF => {
                    ctx.elapse(CYCLES_JOY);
                    info!("Write Joypad I/O: {offset:#06X} = {data:#04X}");
                }
                0x4200..=0x5FFF => {
                    ctx.elapse(CYCLES_FAST);
                    info!("Write I/O: {offset:#06X} = {data:#04X}");
                }
                0x6000..=0x7FFF => {
                    panic!("Write expantion region: {bank:02X}:{offset:04X}")
                }
                0x8000..=0xFFFF => {
                    // WS1 LoROM
                    ctx.elapse(CYCLES_SLOW);
                    panic!("Write rom region: {bank:02X}:{offset:04X}")
                }
            },
            0x7E..=0x7F => {
                ctx.elapse(CYCLES_SLOW);
                self.wram[(addr & 0x1FFFF) as usize] = data
            }

            _ => todo!("Write:  {bank:02X}:{offset:04X} = {data:#04X}"),
        }
    }
}
