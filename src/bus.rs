use log::{debug, info, trace, warn};
use modular_bitfield::prelude::*;

use crate::context;

const CYCLES_SLOW: u64 = 8;
const CYCLES_FAST: u64 = 6;
const CYCLES_JOY: u64 = 12;

pub trait Context:
    context::Ppu + context::Spc + context::Cartridge + context::Interrupt + context::Timing
{
}
impl<
        T: context::Ppu + context::Spc + context::Cartridge + context::Interrupt + context::Timing,
    > Context for T
{
}

pub struct Bus {
    ws2_access_cycle: u64,
    interrupt_enable: InterruptEnable,
    mul_a: u8,
    mul_b: u8,
    div_a: u16,
    div_b: u8,
    div_quot: u16,
    div_rem_or_mul: u16,
    h_count: u16,
    v_count: u16,
    gdma_enable: u8,
    hdma_enable: u8,
    dma: [Dma; 8],

    gdma_do_transfer: bool,
    controller_port: [ControllerPort; 2],
    open_bus: u8,

    wram: Vec<u8>,
    wram_addr: u32,
}

#[bitfield(bits = 8)]
#[derive(Default, Debug)]
struct InterruptEnable {
    joypad_enable: bool, // Enable Automatic Reading of Joypad
    #[skip]
    __: B3,
    hvirq_enable: B2,
    #[skip]
    __: B1,
    vblank_nmi_enable: bool,
}

#[derive(Default)]
struct Dma {
    param: DmaParam,
    iobus_addr: u8,
    cur_addr: u16, // Or HDMA table start table address
    cur_bank: u8,  // Or HDMA table start table bank
    byte_count_or_indirect_hdma_addr: u16,
    indirect_hdma_bank: u8,
    hdma_cur_addr: u16,
    hdma_line_counter: u8,
    unused: u8,

    gdma_do_transfer: bool,
    hdma_do_transfer: bool,
    hdma_done_transfer: bool,
}

impl Dma {
    fn hdma_addr(&mut self, inc: u16) -> u32 {
        let ret = (self.cur_bank as u32) << 16 | self.hdma_cur_addr as u32;
        self.hdma_cur_addr = self.hdma_cur_addr.wrapping_add(inc);
        ret
    }

    fn hdma_indirect_addr(&mut self, inc: u16) -> u32 {
        let ret =
            (self.indirect_hdma_bank as u32) << 16 | self.byte_count_or_indirect_hdma_addr as u32;
        self.byte_count_or_indirect_hdma_addr =
            self.byte_count_or_indirect_hdma_addr.wrapping_add(inc);
        ret
    }

    fn transfer_unit(&self) -> &'static [usize] {
        match self.param.transfer_unit() {
            0 => &[0],
            1 => &[0, 1],
            2 | 6 => &[0, 0],
            3 | 7 => &[0, 0, 1, 1],
            4 => &[0, 1, 2, 3],
            5 => &[0, 1, 0, 1],
            _ => unreachable!(),
        }
    }
}

#[bitfield(bits = 8)]
#[derive(Default, Debug)]
struct DmaParam {
    transfer_unit: B3,
    abus_addr_step: DmaAddrStep,
    #[skip]
    __: B1,
    addr_mode: DmaAddrMode,
    transfer_dir: DmaTransferDir,
}

#[derive(BitfieldSpecifier)]
#[bits = 2]
#[derive(Default, Debug)]
enum DmaAddrStep {
    #[default]
    Increment = 0,
    Fixed = 1,
    Decrement = 2,
    Fixed2 = 3, // same as Fixed
}

#[derive(BitfieldSpecifier, Debug)]
#[bits = 1]
#[derive(Default)]
enum DmaAddrMode {
    #[default]
    Direct = 0,
    Indirect = 1,
}

#[derive(BitfieldSpecifier)]
#[bits = 1]
#[derive(Default, Debug)]
enum DmaTransferDir {
    #[default]
    CpuToIo = 0,
    IoToCpu = 1,
}

impl Default for Bus {
    fn default() -> Self {
        Self {
            ws2_access_cycle: 8,
            interrupt_enable: InterruptEnable::default(),
            mul_a: 0xFF,
            mul_b: 0xFF,
            div_a: 0xFFFF,
            div_b: 0xFF,
            div_quot: 0x0000,
            div_rem_or_mul: 0x0000,
            h_count: 0x1FF,
            v_count: 0x1FF,
            gdma_enable: 0,
            hdma_enable: 0,
            dma: Default::default(),
            gdma_do_transfer: false,
            controller_port: Default::default(),
            open_bus: 0,
            wram: vec![0; 0x20000], // 128KB
            wram_addr: 0,
        }
    }
}

#[derive(Default)]
struct ControllerPort {
    enable: bool,
    pad_data: [u16; 2],
    clk: bool,
    pos: usize,
}

const NOT_DMA: u8 = 0;
const DMA_A: u8 = 1;
const DMA_B: u8 = 2;

impl Bus {
    pub fn set_input(&mut self, input: &meru_interface::InputData) {
        for i in 0..4 {
            if i >= input.controllers.len() {
                self.controller_port[i % 2].pad_data[i / 2] = 0;
                continue;
            }

            let mut data = 0;

            for (name, value) in &input.controllers[i] {
                match name.as_str() {
                    "B" => data |= (*value as u16) << 15,
                    "Y" => data |= (*value as u16) << 14,
                    "Select" => data |= (*value as u16) << 13,
                    "Start" => data |= (*value as u16) << 12,
                    "Up" => data |= (*value as u16) << 11,
                    "Down" => data |= (*value as u16) << 10,
                    "Left" => data |= (*value as u16) << 9,
                    "Right" => data |= (*value as u16) << 8,
                    "A" => data |= (*value as u16) << 7,
                    "X" => data |= (*value as u16) << 6,
                    "L" => data |= (*value as u16) << 5,
                    "R" => data |= (*value as u16) << 4,
                    _ => unreachable!(),
                }
            }

            self.controller_port[i % 2].pad_data[i / 2] = data;
        }
    }

    pub fn tick(&mut self, ctx: &mut impl Context) {
        self.dma_exec(ctx, true);
    }

    pub fn read<C: Context, const DMA: u8>(&mut self, ctx: &mut C, addr: u32) -> u8 {
        let bank = addr >> 16;
        let offset = addr as u16;

        let data = match bank {
            0x00..=0x3F | 0x80..=0xBF => match offset {
                0x0000..=0x1FFF => {
                    if DMA == NOT_DMA {
                        ctx.elapse(CYCLES_SLOW);
                    }
                    self.wram[offset as usize]
                }
                0x2000..=0x20FF => {
                    if DMA == NOT_DMA {
                        ctx.elapse(CYCLES_FAST);
                    }
                    warn!("Read unused region (open bus): {bank:02X}:{offset:04X}");
                    self.open_bus
                }
                0x2100..=0x21FF => {
                    if DMA == NOT_DMA {
                        ctx.elapse(CYCLES_FAST);
                    }

                    if DMA != DMA_A {
                        self.io_read(ctx, offset)
                    } else {
                        self.open_bus
                    }
                }
                0x2200..=0x3FFF => {
                    if DMA == NOT_DMA {
                        ctx.elapse(CYCLES_FAST);
                    }
                    warn!("Read unused region (open bus): {bank:02X}:{offset:04X}");
                    self.open_bus
                }
                0x4000..=0x41FF => {
                    if DMA == NOT_DMA {
                        ctx.elapse(CYCLES_JOY);
                    }
                    if DMA != DMA_A {
                        self.io_read(ctx, offset)
                    } else {
                        self.open_bus | if addr == 0x4017 { 0x1C } else { 0 }
                    }
                }
                0x4200..=0x5FFF => {
                    if DMA == NOT_DMA {
                        ctx.elapse(CYCLES_FAST);
                    }
                    if DMA != DMA_A {
                        self.io_read(ctx, offset)
                    } else {
                        match addr {
                            0x4210..=0x421F => self.io_read(ctx, offset),
                            0x4300..=0x437F => 0xFF, // Special open bus
                            _ => self.open_bus,
                        }
                    }
                }
                0x6000..=0x7FFF => {
                    if DMA == NOT_DMA {
                        ctx.elapse(CYCLES_SLOW);
                    }
                    ctx.cartridge().read(addr)
                }
                0x8000..=0xFFFF => {
                    if DMA == NOT_DMA {
                        ctx.elapse(if bank & 0x80 == 0 {
                            CYCLES_SLOW
                        } else {
                            self.ws2_access_cycle
                        });
                    }
                    ctx.cartridge().read(addr)
                }
            },
            0x40..=0x7D => {
                if DMA == NOT_DMA {
                    ctx.elapse(CYCLES_SLOW);
                }
                ctx.cartridge().read(addr)
            }
            0x7E..=0x7F => {
                if DMA == NOT_DMA {
                    ctx.elapse(CYCLES_SLOW);
                }
                self.wram[(addr & 0x1FFFF) as usize]
            }
            0xC0..=0xFF => {
                if DMA == NOT_DMA {
                    ctx.elapse(self.ws2_access_cycle);
                }
                ctx.cartridge().read(addr)
            }
            _ => unreachable!(),
        };
        trace!("Read:  {bank:02X}:{offset:04X} = {data:#04X}");
        self.open_bus = data;
        data
    }

    pub fn read_pure(&self, ctx: &impl Context, addr: u32) -> Option<u8> {
        let bank = addr >> 16;
        let offset = addr as u16;

        Some(match bank {
            0x00..=0x3F | 0x80..=0xBF => match offset {
                0x0000..=0x1FFF => self.wram[offset as usize],
                0x6000..=0xFFFF => ctx.cartridge().read(addr),
                _ => None?,
            },
            0x40..=0x7D => ctx.cartridge().read(addr),
            0x7E..=0x7F => self.wram[(addr & 0x1FFFF) as usize],
            0xC0..=0xFF => ctx.cartridge().read(addr),
            _ => unreachable!(),
        })
    }

    pub fn write<C: Context, const DMA: u8>(&mut self, ctx: &mut C, addr: u32, data: u8) {
        let bank = addr >> 16;
        let offset = addr as u16;

        trace!("Write:  {bank:02X}:{offset:04X} = {data:#04X}");
        self.open_bus = data;

        match bank {
            0x00..=0x3F | 0x80..=0xBF => match offset {
                0x0000..=0x1FFF => {
                    if DMA == NOT_DMA {
                        ctx.elapse(CYCLES_SLOW);
                    }
                    self.wram[offset as usize] = data;
                }
                0x2000..=0x20FF => {
                    if DMA == NOT_DMA {
                        ctx.elapse(CYCLES_FAST);
                    }
                    warn!("Write unused region: {bank:02X}:{offset:04X}");
                }
                0x2100..=0x21FF => {
                    if DMA == NOT_DMA {
                        ctx.elapse(CYCLES_FAST);
                    }
                    if DMA != DMA_A {
                        self.io_write(ctx, offset, data);
                    }
                }
                0x2200..=0x3FFF => {
                    if DMA == NOT_DMA {
                        ctx.elapse(CYCLES_FAST);
                    }
                    warn!("Write unused region: {bank:02X}:{offset:04X}")
                }
                0x4000..=0x41FF => {
                    if DMA == NOT_DMA {
                        ctx.elapse(CYCLES_JOY);
                    }
                    if DMA != DMA_A {
                        self.io_write(ctx, offset, data);
                    }
                }
                0x4200..=0x5FFF => {
                    if DMA == NOT_DMA {
                        ctx.elapse(CYCLES_FAST);
                    }
                    if DMA != DMA_A {
                        self.io_write(ctx, offset, data);
                    } else {
                        match addr {
                            0x4210..=0x421F => self.io_write(ctx, offset, data),
                            _ => {}
                        }
                    }
                }
                0x6000..=0x7FFF => {
                    if DMA == NOT_DMA {
                        ctx.elapse(CYCLES_SLOW);
                    }
                    ctx.cartridge_mut().write(addr, data);
                }
                0x8000..=0xFFFF => {
                    if DMA == NOT_DMA {
                        ctx.elapse(if bank & 0x80 == 0 {
                            CYCLES_SLOW
                        } else {
                            self.ws2_access_cycle
                        });
                    }
                    ctx.cartridge_mut().write(addr, data);
                }
            },
            0x40..=0x7D => {
                if DMA == NOT_DMA {
                    ctx.elapse(CYCLES_SLOW);
                }
                ctx.cartridge_mut().write(addr, data)
            }
            0x7E..=0x7F => {
                if DMA == NOT_DMA {
                    ctx.elapse(CYCLES_SLOW);
                }
                self.wram[(addr & 0x1FFFF) as usize] = data
            }
            0xC0..=0xFF => {
                if DMA == NOT_DMA {
                    ctx.elapse(self.ws2_access_cycle);
                }
                ctx.cartridge_mut().write(addr, data);
            }
            _ => unreachable!(),
        }
    }

    fn read16<C: Context, const DMA: u8>(&mut self, ctx: &mut C, addr: u32) -> u16 {
        let b0 = self.read::<_, DMA>(ctx, addr);
        let b1 = self.read::<_, DMA>(ctx, addr & 0xFF0000 | (addr as u16).wrapping_add(1) as u32);
        (b1 as u16) << 8 | b0 as u16
    }

    fn io_read(&mut self, ctx: &mut impl Context, addr: u16) -> u8 {
        // Sync PPU counter
        // FIXME: render flag
        ctx.ppu_tick(true);
        self.dma_exec(ctx, false);

        let data = match addr {
            // FIXME: Does PPU open bus affect CPU open bus?
            0x2100..=0x213F => ctx.ppu_read(addr, self.open_bus),

            0x2140..=0x217F => {
                let now = ctx.now();
                let ret = ctx.spc_mut().read_port((addr & 3) as _, now);
                if addr & 3 == 1 {
                    debug!("SPC {} -> {:02X} @ {}", addr & 3, ret, ctx.now());
                }
                ret
            }

            // WMDATA  - WRAM Data Read/Write
            0x2180 => {
                let ret = self.wram[self.wram_addr as usize];
                self.wram_addr = (self.wram_addr + 1) & 0x1FFFF;
                ret
            }

            // CPU On-Chip I/O Ports
            // JOYA - Joypad Input Register A (R)
            0x4016 => {
                info!("Read JOYA");
                let b0 = !self.controller_read(0, 4);
                let b1 = !self.controller_read(0, 5);
                self.controller_write(0, 2, true);
                self.controller_write(0, 2, false);
                b0 as u8 | (b1 as u8) << 1 | self.open_bus & 0x7C
            }
            // JOYB - Joypad Input Register B (R)
            0x4017 => {
                info!("Read JOYB");
                // FIXME: Manual read from joy pad and strobe
                let b0 = !self.controller_read(1, 4);
                let b1 = !self.controller_read(1, 5);
                self.controller_write(1, 2, true);
                self.controller_write(1, 2, false);
                b0 as u8 | (b1 as u8) << 1 | 0x1C | self.open_bus & 0xE0
            }

            // CPU On-Chip I/O Ports (Read-only)
            // RDNMI - V-Blank NMI Flag and CPU Version Number (Read/Ack)
            0x4210 => {
                let nmi_flag = ctx.interrupt_mut().nmi_flag();
                let cpu_version = 2; // ???
                (nmi_flag as u8) << 7 | cpu_version | self.open_bus & 0x70
            }

            // TIMEUP - H/V-Timer IRQ Flag (Read/Ack)
            0x4211 => {
                let ret = (ctx.interrupt().irq() as u8) << 7;
                // 0-6: FIXME: open-bus
                ctx.interrupt_mut().set_irq(false);
                ret | self.open_bus & 0x7F
            }
            // HVBJOY - H/V-Blank flag and Joypad Busy flag (R)
            0x4212 => {
                let mut ret = 0;
                // FIXME: bit 0 = Auto-Joypad-Read Busy Flag (1=Busy)
                ret |= (ctx.ppu().hblank() as u8) << 6;
                ret |= (ctx.ppu().vblank() as u8) << 7;
                ret | self.open_bus & 0x3E
            }

            // RDIO    - Joypad Programmable I/O Port (Input)
            0x4213 => {
                info!("Read RDIO");
                let b6 = self.controller_read(0, 6);
                let b7 = self.controller_read(1, 6);
                (b6 as u8) << 6 | (b7 as u8) << 7
            }

            // RDDIVL/H - Unsigned Division Result (Quotient)
            0x4214 => self.div_quot as u8,
            0x4215 => (self.div_quot >> 8) as u8,
            // RDMPYL/H  - Unsigned Division Remainder / Multiply Product
            0x4216 => self.div_rem_or_mul as u8,
            0x4217 => (self.div_rem_or_mul >> 8) as u8,

            0x4218..=0x421F => {
                let i = (addr - 0x4218) as usize / 2;
                let h = (addr - 0x4218) as usize % 2;
                info!("JOY{i}{}", if h == 0 { 'L' } else { 'H' });
                (self.controller_port[i % 2].pad_data[i / 2] >> (8 * h)) as u8
            }

            // CPU DMA, For below ports, x = Channel number 0..7 (R/W)
            0x4300..=0x437F => self.dma_read(((addr >> 4) & 0x7) as _, (addr & 0xF) as _),

            0x2181..=0x3FFF
            | 0x4000..=0x4015
            | 0x4018..=0x420F
            | 0x4220..=0x42FF
            | 0x4380..=0x5FFF => {
                warn!("Read open bus: {addr:04X}");
                self.open_bus
            }

            _ => unreachable!("IO Read: {addr:#06X}"),
        };

        trace!(
            "IO Read: {addr:#06X}{} = {data:#04X}",
            ioreg_info(addr).map_or_else(|| "".to_string(), |info| format!("({})", info.name))
        );
        data
    }

    fn io_write(&mut self, ctx: &mut impl Context, addr: u16, data: u8) {
        // FIXME: hack for sub-instruction cycle accuracy
        // FIXME: render flag
        ctx.ppu_tick(true);
        self.dma_exec(ctx, false);

        match addr {
            0x2100..=0x213F => ctx.ppu_write(addr, data),
            0x2140..=0x217F => {
                if addr & 3 == 1 {
                    debug!("SPC {} <- {:02X} @ {}", addr & 3, data, ctx.now());
                }
                let now = ctx.now();
                ctx.spc_mut().write_port((addr & 3) as _, data, now);
            }

            0x2180 => {
                self.wram[self.wram_addr as usize] = data;
                self.wram_addr = (self.wram_addr + 1) & 0x1FFFF;
            }
            0x2181 => self.wram_addr = (self.wram_addr & 0x1FF00) | data as u32,
            0x2182 => self.wram_addr = (self.wram_addr & 0x100FF) | ((data as u32) << 8),
            0x2183 => self.wram_addr = (self.wram_addr & 0x0FFFF) | ((data as u32 & 1) << 16),

            // CPU On-Chip I/O Ports
            0x4016 => {
                info!("WRITE JOYWR: {data:02X}");
                if data & 1 != 0 {
                    self.controller_write(0, 3, true);
                    self.controller_write(1, 3, true);
                }
            }

            // CPU On-Chip I/O Ports (Write-only) (Read=open bus)
            0x4200 => {
                self.interrupt_enable.bytes[0] = data;
                let interrupt = ctx.interrupt_mut();
                interrupt.set_nmi_enable(self.interrupt_enable.vblank_nmi_enable());
                interrupt.set_hvirq_enable(self.interrupt_enable.hvirq_enable());
                debug!("NMITIMEN = {:?}", self.interrupt_enable);
            }
            0x4201 => {
                self.controller_port[0].enable = data & (1 << 6) != 0;
                self.controller_port[1].enable = data & (1 << 7) != 0;
            }
            0x4202 => self.mul_a = data,
            0x4203 => {
                self.mul_b = data;
                // FIXME: Delay 8 cycles
                // FIXME: Write during mul should clear intermediate result
                self.div_rem_or_mul = self.mul_a as u16 * self.mul_b as u16;
                self.div_quot = self.mul_b as u16;
            }
            0x4204 => self.div_a = self.div_a & 0xFF00 | data as u16,
            0x4205 => self.div_a = self.div_a & 0x00FF | ((data as u16) << 8),
            0x4206 => {
                self.div_b = data;
                // FIXME: Delay 16 cycles
                if self.div_b != 0 {
                    self.div_quot = self.div_a / self.div_b as u16;
                    self.div_rem_or_mul = self.div_a % self.div_b as u16;
                } else {
                    self.div_quot = 0xFFFF;
                    self.div_rem_or_mul = self.div_a;
                }
            }
            0x4207 => {
                self.h_count = self.h_count & 0x0100 | data as u16;
                ctx.interrupt_mut().set_h_count(self.h_count);
            }
            0x4208 => {
                self.h_count = self.h_count & 0x00FF | ((data as u16) << 8);
                ctx.interrupt_mut().set_h_count(self.h_count);
            }
            0x4209 => {
                self.v_count = self.v_count & 0x0100 | data as u16;
                ctx.interrupt_mut().set_v_count(self.v_count);
            }
            0x420A => {
                self.v_count = self.v_count & 0x00FF | ((data as u16) << 8);
                ctx.interrupt_mut().set_v_count(self.v_count);
            }
            0x420B => {
                self.gdma_enable = data;
                info!("GDMA Enable: {data:08b} @ y = {}", ctx.counter().y);
            }
            0x420C => {
                self.hdma_enable = data;
                info!("HDMA Enable: {data:08b} @ y = {}", ctx.counter().y);
            }
            0x420D => self.ws2_access_cycle = if data & 1 == 0 { 8 } else { 6 },

            // CPU DMA, For below ports, x = Channel number 0..7 (R/W)
            0x4300..=0x437F => self.dma_write(((addr >> 4) & 0x7) as _, (addr & 0xF) as _, data),

            0x2184..=0x4015 | 0x4017..=0x41FF | 0x420E..=0x42FF | 0x4380..=0x5FFF => {
                warn!("Write to Unused region: {addr:#06X} = {data:#04X}")
            }

            _ => unreachable!("IO Write: {addr:#06X} = {data:#04X}"),
        }
    }

    fn dma_read(&mut self, ch: usize, cmd: u8) -> u8 {
        match cmd {
            0 => self.dma[ch].param.bytes[0],
            1 => self.dma[ch].iobus_addr,
            2 => self.dma[ch].cur_addr as u8,
            3 => (self.dma[ch].cur_addr >> 8) as u8,
            4 => self.dma[ch].cur_bank,
            5 => self.dma[ch].byte_count_or_indirect_hdma_addr as u8,
            6 => (self.dma[ch].byte_count_or_indirect_hdma_addr >> 8) as u8,
            7 => self.dma[ch].indirect_hdma_bank,
            8 => self.dma[ch].hdma_cur_addr as u8,
            9 => (self.dma[ch].hdma_cur_addr >> 8) as u8,
            0xA => self.dma[ch].hdma_line_counter,
            0xB | 0xF => self.dma[ch].unused,
            0xC..=0xE => {
                warn!("Invalid DMA register read: {cmd:0X}");
                self.open_bus
            }
            _ => unreachable!(),
        }
    }

    fn dma_write(&mut self, ch: usize, cmd: u8, data: u8) {
        match cmd {
            0 => self.dma[ch].param.bytes[0] = data,
            1 => self.dma[ch].iobus_addr = data,
            2 => self.dma[ch].cur_addr = self.dma[ch].cur_addr & 0xFF00 | data as u16,
            3 => self.dma[ch].cur_addr = self.dma[ch].cur_addr & 0x00FF | ((data as u16) << 8),
            4 => self.dma[ch].cur_bank = data,
            5 => {
                self.dma[ch].byte_count_or_indirect_hdma_addr =
                    self.dma[ch].byte_count_or_indirect_hdma_addr & 0xFF00 | data as u16
            }
            6 => {
                self.dma[ch].byte_count_or_indirect_hdma_addr =
                    self.dma[ch].byte_count_or_indirect_hdma_addr & 0x00FF | ((data as u16) << 8)
            }
            7 => self.dma[ch].indirect_hdma_bank = data,
            8 => self.dma[ch].hdma_cur_addr = self.dma[ch].hdma_cur_addr & 0xFF00 | data as u16,
            9 => {
                self.dma[ch].hdma_cur_addr =
                    self.dma[ch].hdma_cur_addr & 0x00FF | ((data as u16) << 8)
            }
            0xA => self.dma[ch].hdma_line_counter = data,
            0xB | 0xF => self.dma[ch].unused = data,

            0xC..=0xE => {
                warn!("Invalid DMA register write: {cmd:0X} = {data:#04X}")
            }

            _ => unreachable!(),
        }
    }

    fn dma_exec(&mut self, ctx: &mut impl Context, exec_gdma: bool) {
        if ctx.ppu_mut().hdma_reload() {
            for ch in 0..8 {
                self.dma[ch].hdma_done_transfer = false;
                self.dma[ch].hdma_do_transfer = false;
            }

            if self.hdma_enable != 0 {
                ctx.elapse(18);
            }

            // FIXME: Set do_transfer = true if any HDMA are enabled

            for ch in 0..8 {
                if self.hdma_enable & (1 << ch) != 0 {
                    self.hdma_reload(ctx, ch);
                }
            }
        }

        if ctx.ppu_mut().hdma_transfer()
            && self.hdma_enable != 0
            && self.dma.iter().any(|dma| !dma.hdma_done_transfer)
        {
            ctx.elapse(18);
            for ch in 0..8 {
                if self.hdma_enable & (1 << ch) != 0 && !self.dma[ch].hdma_done_transfer {
                    self.hdma_exec(ctx, ch);
                }
            }
        }

        if exec_gdma && self.gdma_enable != 0 {
            if !self.gdma_do_transfer {
                // Wait 2~8 master cycle to adjust master cycle to multiple of 8
                ctx.elapse(8 - (ctx.now() & 7));
                self.gdma_do_transfer = true;
            }

            let ch = self.gdma_enable.trailing_zeros() as usize;
            self.gdma_exec(ctx, ch);

            if self.gdma_enable == 0 {
                self.gdma_do_transfer = false;
                // Overhead for dma done
                ctx.elapse(8);
                // FIXME: Adjust master cycle to multiple of CPU speed (?)
                ctx.elapse(8);
            }
            return;
        }
    }

    fn gdma_exec(&mut self, ctx: &mut impl Context, ch: usize) {
        // FIXME: WRAM to WRAM DMA is not possible.
        // FIXME: Accessing B-bus adddress from A-bus address causes open-bus access

        if !self.dma[ch].gdma_do_transfer {
            self.dma[ch].gdma_do_transfer = true;
            // 8 cycles for init DMA channel
            ctx.elapse(8);
        }

        let transfer_unit: &[usize] = self.dma[ch].transfer_unit();

        let mut a_addr = self.dma[ch].cur_addr;
        let b_addr = self.dma[ch].iobus_addr;

        debug!(
            "GDMA[{ch}]: {:02X}:{a_addr:04X} {} 21{b_addr:02X}, trans: {transfer_unit:?}, count: {}",
            self.dma[ch].cur_bank,
            if matches!(self.dma[ch].param.transfer_dir(), DmaTransferDir::CpuToIo) {
                "->"
            } else {
                "<-"
            },
            self.dma[ch].byte_count_or_indirect_hdma_addr,
        );

        let a_inc = match self.dma[ch].param.abus_addr_step() {
            DmaAddrStep::Increment => 1,
            DmaAddrStep::Decrement => (-1_i16) as u16,
            DmaAddrStep::Fixed | DmaAddrStep::Fixed2 => 0,
        };

        let transfer_dir = self.dma[ch].param.transfer_dir();

        for i in 0..transfer_unit.len() {
            let cpu_addr = (self.dma[ch].cur_bank as u32) << 16 | a_addr as u32;
            let io_addr = 0x2100 | b_addr.wrapping_add(transfer_unit[i] as u8) as u32;

            match transfer_dir {
                DmaTransferDir::CpuToIo => {
                    let data = self.read::<_, DMA_A>(ctx, cpu_addr);
                    self.write::<_, DMA_B>(ctx, io_addr, data);
                }
                DmaTransferDir::IoToCpu => {
                    let data = self.read::<_, DMA_B>(ctx, io_addr);
                    self.write::<_, DMA_A>(ctx, cpu_addr, data);
                }
            }
            // 8 cycles for each byte
            ctx.elapse(8);

            a_addr = a_addr.wrapping_add(a_inc);

            self.dma[ch].byte_count_or_indirect_hdma_addr = self.dma[ch]
                .byte_count_or_indirect_hdma_addr
                .wrapping_sub(1);
            if self.dma[ch].byte_count_or_indirect_hdma_addr == 0 {
                self.gdma_enable &= !(1 << ch);
                self.dma[ch].gdma_do_transfer = false;
                break;
            }
        }

        self.dma[ch].cur_addr = a_addr;
    }

    fn hdma_reload(&mut self, ctx: &mut impl Context, ch: usize) {
        debug!("HDMA{ch} Init: param = {:?}", self.dma[ch].param);

        // HDMA cancels same channel' GDMA
        self.dma[ch].gdma_do_transfer = false;
        self.gdma_enable &= !(1 << ch);

        self.dma[ch].hdma_cur_addr = self.dma[ch].cur_addr;

        let addr = self.dma[ch].hdma_addr(1);
        let v = self.read::<_, DMA_A>(ctx, addr);
        // ctx.elapse(8);
        if v == 0 {
            info!("HDMA{ch}: Empty table");
            self.dma[ch].hdma_done_transfer = true;
            return;
        }
        self.dma[ch].hdma_line_counter = v;

        if matches!(self.dma[ch].param.addr_mode(), DmaAddrMode::Indirect) {
            let addr = self.dma[ch].hdma_addr(2);
            self.dma[ch].byte_count_or_indirect_hdma_addr = self.read16::<_, DMA_A>(ctx, addr);
            debug!(
                "HDMA{ch}: Indirect addr = {:04X}",
                self.dma[ch].byte_count_or_indirect_hdma_addr
            );
            ctx.elapse(16);
        }

        self.dma[ch].hdma_do_transfer = true;

        debug!(
            "HDMA {ch} Reload: Line counter: {:02X}, addr: {:04X} -> 21{:02X}",
            self.dma[ch].hdma_line_counter, self.dma[ch].hdma_cur_addr, self.dma[ch].iobus_addr,
        );
    }

    fn hdma_exec(&mut self, ctx: &mut impl Context, ch: usize) {
        debug!(
            "HDMA {ch}: Begin exec at line {}:{}",
            ctx.counter().y,
            ctx.counter().x
        );

        // HDMA cancels same channel' GDMA
        self.dma[ch].gdma_do_transfer = false;
        self.gdma_enable &= !(1 << ch);

        // ctx.elapse(8);

        if self.dma[ch].hdma_do_transfer {
            let transfer_unit = self.dma[ch].transfer_unit();

            debug!("HDMA {ch}: Do trans {} bytes", transfer_unit.len());

            for i in 0..transfer_unit.len() {
                let cpu_addr = match self.dma[ch].param.addr_mode() {
                    DmaAddrMode::Direct => self.dma[ch].hdma_addr(1),
                    DmaAddrMode::Indirect => self.dma[ch].hdma_indirect_addr(1),
                };
                let io_addr =
                    0x2100 | self.dma[ch].iobus_addr.wrapping_add(transfer_unit[i] as u8) as u32;

                if matches!(self.dma[ch].param.transfer_dir(), DmaTransferDir::CpuToIo) {
                    let data = self.read::<_, DMA_A>(ctx, cpu_addr);
                    self.write::<_, DMA_B>(ctx, io_addr, data);
                    debug!("HDMA: {cpu_addr:06X} -> {io_addr:04X} = {data:02X}");
                } else {
                    let data = self.read::<_, DMA_B>(ctx, io_addr);
                    self.write::<_, DMA_A>(ctx, cpu_addr, data);
                    debug!("HDMA: {io_addr:06X} -> {cpu_addr:04X} = {data:02X}");
                }

                ctx.elapse(8);
            }
        }

        self.dma[ch].hdma_line_counter -= 1;
        self.dma[ch].hdma_do_transfer = self.dma[ch].hdma_line_counter & 0x80 != 0;

        debug!(
            "HDMA {ch}: Line counter: {:02X}, Do transfer: {}",
            self.dma[ch].hdma_line_counter, self.dma[ch].hdma_do_transfer
        );

        // FIXME: read one byte unless line counter != 0

        if self.dma[ch].hdma_line_counter & 0x7F == 0 {
            let addr = self.dma[ch].hdma_addr(1);
            let data = self.read::<_, DMA_A>(ctx, addr);
            ctx.elapse(8);
            self.dma[ch].hdma_line_counter = data;

            debug!(
                "HDMA {ch}: New line counter: {:02X}",
                self.dma[ch].hdma_line_counter
            );

            if matches!(self.dma[ch].param.addr_mode(), DmaAddrMode::Indirect) {
                if self.dma[ch].hdma_line_counter != 0 {
                    let addr = self.dma[ch].hdma_addr(2);
                    self.dma[ch].byte_count_or_indirect_hdma_addr =
                        self.read16::<_, DMA_A>(ctx, addr);
                    ctx.elapse(16);
                } else {
                    // Odd behavior: if the line counter is 0, only 1 byte read and it is placed in MSB
                    let addr = self.dma[ch].hdma_addr(1);
                    self.dma[ch].byte_count_or_indirect_hdma_addr =
                        (self.read::<_, DMA_A>(ctx, addr) as u16) << 8;
                    ctx.elapse(8);
                }
            }

            if self.dma[ch].hdma_line_counter == 0 {
                self.dma[ch].hdma_done_transfer = true;
                debug!("HDMA {ch}: Done");
            }

            self.dma[ch].hdma_do_transfer = true;
        }

        debug!(
            "HDMA {ch}: End exec, Do transfer: {}",
            self.dma[ch].hdma_do_transfer
        );
    }

    fn controller_read(&mut self, port: usize, pin: usize) -> bool {
        let controller = &self.controller_port[port];

        if !controller.enable {
            return false;
        }

        match pin {
            4 => controller.pos < 16 && controller.pad_data[0] & (1 << controller.pos) != 0,
            5 => controller.pos < 16 && controller.pad_data[1] & (1 << controller.pos) != 0,
            6 => {
                // FIXME
                warn!("Read controller port {port}, pin {pin}");
                true
            }
            _ => unreachable!(),
        }
    }

    fn controller_write(&mut self, port: usize, pin: usize, data: bool) {
        match pin {
            2 => {
                let prev = self.controller_port[port].clk;
                self.controller_port[port].clk = data;
                if !prev && data {
                    self.controller_port[port].pos += 1;
                }
            }
            3 => {
                self.controller_port[port].pos = 0;
                self.controller_port[port].clk = false;
            }
            6 => {
                // FIXME
                warn!("Write controller port {port}, pin {pin} = {data}");
            }
            _ => unreachable!(),
        }
    }
}

struct IoRegInfo {
    addr: u16,
    name: &'static str,
    desc: &'static str,
}

fn ioreg_info(addr: u16) -> Option<&'static IoRegInfo> {
    IO_MAP.iter().find(|info| info.addr == addr)
}

macro_rules! ioregs {
    (@entry $addr:literal, $name:ident, $desc:literal) => {
        IoRegInfo {
            addr: $addr,
            name: stringify!($name),
            desc: $desc,
        }
    };

    ($($addr:literal - $name:ident - $desc:literal)*) => {
        &[$(ioregs!(@entry $addr, $name, $desc), )*]
    };
}

const IO_MAP: &[IoRegInfo] = ioregs! {
    // PPU Picture Processing Unit (Write-Only Ports)
    0x2100 - INIDISP - "Display Control 1"
    0x2101 - OBSEL   - "Object Size and Object Base"
    0x2102 - OAMADDL - "OAM Address (lower 8bit)"
    0x2103 - OAMADDH - "OAM Address (upper 1bit) and Priority Rotation"
    0x2104 - OAMDATA - "OAM Data Write (write-twice)"
    0x2105 - BGMODE  - "BG Mode and BG Character Size"
    0x2106 - MOSAIC  - "Mosaic Size and Mosaic Enable"
    0x2107 - BG1SC   - "BG1 Screen Base and Screen Size"
    0x2108 - BG2SC   - "BG2 Screen Base and Screen Size"
    0x2109 - BG3SC   - "BG3 Screen Base and Screen Size"
    0x210A - BG4SC   - "BG4 Screen Base and Screen Size"
    0x210B - BG12NBA - "BG Character Data Area Designation"
    0x210C - BG34NBA - "BG Character Data Area Designation"
    0x210D - BG1HOFS - "BG1 Horizontal Scroll (X) (write-twice) / M7HOFS"
    0x210E - BG1VOFS - "BG1 Vertical Scroll (Y)   (write-twice) / M7VOFS"
    0x210F - BG2HOFS - "BG2 Horizontal Scroll (X) (write-twice)"
    0x2110 - BG2VOFS - "BG2 Vertical Scroll (Y)   (write-twice)"
    0x2111 - BG3HOFS - "BG3 Horizontal Scroll (X) (write-twice)"
    0x2112 - BG3VOFS - "BG3 Vertical Scroll (Y)   (write-twice)"
    0x2113 - BG4HOFS - "BG4 Horizontal Scroll (X) (write-twice)"
    0x2114 - BG4VOFS - "BG4 Vertical Scroll (Y)   (write-twice)"
    0x2115 - VMAIN   - "VRAM Address Increment Mode"
    0x2116 - VMADDL  - "VRAM Address (lower 8bit)"
    0x2117 - VMADDH  - "VRAM Address (upper 8bit)"
    0x2118 - VMDATAL - "VRAM Data Write (lower 8bit)"
    0x2119 - VMDATAH - "VRAM Data Write (upper 8bit)"
    0x211A - M7SEL   - "Rotation/Scaling Mode Settings"
    0x211B - M7A     - "Rotation/Scaling Parameter A & Maths 16bit operand"
    0x211C - M7B     - "Rotation/Scaling Parameter B & Maths 8bit operand"
    0x211D - M7C     - "Rotation/Scaling Parameter C         (write-twice)"
    0x211E - M7D     - "Rotation/Scaling Parameter D         (write-twice)"
    0x211F - M7X     - "Rotation/Scaling Center Coordinate X (write-twice)"
    0x2120 - M7Y     - "Rotation/Scaling Center Coordinate Y (write-twice)"
    0x2121 - CGADD   - "Palette CGRAM Address"
    0x2122 - CGDATA  - "Palette CGRAM Data Write             (write-twice)"
    0x2123 - W12SEL  - "Window BG1/BG2 Mask Settings"
    0x2124 - W34SEL  - "Window BG3/BG4 Mask Settings"
    0x2125 - WOBJSEL - "Window OBJ/MATH Mask Settings"
    0x2126 - WH0     - "Window 1 Left Position (X1)"
    0x2127 - WH1     - "Window 1 Right Position (X2)"
    0x2128 - WH2     - "Window 2 Left Position (X1)"
    0x2129 - WH3     - "Window 2 Right Position (X2)"
    0x212A - WBGLOG  - "Window 1/2 Mask Logic (BG1-BG4)"
    0x212B - WOBJLOG - "Window 1/2 Mask Logic (OBJ/MATH)"
    0x212C - TM      - "Main Screen Designation"
    0x212D - TS      - "Sub Screen Designation"
    0x212E - TMW     - "Window Area Main Screen Disable"
    0x212F - TSW     - "Window Area Sub Screen Disable"
    0x2130 - CGWSEL  - "Color Math Control Register A"
    0x2131 - CGADSUB - "Color Math Control Register B"
    0x2132 - COLDATA - "Color Math Sub Screen Backdrop Color"
    0x2133 - SETINI  - "Display Control 2"
    // PPU Picture Processing Unit (Read-Only Ports)
    0x2134 - MPYL    - "PPU1 Signed Multiply Result   (lower 8bit)"
    0x2135 - MPYM    - "PPU1 Signed Multiply Result   (middle 8bit)"
    0x2136 - MPYH    - "PPU1 Signed Multiply Result   (upper 8bit)"
    0x2137 - SLHV    - "PPU1 Latch H/V-Counter by Software (Read=Strobe)"
    0x2138 - RDOAM   - "PPU1 OAM Data Read            (read-twice)"
    0x2139 - RDVRAML - "PPU1 VRAM Data Read           (lower 8bits)"
    0x213A - RDVRAMH - "PPU1 VRAM Data Read           (upper 8bits)"
    0x213B - RDCGRAM - "PPU2 CGRAM Data Read (Palette)(read-twice)"
    0x213C - OPHCT   - "PPU2 Horizontal Counter Latch (read-twice)"
    0x213D - OPVCT   - "PPU2 Vertical Counter Latch   (read-twice)"
    0x213E - STAT77  - "PPU1 Status and PPU1 Version Number"
    0x213F - STAT78  - "PPU2 Status and PPU2 Version Number"
    // APU Audio Processing Unit (R/W)
    0x2140 - APUI00  - "Main CPU to Sound CPU Communication Port 0"
    0x2141 - APUI01  - "Main CPU to Sound CPU Communication Port 1"
    0x2142 - APUI02  - "Main CPU to Sound CPU Communication Port 2"
    0x2143 - APUI03  - "Main CPU to Sound CPU Communication Port 3"
    //   2144h..217Fh    - APU Ports 2140-2143h mirrored to 2144h..217Fh
    // WRAM Access
    0x2180 - WMDATA  - "WRAM Data Read/Write       (R/W)"
    0x2181 - WMADDL  - "WRAM Address (lower 8bit)  (W)"
    0x2182 - WMADDM  - "WRAM Address (middle 8bit) (W)"
    0x2183 - WMADDH  - "WRAM Address (upper 1bit)  (W)"
    //   2184h..21FFh    - Unused region (open bus) / Expansion (B-Bus)          -
    //   2200h..3FFFh    - Unused region (open bus) / Expansion (A-Bus)          -
    // CPU On-Chip I/O Ports
    //   4000h..4015h        - Unused region (open bus)      ;\These ports have  -
    0x4016 - JOYWR - "Joypad Output (W)"
    0x4016 - JOYA  - "Joypad Input Register A (R)"
    0x4017 - JOYB  - "Joypad Input Register B (R)"
    //   4018h..41FFh        - Unused region (open bus)      ;/are 3.5MHz fast)  -
    // CPU On-Chip I/O Ports (Write-only) (Read=open bus)
    0x4200 - NMITIMEN- "Interrupt Enable and Joypad Request"
    0x4201 - WRIO    - "Joypad Programmable I/O Port (Open-Collector Output)"
    0x4202 - WRMPYA  - "Set unsigned 8bit Multiplicand"
    0x4203 - WRMPYB  - "Set unsigned 8bit Multiplier and Start Multiplication"
    0x4204 - WRDIVL  - "Set unsigned 16bit Dividend (lower 8bit)"
    0x4205 - WRDIVH  - "Set unsigned 16bit Dividend (upper 8bit)"
    0x4206 - WRDIVB  - "Set unsigned 8bit Divisor and Start Division"
    0x4207 - HTIMEL  - "H-Count Timer Setting (lower 8bits)"
    0x4208 - HTIMEH  - "H-Count Timer Setting (upper 1bit)"
    0x4209 - VTIMEL  - "V-Count Timer Setting (lower 8bits)"
    0x420A - VTIMEH  - "V-Count Timer Setting (upper 1bit)"
    0x420B - MDMAEN  - "Select General Purpose DMA Channel(s) and Start Transfer"
    0x420C - HDMAEN  - "Select H-Blank DMA (H-DMA) Channel(s)"
    0x420D - MEMSEL  - "Memory-2 Waitstate Control"
    //   420Eh..420Fh    - Unused region (open bus)                                 -
    // CPU On-Chip I/O Ports (Read-only)
    0x4210 - RDNMI   - "V-Blank NMI Flag and CPU Version Number (Read/Ack)"
    0x4211 - TIMEUP  - "H/V-Timer IRQ Flag (Read/Ack)"
    0x4212 - HVBJOY  - "H/V-Blank flag and Joypad Busy flag (R)"
    0x4213 - RDIO    - "Joypad Programmable I/O Port (Input)"
    0x4214 - RDDIVL  - "Unsigned Division Result (Quotient) (lower 8bit)"
    0x4215 - RDDIVH  - "Unsigned Division Result (Quotient) (upper 8bit)"
    0x4216 - RDMPYL  - "Unsigned Division Remainder / Multiply Product (lower 8bit)"
    0x4217 - RDMPYH  - "Unsigned Division Remainder / Multiply Product (upper 8bit)"
    0x4218 - JOY1L   - "Joypad 1 (gameport 1, pin 4) (lower 8bit)"
    0x4219 - JOY1H   - "Joypad 1 (gameport 1, pin 4) (upper 8bit)"
    0x421A - JOY2L   - "Joypad 2 (gameport 2, pin 4) (lower 8bit)"
    0x421B - JOY2H   - "Joypad 2 (gameport 2, pin 4) (upper 8bit)"
    0x421C - JOY3L   - "Joypad 3 (gameport 1, pin 5) (lower 8bit)"
    0x421D - JOY3H   - "Joypad 3 (gameport 1, pin 5) (upper 8bit)"
    0x421E - JOY4L   - "Joypad 4 (gameport 2, pin 5) (lower 8bit)"
    0x421F - JOY4H   - "Joypad 4 (gameport 2, pin 5) (upper 8bit)"
    // 4220..42FFh    - Unused region (open bus)                                -
    // CPU DMA, For below ports, x = Channel number 0..7 (R/W)
    //   (additional DMA control registers are 420Bh and 420Ch, see above)
    //   43x0h - DMAPx   - DMA/HDMA Parameters                                   (FFh)
    //   43x1h - BBADx   - DMA/HDMA I/O-Bus Address (PPU-Bus aka B-Bus)          (FFh)
    //   43x2h - A1TxL   - HDMA Table Start Address (low)  / DMA Curr Addr (low) (FFh)
    //   43x3h - A1TxH   - HDMA Table Start Address (high) / DMA Curr Addr (high)(FFh)
    //   43x4h - A1Bx    - HDMA Table Start Address (bank) / DMA Curr Addr (bank)(xxh)
    //   43x5h - DASxL   - Indirect HDMA Address (low)  / DMA Byte-Counter (low) (FFh)
    //   43x6h - DASxH   - Indirect HDMA Address (high) / DMA Byte-Counter (high)(FFh)
    //   43x7h - DASBx   - Indirect HDMA Address (bank)                          (FFh)
    //   43x8h - A2AxL   - HDMA Table Current Address (low)                      (FFh)
    //   43x9h - A2AxH   - HDMA Table Current Address (high)                     (FFh)
    //   43xAh - NTRLx   - HDMA Line-Counter (from current Table entry)          (FFh)
    //   43xBh - UNUSEDx - Unused byte (read/write-able)                         (FFh)
    //   43xCh+  -         Unused region (open bus)                                -
    //   43xFh - MIRRx   - Mirror of 43xBh (R/W)                                 (FFh)
    //   4380h..5FFFh    - Unused region (open bus)
};
