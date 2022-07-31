#![allow(unused_braces)]

use educe::Educe;
use log::{debug, warn};
use meru_interface::{FrameBuffer, Pixel};
use modular_bitfield::prelude::*;

use crate::{consts::*, context};

pub trait Context: context::Interrupt + context::Timing {}
impl<T: context::Interrupt + context::Timing> Context for T {}

#[derive(Educe)]
#[educe(Default)]
pub struct Ppu {
    display_ctrl: DisplayCtrl,
    obj_sel: ObjSel,
    oam_addr: OamAddr,
    bg_mode: BgMode,
    mosaic: Mosaic,
    bg_sc: [ScreenBaseAndSize; 4],
    bg_tile_base_addr: [u8; 4],
    bg_hofs: [u16; 4],
    bg_vofs: [u16; 4],
    m7_hofs: u16,
    m7_vofs: u16,
    rot_setting: RotSetting,
    rot_param: RotParam,
    mul_result: i32,
    win_mask: WinMask,
    win_pos: [WinPos; 2],
    win_logic: WinLogic,
    screen_desig_main: ScreenDesig,
    screen_desig_sub: ScreenDesig,
    win_disable_main: ScreenDesig,
    win_disable_sub: ScreenDesig,
    color_math_ctrl: ColorMathCtrl,
    sub_backdrop: Rgb555,

    #[educe(Default(expression = "vec![0; 0x10000]"))]
    vram: Vec<u8>,
    vram_addr_inc_mode: VramAddrIncMode,
    vram_addr: u16,
    vram_prefetch: [u8; 2],

    #[educe(Default(expression = "vec![0; 0x220]"))]
    oam: Vec<u8>,
    oam_addr_internal: u16,
    oam_lsb: u8,

    #[educe(Default(expression = "vec![0; 0x100]"))]
    cgram: Vec<u16>,
    cgram_addr: u16,
    cgram_lsb: u8,

    x: u32,
    y: u32,
    frame: u64,
    counter: u64,
    hblank: bool,
    vblank: bool,
    obj_range_overflow: bool,
    obj_time_overflow: bool,
    hdma_reload: bool,
    hdma_transfer: bool,

    h_latch: u32,
    h_flipflop: bool,
    v_latch: u32,
    v_flipflop: bool,
    hv_latched: bool,
    bg_old: u8,
    m7_old: u8,

    #[educe(Default(
        expression = "FrameBuffer::new(SCREEN_WIDTH.try_into().unwrap(), SCREEN_HEIGHT.try_into().unwrap())"
    ))]
    frame_buffer: FrameBuffer,

    #[educe(Default(expression = "vec![0; SCREEN_WIDTH.try_into().unwrap()]"))]
    line_buffer_main: Vec<u16>,
    #[educe(Default(expression = "vec![0; SCREEN_WIDTH.try_into().unwrap()]"))]
    line_buffer_sub: Vec<u16>,
    #[educe(Default(expression = "vec![0; SCREEN_WIDTH.try_into().unwrap()]"))]
    z_buffer_main: Vec<u8>,
    #[educe(Default(expression = "vec![0; SCREEN_WIDTH.try_into().unwrap()]"))]
    z_buffer_sub: Vec<u8>,
}

#[bitfield(bits = 16)]
#[derive(Default, Clone, Copy)]
#[repr(u16)]
struct Rgb555 {
    r: B5,
    g: B5,
    b: B5,
    #[skip]
    unused: B1,
}

impl Rgb555 {
    #[inline]
    fn blend<const ADD: bool, const HALF: bool>(&self, rhs: Self) -> Self {
        use std::cmp::min;
        if ADD {
            let r = min(0x1F, (self.r() + rhs.r()) / if HALF { 2 } else { 1 });
            let g = min(0x1F, (self.g() + rhs.g()) / if HALF { 2 } else { 1 });
            let b = min(0x1F, (self.b() + rhs.b()) / if HALF { 2 } else { 1 });
            Self::new().with_r(r).with_g(g).with_b(b)
        } else {
            let r = (self.r().saturating_sub(rhs.r())) / if HALF { 2 } else { 1 };
            let g = (self.g().saturating_sub(rhs.g())) / if HALF { 2 } else { 1 };
            let b = (self.b().saturating_sub(rhs.b())) / if HALF { 2 } else { 1 };
            Self::new().with_r(r).with_g(g).with_b(b)
        }
    }

    #[inline]
    fn fade(&self, level: u8) -> Self {
        let r = ((self.r() as u16 * level as u16) / 16) as u8;
        let g = ((self.g() as u16 * level as u16) / 16) as u8;
        let b = ((self.b() as u16 * level as u16) / 16) as u8;
        Self::new().with_r(r).with_g(g).with_b(b)
    }
}

#[bitfield(bits = 16)]
#[derive(Default)]
struct DisplayCtrl {
    master_brightness: B4,
    #[skip]
    __: B3,
    force_blank: bool,
    v_scanning: B1,    // 0: Non interlace, 1: Interlace
    obj_v_display: B1, // 0: Low, 1: High resuoltion
    bg_v_display: B1,  // 0: 224 lines, 1: 239 lines
    horizontal_pseudo_512: bool,
    #[skip]
    __: B2,
    external_sync: B1, // 0: Normal, 1: Super impose and etc.
    extbg_mode: bool,
}

#[bitfield(bits = 8)]
#[derive(Default)]
struct ObjSel {
    obj_base_addr: B3,
    obj_addr_gap: B2,
    obj_size_sel: ObjSizeSel,
}

impl ObjSel {
    fn addr_gap(&self) -> u16 {
        self.obj_addr_gap() as u16 * 0x2000
    }

    fn tile_base_addr(&self) -> u16 {
        self.obj_base_addr() as u16 * 0x4000
    }
}

#[derive(BitfieldSpecifier)]
#[bits = 3]
enum ObjSizeSel {
    Size8x8_16x16 = 0,
    Size8x8_32x32 = 1,
    Size8x8_64x64 = 2,
    Size16x16_32x32 = 3,
    Size16x16_64x64 = 4,
    Size32x32_64x64 = 5,
}

impl ObjSizeSel {
    fn size_small(&self) -> (usize, usize) {
        match self {
            ObjSizeSel::Size8x8_16x16 => (8, 8),
            ObjSizeSel::Size8x8_32x32 => (8, 8),
            ObjSizeSel::Size8x8_64x64 => (8, 8),
            ObjSizeSel::Size16x16_32x32 => (16, 16),
            ObjSizeSel::Size16x16_64x64 => (16, 16),
            ObjSizeSel::Size32x32_64x64 => (32, 32),
        }
    }

    fn size_large(&self) -> (usize, usize) {
        match self {
            ObjSizeSel::Size8x8_16x16 => (16, 16),
            ObjSizeSel::Size8x8_32x32 => (32, 32),
            ObjSizeSel::Size8x8_64x64 => (64, 64),
            ObjSizeSel::Size16x16_32x32 => (32, 32),
            ObjSizeSel::Size16x16_64x64 => (64, 64),
            ObjSizeSel::Size32x32_64x64 => (64, 64),
        }
    }
}

#[bitfield(bits = 8)]
#[derive(Default)]
struct VramAddrIncMode {
    step: B2,        // 0..3 = Increment Word-Address by 1,32,128,128
    translation: B2, // 0..3 = 0bit/None, 8bit, 9bit, 10bit
    #[skip]
    __: B3,
    inc_after_high_byte: bool,
}

impl VramAddrIncMode {
    fn translate(&self, addr: u16) -> u16 {
        match self.translation() {
            0 => addr,
            // aaaaaaaaYYYxxxxx => aaaaaaaaxxxxxYYY
            1 => addr & 0xFF00 | (addr & 0x001F) << 3 | (addr & 0x00E0) >> 5,
            // aaaaaaaYYYxxxxxP --> aaaaaaaxxxxxPYYY
            2 => addr & 0xFE00 | (addr & 0x003F) << 3 | (addr & 0x01C0) >> 6,
            // aaaaaaYYYxxxxxPP --> aaaaaaxxxxxPPYYY
            3 => addr & 0xFC00 | (addr & 0x007F) << 3 | (addr & 0x0380) >> 7,
            _ => unreachable!(),
        }
    }

    fn inc_words(&self) -> u16 {
        match self.step() {
            0 => 1,
            1 => 32,
            2 => 128,
            3 => 128,
            _ => unreachable!(),
        }
    }
}

#[bitfield(bits = 16)]
#[derive(Default)]
struct OamAddr {
    addr: B9,
    #[skip]
    __: B6,
    priority_rotation: bool,
}

#[bitfield(bits = 8)]
#[derive(Default, Debug)]
struct BgMode {
    mode: B3,
    bg3_priority_is_high: bool,
    tile_sizes: B4,
}

impl BgMode {
    fn tile_size(&self, i: usize) -> bool {
        self.tile_sizes() & (1 << i) != 0
    }
}

#[bitfield(bits = 8)]
#[derive(Default)]
struct Mosaic {
    size: B4,
    enable: B4,
}

#[bitfield(bits = 8)]
#[derive(Default)]
struct ScreenBaseAndSize {
    size: ScreenSize,
    base_addr: B6,
}

#[derive(BitfieldSpecifier)]
#[bits = 2]
#[derive(Default)]
enum ScreenSize {
    #[default]
    OneScreen = 0,
    VMirror = 1,
    HMirror = 2,
    FourScren = 3,
}

#[bitfield(bits = 8)]
#[derive(Default)]
struct RotSetting {
    h_flip: bool,
    v_flip: bool,
    #[skip]
    __: B4,
    screen_over: B2, // 0, 1: wrap, 2: transparent, 3: filled by tile 0x00
}

#[derive(Default)]
struct RotParam {
    // 1bit sign, 7bit integer, 8bit fraction
    a: u16,
    b: u16,
    c: u16,
    d: u16,
    // 1bit sign, 12bit integer
    x: u16,
    y: u16,
}

#[derive(Default, Debug)]
struct WinMask {
    bg: [WinMaskSettings; 4],
    obj: WinMaskSettings,
    math: WinMaskSettings,
}

#[bitfield(bits = 8)]
#[derive(Default, Debug)]
struct WinMaskSettings {
    area1: WinMaskSetting,
    area2: WinMaskSetting,
    #[skip]
    __: B4,
}

#[bitfield(bits = 2)]
#[derive(BitfieldSpecifier, Default, Debug)]
struct WinMaskSetting {
    outside: bool,
    enable: bool,
}

#[derive(Default)]
struct WinPos {
    left: u8,
    right: u8,
}

#[bitfield(bits = 16)]
#[derive(Default)]
struct WinLogic {
    bg1: MaskLogic,
    bg2: MaskLogic,
    bg3: MaskLogic,
    bg4: MaskLogic,
    obj: MaskLogic,
    math: MaskLogic,
    #[skip]
    __: B4,
}

impl WinLogic {
    fn bg(&self, i: usize) -> MaskLogic {
        match i {
            0 => self.bg1(),
            1 => self.bg2(),
            2 => self.bg3(),
            3 => self.bg4(),
            _ => unreachable!(),
        }
    }
}

#[derive(BitfieldSpecifier)]
#[bits = 2]
#[derive(Default, Clone, Copy)]
enum MaskLogic {
    #[default]
    Or = 0,
    And = 1,
    Xor = 2,
    Xnor = 3,
}

#[bitfield(bits = 8)]
#[derive(Default, Debug)]
struct ScreenDesig {
    bg1: bool,
    bg2: bool,
    bg3: bool,
    bg4: bool,
    obj: bool,
    #[skip]
    __: B3,
}

impl ScreenDesig {
    fn bg(&self, i: usize) -> bool {
        match i {
            0 => self.bg1(),
            1 => self.bg2(),
            2 => self.bg3(),
            3 => self.bg4(),
            _ => unreachable!(),
        }
    }
}

#[bitfield(bits = 16)]
#[derive(Default, Debug)]
struct ColorMathCtrl {
    direct_color: bool,
    sub_screen_enable: bool,
    #[skip]
    __: B2,
    color_math_enable: ColorMathEnable,
    force_main_screen_black: ForceMainScreenBlack,
    color_math_enable_kind: B6,
    color_math_half: bool,
    color_math_subtract: bool,
}

#[derive(BitfieldSpecifier)]
#[bits = 2]
#[derive(Default, Debug)]
enum ColorMathEnable {
    #[default]
    Always = 0,
    MathWindow = 1,
    NotMathWindow = 2,
    Never = 3,
}

#[derive(BitfieldSpecifier)]
#[bits = 2]
#[derive(Default, Debug)]
enum ForceMainScreenBlack {
    #[default]
    Never = 0,
    NotMathWindow = 1,
    MathWindow = 2,
    Always = 3,
}

impl Ppu {
    pub fn frame(&self) -> u64 {
        self.frame
    }

    pub fn frame_buffer(&self) -> &FrameBuffer {
        &self.frame_buffer
    }

    pub fn vblank(&self) -> bool {
        self.vblank
    }

    pub fn hblank(&self) -> bool {
        self.hblank
    }

    pub fn hdma_reload(&mut self) -> bool {
        let ret = self.hdma_reload;
        self.hdma_reload = false;
        ret
    }

    pub fn hdma_transfer(&mut self) -> bool {
        let ret = self.hdma_transfer;
        self.hdma_transfer = false;
        ret
    }
}

impl Ppu {
    pub fn tick(&mut self, ctx: &mut impl Context, render: bool) {
        let now = ctx.now();

        while self.counter + CYCLES_PER_DOT as u64 <= now {
            self.counter += CYCLES_PER_DOT as u64;

            self.x += 1;
            if self.x == DOTS_PER_LINE {
                self.x = 0;
                self.y += 1;

                if self.y == LINES_PER_FRAME {
                    self.y = 0;
                    self.frame += 1;
                    debug!("Start Frame: {}", self.frame);

                    // End of VBlank

                    self.vblank = false;
                    ctx.interrupt_mut().set_nmi_flag(false);

                    if !self.display_ctrl.force_blank() {
                        self.obj_range_overflow = false;
                        self.obj_time_overflow = false;
                    }
                }

                debug!("Line: {}:{}", self.frame, self.y);

                if self.y == VBLANK_START {
                    self.vblank = true;
                }
            }

            // FIXME: x == 0.5, not 1
            if (self.x, self.y) == (1, VBLANK_START) {
                ctx.interrupt_mut().set_nmi_flag(true);
            }

            if self.x == 1 {
                self.hblank = false;
            }

            if (self.x, self.y) == (6, 0) {
                self.hdma_reload = true;
            }

            if (self.x, self.y) == (10, 225) {
                self.oam_addr_internal = self.oam_addr.addr() << 1;
            }

            if render && self.x == 22 {
                if (1..1 + SCREEN_HEIGHT).contains(&self.y) {
                    self.render_line(self.y);
                    self.copy_line_buffer(self.y - 1);
                }
            }

            // FIXME: 133.5
            if self.x == 134 {
                // DRAM refresh
                ctx.elapse(40);
            }

            if self.x == 274 {
                self.hblank = true;
            }

            if self.x == 278 && (0..=224).contains(&self.y) {
                self.hdma_transfer = true;
            }

            let raise_irq = match ctx.interrupt().hvirq_enable() {
                1 => self.x == (ctx.interrupt().h_count() as u32 + 4),
                2 => self.x == 3 && self.y == ctx.interrupt().v_count() as u32,
                3 => {
                    self.x == (ctx.interrupt().h_count() as u32 + 4)
                        && self.y == ctx.interrupt().v_count() as u32
                }
                _ => false,
            };
            if raise_irq {
                ctx.interrupt_mut().set_irq(true);
            }
        }
    }

    pub fn read(&mut self, ctx: &mut impl Context, addr: u16) -> u8 {
        match addr {
            // PPU Picture Processing Unit (Read-Only Ports)
            // 0x2134 - MPYL    - "PPU1 Signed Multiply Result   (lower 8bit)"
            // 0x2135 - MPYM    - "PPU1 Signed Multiply Result   (middle 8bit)"
            // 0x2136 - MPYH    - "PPU1 Signed Multiply Result   (upper 8bit)"
            0x2134 => self.mul_result as u8,
            0x2135 => (self.mul_result >> 8) as u8,
            0x2136 => (self.mul_result >> 16) as u8,
            // 0x2137 - SLHV    - "PPU1 Latch H/V-Counter by Software (Read=Strobe)"
            0x2137 => {
                self.hv_latched = true;
                self.h_latch = self.x;
                self.v_latch = self.y;
                // FIXME: open-bus
                0
            }
            // 0x2138 - RDOAM   - "PPU1 OAM Data Read            (read-twice)"
            0x2138 => {
                let ret = if self.oam_addr_internal < 0x200 {
                    self.oam[self.oam_addr_internal as usize]
                } else {
                    // addresses 220h..3FFh are mirrors of 200h..21Fh
                    self.oam[self.oam_addr_internal as usize & 0x21F]
                };
                self.oam_addr_internal = (self.oam_addr_internal + 1) & 0x3FF;
                ret
            }
            // 0x2139 - RDVRAML - "PPU1 VRAM Data Read           (lower 8bits)"
            // 0x213A - RDVRAMH - "PPU1 VRAM Data Read           (upper 8bits)"
            0x2139 | 0x213A => {
                let ofs = addr - 0x2139;
                let ret = self.vram_prefetch[ofs as usize];
                if self.vram_addr_inc_mode.inc_after_high_byte() == (ofs == 1) {
                    // Prefetch BEFORE incrementing vram address
                    let vram_addr = self.vram_addr_inc_mode.translate(self.vram_addr) * 2;
                    self.vram_prefetch[0] = self.vram[vram_addr as usize];
                    self.vram_prefetch[1] = self.vram[(vram_addr + 1) as usize];
                    self.vram_addr =
                        (self.vram_addr + self.vram_addr_inc_mode.inc_words()) & 0x7FFF;
                }
                ret
            }
            // 0x213B - RDCGRAM - "PPU2 CGRAM Data Read (Palette)(read-twice)"
            0x213B => {
                let word = self.cgram[self.cgram_addr as usize / 2];
                let ret = if self.cgram_addr & 1 == 0 {
                    word as u8
                } else {
                    (word >> 8) as u8
                };
                self.cgram_addr = (self.cgram_addr + 1) & 0x1FF;
                ret
            }
            // 0x213C - OPHCT   - "PPU2 Horizontal Counter Latch (read-twice)"
            0x213C => {
                self.hv_latched = false; // ??
                self.h_flipflop = !self.h_flipflop;
                if self.h_flipflop {
                    self.h_latch as u8
                } else {
                    // FIXME: Bit 1-7 is open-bus
                    (self.h_latch >> 8) as u8 & 1
                }
            }
            // 0x213D - OPVCT   - "PPU2 Vertical Counter Latch   (read-twice)"
            0x213D => {
                self.hv_latched = false; // ??
                self.v_flipflop = !self.v_flipflop;
                if self.v_flipflop {
                    self.v_latch as u8
                } else {
                    // FIXME: Bit 1-7 is open-bus
                    (self.v_latch >> 8) as u8 & 1
                }
            }
            // 0x213E - STAT77  - "PPU1 Status and PPU1 Version Number"
            0x213E => {
                let mut ret = 0;

                const PPU1_VERSION: u8 = 1;
                const PPU1_MASTER: u8 = 0;

                // 3-0  PPU1 5C77 Version Number (only version 1 exists as far as I know)
                ret |= PPU1_VERSION;
                // FIXME:
                // 4    Not used (PPU1 open bus) (same as last value read from PPU1)
                // 5    Master/Slave Mode (PPU1.Pin25) (0=Normal=Master)
                ret |= PPU1_MASTER << 5;
                // 6    OBJ Range overflow (0=Okay, 1=More than 32 OBJs per scanline)
                ret |= (self.obj_range_overflow as u8) << 6;
                // 7    OBJ Time overflow  (0=Okay, 1=More than 8x34 OBJ pixels per scanline)
                ret |= (self.obj_time_overflow as u8) << 7;

                ret
            }
            // 0x213F - STAT78  - "PPU2 Status and PPU2 Version Number"
            0x213F => {
                let mut ret = 0;

                const PPU2_VERSION: u8 = 1;
                const FRAME_RATE: u8 = 0; // 0: 60Hz, 1: 50Hz

                ret |= PPU2_VERSION;
                ret |= FRAME_RATE << 4;
                // FIXME: 5 is open-bus
                ret |= (self.hv_latched as u8) << 6;
                ret |= ((self.frame & 1) as u8) << 7;

                self.h_flipflop = false;
                self.v_flipflop = false;

                ret
            }

            _ => {
                // FIXME: Open-bus
                warn!("PPU Read (open-bus): {addr:04X}");
                0
            }
        }
    }

    pub fn write(&mut self, ctx: &mut impl Context, addr: u16, data: u8) {
        match addr {
            0x2100 => self.display_ctrl.bytes[0] = data,
            0x2101 => self.obj_sel.bytes[0] = data,
            0x2102 | 0x2103 => {
                self.oam_addr.bytes[addr as usize - 0x2102] = data;
                self.oam_addr_internal = self.oam_addr.addr() << 1;
            }
            0x2104 => {
                if self.oam_addr_internal < 0x200 {
                    if self.oam_addr_internal & 1 == 0 {
                        self.oam_lsb = data;
                    } else {
                        self.oam[self.oam_addr_internal as usize - 1] = self.oam_lsb;
                        self.oam[self.oam_addr_internal as usize] = data;
                    }
                } else {
                    // addresses 220h..3FFh are mirrors of 200h..21Fh
                    self.oam[self.oam_addr_internal as usize & 0x21F] = data;
                }
                self.oam_addr_internal = (self.oam_addr_internal + 1) & 0x3FF;
            }
            0x2105 => {
                self.bg_mode.bytes[0] = data;
                debug!("BG Mode: {:?}", self.bg_mode);
            }
            0x2106 => self.mosaic.bytes[0] = data,
            0x2107..=0x210A => self.bg_sc[(addr - 0x2107) as usize].bytes[0] = data,
            0x210B..=0x210C => {
                let ix = (addr - 0x210B) as usize * 2;
                self.bg_tile_base_addr[ix] = data & 0xF;
                self.bg_tile_base_addr[ix + 1] = data >> 4;
            }

            0x210D | 0x210F | 0x2111 | 0x2113 => {
                let ix = (addr - 0x210D) as usize / 2;
                self.bg_hofs[ix] =
                    (data as u16) << 8 | (self.bg_old & !7) as u16 | ((self.bg_hofs[ix] >> 8) & 7);
                self.bg_old = data;

                if ix == 0 {
                    self.m7_hofs = (data as u16) << 8 | self.m7_old as u16;
                    self.m7_old = data;
                }
            }
            0x210E | 0x2110 | 0x2112 | 0x2114 => {
                let ix = (addr - 0x210E) as usize / 2;
                self.bg_vofs[ix] = (data as u16) << 8 | self.bg_old as u16;
                self.bg_old = data;

                if ix == 0 {
                    self.m7_vofs = (data as u16) << 8 | self.m7_old as u16;
                    self.m7_old = data;
                }
            }
            0x2115 => self.vram_addr_inc_mode.bytes[0] = data,
            0x2116 => {
                self.vram_addr = self.vram_addr & 0x7F00 | data as u16;
                self.vram_prefetch[0] = self.vram[(self.vram_addr * 2) as usize];
                self.vram_prefetch[1] = self.vram[(self.vram_addr * 2 + 1) as usize];
            }
            0x2117 => {
                self.vram_addr = self.vram_addr & 0x00FF | ((data & 0x7F) as u16) << 8;
                self.vram_prefetch[0] = self.vram[(self.vram_addr * 2) as usize];
                self.vram_prefetch[1] = self.vram[(self.vram_addr * 2 + 1) as usize];
            }
            0x2118 | 0x2119 => {
                let ofs = addr - 0x2118;
                let vram_addr = self.vram_addr_inc_mode.translate(self.vram_addr) * 2 + ofs;
                self.vram[vram_addr as usize] = data;
                if self.vram_addr_inc_mode.inc_after_high_byte() == (ofs == 1) {
                    self.vram_addr =
                        (self.vram_addr + self.vram_addr_inc_mode.inc_words()) & 0x7FFF;
                    // Does not prefetch
                }
            }
            0x211A => self.rot_setting.bytes[0] = data,

            0x211B => {
                self.rot_param.a = (data as u16) << 8 | self.m7_old as u16;
                self.m7_old = data;
                self.mul_result = self.rot_param.a as i16 as i32 * self.rot_param.b as i8 as i32;
            }
            0x211C => {
                self.rot_param.b = (data as u16) << 8 | self.m7_old as u16;
                self.m7_old = data;
                self.mul_result = self.rot_param.a as i16 as i32 * data as i8 as i32;
            }
            0x211D => {
                self.rot_param.c = (data as u16) << 8 | self.m7_old as u16;
                self.m7_old = data;
            }
            0x211E => {
                self.rot_param.d = (data as u16) << 8 | self.m7_old as u16;
                self.m7_old = data;
            }
            0x211F => {
                self.rot_param.x = (data as u16) << 8 | self.m7_old as u16;
                self.m7_old = data;
            }
            0x2120 => {
                self.rot_param.y = (data as u16) << 8 | self.m7_old as u16;
                self.m7_old = data;
            }

            0x2121 => self.cgram_addr = data as u16 * 2,
            0x2122 => {
                debug!("CGRAM: {:03X} = {data:02X}", self.cgram_addr);

                if self.cgram_addr & 1 == 0 {
                    self.cgram_lsb = data;
                } else {
                    self.cgram[self.cgram_addr as usize / 2] =
                        self.cgram_lsb as u16 | (data as u16) << 8;
                }
                self.cgram_addr = (self.cgram_addr + 1) & 0x1FF;
            }
            0x2123 => {
                self.win_mask.bg[0].bytes[0] = data & 0xF;
                self.win_mask.bg[1].bytes[0] = data >> 4;
            }
            0x2124 => {
                self.win_mask.bg[2].bytes[0] = data & 0xF;
                self.win_mask.bg[3].bytes[0] = data >> 4;
            }
            0x2125 => {
                self.win_mask.obj.bytes[0] = data & 0xF;
                self.win_mask.math.bytes[0] = data >> 4;
            }
            0x2126 | 0x2128 => {
                let ix = (addr - 0x2126) as usize / 2;
                debug!("Win{ix} L = {data}");
                self.win_pos[ix].left = data;
            }
            0x2127 | 0x2129 => {
                let ix = (addr - 0x2127) as usize / 2;
                debug!("Win{ix} R = {data}");
                self.win_pos[ix].right = data;
            }
            0x212A => self.win_logic.bytes[0] = data,
            0x212B => self.win_logic.bytes[1] = data,
            0x212C => self.screen_desig_main.bytes[0] = data,
            0x212D => self.screen_desig_sub.bytes[0] = data,
            0x212E => self.win_disable_main.bytes[0] = data,
            0x212F => self.win_disable_sub.bytes[0] = data,
            0x2130 => self.color_math_ctrl.bytes[0] = data,
            0x2131 => self.color_math_ctrl.bytes[1] = data,
            0x2132 => {
                let intensity = data & 0x1F;
                if data & 0x20 != 0 {
                    self.sub_backdrop.set_r(intensity);
                }
                if data & 0x40 != 0 {
                    self.sub_backdrop.set_g(intensity);
                }
                if data & 0x80 != 0 {
                    self.sub_backdrop.set_b(intensity);
                }
            }
            0x2133 => self.display_ctrl.bytes[1] = data,

            0x2134..=0x213F => warn!("Write to readonly register: {addr:04X} = {data:#04X}"),

            _ => unreachable!(""),
        }
    }
}

#[bitfield(bits = 16)]
struct BgMapEntry {
    char_num: B10,
    pal_num: B3,
    priority: B1,
    x_flip: bool,
    y_flip: bool,
}

#[bitfield(bits = 32)]
struct ObjEntry {
    x: u8,
    y: u8,
    tile_num: B9,
    pal_num: B3,
    priority: B2,
    x_flip: bool,
    y_flip: bool,
}

struct WinCalc {
    enable: [bool; 2],
    outside: [bool; 2],
    logic: MaskLogic,
    range: [(u32, u32); 2],
}

impl WinCalc {
    fn contains(&self, x: u32) -> bool {
        let in_win1 = if self.enable[0] {
            Some((self.range[0].0 <= x && x <= self.range[0].1) ^ self.outside[0])
        } else {
            None
        };

        let in_win2 = if self.enable[1] {
            Some((self.range[1].0 <= x && x <= self.range[1].1) ^ self.outside[1])
        } else {
            None
        };

        match (in_win1, in_win2) {
            (None, None) => false,
            (Some(w), None) | (None, Some(w)) => w,
            (Some(w1), Some(w2)) => match self.logic {
                MaskLogic::Or => w1 || w2,
                MaskLogic::And => w1 && w2,
                MaskLogic::Xor => w1 ^ w2,
                MaskLogic::Xnor => !(w1 ^ w2),
            },
        }
    }
}

impl Ppu {
    fn win_calc_bg(&self, i: usize) -> WinCalc {
        let enable = [
            self.win_mask.bg[i].area1().enable(),
            self.win_mask.bg[i].area2().enable(),
        ];

        let outside = [
            self.win_mask.bg[i].area1().outside(),
            self.win_mask.bg[i].area2().outside(),
        ];

        let logic = self.win_logic.bg(i);

        let range = [
            (self.win_pos[0].left as u32, self.win_pos[0].right as u32),
            (self.win_pos[1].left as u32, self.win_pos[1].right as u32),
        ];

        WinCalc {
            enable,
            outside,
            logic,
            range,
        }
    }

    fn win_calc_obj(&self) -> WinCalc {
        let enable = [
            self.win_mask.obj.area1().enable(),
            self.win_mask.obj.area2().enable(),
        ];

        let outside = [
            self.win_mask.obj.area1().outside(),
            self.win_mask.obj.area2().outside(),
        ];

        let logic = self.win_logic.obj();

        let range = [
            (self.win_pos[0].left as u32, self.win_pos[0].right as u32),
            (self.win_pos[1].left as u32, self.win_pos[1].right as u32),
        ];

        WinCalc {
            enable,
            outside,
            logic,
            range,
        }
    }

    fn win_calc_math(&self) -> WinCalc {
        let enable = [
            self.win_mask.math.area1().enable(),
            self.win_mask.math.area2().enable(),
        ];

        let outside = [
            self.win_mask.math.area1().outside(),
            self.win_mask.math.area2().outside(),
        ];

        let logic = self.win_logic.math();

        let range = [
            (self.win_pos[0].left as u32, self.win_pos[0].right as u32),
            (self.win_pos[1].left as u32, self.win_pos[1].right as u32),
        ];

        WinCalc {
            enable,
            outside,
            logic,
            range,
        }
    }
}

const PIXEL_KIND_BG1: u8 = 0;
const PIXEL_KIND_BG2: u8 = 1;
const PIXEL_KIND_BG3: u8 = 2;
const PIXEL_KIND_BG4: u8 = 3;
const PIXEL_KIND_OBJ_HIPAL: u8 = 4;
const PIXEL_KIND_OBJ_LOPAL: u8 = 6;
const PIXEL_KIND_BACKDROP: u8 = 5;

impl Ppu {
    fn render_line(&mut self, y: u32) {
        if self.display_ctrl.force_blank() {
            self.line_buffer_main.fill(0);
            return;
        }

        // Backdrop
        self.line_buffer_main.fill(self.cgram[0]);
        self.z_buffer_main.fill(0xF0 | PIXEL_KIND_BACKDROP);
        self.line_buffer_sub.fill(self.sub_backdrop.into());
        self.z_buffer_sub.fill(0xF0 | PIXEL_KIND_BACKDROP);

        //     Mode0    Mode1    Mode2    Mode3    Mode4    Mode5    Mode6    Mode7
        // 0:  -        BG3.1a   -        -        -        -        -        -
        // 1:  OBJ.3    OBJ.3    OBJ.3    OBJ.3    OBJ.3    OBJ.3    OBJ.3    OBJ.3
        // 2:  BG1.1    BG1.1    BG1.1    BG1.1    BG1.1    BG1.1    BG1.1    -
        // 3:  BG2.1    BG2.1    -        -        -        -        -        -
        // 4:  OBJ.2    OBJ.2    OBJ.2    OBJ.2    OBJ.2    OBJ.2    OBJ.2    OBJ.2
        // 5:  BG1.0    BG1.0    BG2.1    BG2.1    BG2.1    BG2.1    -        BG2.1p
        // 6:  BG2.0    BG2.0    -        -        -        -        -        -
        // 7:  OBJ.1    OBJ.1    OBJ.1    OBJ.1    OBJ.1    OBJ.1    OBJ.1    OBJ.1
        // 8:  BG3.1    BG3.1b   BG1.0    BG1.0    BG1.0    BG1.0    BG1.0    BG1
        // 9:  BG4.1    -        -        -        -        -        -        -
        // 10: OBJ.0    OBJ.0    OBJ.0    OBJ.0    OBJ.0    OBJ.0    OBJ.0    OBJ.0
        // 11: BG3.0    BG3.0a   BG2.0    BG2.0    BG2.0    BG2.0    -        BG2.0p
        // 12: BG4.0    BG3.0b   -        -        -        -        -        -
        // 15: Backdrop Backdrop Backdrop Backdrop Backdrop Backdrop Backdrop Backdrop

        match self.bg_mode.mode() {
            0 => {
                self.render_bg(y, 0, 2, 5, 2, 0x00);
                self.render_bg(y, 1, 2, 6, 3, 0x20);
                self.render_bg(y, 2, 2, 11, 8, 0x40);
                self.render_bg(y, 3, 2, 12, 9, 0x60);
            }
            1 => {
                self.render_bg(y, 0, 4, 5, 2, 0x00);
                self.render_bg(y, 1, 4, 6, 3, 0x00);
                if !self.bg_mode.bg3_priority_is_high() {
                    self.render_bg(y, 2, 2, 12, 8, 0x00);
                } else {
                    self.render_bg(y, 2, 2, 11, 0, 0x00);
                }
            }
            2 => todo!("BG Mode 2"),
            3 => {
                self.render_bg(y, 0, 8, 8, 2, 0x00);
                self.render_bg(y, 1, 4, 11, 5, 0x00);
            }
            4 => todo!("BG Mode 4"),
            5 => todo!("BG Mode 5"),
            6 => todo!("BG Mode 6"),
            7 => {
                self.render_bg_mode7(y, 8);
                // TODO: EXTBG
            }
            _ => unreachable!(),
        }

        self.render_obj(y);
        self.color_math();
    }

    fn copy_line_buffer(&mut self, y: u32) {
        for x in 0..SCREEN_WIDTH {
            *self.frame_buffer.pixel_mut(x as usize, y as usize) =
                u16_to_pixel(self.line_buffer_main[x as usize]);
        }
    }

    fn render_bg(&mut self, y: u32, i: usize, bpp: usize, zl: u8, zh: u8, pal_base: u8) {
        if !self.bg_mode.tile_size(i) {
            self.render_bg_::<8>(y, i, bpp, zl, zh, pal_base);
        } else {
            self.render_bg_::<16>(y, i, bpp, zl, zh, pal_base);
        }
    }

    fn render_bg_<const TILE_SIZE: usize>(
        &mut self,
        y: u32,
        i: usize,
        bpp: usize,
        zl: u8,
        zh: u8,
        pal_base: u8,
    ) {
        let enable_main = self.screen_desig_main.bg(i);
        let enable_sub = self.screen_desig_sub.bg(i);
        if !enable_main && !enable_sub {
            return;
        }

        let win_calc = self.win_calc_bg(i);

        // FIXME: base_addr is 6bit (0x3F * 2K = 126K), but VRAM is 64KB ???
        let sc_base_addr = self.bg_sc[i].base_addr() as usize * 2 * 1024;
        let tile_base_addr = self.bg_tile_base_addr[i] as usize * 8 * 1024;

        let hofs = (self.bg_hofs[i] & 0x3FF) as usize;
        let vofs = (self.bg_vofs[i] & 0x3FF) as usize;

        // TODO: mosaic

        let pal_size = 1 << bpp;

        let (sc_w, sc_h) = match self.bg_sc[i].size() {
            ScreenSize::OneScreen => (1, 1),
            ScreenSize::VMirror => (2, 1),
            ScreenSize::HMirror => (1, 2),
            ScreenSize::FourScren => (2, 2),
        };

        let sy = y as usize + vofs;

        const SC_SIZE: usize = 32 * 32 * 2;

        let pixel_kind = match i {
            0 => PIXEL_KIND_BG1,
            1 => PIXEL_KIND_BG2,
            2 => PIXEL_KIND_BG3,
            3 => PIXEL_KIND_BG4,
            _ => unreachable!(),
        };

        // FIXME: optimize
        for x in 0..SCREEN_WIDTH {
            let in_win = win_calc.contains(x);
            let render_main = enable_main && !in_win;
            let render_sub = enable_sub && !in_win;
            if !render_main && !render_sub {
                continue;
            }

            let sx = x as usize + hofs;
            let sc_x = sx / TILE_SIZE / 32 % sc_w;
            let sc_y = sy / TILE_SIZE / 32 % sc_h;
            let tile_x = sx / TILE_SIZE % 32;
            let tile_y = sy / TILE_SIZE % 32;

            let sc_addr = sc_base_addr + (sc_x + sc_y * sc_w) * SC_SIZE;
            let entry_addr = (sc_addr + (tile_x + tile_y * 32) * 2) & 0xFFFE;

            let entry =
                BgMapEntry::from_bytes(self.vram[entry_addr..entry_addr + 2].try_into().unwrap());

            let pixel_x = (sx % TILE_SIZE) ^ if !entry.x_flip() { 0 } else { TILE_SIZE - 1 };
            let pixel_y = (sy % TILE_SIZE) ^ if !entry.y_flip() { 0 } else { TILE_SIZE - 1 };

            let z = if entry.priority() == 0 { zl } else { zh } * 0x10 + pixel_kind;

            let char_num = (entry.char_num() as usize + pixel_x / 8 + pixel_y / 8 * 0x10) & 0x3FF;
            let pixel_x = pixel_x % 8;
            let pixel_y = pixel_y % 8;

            let tile_addr = tile_base_addr + char_num * bpp as usize * 8;

            let mut pixel = 0;
            for i in 0..bpp / 2 {
                let addr = (tile_addr + i * 16 + pixel_y * 2) & 0xFFFE;
                let b0 = self.vram[addr];
                let b1 = self.vram[addr + 1];
                pixel |= ((b0 >> (7 - pixel_x)) & 1) << (i * 2);
                pixel |= ((b1 >> (7 - pixel_x)) & 1) << (i * 2 + 1);
            }

            if pixel != 0 {
                let col = self.cgram[(pal_base + entry.pal_num() * pal_size + pixel) as usize];
                if render_main && z < self.z_buffer_main[x as usize] {
                    self.line_buffer_main[x as usize] = col;
                    self.z_buffer_main[x as usize] = z;
                }
                if render_sub && z < self.z_buffer_sub[x as usize] {
                    self.line_buffer_sub[x as usize] = col;
                    self.z_buffer_sub[x as usize] = z;
                }
            }
        }
    }

    fn render_bg_mode7(&mut self, y: u32, z: u8) {
        if self.color_math_ctrl.direct_color() {
            todo!("Direct color in Mode7");
        }

        let enable_main = self.screen_desig_main.bg(0);
        let enable_sub = self.screen_desig_sub.bg(0);
        if !enable_main && !enable_sub {
            return;
        }
        let win_calc = self.win_calc_bg(0);

        let z = z * 0x10 + PIXEL_KIND_BG1;

        let x_flip = if self.rot_setting.h_flip() { 0xFF } else { 0 };
        let y_flip = if self.rot_setting.v_flip() { 0xFF } else { 0 };
        let screen_over = self.rot_setting.screen_over();

        fn sext16(n: u16) -> i32 {
            ((n as i32) << 16) >> 16
        }
        fn sext13(n: u16) -> i32 {
            ((n as i32) << 19) >> 19
        }

        let sy = (y ^ y_flip) as i32;
        let sx = x_flip as i32;

        let m7a = sext16(self.rot_param.a);
        let m7b = sext16(self.rot_param.b);
        let m7c = sext16(self.rot_param.c);
        let m7d = sext16(self.rot_param.d);
        let m7x = sext13(self.rot_param.x);
        let m7y = sext13(self.rot_param.y);
        let m7vofs = sext13(self.m7_vofs);
        let m7hofs = sext13(self.m7_hofs);

        let mut orgx = (m7hofs - m7x) & !0x1C00;
        if orgx < 0 {
            orgx |= 0x1C00;
        }
        let mut orgy = (m7vofs - m7y) & !0x1C00;
        if orgy < 0 {
            orgy |= 0x1C00;
        }

        let lx = ((m7a * orgx) & !0x3F) + ((m7b * orgy) & !0x3F) + m7x * 0x100;
        let ly = ((m7c * orgx) & !0x3F) + ((m7d * orgy) & !0x3F) + m7y * 0x100;
        let lx = lx + ((m7b * sy) & !0x3F) + (m7a * sx) as i32;
        let ly = ly + ((m7d * sy) & !0x3F) + (m7c * sx) as i32;

        let dx = if x_flip != 0 { -m7a } else { m7a };
        let dy = if y_flip != 0 { -m7c } else { m7c };

        for x in 0..SCREEN_WIDTH {
            let vx = lx + dx * (x as i32);
            let vy = ly + dy * (x as i32);

            let in_win = win_calc.contains(x);
            let render_main = enable_main && !in_win;
            let render_sub = enable_sub && !in_win;
            if !render_main && !render_sub {
                continue;
            }

            let ofs_x = ((vx >> 8) & 7) as usize;
            let ofs_y = ((vy >> 8) & 7) as usize;
            let tile_x = ((vx >> 11) & 0x7F) as usize;
            let tile_y = ((vy >> 11) & 0x7F) as usize;

            let (tile_x, tile_y) = if vx >> 18 != 0 || vy >> 18 != 0 {
                match screen_over {
                    // Wrap
                    0 | 1 => (tile_x, tile_y),
                    // Transparent
                    2 => continue,
                    // Filled by tile 0x00
                    3 => (0, 0),
                    _ => unreachable!(),
                }
            } else {
                (tile_x, tile_y)
            };

            let tile_addr = (tile_x + tile_y * 128) * 2;
            let char_num = self.vram[tile_addr] as usize;
            let char_addr = char_num * 128 + ofs_y * 16 + ofs_x * 2 + 1;
            let pixel = self.vram[char_addr];

            if pixel != 0 {
                let col = self.cgram[pixel as usize];
                if render_main && z < self.z_buffer_main[x as usize] {
                    self.line_buffer_main[x as usize] = col;
                    self.z_buffer_main[x as usize] = z;
                }
                if render_sub && z < self.z_buffer_sub[x as usize] {
                    self.line_buffer_sub[x as usize] = col;
                    self.z_buffer_sub[x as usize] = z;
                }
            }
        }
    }

    fn render_obj(&mut self, y: u32) {
        let enable_main = self.screen_desig_main.obj();
        let enable_sub = self.screen_desig_sub.obj();

        if !enable_main && !enable_sub {
            return;
        }

        let y = y as usize;

        const BPP: usize = 4;
        const PAL_SIZE: u8 = 16;

        const OBJ_Z: [u8; 4] = [10, 7, 4, 1];

        let tile_base_addr = self.obj_sel.tile_base_addr() as usize;
        let addr_gap = self.obj_sel.addr_gap() as usize;

        let sizes = [
            self.obj_sel.obj_size_sel().size_small(),
            self.obj_sel.obj_size_sel().size_large(),
        ];

        let win_calc = self.win_calc_obj();

        let mut render_obj = 0;
        let mut render_block = 0;

        let priority_rot = if self.oam_addr.priority_rotation() {
            (self.oam_addr.addr() >> 1) & 0x7F
        } else {
            0
        };

        for i in 0..0x80 {
            let i = ((i + priority_rot) & 0x7F) as usize;

            let entry = ObjEntry::from_bytes(self.oam[i * 4..i * 4 + 4].try_into().unwrap());
            let extra = (self.oam[0x200 + i / 4] >> ((i & 3) * 2)) & 3;

            let (ow, oh) = sizes[(extra >> 1) as usize];
            let oy = entry.y() as usize + 1;
            if !(oy..oy + oh).contains(&y) {
                continue;
            }

            render_obj += 1;
            if render_obj > 32 {
                self.obj_range_overflow = true;
                break;
            }

            let ox = ((extra as usize & 1) << 8) + entry.x() as usize;
            let dy = y - oy;
            let pixel_y = if !entry.y_flip() { dy } else { oh - 1 - dy };

            let z = OBJ_Z[entry.priority() as usize] * 0x10
                + if entry.pal_num() < 4 {
                    PIXEL_KIND_OBJ_LOPAL
                } else {
                    PIXEL_KIND_OBJ_HIPAL
                };

            let tile_num = entry.tile_num() as usize;
            let tile_num = tile_num & 0x10F | ((tile_num & 0xF0) + pixel_y / 8 * 16) & 0xF0;

            for dx in 0..ow {
                let x = ox + dx;
                if !(0..SCREEN_WIDTH as usize).contains(&x) {
                    continue;
                }

                let in_win = win_calc.contains(x as u32);
                let render_main = enable_main && !in_win;
                let render_sub = enable_sub && !in_win;
                if !render_main && !render_sub {
                    continue;
                }

                let pixel_x = if !entry.x_flip() { dx } else { ow - 1 - dx } as usize;

                let tile_num = tile_num & 0x1F0 | ((tile_num & 0xF) + pixel_x / 8) & 0xF;
                let tile_addr =
                    tile_base_addr + (tile_num >> 8) * addr_gap + tile_num as usize * BPP * 8;

                let mut pixel = 0;
                for i in 0..BPP / 2 {
                    let addr = tile_addr + i * 16 + pixel_y % 8 * 2;
                    let b0 = self.vram[addr];
                    let b1 = self.vram[addr + 1];
                    pixel |= ((b0 >> (7 - pixel_x % 8)) & 1) << (i * 2);
                    pixel |= ((b1 >> (7 - pixel_x % 8)) & 1) << (i * 2 + 1);
                }

                if pixel != 0 {
                    let col = self.cgram[(0x80 + entry.pal_num() * PAL_SIZE + pixel) as usize];
                    if render_main && z & 0xF0 < self.z_buffer_main[x as usize] & 0xF0 {
                        self.line_buffer_main[x as usize] = col;
                        self.z_buffer_main[x as usize] = z;
                    }
                    if render_sub && z & 0xF0 < self.z_buffer_sub[x as usize] & 0xF0 {
                        self.line_buffer_sub[x as usize] = col;
                        self.z_buffer_sub[x as usize] = z;
                    }
                }
            }

            render_block += ow / 8;
            if render_block > 34 {
                self.obj_time_overflow = true;
                break;
            }
        }
    }

    fn color_math(&mut self) {
        let win_calc = self.win_calc_math();
        let master_brightness = self.display_ctrl.master_brightness();

        for x in 0..SCREEN_WIDTH {
            let in_win = win_calc.contains(x);

            let force_main_black = match self.color_math_ctrl.force_main_screen_black() {
                ForceMainScreenBlack::Never => false,
                ForceMainScreenBlack::NotMathWindow => !in_win,
                ForceMainScreenBlack::MathWindow => in_win,
                ForceMainScreenBlack::Always => true,
            };

            let main = Rgb555::from(if force_main_black {
                0
            } else {
                self.line_buffer_main[x as usize]
            });

            let sub = Rgb555::from(if self.color_math_ctrl.sub_screen_enable() {
                self.line_buffer_sub[x as usize]
            } else {
                self.sub_backdrop.into()
            });

            let enable = match self.color_math_ctrl.color_math_enable() {
                ColorMathEnable::Always => true,
                ColorMathEnable::MathWindow => in_win,
                ColorMathEnable::NotMathWindow => !in_win,
                ColorMathEnable::Never => false,
            };

            let enable = if !enable {
                false
            } else {
                let pixel_kind = self.z_buffer_main[x as usize] & 0xF;
                self.color_math_ctrl.color_math_enable_kind() & (1 << pixel_kind) != 0
            };

            let result = if enable {
                if self.color_math_ctrl.color_math_subtract() {
                    if self.color_math_ctrl.color_math_half() {
                        main.blend::<false, true>(sub)
                    } else {
                        main.blend::<false, false>(sub)
                    }
                } else {
                    if self.color_math_ctrl.color_math_half() {
                        main.blend::<true, true>(sub)
                    } else {
                        main.blend::<true, false>(sub)
                    }
                }
            } else {
                main
            };

            let result = if master_brightness == 0 {
                Rgb555::from(0)
            } else {
                result.fade(master_brightness + 1)
            };

            self.line_buffer_main[x as usize] = result.into();
        }
    }
}

fn u16_to_pixel(p: u16) -> Pixel {
    let r = (p & 0x1F) as u8;
    let g = ((p >> 5) & 0x1F) as u8;
    let b = ((p >> 10) & 0x1F) as u8;
    Pixel::new(extend(r), extend(g), extend(b))
}

fn extend(c: u8) -> u8 {
    c << 3 | c >> 2
}
