#![allow(unused_braces)]

use educe::Educe;
use log::debug;
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
    color_math_data: ColorMathData,

    #[educe(Default(expression = "vec![0; 0x10000]"))]
    vram: Vec<u8>,
    vram_addr_inc_mode: VramAddrIncMode,
    vram_addr: u16,

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
    hdma_reload: bool,
    hdma_transfer: bool,

    h_latch: u32,
    h_flipflop: bool,
    v_latch: u32,
    v_flipflop: bool,
    hv_latched: bool,

    #[educe(Default(
        expression = "FrameBuffer::new(SCREEN_WIDTH.try_into().unwrap(), SCREEN_HEIGHT.try_into().unwrap())"
    ))]
    frame_buffer: FrameBuffer,

    #[educe(Default(expression = "vec![0; SCREEN_WIDTH.try_into().unwrap()]"))]
    line_buffer: Vec<u16>,
    #[educe(Default(expression = "vec![0; SCREEN_WIDTH.try_into().unwrap()]"))]
    line_buffer_sub: Vec<u16>,
    #[educe(Default(expression = "vec![0; SCREEN_WIDTH.try_into().unwrap()]"))]
    z_buffer: Vec<u8>,
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

#[derive(Default)]
struct WinMask {
    bg: [WinMaskSettings; 4],
    obj: WinMaskSettings,
    math: WinMaskSettings,
}

#[bitfield(bits = 8)]
#[derive(Default)]
struct WinMaskSettings {
    area1: WinMaskSetting,
    area2: WinMaskSetting,
    #[skip]
    __: B4,
}

#[derive(BitfieldSpecifier)]
#[bits = 2]
#[derive(Default)]
enum WinMaskSetting {
    #[default]
    Disable = 0,
    Insize = 1,
    Outsize = 2,
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
#[derive(Default)]
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
#[derive(Default)]
struct ColorMathCtrl {
    direct_color: bool,
    sub_screen_enable: bool,
    #[skip]
    __: B2,
    color_math_enable: ColorMathEnable,
    force_main_screen_black: ForceMainScreenBlack,
    color_math_bg1: bool,
    color_math_bg2: bool,
    color_math_bg3: bool,
    color_math_bg4: bool,
    color_math_obj_pal47: bool,
    color_math_backdrop: bool,
    color_math_half: bool,
    color_math_add: bool,
}

#[derive(BitfieldSpecifier)]
#[bits = 2]
#[derive(Default)]
enum ColorMathEnable {
    #[default]
    Always = 0,
    MathWindow = 1,
    NotMathWindow = 2,
    Never = 3,
}

#[derive(BitfieldSpecifier)]
#[bits = 2]
#[derive(Default)]
enum ForceMainScreenBlack {
    #[default]
    Never = 0,
    NotMathWindow = 1,
    MathWindow = 2,
    Always = 3,
}

#[bitfield(bits = 8)]
#[derive(Default)]
struct ColorMathData {
    intensity: B5,
    r: bool,
    g: bool,
    b: bool,
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
        let ret = self.hdma_reload;
        self.hdma_reload = false;
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

                    // clear vblank flag, reset NMI flag
                    // TODO: clear vblank flag
                    self.vblank = false;
                    ctx.interrupt_mut().set_nmi_flag(false);
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
                    self.render_line(self.y - 1);
                    self.copy_line_buffer(self.y - 1);
                }
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
            // 0x2139 - RDVRAML - "PPU1 VRAM Data Read           (lower 8bits)"
            // 0x213A - RDVRAMH - "PPU1 VRAM Data Read           (upper 8bits)"
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
            _ => todo!("PPU Read: {addr:04X}"),
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
                self.bg_hofs[ix] = self.bg_hofs[ix] >> 8 | (data as u16) << 8;
            }
            0x210E | 0x2110 | 0x2112 | 0x2114 => {
                let ix = (addr - 0x210E) as usize / 2;
                self.bg_vofs[ix] = self.bg_vofs[ix] >> 8 | (data as u16) << 8;
            }
            0x2115 => self.vram_addr_inc_mode.bytes[0] = data,
            0x2116 => self.vram_addr = self.vram_addr & 0x7F00 | data as u16,
            0x2117 => self.vram_addr = self.vram_addr & 0x00FF | ((data & 0x7F) as u16) << 8,
            0x2118 | 0x2119 => {
                let ofs = addr - 0x2118;
                let vram_addr = self.vram_addr_inc_mode.translate(self.vram_addr);
                self.vram[(vram_addr * 2 + ofs) as usize] = data;
                if self.vram_addr_inc_mode.inc_after_high_byte() == (ofs == 1) {
                    self.vram_addr =
                        (self.vram_addr + self.vram_addr_inc_mode.inc_words()) & 0x7FFF;
                }
            }
            0x211A => self.rot_setting.bytes[0] = data,
            0x211B => {
                self.rot_param.a = self.rot_param.a >> 8 | (data as u16) << 8;
                self.mul_result = self.rot_param.a as i16 as i32 * self.rot_param.b as i8 as i32;
            }
            0x211C => {
                self.rot_param.b = self.rot_param.b >> 8 | (data as u16) << 8;
                self.mul_result = self.rot_param.a as i16 as i32 * self.rot_param.b as i8 as i32;
            }
            0x211D => self.rot_param.c = self.rot_param.c >> 8 | (data as u16) << 8,
            0x211E => self.rot_param.d = self.rot_param.d >> 8 | (data as u16) << 8,
            0x211F => self.rot_param.x = self.rot_param.x >> 8 | (data as u16) << 8,
            0x2120 => self.rot_param.y = self.rot_param.y >> 8 | (data as u16) << 8,
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
                self.win_pos[ix].left = data;
            }
            0x2127 | 0x2129 => {
                let ix = (addr - 0x2127) as usize / 2;
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
            0x2132 => self.color_math_data.bytes[0] = data,
            0x2133 => self.display_ctrl.bytes[1] = data,
            _ => todo!("PPU Write: {addr:#06X} = {data:#04X}"),
        }
    }
}

enum ColorMode {
    Color4,
    Color16,
    Color256,
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

impl Ppu {
    fn render_line(&mut self, y: u32) {
        if self.display_ctrl.force_blank() {
            self.line_buffer.fill(0);
            return;
        }

        // Backdrop
        self.line_buffer.fill(self.cgram[0]);
        self.z_buffer.fill(0xFF);

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
        // FF: Backdrop Backdrop Backdrop Backdrop Backdrop Backdrop Backdrop Backdrop

        match self.bg_mode.mode() {
            0 => {
                self.render_bg(y, 0, 2, 5, 2);
                self.render_bg(y, 1, 2, 6, 3);
                self.render_bg(y, 2, 2, 11, 8);
                self.render_bg(y, 3, 2, 12, 9);
            }
            1 => {
                self.render_bg(y, 0, 4, 5, 2);
                self.render_bg(y, 1, 4, 6, 3);
                self.render_bg(y, 2, 2, 11, 0); // FIXME
            }
            2 => todo!("BG Mode 2"),
            3 => todo!("BG Mode 3"),
            4 => todo!("BG Mode 4"),
            5 => todo!("BG Mode 5"),
            6 => todo!("BG Mode 6"),
            7 => log::warn!("BG Mode 7"),
            _ => unreachable!(),
        }

        self.render_obj(y);

        // todo!()
    }

    fn copy_line_buffer(&mut self, y: u32) {
        for x in 0..SCREEN_WIDTH {
            *self.frame_buffer.pixel_mut(x as usize, y as usize) =
                u16_to_pixel(self.line_buffer[x as usize]);
        }
    }

    fn render_bg(&mut self, y: u32, i: usize, bpp: usize, zl: u8, zh: u8) {
        if !self.screen_desig_main.bg(i) {
            return;
        }

        if self.bg_mode.tile_size(i) {
            todo!("16 x 16 Tile mode");
        }

        if y == 0 {
            debug!("Render: BG{i}");
        }

        // FIXME: base_addr is 6bit (0x3F * 2K = 126K), but VRAM is 64KB. ???
        let sc_base_addr = self.bg_sc[i].base_addr() as usize * 2 * 1024;
        let sc_size = self.bg_sc[i].size();

        let tile_base_addr = self.bg_tile_base_addr[i] as usize * 8 * 1024;

        let hofs = (self.bg_hofs[i] & 0x3FF) as usize;
        let vofs = (self.bg_vofs[i] & 0x3FF) as usize;

        // TODO: mosaic
        // TODO: window

        let pal_size = 1 << bpp;

        let (sc_w, sc_h) = match sc_size {
            ScreenSize::OneScreen => (1, 1),
            ScreenSize::VMirror => (1, 2),
            ScreenSize::HMirror => (2, 1),
            ScreenSize::FourScren => (2, 2),
        };

        let sy = y as usize + vofs;

        const SC_SIZE: usize = 32 * 32 * 2;

        // FIXME: optimize
        for x in 0..SCREEN_WIDTH {
            let sx = x as usize + hofs;
            let sc_x = sx / 8 / 32 % sc_w;
            let sc_y = sy / 8 / 32 % sc_h;
            let tile_x = sx / 8 % 32;
            let tile_y = sy / 8 % 32;
            let pixel_x = sx % 8;
            let pixel_y = sy % 8;

            let sc_addr = sc_base_addr + (sc_x + sc_y * sc_w) * SC_SIZE;
            let entry_addr = (sc_addr + (tile_x + tile_y * 32) * 2) & 0xFFFE;

            // debug!(
            //     "SC Base: {}, base addr: {sc_base_addr:04X}, sc: ({sc_x}/{sc_w}, {sc_y}/{sc_h}), tile: ({tile_x}, {tile_y}), entry: {entry_addr:04X}",
            //     self.bg_sc[i].base_addr()
            // );

            let entry =
                BgMapEntry::from_bytes(self.vram[entry_addr..entry_addr + 2].try_into().unwrap());

            let z = if entry.priority() == 0 { zl } else { zh };

            let pixel_x = if entry.x_flip() { pixel_x } else { 7 - pixel_x };
            let pixel_y = if entry.y_flip() { 7 - pixel_y } else { pixel_y };

            let tile_addr = tile_base_addr + entry.char_num() as usize * bpp as usize * 8;

            let mut pixel = 0;
            for i in 0..bpp / 2 {
                let addr = tile_addr + i * 16 + pixel_y * 2;
                let b0 = self.vram[addr];
                let b1 = self.vram[addr + 1];
                pixel |= ((b0 >> pixel_x) & 1) | (((b1 >> pixel_x) & 1) << 1) << (i * 2);
            }

            if pixel != 0 && z < self.z_buffer[x as usize] {
                let col = self.cgram[entry.pal_num() as usize * pal_size + pixel as usize];
                self.line_buffer[x as usize] = col;
                self.z_buffer[x as usize] = z;
            }
        }
    }

    fn render_obj(&mut self, y: u32) {
        if !self.screen_desig_main.obj() {
            return;
        }

        let y = y as usize;

        const OBJ_Z_TABLE: [u8; 4] = [10, 7, 4, 1];
        const BPP: usize = 4;
        const PAL_SIZE: usize = 16;

        let tile_base_addr = self.obj_sel.tile_base_addr() as usize;
        let addr_gap = self.obj_sel.addr_gap() as usize;

        let sizes = [
            self.obj_sel.obj_size_sel().size_small(),
            self.obj_sel.obj_size_sel().size_large(),
        ];

        for i in 0..128 {
            let entry = ObjEntry::from_bytes(self.oam[i * 4..i * 4 + 4].try_into().unwrap());
            let extra = (self.oam[0x200 + i / 4] >> ((i & 3) * 2)) & 3;

            let (ow, oh) = sizes[(extra >> 1) as usize];
            let oy = entry.y() as usize;
            if !(oy..oy + oh).contains(&y) {
                continue;
            }
            let ox = ((extra as usize & 1) << 8) + entry.x() as usize;
            let dy = y - oy;
            let pixel_y = if !entry.y_flip() { dy } else { oh - 1 - dy };

            let z = OBJ_Z_TABLE[entry.priority() as usize];

            log::info!(
                "Render OBJ {i}: ({ox}, {oy}), tile num: {}",
                entry.tile_num()
            );

            let tile_num = entry.tile_num() as usize;
            let tile_num = tile_num & 0x10F | ((tile_num & 0xF0) + pixel_y / 8 * 16) & 0xF0;

            for dx in 0..ow {
                let x = ox + dx;
                if !(0..SCREEN_WIDTH as usize).contains(&x) {
                    continue;
                }
                let pixel_x = if !entry.x_flip() { dx } else { ow - 1 - dx } as usize;

                let tile_num = tile_num & 0x1F0 | ((tile_num & 0xF) + pixel_x / 8) & 0xF;
                let tile_addr = if tile_num & 0x100 == 0 {
                    tile_base_addr + tile_num as usize * BPP * 8
                } else {
                    tile_base_addr + addr_gap + (tile_num as usize & 0xFF) * BPP * 8
                };

                let mut pixel = 0;
                for i in 0..BPP / 2 {
                    let addr = tile_addr + i * 16 + pixel_y % 8 * 2;
                    let b0 = self.vram[addr];
                    let b1 = self.vram[addr + 1];
                    pixel |= ((b0 >> (7 - pixel_x % 8)) & 1) << (i * 2);
                    pixel |= ((b1 >> (7 - pixel_x % 8)) & 1) << (i * 2 + 1);
                }

                if pixel != 0 && z < self.z_buffer[x as usize] {
                    let col =
                        self.cgram[0x80 + entry.pal_num() as usize * PAL_SIZE + pixel as usize];
                    self.line_buffer[x as usize] = col;
                    self.z_buffer[x as usize] = z;
                }
            }
        }
    }
}

fn u16_to_pixel(p: u16) -> Pixel {
    let r = (p & 0x1F) as u8;
    let g = ((p >> 5) & 0x1F) as u8;
    let b = ((p >> 10) & 0x1F) as u8;

    fn extend(c: u8) -> u8 {
        c << 3 | c >> 2
    }
    Pixel::new(extend(r), extend(g), extend(b))
}
