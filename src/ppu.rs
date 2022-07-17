#![allow(unused_braces)]

use educe::Educe;
use log::debug;
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
    bg_horizontal_scroll: [u8; 4],
    bg_vertical_scroll: [u8; 4],
    rot_param: RotParam,
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

    #[educe(Default(expression = "vec![0; 0x200]"))]
    cgram: Vec<u8>,
    cgram_addr: u16,
    cgram_lsb: u8,

    x: u32,
    y: u32,
    frame: u64,
    counter: u64,
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
    fn size_small(&self) -> (u32, u32) {
        match self {
            ObjSizeSel::Size8x8_16x16 => (8, 8),
            ObjSizeSel::Size8x8_32x32 => (8, 8),
            ObjSizeSel::Size8x8_64x64 => (8, 8),
            ObjSizeSel::Size16x16_32x32 => (16, 16),
            ObjSizeSel::Size16x16_64x64 => (16, 16),
            ObjSizeSel::Size32x32_64x64 => (32, 32),
        }
    }

    fn size_large(&self) -> (u32, u32) {
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
#[derive(Default)]
struct BgMode {
    bg_mode: B3,
    bg3_priority_is_high: bool,
    bg_tile_size: B4,
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
}

impl Ppu {
    pub fn tick(&mut self, ctx: &mut impl Context) {
        let now = ctx.now();

        while self.counter + CYCLES_PER_DOT as u64 <= now {
            self.counter += CYCLES_PER_DOT as u64;

            self.x += 1;
            if self.x == DOTS_PER_LINE {
                self.x = 0;
                self.y += 1;

                debug!("Line: {}:{}", self.frame, self.y);

                if self.y == LINES_PER_FRAME {
                    self.y = 0;
                    self.frame += 1;
                    debug!("Start Frame: {}", self.frame);

                    // clear vblank flag, reset NMI flag
                    // TODO: clear vblank flag
                    ctx.set_nmi_flag(false);
                }

                if self.y == VBLANK_START {
                    // TODO: set vblank flag
                }
            }

            // FIXME: x == 0.5, not 1
            if (self.x, self.y) == (1, VBLANK_START) {
                ctx.set_nmi_flag(true);
            }

            if self.x == 1 {
                // FIXME: clear hblank flag
            }

            if self.x == 274 {
                // FIXME: set hblank flag

                // self.render_line();
            }

            if self.x == 278 {
                // TODO:  perform HDMA transfers
            }
        }
    }

    pub fn read(&mut self, ctx: &mut impl Context, addr: u16) -> u8 {
        todo!();
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
            0x2105 => self.bg_mode.bytes[0] = data,
            0x2106 => self.mosaic.bytes[0] = data,
            0x2107..=0x210A => self.bg_sc[(addr - 0x2107) as usize].bytes[0] = data,
            0x210B..=0x210C => {
                let ix = (addr - 0x210B) as usize * 2;
                self.bg_tile_base_addr[ix] = data & 0xF;
                self.bg_tile_base_addr[ix + 1] = data >> 4;
            }
            0x210D | 0x210F | 0x2111 | 0x2113 => {
                let ix = (addr - 0x210D) as usize / 2;
                self.bg_horizontal_scroll[ix] = data;
            }
            0x210E | 0x2110 | 0x2112 | 0x2114 => {
                let ix = (addr - 0x210E) as usize / 2;
                self.bg_vertical_scroll[ix] = data;
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
            0x211B => self.rot_param.a = self.rot_param.a << 8 | data as u16,
            0x211C => self.rot_param.b = self.rot_param.b << 8 | data as u16,
            0x211D => self.rot_param.c = self.rot_param.c << 8 | data as u16,
            0x211E => self.rot_param.d = self.rot_param.d << 8 | data as u16,
            0x211F => self.rot_param.x = self.rot_param.x << 8 | data as u16,
            0x2120 => self.rot_param.y = self.rot_param.y << 8 | data as u16,
            0x2121 => self.cgram_addr = data as u16 * 2,
            0x2122 => {
                if self.cgram_addr & 1 == 0 {
                    self.cgram_lsb = data;
                } else {
                    self.cgram[self.cgram_addr as usize - 1] = self.cgram_lsb;
                    self.cgram[self.cgram_addr as usize] = data;
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
