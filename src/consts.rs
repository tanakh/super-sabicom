pub const CYCLES_PER_DOT: u32 = 4;
pub const DOTS_PER_LINE: u32 = 341;
// FIXME:
// Except, Line F0h in Field.Bit=1 of Interlace: 1360 master cycles
pub const CYCLES_PER_LINE: u32 = CYCLES_PER_DOT * DOTS_PER_LINE;
pub const LINES_PER_FRAME: u32 = 262;

pub const SCREEN_HEIGHT: u32 = 224;
pub const SCREEN_WIDTH: u32 = 224;
pub const VBLANK_START: u32 = 225;
