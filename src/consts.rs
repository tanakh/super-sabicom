pub const CYCLES_PER_DOT: u64 = 4;
pub const DOTS_PER_LINE: u64 = 341;
// FIXME:
// Except, Line F0h in Field.Bit=1 of Interlace: 1360 master cycles
pub const CYCLES_PER_LINE: u64 = CYCLES_PER_DOT * DOTS_PER_LINE;
pub const LINES_PER_FRAME: u64 = 262;
