#![allow(unused_braces)]

use std::ops::{BitAnd, BitXor, Shl};

use crate::context;
use log::{debug, trace};
use modular_bitfield::prelude::*;
use super_sabicom_macro::opcodes;

pub trait Context: context::Bus + context::Interrupt + context::Timing {}
impl<T: context::Bus + context::Interrupt + context::Timing> Context for T {}

const RESET_VECTOR: u16 = 0xFFFC;

const INTERNAL_CYCLE: u64 = 6;

#[derive(Default)]
pub struct Cpu {
    pub regs: Registers,
    stop: bool,
    halt: bool,
}

#[derive(Debug)]
enum Exception {
    Brk,
    Cop,
    Nmi,
    Irq,
}

impl Exception {
    fn vector_addr(&self, is_6502_mode: bool) -> u16 {
        match self {
            Exception::Brk => {
                if is_6502_mode {
                    0xFFFE
                } else {
                    0xFFE6
                }
            }
            Exception::Cop => {
                if is_6502_mode {
                    0xFFF4
                } else {
                    0xFFE4
                }
            }
            Exception::Irq => {
                if is_6502_mode {
                    0xFFFE
                } else {
                    0xFFEE
                }
            }
            Exception::Nmi => {
                if is_6502_mode {
                    0xFFFA
                } else {
                    0xFFEA
                }
            }
        }
    }
}

pub struct Registers {
    pub a: u16,
    pub x: u16,
    pub y: u16,
    pub pc: u16,   // Program counter
    pub s: u16,    // Stack pointer
    pub p: Status, // Processor status
    pub e: bool,   // 0: 16bit, 1=8bit
    pub d: u16,    // Zeropage offset
    pub db: u8,    // Data bank
    pub pb: u8,    // Program counter bank
}

#[bitfield]
#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub struct Status {
    pub c: bool, // Carry
    pub z: bool, // Zero
    pub i: bool, // IRQ Disable
    pub d: bool, // Decimal Mode
    pub x: bool, // X/Y is 8bit / Break
    pub m: bool, // A/mem is 8bit / Unused
    pub v: bool, // Overflow
    pub n: bool, // Negative
}

impl Default for Registers {
    fn default() -> Self {
        Registers {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            s: 0x01FF,
            p: Status::new().with_i(true).with_x(true).with_m(true),
            e: true,
            d: 0,
            db: 0,
            pb: 0,
        }
    }
}

impl Registers {
    #[inline]
    fn pc24(&self) -> u32 {
        (self.pb as u32) << 16 | self.pc as u32
    }

    #[inline]
    fn set_nz(&mut self, val: impl Value) {
        self.p.set_z(val.zero());
        self.p.set_n(val.msb());
    }

    fn set_a(&mut self, val: impl Value) {
        self.a = val.set(self.a);
    }

    fn set_x(&mut self, val: impl Value) {
        self.x = val.get();
    }

    fn set_y(&mut self, val: impl Value) {
        self.y = val.get();
    }

    fn set_s(&mut self, val: impl Value) {
        self.s = val.get();
    }

    fn set_d(&mut self, val: impl Value) {
        self.d = val.get();
    }
}

trait Value:
    Copy
    + BitXor<Output = Self>
    + BitAnd<Output = Self>
    + Shl<u32, Output = Self>
    + Eq
    + std::fmt::Debug
{
    const BITS: u32;

    fn from_u8(v: u8) -> Self;

    fn zero(&self) -> bool;
    fn msb(&self) -> bool;

    fn get(&self) -> u16;

    fn set(&self, other: u16) -> u16;
}

impl Value for u8 {
    const BITS: u32 = 8;

    #[inline]
    fn from_u8(v: u8) -> Self {
        v as Self
    }

    #[inline]
    fn zero(&self) -> bool {
        *self == 0
    }
    #[inline]
    fn msb(&self) -> bool {
        *self & 1 << (Self::BITS - 1) != 0
    }
    #[inline]
    fn get(&self) -> u16 {
        *self as u16
    }
    #[inline]
    fn set(&self, other: u16) -> u16 {
        (*self as u16) | (other & !0xFF)
    }
}

impl Value for u16 {
    const BITS: u32 = 8;

    #[inline]
    fn from_u8(v: u8) -> Self {
        v as Self
    }

    #[inline]
    fn zero(&self) -> bool {
        *self == 0
    }
    #[inline]
    fn msb(&self) -> bool {
        *self & 1 << (Self::BITS - 1) != 0
    }
    #[inline]
    fn get(&self) -> u16 {
        *self
    }
    #[inline]
    fn set(&self, _other: u16) -> u16 {
        *self
    }
}

// It generates `match_opcode` macro.
// `match_opcode(opcode, m)` is expanded to:
// > match opcode {
// >     0 => m!(asm...),
// >     1 => m!(asm...),
// >     ...
// > }
#[rustfmt::skip]
opcodes! {
    match_opcode,
    // 0/8      1/9         2/A         3/B         4/C         5/D         6/E        7/F
    brk       ; or a, inx ; cop       ; or a, sp  ; tsb zp    ; or a, zp  ; asl zp   ; or a, inf ; // 00
    push p    ; or a, imm ; asl a     ; push d    ; tsb abs   ; or a, abs ; asl abs  ; or a, far ; // 08
    bpl disp8 ; or a, iny ; or a, ind ; or a, siy ; trb zp    ; or a, zpx ; asl zpx  ; or a, ify ; // 10
    clc       ; or a, aby ; inc a     ; mov s, a  ; trb abs   ; or a, abx ; asl abx  ; or a, fax ; // 18
    jsr abs   ; and a, inx; jsr far   ; and a, sp ; bit a, zp ; and a, zp ; rol zp   ; and a, inf; // 20
    pop p     ; and a, imm; rol a     ; pop d     ; bit a, abs; and a, abs; rol abs  ; and a, far; // 28
    bmi disp8 ; and a, iny; and a, ind; and a, siy; bit a, zpx; and a, zpx; rol zpx  ; and a, ify; // 30
    sec       ; and a, aby; dec a     ; mov a, s  ; bit a, abx; and a, abx; rol abx  ; and a, fax; // 38
    rti       ; eor a, inx; wdm imm   ; eor a, sp ; mvp       ; eor a, zp ; lsr zp   ; eor a, inf; // 40
    push a    ; eor a, imm; lsr a     ; push pb   ; jmp abs   ; eor a, abs; lsr abs  ; eor a, far; // 48
    bvc disp8 ; eor a, iny; eor a, ind; eor a, siy; mvn       ; eor a, zpx; lsr zpx  ; eor a, ify; // 50
    cli       ; eor a, aby; push y    ; mov d, a  ; jmp far   ; eor a, abx; lsr abx  ; eor a, fax; // 58
    rts       ; adc a, inx; push rel  ; adc a, sp ; st zp, zr ; adc a, zp ; ror zp   ; adc a, inf; // 60
    pop a     ; adc a, imm; ror a     ; rtl       ; jmp abi   ; adc a, abs; ror abs  ; adc a, far; // 68
    bvs disp8 ; adc a, iny; adc a, ind; adc a, siy; st zpx, zr; adc a, zpx; ror zpx  ; adc a, ify; // 70
    sei       ; adc a, aby; pop y     ; mov a, d  ; jmp aix   ; adc a, abx; ror abx  ; adc a, fax; // 78
    jmp disp8 ; st inx, a ; jmp disp16; st sp, a  ; st zp, y  ; st zp, a  ; st zp, x ; st inf, a ; // 80
    dec y     ; bit a, imm; mov a, x  ; push db   ; st abs, y ; st abs, a ; st abs, x; st far, a ; // 88
    bcc disp8 ; st iny, a ; st ind, a ; st siy, a ; st zpx, y ; st zpx, a ; st zpy, x; st ify, a ; // 90
    mov a, y  ; st aby, a ; mov s, x  ; mov y, x  ; st abs, zr; st abx, a ; st abx,zr; st fax, a ; // 98
    ld y, imm ; ld a, inx ; ld x, imm ; ld a, sp  ; ld y, zp  ; ld a, zp  ; ld x, zp ; ld a, inf ; // A0
    mov y, a  ; ld a, imm ; mov x, a  ; pop db    ; ld y, abs ; ld a, abs ; ld x, abs; ld a, far ; // A8
    bcs disp8 ; ld a, iny ; ld a, ind ; ld a, siy ; ld y, zpx ; ld a, zpx ; ld x, zpy; ld a, ify ; // B0
    clv       ; ld a, aby ; mov x, s  ; mov x, y  ; ld y, abx ; ld a, abx ; ld x, aby; ld a, fax ; // B8
    cmp y, imm; cmp a, inx; rep imm   ; cmp a, sp ; cmp y, zp ; cmp a, zp ; dec zp   ; cmp a, inf; // C0
    inc y     ; cmp a, imm; dec x     ; wai       ; cmp y, abs; cmp a, abs; dec abs  ; cmp a, far; // C8
    bne disp8 ; cmp a, iny; cmp a, ind; cmp a, siy; push ind  ; cmp a, zpx; dec zpx  ; cmp a, ify; // D0
    cld       ; cmp a, aby; push x    ; stp       ; jmp aif   ; cmp a, abx; dec abx  ; cmp a, fax; // D8
    cmp x, imm; sbc a, inx; sep imm   ; sbc a, sp ; cmp x, zp ; sbc a, zp ; inc zp   ; sbc a, inf; // E0
    inc x     ; sbc a, imm; nop       ; xba       ; cmp x, abs; sbc a, abs; inc abs  ; sbc a, far; // E8
    beq disp8 ; sbc a, iny; sbc a, ind; sbc a, siy; push abs  ; sbc a, zpx; inc zpx  ; sbc a, ify; // F0
    sed       ; sbc a, aby; pop x     ; xce       ; jsr aix   ; sbc a, abx; inc abx  ; sbc a, fax; // F8
}

fn read8(ctx: &mut impl Context, addr: u32) -> u8 {
    ctx.read(addr)
}

fn read16(ctx: &mut impl Context, addr: u32) -> u16 {
    let b0 = ctx.read(addr);
    let b1 = ctx.read((addr + 1) & 0x00FFFFFF);
    (b0 as u16) | ((b1 as u16) << 8)
}

fn read24(ctx: &mut impl Context, addr: u32) -> u32 {
    let b0 = ctx.read(addr);
    let b1 = ctx.read((addr + 1) & 0x00FFFFFF);
    let b2 = ctx.read((addr + 2) & 0x00FFFFFF);
    (b0 as u32) | ((b1 as u32) << 8) | ((b2 as u32) << 16)
}

fn write8(ctx: &mut impl Context, addr: u32, data: u8) {
    ctx.write(addr, data);
}

fn write16(ctx: &mut impl Context, addr: u32, data: u16) {
    ctx.write(addr, data as u8);
    ctx.write((addr + 1) & 0x00FFFFFF, (data >> 8) as u8);
}

#[rustfmt::skip]
macro_rules! is_8bit {
    ($cpu:expr, mem) => { $cpu.regs.e || $cpu.regs.p.m() };
    ($cpu:expr, a) => { $cpu.regs.e || $cpu.regs.p.m() };
    ($cpu:expr, x) => { $cpu.regs.e || $cpu.regs.p.x() };
    ($cpu:expr, y) => { $cpu.regs.e || $cpu.regs.p.x() };
    ($cpu:expr, s) => { $cpu.regs.e };
    ($cpu:expr, d) => { false };
    ($cpu:expr, pb) => { true };
    ($cpu:expr, db) => { true };
}

impl Cpu {
    pub fn reset(&mut self, ctx: &mut impl Context) {
        self.regs = Registers::default();
        self.regs.pc = read16(ctx, RESET_VECTOR as u32);
    }

    fn fetch8(&mut self, ctx: &mut impl Context) -> u8 {
        let data = ctx.read(self.regs.pc24());
        self.regs.pc = self.regs.pc.wrapping_add(1);
        data
    }
    fn fetch16(&mut self, ctx: &mut impl Context) -> u16 {
        let b0 = self.fetch8(ctx);
        let b1 = self.fetch8(ctx);
        (b0 as u16) | ((b1 as u16) << 8)
    }
    fn fetch24(&mut self, ctx: &mut impl Context) -> u32 {
        let b0 = self.fetch8(ctx);
        let b1 = self.fetch8(ctx);
        let b2 = self.fetch8(ctx);
        (b0 as u32) | ((b1 as u32) << 8) | ((b2 as u32) << 16)
    }

    fn push8(&mut self, ctx: &mut impl Context, data: u8) {
        ctx.write(self.regs.s as u32, data);
        self.regs.s = self.regs.s.wrapping_sub(1);
    }
    fn push16(&mut self, ctx: &mut impl Context, data: u16) {
        self.push8(ctx, (data >> 8) as u8);
        self.push8(ctx, data as u8);
    }
    // fn push24(&mut self, ctx: &mut impl Context, data: u32) {
    //     self.push8(ctx, (data >> 16) as u8);
    //     self.push8(ctx, (data >> 8) as u8);
    //     self.push8(ctx, data as u8);
    // }

    fn pop8(&mut self, ctx: &mut impl Context) -> u8 {
        self.regs.s = self.regs.s.wrapping_add(1);
        ctx.read(self.regs.s as u32)
    }
    fn pop16(&mut self, ctx: &mut impl Context) -> u16 {
        let b0 = self.pop8(ctx);
        let b1 = self.pop8(ctx);
        (b0 as u16) | ((b1 as u16) << 8)
    }
    // fn pop24(&mut self, ctx: &mut impl Context) -> u32 {
    //     let b0 = self.pop8(ctx);
    //     let b1 = self.pop8(ctx);
    //     let b2 = self.pop8(ctx);
    //     (b0 as u32) | ((b1 as u32) << 8) | ((b2 as u32) << 16)
    // }

    fn exception(&mut self, ctx: &mut impl Context, e: Exception) {
        debug!("Exception: {e:?}");

        self.halt = false;
        ctx.elapse(INTERNAL_CYCLE * 2);

        if self.regs.e {
            // set B (X) flag
            self.regs
                .p
                .set_x(matches!(e, Exception::Brk | Exception::Cop));
        }
        self.push16(ctx, self.regs.pc);
        if !self.regs.e {
            self.push8(ctx, self.regs.pb);
        }
        self.push8(ctx, self.regs.p.into());

        self.regs.p.set_d(false);
        self.regs.p.set_i(true);
        self.regs.pb = 0;
        self.regs.pc = read16(ctx, e.vector_addr(self.regs.e) as u32);
    }

    pub fn exec_one(&mut self, ctx: &mut impl Context) {
        if ctx.bus().locked() {
            return;
        }

        if self.stop {
            ctx.elapse(INTERNAL_CYCLE);
            return;
        }

        if ctx.nmi() {
            self.exception(ctx, Exception::Nmi);
            return;
        }

        if (self.halt || !self.regs.p.i()) && ctx.irq() {
            self.exception(ctx, Exception::Irq);
            return;
        }

        if self.halt {
            ctx.elapse(INTERNAL_CYCLE);
            return;
        }

        if log::log_enabled!(log::Level::Trace) {
            self.trace(ctx);
        }

        let opcode = self.fetch8(ctx);

        macro_rules! exec_instr {
            (mov s, $src:ident) => {
                self.regs.s = reg!($src)
            };
            (mov $dst:ident, $src:ident) => {
                if is_8bit!(self, $dst) {
                    let v = reg!($src) as u8;
                    self.regs.set_nz(v);
                    set_reg!($dst, v)
                } else {
                    let v = reg!($src);
                    self.regs.set_nz(v);
                    set_reg!($dst, v)
                }
            };

            // ld
            (ld $reg:ident, $addrmode:ident) => {
                if is_8bit!(self, $reg) {
                    let addr = addr!($addrmode, u8);
                    let v = read8(ctx, addr);
                    self.regs.set_nz(v);
                    set_reg!($reg, v)
                } else {
                    let addr = addr!($addrmode, u16);
                    let v = read16(ctx, addr);
                    self.regs.set_nz(v);
                    set_reg!($reg, v)
                }
            };

            // st
            (st $addrmode:ident, zr) => {{
                let addr = addr!($addrmode);
                if is_8bit!(self, mem) {
                    write8(ctx, addr, 0);
                } else {
                    write16(ctx, addr, 0);
                }
            }};
            (st $addrmode:ident, $reg:ident) => {
                if is_8bit!(self, $reg) {
                    let addr = addr!($addrmode);
                    write8(ctx, addr, reg!($reg) as u8);
                } else {
                    let addr = addr!($addrmode);
                    write16(ctx, addr, reg!($reg));
                }
            };

            // push
            (push p) => {{
                let mut p = self.regs.p;
                if self.regs.e {
                    p.set_m(true);
                    p.set_x(true);
                }
                self.push8(ctx, p.into());
            }};
            (push pb) => {
                self.push8(ctx, self.regs.pb)
            };
            (push db) => {
                self.push8(ctx, self.regs.db)
            };
            (push ind) => {{
                let addr = addr!(ind) as u16;
                self.push16(ctx, addr)
            }};
            (push abs) => {{
                let addr = addr!(abs) as u16;
                self.push16(ctx, addr)
            }};
            (push rel) => {{
                let disp = self.fetch16(ctx);
                self.push16(ctx, self.regs.pc.wrapping_add(disp));
            }};
            (push $reg:ident) => {
                if is_8bit!(self, $reg) {
                    self.push8(ctx, reg!($reg) as u8)
                } else {
                    self.push16(ctx, reg!($reg))
                }
            };

            // pop
            (pop p) => {{
                self.regs.p = self.pop8(ctx).into();
                if self.regs.e {
                    self.regs.p.set_x(true);
                    self.regs.p.set_m(true);
                }
            }};
            (pop db) => {{
                let v = self.pop8(ctx);
                self.regs.set_nz(v);
                self.regs.db = v;
            }};
            (pop $reg:ident) => {
                if is_8bit!(self, $reg) {
                    let v = self.pop8(ctx);
                    self.regs.set_nz(v);
                    set_reg!($reg, v);
                } else {
                    let v = self.pop16(ctx);
                    set_reg!($reg, v);
                    self.regs.set_nz(v);
                }
            };

            // block trans
            (mvp) => {
                block_trans!(-1_i16 as u16)
            };
            (mvn) => {
                block_trans!(1_u16)
            };

            // alu
            (or $reg:ident, $opr:ident) => {
                alu!(or, $reg, $opr)
            };
            (and $reg:ident, $opr:ident) => {
                alu!(and, $reg, $opr)
            };
            (eor $reg:ident, $opr:ident) => {
                alu!(eor, $reg, $opr)
            };
            (adc $reg:ident, $opr:ident) => {
                alu!(adc, $reg, $opr)
            };
            (sbc $reg:ident, $opr:ident) => {
                alu!(sbc, $reg, $opr)
            };
            (cmp $reg:ident, $opr:ident) => {
                cmp!($reg, $opr)
            };

            (bit a, $opr:ident) => {{
                macro_rules! set_nv {
                    (imm, $_b:ident, $_:ty) => {};
                    ($_:ident, $b:ident, $ty:ty) => {{
                        self.regs.p.set_n($b & (1 << (<$ty>::BITS - 1)) != 0);
                        self.regs.p.set_v($b & (1 << (<$ty>::BITS - 2)) != 0);
                    }};
                }
                if is_8bit!(self, a) {
                    let addr = addr!($opr, u8);
                    let a = self.regs.a as u8;
                    let b = read8(ctx, addr);
                    self.regs.p.set_z(a & b == 0);
                    set_nv!($opr, b, u8);
                } else {
                    let addr = addr!($opr, u16);
                    let a = self.regs.a;
                    let b = read16(ctx, addr);
                    self.regs.p.set_z(a & b == 0);
                    set_nv!($opr, b, u8);
                }
            }};

            // rmw
            (inc $opr:ident) => {
                rmw!(inc, $opr)
            };
            (dec $opr:ident) => {
                rmw!(dec, $opr)
            };
            (tsb $opr:ident) => {
                rmw!(tsb, $opr)
            };
            (trb $opr:ident) => {
                rmw!(trb, $opr)
            };
            (asl $opr:ident) => {
                rmw!(asl, $opr)
            };
            (lsr $opr:ident) => {
                rmw!(lsr, $opr)
            };
            (rol $opr:ident) => {
                rmw!(rol, $opr)
            };
            (ror $opr:ident) => {
                rmw!(ror, $opr)
            };

            // jump
            (jmp disp8) => {{
                let disp = self.fetch8(ctx) as i8;
                self.regs.pc = self.regs.pc.wrapping_add(disp as u16);
            }};
            (jmp disp16) => {{
                let disp = self.fetch16(ctx);
                self.regs.pc = self.regs.pc.wrapping_add(disp);
            }};
            (jmp far) => {{
                let addr = self.fetch24(ctx);
                self.regs.pc = addr as u16;
                self.regs.pb = (addr >> 16) as u8;
            }};
            (jmp aif) => {{
                let addr = self.fetch16(ctx) as u32;
                let addr = read24(ctx, addr);
                self.regs.pc = addr as u16;
                self.regs.pb = (addr >> 16) as u8;
            }};
            (jmp $addr:ident) => {{
                let addr = addr!($addr);
                self.regs.pc = addr as u16;
            }};

            (jsr abs) => {{
                let addr = self.fetch16(ctx);
                self.push16(ctx, self.regs.pc);
                self.regs.pc = addr;
            }};
            (jsr far) => {{
                let addr = self.fetch24(ctx);
                self.push16(ctx, self.regs.pc);
                self.push8(ctx, self.regs.pb);
                self.regs.pc = addr as u16;
                self.regs.pb = (addr >> 16) as u8;
            }};
            (jsr aix) => {{
                let addr = addr!(aix);
                self.push16(ctx, self.regs.pc);
                self.regs.pc = addr as u16;
            }};

            (rti) => {{
                // FIXME: RTI cannot modify the B-Flag or the unused flag.
                self.regs.p = self.pop8(ctx).into();
                self.regs.pb = self.pop8(ctx);
                self.regs.pc = self.pop16(ctx);
            }};
            (rtl) => {{
                self.regs.pb = self.pop8(ctx);
                self.regs.pc = self.pop16(ctx);
            }};
            (rts) => {{
                self.regs.pc = self.pop16(ctx);
            }};

            // cond branch
            // FIXME: additional cycle for crossing page boundary
            (bpl disp8) => {
                cond_branch!(pl)
            };
            (bmi disp8) => {
                cond_branch!(mi)
            };
            (bvc disp8) => {
                cond_branch!(vc)
            };
            (bvs disp8) => {
                cond_branch!(vs)
            };
            (bcc disp8) => {
                cond_branch!(cc)
            };
            (bcs disp8) => {
                cond_branch!(cs)
            };
            (bne disp8) => {
                cond_branch!(ne)
            };
            (beq disp8) => {
                cond_branch!(eq)
            };

            // misc
            (brk) => {
                self.exception(ctx, Exception::Brk)
            };
            (cop) => {
                self.exception(ctx, Exception::Cop)
            };

            // flags
            (clc) => {
                self.regs.p.set_c(false)
            };
            (cli) => {
                self.regs.p.set_i(false)
            };
            (cld) => {
                self.regs.p.set_d(false)
            };
            (clv) => {
                self.regs.p.set_v(false)
            };
            (sec) => {
                self.regs.p.set_c(true)
            };
            (sei) => {
                self.regs.p.set_i(true)
            };
            (sed) => {
                self.regs.p.set_d(true)
            };
            (rep imm) => {{
                let v = self.fetch8(ctx);
                let mut p: Status = (u8::from(self.regs.p) & !v).into();
                if self.regs.e {
                    p.set_x(true);
                    p.set_m(true);
                }
                self.regs.p = p;
            }};
            (sep imm) => {{
                let v = self.fetch8(ctx);
                self.regs.p = (u8::from(self.regs.p) | v).into();
            }};
            (xce) => {{
                let c = self.regs.p.c();
                self.regs.p.set_c(self.regs.e);
                self.regs.e = c;
                if self.regs.e {
                    self.regs.p.set_x(true);
                    self.regs.p.set_m(true);
                    self.regs.x &= 0xFF;
                    self.regs.y &= 0xFF;
                    self.regs.s = 0x100 | self.regs.s & 0xFF;
                }
            }};

            // special
            (stp) => {
                self.stop = true
            };
            (xba) => {{
                self.regs.a = self.regs.a.rotate_right(8);
                self.regs.set_nz(self.regs.a as u8);
            }};
            (wai) => {
                self.halt = true
            };
            (wdm imm) => {{
                // This is 2 byte nop opcode
                let _dmy = self.fetch8(ctx);
            }};
            (nop) => {
                ()
            };
        }

        macro_rules! block_trans {
            ($inc:expr) => {{
                let db = self.fetch8(ctx);
                let sb = self.fetch8(ctx);
                self.regs.db = db;

                let v = read8(ctx, (sb as u32) << 16 | self.regs.x as u32);
                write8(ctx, (db as u32) << 16 | self.regs.y as u32, v);

                if is_8bit!(self, x) {
                    self.regs.x = (self.regs.x as u8).wrapping_add($inc as u8) as u16;
                    self.regs.y = (self.regs.y as u8).wrapping_add($inc as u8) as u16;
                } else {
                    self.regs.x = self.regs.x.wrapping_add($inc);
                    self.regs.y = self.regs.y.wrapping_add($inc);
                }
                self.regs.a = self.regs.a.wrapping_sub(1);

                if self.regs.a != 0xFFFF {
                    self.regs.pc = self.regs.pc.wrapping_sub(3);
                }
            }};
        }

        macro_rules! alu {
            ($op:ident, $reg:ident, $opr:ident) => {{
                if is_8bit!(self, $reg) {
                    let addr = addr!($opr, u8);
                    let a = reg!($reg) as u8;
                    let b = read8(ctx, addr);
                    let c = alu_op!($op, a, b, u8);
                    self.regs.set_nz(c);
                    set_reg!($reg, c);
                } else {
                    let addr = addr!($opr, u16);
                    let a = reg!($reg);
                    let b = read16(ctx, addr);
                    let c = alu_op!($op, a, b, u16);
                    self.regs.set_nz(c);
                    set_reg!($reg, c);
                }
            }};
        }

        macro_rules! cmp {
            ($reg:ident, $opr:ident) => {{
                if is_8bit!(self, $reg) {
                    let addr = addr!($opr, u8);
                    let a = reg!($reg) as u8;
                    let b = read8(ctx, addr);
                    let (c, carry) = a.overflowing_sub(b);
                    self.regs.set_nz(c);
                    self.regs.p.set_c(!carry);
                } else {
                    let addr = addr!($opr, u16);
                    let a = reg!($reg);
                    let b = read16(ctx, addr);
                    let (c, carry) = a.overflowing_sub(b);
                    self.regs.set_nz(c);
                    self.regs.p.set_c(!carry);
                }
            }};
        }

        macro_rules! alu_op {
            (or, $a:ident, $b:ident, $_:ty) => {
                $a | $b
            };
            (and, $a:ident, $b:ident, $_:ty) => {
                $a & $b
            };
            (eor, $a:ident, $b:ident, $_:ty) => {
                $a ^ $b
            };
            (adc, $a:ident, $b:ident, $ty:ident) => {{
                if self.regs.p.d() {
                    let a = $a as u32;
                    let b = $b as u32;

                    let mut cl = (a & 0x0F) + (b & 0x0F) + self.regs.p.c() as u32;
                    if cl >= 0xA {
                        cl = ((cl + 6) & 0xF) + 0x10;
                    }

                    macro_rules! calc {
                        (u8) => {{
                            let mut c = (a & 0xF0) + (b & 0xF0) + cl;
                            if c >= 0xA0 {
                                c += 0x60;
                            }
                            self.regs.p.set_c(c > 0xFF);

                            let d = (a & 0xF0) as i8 as i32 + (b & 0xF0) as i8 as i32 + cl as i32;
                            self.regs.p.set_v(d < -0x80 || d > 0x7F);

                            c as u8
                        }};
                        (u16) => {{
                            cl = (a & 0xF0) + (b & 0xF0) + cl;
                            if cl >= 0xA0 {
                                cl = ((cl + 0x60) & 0xFF) + 0x100;
                            }
                            cl = (a & 0xF00) + (b & 0xF00) + cl;
                            if cl >= 0xA00 {
                                cl = ((cl + 0x600) & 0xFFF) + 0x1000;
                            }
                            let mut c = (a & 0xF000) + (b & 0xF000) + cl;
                            if c >= 0xA000 {
                                c += 0x6000;
                            }
                            self.regs.p.set_c(c > 0xFFFF);

                            let d =
                                (a & 0xF000) as i16 as i32 + (b & 0xF000) as i16 as i32 + cl as i32;
                            self.regs.p.set_v(d < -0x8000 || d > 0x7FFF);

                            c as u16
                        }};
                    }

                    calc!($ty)
                } else {
                    let a = $a as u32;
                    let b = $b as u32;
                    let c = a + b + self.regs.p.c() as u32;
                    let ovf = !(a ^ b) & (a ^ c) & (1 << (<$ty>::BITS - 1)) != 0;

                    self.regs.p.set_c(c > <$ty>::MAX as u32);
                    self.regs.p.set_v(ovf);

                    c as $ty
                }
            }};
            (sbc, $a:ident, $b:ident, $ty:ident) => {{
                let a = $a as u32;
                let b = $b as u32;
                let borrow = !self.regs.p.c();
                let c = a.wrapping_sub(b).wrapping_sub(borrow as u32);
                let ovf = (a ^ b) & (a ^ c) & (1 << (<$ty>::BITS - 1)) != 0;

                self.regs.p.set_c(c <= <$ty>::MAX as u32);
                self.regs.p.set_v(ovf);

                if self.regs.p.d() {
                    let a = $a as i32;
                    let b = $b as i32;
                    let mut cl = (a & 0xF) - (b & 0xF) - borrow as i32;
                    if cl < 0 {
                        cl = ((cl - 6) & 0xF) - 0x10;
                    }
                    let mut c = (a & 0xF0) - (b & 0xF0) + cl;
                    if c < 0 {
                        c = (c - 0x60) & 0xFF - 0x100;
                    }

                    macro_rules! calc {
                        (u8) => {};
                        (u16) => {
                            c = (a & 0xF00) - (b & 0xF00) + c;
                            if c < 0 {
                                c = (c - 0x600) & 0xFFF - 0x1000;
                            }
                            c = (a & 0xF000) - (b & 0xF000) + c;
                            if c < 0 {
                                c -= 0x6000;
                            }
                        };
                    }

                    calc!($ty);

                    c as $ty
                } else {
                    c as $ty
                }
            }};
        }

        macro_rules! rmw {
            ($op:ident, a) => {
                rmw_reg!($op, a)
            };
            ($op:ident, x) => {
                rmw_reg!($op, x)
            };
            ($op:ident, y) => {
                rmw_reg!($op, y)
            };

            ($op:ident, $addrmode:ident) => {{
                let addr = addr!($addrmode);
                if is_8bit!(self, mem) {
                    let v = read8(ctx, addr);
                    write8(ctx, addr, modop!($op, v, u8));
                } else {
                    let v = read16(ctx, addr);
                    write16(ctx, addr, modop!($op, v, u16));
                }
            }};
        }

        macro_rules! rmw_reg {
            ($op:ident, $reg:ident) => {{
                if is_8bit!(self, $reg) {
                    let v = self.regs.$reg as u8;
                    set_reg!($reg, modop!($op, v, u8));
                } else {
                    let v = self.regs.$reg;
                    set_reg!($reg, modop!($op, v, u16));
                }
            }};
        }

        macro_rules! modop {
            (inc, $v:ident, $_:ty) => {{
                let v = $v.wrapping_add(1);
                self.regs.set_nz(v);
                v
            }};
            (dec, $v:ident, $_:ty) => {{
                let v = $v.wrapping_sub(1);
                self.regs.set_nz(v);
                v
            }};

            (tsb, $v:ident, $ty:ty) => {{
                let v = $v;
                let a = reg!(a) as $ty;
                self.regs.p.set_z(v & a == 0);
                v | a
            }};
            (trb, $v:ident, $ty:ty) => {{
                let v = $v;
                let a = reg!(a) as $ty;
                self.regs.p.set_z(v & a == 0);
                v & !a
            }};
            (asl, $v:ident, $ty:ty) => {{
                let v = $v << 1;
                self.regs.set_nz(v);
                self.regs.p.set_c($v & (1 << (<$ty>::BITS - 1)) != 0);
                v
            }};
            (lsr, $v:ident, $_:ty) => {{
                let v = $v;
                self.regs.p.set_c(v & 1 != 0);
                let v = v >> 1;
                self.regs.set_nz(v);
                v
            }};
            (rol, $v:ident, $ty:ty) => {{
                let c = self.regs.p.c();
                let v = $v.rotate_left(1);
                self.regs.p.set_c(v & 1 != 0);
                let v = v & !1 | (c as $ty);
                self.regs.set_nz(v);
                v
            }};
            (ror, $v:ident, $ty:ty) => {{
                let c = self.regs.p.c();
                self.regs.p.set_c($v & 1 != 0);
                let v = ($v & !1 | c as $ty).rotate_right(1);
                self.regs.set_nz(v);
                v
            }};
        }

        macro_rules! cond_branch {
            ($cond:ident) => {{
                let disp = self.fetch8(ctx) as i8 as u16;
                if cond!($cond) {
                    ctx.elapse(INTERNAL_CYCLE);
                    self.regs.pc = self.regs.pc.wrapping_add(disp);
                }
            }};
        }

        #[rustfmt::skip]
        macro_rules! cond {
            (pl) => { !self.regs.p.n() };
            (mi) => { self.regs.p.n() };
            (vc) => { !self.regs.p.v() };
            (vs) => { self.regs.p.v() };
            (cc) => { !self.regs.p.c() };
            (cs) => { self.regs.p.c() };
            (ne) => { !self.regs.p.z() };
            (eq) => { self.regs.p.z() };
        }

        #[rustfmt::skip]
        macro_rules! reg {
            (a) => { self.regs.a };
            (x) => { self.regs.x };
            (y) => { self.regs.y };
            (s) => { self.regs.s };
            (d) => { self.regs.d };
            (pb) => { self.regs.pb };
            (db) => { self.regs.db };
        }

        #[rustfmt::skip]
        macro_rules! set_reg {
            (a, $v:expr) => {{ let v = $v; self.regs.set_a(v); }};
            (x, $v:expr) => {{ let v = $v; self.regs.set_x(v); }};
            (y, $v:expr) => {{ let v = $v; self.regs.set_y(v); }};
            (s, $v:expr) => {{ let v = $v; self.regs.set_s(v); }};
            (d, $v:expr) => {{ let v = $v; self.regs.set_d(v); }};
        }

        // FIXME: add one cycle for crossing a page boundary
        macro_rules! addr {
            (imm, u8) => {{
                let addr = self.regs.pc24();
                self.regs.pc = self.regs.pc.wrapping_add(1);
                addr
            }};
            (imm, u16) => {{
                let addr = self.regs.pc24();
                self.regs.pc = self.regs.pc.wrapping_add(2);
                addr
            }};

            ($addrmode:ident, $_:ty) => {
                addr!($addrmode)
            };

            (zp) => {{
                let offset = self.fetch8(ctx);
                self.regs.d.wrapping_add(offset as u16) as u32
            }};
            (zpx) => {{
                let offset = self.fetch8(ctx);
                self.regs
                    .d
                    .wrapping_add(offset as u16)
                    .wrapping_add(self.regs.x) as u32
            }};
            (zpy) => {{
                let offset = self.fetch8(ctx);
                self.regs
                    .d
                    .wrapping_add(offset as u16)
                    .wrapping_add(self.regs.y) as u32
            }};
            (inf) => {{
                let addr = addr!(zp);
                read24(ctx, addr)
            }};
            (ify) => {{
                let addr = addr!(zp);
                read24(ctx, addr).wrapping_add(self.regs.y as u32)
            }};
            (sp) => {{
                let offset = self.fetch8(ctx) as u16;
                self.regs.s.wrapping_add(offset) as u32
            }};
            (siy) => {{
                let addr = addr!(sp);
                read16(ctx, addr).wrapping_add(self.regs.y) as u32
            }};
            (abs) => {{
                let offset = self.fetch16(ctx);
                ((self.regs.db as u32) << 16) | offset as u32
            }};
            (abx) => {{
                let offset = self.fetch16(ctx);
                ((self.regs.db as u32) << 16) | offset.wrapping_add(self.regs.x) as u32
            }};
            (aby) => {{
                let offset = self.fetch16(ctx);
                ((self.regs.db as u32) << 16) | offset.wrapping_add(self.regs.y) as u32
            }};
            (abi) => {{
                let addr = self.fetch16(ctx) as u32;
                read16(ctx, addr) as u32
            }};
            (aix) => {{
                let addr = self.fetch16(ctx).wrapping_add(self.regs.x);
                read16(ctx, (self.regs.pb as u32) << 16 | (addr as u32)) as u32
            }};
            (ind) => {{
                let addr = addr!(zp);
                read16(ctx, addr) as u32
            }};
            (inx) => {{
                let addr = addr!(zpx);
                read16(ctx, addr) as u32
            }};
            (iny) => {{
                let addr = addr!(zp);
                read16(ctx, addr).wrapping_add(self.regs.y) as u32
            }};
            (far) => {
                self.fetch24(ctx)
            };
            (fax) => {
                addr!(far).wrapping_add(self.regs.x as u32)
            };
        }

        match_opcode!(opcode, exec_instr);
    }

    fn trace(&self, ctx: &impl Context) {
        let asm = self.disasm(ctx).map_or_else(
            || "XX          invalid".into(),
            |(bytes, asm)| {
                format!(
                    "{:12} {asm}",
                    bytes
                        .into_iter()
                        .map(|b| format!("{b:02X}"))
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            },
        );

        trace!(
            "{:02X}:{:04X}  {asm:32} A:{:04X} X:{:04X} Y:{:04X} S:{:04X} D:{:04X} DB:{:02X} PB:{:02X} P:{}{}{}{}{}{}{}{}{} CYC: {}",
            self.regs.pb,
            self.regs.pc,
            self.regs.a,
            self.regs.x,
            self.regs.y,
            self.regs.s,
            self.regs.d,
            self.regs.db,
            self.regs.pb,
            if self.regs.e {'E'} else {'e'},
            if self.regs.p.n() {'N'} else {'n'},
            if self.regs.p.v() {'V'} else {'v'},
            if self.regs.p.m() {'M'} else {'m'},
            if self.regs.p.x() {'X'} else {'x'},
            if self.regs.p.d() {'D'} else {'d'},
            if self.regs.p.i() {'I'} else {'i'},
            if self.regs.p.z() {'Z'} else {'z'},
            if self.regs.p.c() {'C'} else {'c'},
            ctx.now(),
        );
    }

    fn disasm(&self, ctx: &impl Context) -> Option<(Vec<u8>, String)> {
        let mut pc = self.regs.pc;
        let mut bytes = vec![];

        macro_rules! fetch {
            (8) => {{
                let v = ctx.read_pure((self.regs.pb as u32) << 16 | pc as u32)?;
                #[allow(unused_assignments)]
                {
                    pc = pc.wrapping_add(1);
                }
                bytes.push(v);
                v
            }};
            (16) => {{
                let b0 = fetch!(8);
                let b1 = fetch!(8);
                (b1 as u16) << 8 | b0 as u16
            }};
            (24) => {{
                let b0 = fetch!(8);
                let b1 = fetch!(8);
                let b2 = fetch!(8);
                (b2 as u32) << 16 | (b1 as u32) << 8 | b0 as u32
            }};
        }

        macro_rules! disasm_instr {
            ($mne:ident) => {
                stringify!($mne).to_string()
            };
            ($mne:ident $op:ident) => {
                format!("{} {}", stringify!($mne), operand!($op, none))
            };
            ($mne:ident $op1:ident, $op2:ident) => {
                format!(
                    "{} {}, {}",
                    stringify!($mne),
                    operand!($op1, $op2),
                    operand!($op2, $op1)
                )
            };
        }

        macro_rules! operand {
            (zr, $_:ident) => {
                "0".to_string()
            };

            (a, $_:ident) => {
                "a".to_string()
            };
            (x, $_:ident) => {
                "x".to_string()
            };
            (y, $_:ident) => {
                "y".to_string()
            };
            (s, $_:ident) => {
                "s".to_string()
            };
            (p, $_:ident) => {
                "p".to_string()
            };
            (d, $_:ident) => {
                "d".to_string()
            };
            (db, $_:ident) => {
                "db".to_string()
            };
            (pb, $_:ident) => {
                "pb".to_string()
            };

            (imm, none) => {
                format!("#{:#04X}", fetch!(8))
            };
            (imm, $reg:ident) => {{
                if is_8bit!(self, $reg) {
                    format!("#{:#04X}", fetch!(8))
                } else {
                    format!("#{:#06X}", fetch!(16))
                }
            }};
            (zp, $_:ident) => {
                format!("{:#04X}", fetch!(8))
            };
            (zpx, $_:ident) => {
                format!("{:#04X}, x", fetch!(8))
            };
            (zpy, $_:ident) => {
                format!("{:#04X}, y", fetch!(8))
            };
            (abs, $_:ident) => {
                format!("{:#06X}", fetch!(16))
            };
            (abx, $_:ident) => {
                format!("{:#06X}, x", fetch!(16))
            };
            (aby, $_:ident) => {
                format!("{:#06X}, y", fetch!(16))
            };
            (abi, $_:ident) => {
                format!("({:#06X})", fetch!(16))
            };
            (aix, $_:ident) => {
                format!("({:#06X}, x)", fetch!(16))
            };
            (aif, $_:ident) => {
                format!("far({:#06X})", fetch!(16))
            };
            (ind, $_:ident) => {
                format!("({:#04X})", fetch!(8))
            };
            (inx, $_:ident) => {
                format!("({:#04X}, x)", fetch!(8))
            };
            (iny, $_:ident) => {
                format!("({:#04X}), y", fetch!(8))
            };
            (far, $_:ident) => {
                format!("{:#08X}", fetch!(24))
            };
            (fax, $_:ident) => {
                format!("{:#08X}, x", fetch!(24))
            };
            (inf, $_:ident) => {
                format!("[{:#04X}]", fetch!(8))
            };
            (ify, $_:ident) => {
                format!("[{:#04X}], y", fetch!(8))
            };
            (sp, $_:ident) => {
                format!("{:#04X}, s", fetch!(8))
            };
            (siy, $_:ident) => {
                format!("({:#04X}, s), y", fetch!(8))
            };

            (rel, $_:ident) => {{
                let disp = fetch!(16);
                format!("{:+} (= {:#06X})", disp as i16, pc.wrapping_add(disp))
            }};
            (disp8, $_:ident) => {{
                let disp = fetch!(8) as i8 as u16;
                format!("{:#06X}", pc.wrapping_add(disp))
            }};
            (disp16, $_:ident) => {{
                let disp = fetch!(16) as u16;
                format!("{:#06X}", pc.wrapping_add(disp))
            }};
        }

        let opcode = fetch!(8);
        let asm = match_opcode!(opcode, disasm_instr);
        Some((bytes, asm))
    }
}
