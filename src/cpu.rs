#![allow(unused_braces)]

use std::ops::{BitAnd, BitXor, Shl};

use crate::context;
use log::{debug, error, trace};
use modular_bitfield::prelude::*;
use serde::{Deserialize, Serialize};
use super_sabicom_macro::opcodes;

pub trait Context: context::Bus + context::Interrupt + context::Timing {}
impl<T: context::Bus + context::Interrupt + context::Timing> Context for T {}

const RESET_VECTOR: u16 = 0xFFFC;

const INTERNAL_CYCLE: u64 = 6;

#[derive(Serialize, Deserialize)]
pub struct Cpu {
    pub regs: Registers,
    pub stop: bool,
    pub halt: bool,
    prev_i: bool,
    prev_counter: u64,
}

impl Default for Cpu {
    fn default() -> Self {
        Self {
            regs: Registers::default(),
            stop: false,
            halt: false,
            prev_i: true,
            prev_counter: 0,
        }
    }
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

#[derive(Serialize, Deserialize)]
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
#[derive(Clone, Copy, Serialize, Deserialize)]
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

    fn set_p(&mut self, val: u8) {
        self.p = val.into();

        if self.e {
            self.p.set_m(true);
            self.p.set_x(true);
        }

        if self.p.x() {
            self.x &= 0xFF;
            self.y &= 0xFF;
        }
    }

    fn set_e(&mut self, val: bool) {
        self.e = val;
        if self.e {
            self.p.set_x(true);
            self.p.set_m(true);
            self.x &= 0xFF;
            self.y &= 0xFF;
            self.s = 0x100 | self.s & 0xFF;
        }
    }

    #[inline]
    fn set_nz(&mut self, val: impl Value) {
        self.p.set_z(val.zero());
        self.p.set_n(val.msb());
    }

    #[inline]
    fn set_a(&mut self, val: impl Value) {
        self.a = val.set(self.a);
    }

    #[inline]
    fn set_x(&mut self, val: impl Value) {
        self.x = val.get();
    }

    #[inline]
    fn set_y(&mut self, val: impl Value) {
        self.y = val.get();
    }

    // #[inline]
    // fn set_s(&mut self, val: impl Value) {
    //     self.s = val.get();
    // }

    #[inline]
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

trait Addr<Offset: From<u8>>: Sized {
    fn unwrap(&self) -> u32;
    fn offset(&self, offset: Offset) -> Self;

    fn read8(&self, ctx: &mut impl Context) -> u8 {
        ctx.read(self.unwrap())
    }
    fn read16(&self, ctx: &mut impl Context) -> u16 {
        let b0 = self.read8(ctx);
        let b1 = self.offset(1.into()).read8(ctx);
        (b0 as u16) | (b1 as u16) << 8
    }
    fn read24(&self, ctx: &mut impl Context) -> u32 {
        let b0 = self.read8(ctx);
        let b1 = self.offset(1.into()).read8(ctx);
        let b2 = self.offset(2.into()).read8(ctx);
        (b0 as u32) | (b1 as u32) << 8 | (b2 as u32) << 16
    }

    fn write8(&self, ctx: &mut impl Context, data: u8) {
        ctx.write(self.unwrap(), data);
    }
    fn write16(&self, ctx: &mut impl Context, data: u16) {
        self.write8(ctx, data as u8);
        self.offset(1.into()).write8(ctx, (data >> 8) as u8);
    }
}

struct Wrap8Addr(u32);

impl Addr<u8> for Wrap8Addr {
    fn unwrap(&self) -> u32 {
        self.0
    }
    fn offset(&self, offset: u8) -> Self {
        Self(self.0 & 0xFFFF00 | (self.0 as u8).wrapping_add(offset) as u32)
    }
}

struct Wrap16Addr(u32);

impl Addr<u16> for Wrap16Addr {
    fn unwrap(&self) -> u32 {
        self.0
    }
    fn offset(&self, offset: u16) -> Self {
        Self(self.0 & 0xFF0000 | (self.0 as u16).wrapping_add(offset) as u32)
    }
}

struct Wrap24Addr(u32);

impl Addr<u32> for Wrap24Addr {
    fn unwrap(&self) -> u32 {
        self.0
    }
    fn offset(&self, offset: u32) -> Self {
        Self((self.0 + offset) & 0xFFFFFF)
    }
}

impl Cpu {
    pub fn reset(&mut self, ctx: &mut impl Context) {
        self.regs = Registers::default();
        ctx.elapse(170); // FIXME: startup cycle?
        self.regs.pc = Wrap16Addr(RESET_VECTOR as u32).read16(ctx);
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
        if !self.regs.e {
            self.regs.s = self.regs.s.wrapping_sub(1);
        } else {
            self.regs.s = self.regs.s & 0xFF00 | (self.regs.s as u8).wrapping_sub(1) as u16;
        }
    }
    fn push16(&mut self, ctx: &mut impl Context, data: u16) {
        self.push8(ctx, (data >> 8) as u8);
        self.push8(ctx, data as u8);
    }

    fn pop8(&mut self, ctx: &mut impl Context) -> u8 {
        if !self.regs.e {
            self.regs.s = self.regs.s.wrapping_add(1);
        } else {
            self.regs.s = self.regs.s & 0xFF00 | (self.regs.s as u8).wrapping_add(1) as u16;
        }
        ctx.read(self.regs.s as u32)
    }
    fn pop16(&mut self, ctx: &mut impl Context) -> u16 {
        let b0 = self.pop8(ctx);
        let b1 = self.pop8(ctx);
        (b0 as u16) | ((b1 as u16) << 8)
    }

    fn exception(&mut self, ctx: &mut impl Context, e: Exception) {
        debug!("Exception: {e:?}");

        self.halt = false;

        let mut p = self.regs.p;
        if self.regs.e {
            // set B (X) flag on BRK or COP
            p.set_x(matches!(e, Exception::Brk | Exception::Cop));
        }
        if !self.regs.e {
            self.push8(ctx, self.regs.pb);
        }
        self.push16(ctx, self.regs.pc);
        self.push8(ctx, p.into());

        self.regs.p.set_d(false);
        self.regs.p.set_i(true);
        self.prev_i = true;
        self.regs.pb = 0;
        self.regs.pc = Wrap16Addr(e.vector_addr(self.regs.e) as u32).read16(ctx);
    }

    fn check_invaliant(&self) -> bool {
        if self.regs.e {
            if !self.regs.p.x() {
                error!("X flag is not set in Emulation mode");
                return false;
            }
            if !self.regs.p.m() {
                error!("M flag is not set in Emulation mode");
                return false;
            }
            if self.regs.s & 0xFF00 != 0x0100 {
                error!("S register's msb is not 0x0100 in Emulation mode");
                return false;
            }
        }
        if self.regs.p.x() {
            if self.regs.x & 0xFF00 != 0 {
                error!("X register's msb is not zero when M flag is set");
                return false;
            }
            if self.regs.y & 0xFF00 != 0 {
                error!("Y register's msb is not zero when M flag is set");
                return false;
            }
        }
        true
    }

    pub fn exec_one(&mut self, ctx: &mut impl Context) {
        let prev_i = self.prev_i;
        self.prev_i = self.regs.p.i();

        self.exec_one_(ctx, prev_i);

        self.prev_counter = ctx.now();
    }

    pub fn exec_one_(&mut self, ctx: &mut impl Context, prev_i: bool) {
        // FIXME: delete this when stable
        if !self.check_invaliant() {
            self.trace(ctx);
            panic!("Consistency check failed at cycle = {}", ctx.now());
        }

        if self.prev_counter < ctx.now() {
            return;
        }

        if self.stop {
            ctx.elapse(INTERNAL_CYCLE);
            return;
        }

        if ctx.interrupt_mut().nmi() {
            let _ = ctx.read(self.regs.pc24());
            ctx.elapse(INTERNAL_CYCLE);
            self.exception(ctx, Exception::Nmi);
            return;
        }

        if !prev_i && ctx.interrupt().irq() {
            let _ = ctx.read(self.regs.pc24());
            ctx.elapse(INTERNAL_CYCLE);
            self.exception(ctx, Exception::Irq);
            return;
        }

        if self.halt {
            if ctx.interrupt().irq() {
                self.halt = false;
            } else {
                ctx.elapse(INTERNAL_CYCLE);
                return;
            }
        }

        let opcode = self.fetch8(ctx);

        macro_rules! exec_instr {
            (mov s, $src:ident) => {{
                if !self.regs.e {
                    self.regs.s = reg!($src)
                } else {
                    self.regs.s = self.regs.s & 0xFF00 | reg!($src) & 0xFF;
                }
                ctx.elapse(INTERNAL_CYCLE);
            }};
            (mov a, d) => {{
                self.regs.a = self.regs.d;
                self.regs.set_nz(self.regs.a);
                ctx.elapse(INTERNAL_CYCLE);
            }};
            (mov a, s) => {{
                self.regs.a = self.regs.s;
                self.regs.set_nz(self.regs.a);
                ctx.elapse(INTERNAL_CYCLE);
            }};
            (mov $dst:ident, $src:ident) => {{
                if is_8bit!(self, $dst) {
                    let v = reg!($src) as u8;
                    self.regs.set_nz(v);
                    set_reg!($dst, v)
                } else {
                    let v = reg!($src);
                    self.regs.set_nz(v);
                    set_reg!($dst, v)
                }
                ctx.elapse(INTERNAL_CYCLE);
            }};

            // ld
            (ld $reg:ident, $addrmode:ident) => {
                if is_8bit!(self, $reg) {
                    let v = rd!($addrmode, u8);
                    self.regs.set_nz(v);
                    set_reg!($reg, v)
                } else {
                    let v = rd!($addrmode, u16);
                    self.regs.set_nz(v);
                    set_reg!($reg, v)
                }
            };

            // st
            (st $addrmode:ident, zr) => {{
                if is_8bit!(self, mem) {
                    wr!($addrmode, 0, u8)
                } else {
                    wr!($addrmode, 0, u16)
                }
            }};
            (st $addrmode:ident, $reg:ident) => {
                if is_8bit!(self, $reg) {
                    wr!($addrmode, reg!($reg) as u8, u8)
                } else {
                    wr!($addrmode, reg!($reg), u16)
                }
            };

            // push
            (push p) => {{
                ctx.elapse(INTERNAL_CYCLE);
                self.push8(ctx, self.regs.p.into())
            }};
            (push pb) => {{
                ctx.elapse(INTERNAL_CYCLE);
                self.push8(ctx, self.regs.pb)
            }};
            (push db) => {{
                ctx.elapse(INTERNAL_CYCLE);
                self.push8(ctx, self.regs.db)
            }};
            (push ind) => {{
                let addr = addr!(ind, true).unwrap() as u16;
                self.push16(ctx, addr)
            }};
            (push abs) => {{
                let addr = addr!(abs, true).unwrap() as u16;
                self.push16(ctx, addr)
            }};
            (push rel) => {{
                let disp = self.fetch16(ctx);
                ctx.elapse(INTERNAL_CYCLE);
                self.push16(ctx, self.regs.pc.wrapping_add(disp));
            }};
            (push $reg:ident) => {{
                ctx.elapse(INTERNAL_CYCLE);
                if is_8bit!(self, $reg) {
                    self.push8(ctx, reg!($reg) as u8)
                } else {
                    self.push16(ctx, reg!($reg))
                }
            }};

            // pop
            (pop p) => {{
                ctx.elapse(INTERNAL_CYCLE * 2);
                let p = self.pop8(ctx);
                self.regs.set_p(p);
            }};
            (pop db) => {{
                ctx.elapse(INTERNAL_CYCLE * 2);
                let v = self.pop8(ctx);
                self.regs.set_nz(v);
                self.regs.db = v;
            }};
            (pop $reg:ident) => {{
                ctx.elapse(INTERNAL_CYCLE * 2);
                if is_8bit!(self, $reg) {
                    let v = self.pop8(ctx);
                    self.regs.set_nz(v);
                    set_reg!($reg, v);
                } else {
                    let v = self.pop16(ctx);
                    set_reg!($reg, v);
                    self.regs.set_nz(v);
                }
            }};

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
                    let b = rd!($opr, u8);
                    let a = self.regs.a as u8;
                    self.regs.p.set_z(a & b == 0);
                    set_nv!($opr, b, u8);
                } else {
                    let b = rd!($opr, u16);
                    let a = self.regs.a;
                    self.regs.p.set_z(a & b == 0);
                    set_nv!($opr, b, u16);
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
                ctx.elapse(INTERNAL_CYCLE);
                if self.regs.e && (self.regs.pc & 0xFF) + (disp as u16 & 0xFF) > 0xFF {
                    ctx.elapse(INTERNAL_CYCLE);
                }
                self.regs.pc = self.regs.pc.wrapping_add(disp as u16);
            }};
            (jmp disp16) => {{
                let disp = self.fetch16(ctx);
                ctx.elapse(INTERNAL_CYCLE);
                self.regs.pc = self.regs.pc.wrapping_add(disp);
            }};
            (jmp abs) => {{
                let addr = self.fetch16(ctx);
                self.regs.pc = addr;
            }};
            (jmp abi) => {{
                let addr = Wrap16Addr(self.fetch16(ctx) as u32);
                // FIXME: I don't know this instruction wraps page boundary or not so testing it
                assert!(addr.0 & 0xFF != 0xFF);
                self.regs.pc = addr.read16(ctx);
            }};
            (jmp $addr:ident) => {{
                let addr = addr!($addr, false).unwrap();
                self.regs.pc = addr as u16;
                self.regs.pb = (addr >> 16) as u8;
            }};

            (jsr far) => {{
                let pc = self.fetch16(ctx);
                ctx.elapse(INTERNAL_CYCLE);
                // Before reading PB, JSL pushes PB to stack.
                // This affects the behavior of open-bus.
                self.push8(ctx, self.regs.pb);
                let pb = self.fetch8(ctx);
                self.push16(ctx, self.regs.pc.wrapping_sub(1));
                self.regs.pb = pb;
                self.regs.pc = pc;
            }};
            (jsr abs) => {{
                let addr = addr!(abs, false).unwrap();
                ctx.elapse(INTERNAL_CYCLE);
                self.push16(ctx, self.regs.pc.wrapping_sub(1));
                self.regs.pc = addr as u16;
            }};
            (jsr aix) => {{
                // FIXME: JSR (abs, X) pushes the old address (low byte second)
                // before reading the high byte of the new address.
                let addr = addr!(aix, false).unwrap();
                self.push16(ctx, self.regs.pc.wrapping_sub(1));
                self.regs.pc = addr as u16;
            }};

            (rti) => {{
                ctx.elapse(INTERNAL_CYCLE * 2);
                let p = self.pop8(ctx);
                self.regs.set_p(p);
                self.prev_i = self.regs.p.i();
                self.regs.pc = self.pop16(ctx);
                if !self.regs.e {
                    self.regs.pb = self.pop8(ctx);
                }
            }};
            (rtl) => {{
                ctx.elapse(INTERNAL_CYCLE * 2);
                self.regs.pc = self.pop16(ctx).wrapping_add(1);
                self.regs.pb = self.pop8(ctx);
            }};
            (rts) => {{
                ctx.elapse(INTERNAL_CYCLE * 2);
                self.regs.pc = self.pop16(ctx).wrapping_add(1);
                ctx.elapse(INTERNAL_CYCLE);
            }};

            // cond branch
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
            (brk) => {{
                let _dmy = self.fetch8(ctx);
                // FIXME: Is this correct?
                if self.regs.e {
                    self.regs.db = 0;
                }
                // Detect such code to verify
                assert!(
                    !self.regs.e,
                    "BRK in emulation mode: PC: {:02X}:{:04X}, cycle: {}",
                    self.regs.pb,
                    self.regs.pc,
                    ctx.now()
                );
                self.exception(ctx, Exception::Brk)
            }};
            (cop) => {{
                let _dmy = self.fetch8(ctx);
                // FIXME: Is this correct?
                if self.regs.e {
                    self.regs.db = 0;
                }
                // Detect such code to verify
                assert!(!self.regs.e, "COP in emulation mode");
                self.exception(ctx, Exception::Cop)
            }};

            // flags
            (clc) => {{
                ctx.elapse(INTERNAL_CYCLE);
                self.regs.p.set_c(false)
            }};
            (cli) => {{
                ctx.elapse(INTERNAL_CYCLE);
                self.regs.p.set_i(false)
            }};
            (cld) => {{
                ctx.elapse(INTERNAL_CYCLE);
                self.regs.p.set_d(false)
            }};
            (clv) => {{
                ctx.elapse(INTERNAL_CYCLE);
                self.regs.p.set_v(false)
            }};
            (sec) => {{
                ctx.elapse(INTERNAL_CYCLE);
                self.regs.p.set_c(true)
            }};
            (sei) => {{
                ctx.elapse(INTERNAL_CYCLE);
                self.regs.p.set_i(true)
            }};
            (sed) => {{
                ctx.elapse(INTERNAL_CYCLE);
                self.regs.p.set_d(true)
            }};
            (rep imm) => {{
                let v = self.fetch8(ctx);
                ctx.elapse(INTERNAL_CYCLE);
                let p = u8::from(self.regs.p) & !v;
                self.regs.set_p(p);
            }};
            (sep imm) => {{
                let v = self.fetch8(ctx);
                ctx.elapse(INTERNAL_CYCLE);
                let p = u8::from(self.regs.p) | v;
                self.regs.set_p(p);
            }};
            (xce) => {{
                ctx.elapse(INTERNAL_CYCLE);
                let c = self.regs.p.c();
                self.regs.p.set_c(self.regs.e);
                self.regs.set_e(c);
            }};

            // special
            (stp) => {{
                ctx.elapse(INTERNAL_CYCLE * 2);
                self.stop = true
            }};
            (xba) => {{
                ctx.elapse(INTERNAL_CYCLE * 2);
                self.regs.a = self.regs.a.rotate_right(8);
                self.regs.set_nz(self.regs.a as u8);
            }};
            (wai) => {{
                ctx.elapse(INTERNAL_CYCLE * 2);
                self.halt = true
            }};
            (wdm imm) => {{
                // This is 2 byte nop opcode
                let _dmy = self.fetch8(ctx);
            }};
            (nop) => {{
                ctx.elapse(INTERNAL_CYCLE);
            }};
        }

        macro_rules! block_trans {
            ($inc:expr) => {{
                let db = self.fetch8(ctx);
                let sb = self.fetch8(ctx);
                self.regs.db = db;

                let v = ctx.read((sb as u32) << 16 | self.regs.x as u32);
                ctx.write((db as u32) << 16 | self.regs.y as u32, v);

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

                ctx.elapse(INTERNAL_CYCLE * 2);
            }};
        }

        macro_rules! alu {
            ($op:ident, $reg:ident, $opr:ident) => {{
                if is_8bit!(self, $reg) {
                    let a = reg!($reg) as u8;
                    let b = rd!($opr, u8);
                    let c = alu_op!($op, a, b, u8);
                    self.regs.set_nz(c);
                    set_reg!($reg, c);
                } else {
                    let a = reg!($reg);
                    let b = rd!($opr, u16);
                    let c = alu_op!($op, a, b, u16);
                    self.regs.set_nz(c);
                    set_reg!($reg, c);
                }
            }};
        }

        macro_rules! cmp {
            ($reg:ident, $opr:ident) => {{
                if is_8bit!(self, $reg) {
                    let a = reg!($reg) as u8;
                    let b = rd!($opr, u8);
                    let (c, carry) = a.overflowing_sub(b);
                    self.regs.set_nz(c);
                    self.regs.p.set_c(!carry);
                } else {
                    let a = reg!($reg);
                    let b = rd!($opr, u16);
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

                    let mut borrow = borrow as i32;
                    let mut c0 = (a & 0xF) - (b & 0xF) - borrow;
                    borrow = 0;
                    if c0 < 0 {
                        c0 = (c0 - 6) & 0xF;
                        borrow = 0x10;
                    }
                    let mut c1 = (a & 0xF0) - (b & 0xF0) - borrow;

                    macro_rules! calc {
                        (u8) => {{
                            if c1 < 0 {
                                c1 = (c1 - 0x60) & 0xF0;
                            }
                            c1 | c0
                        }};
                        (u16) => {{
                            borrow = 0;
                            if c1 < 0 {
                                c1 = (c1 - 0x60) & 0xF0;
                                borrow = 0x100;
                            }
                            let mut c2 = (a & 0xF00) - (b & 0xF00) - borrow;
                            borrow = 0;
                            if c2 < 0 {
                                c2 = (c2 - 0x600) & 0xF00;
                                borrow = 0x1000;
                            }
                            let mut c3 = (a & 0xF000) - (b & 0xF000) - borrow;
                            if c3 < 0 {
                                c3 = (c3 - 0x6000) & 0xF000;
                            }
                            c3 | c2 | c1 | c0
                        }};
                    }

                    let c = calc!($ty);

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
                let addr = addr!($addrmode, true);
                if is_8bit!(self, mem) {
                    let v = addr.read8(ctx);
                    ctx.elapse(INTERNAL_CYCLE);
                    addr.write8(ctx, modop!($op, v, u8));
                } else {
                    let v = addr.read16(ctx);
                    ctx.elapse(INTERNAL_CYCLE);
                    addr.write16(ctx, modop!($op, v, u16));
                }
            }};
        }

        macro_rules! rmw_reg {
            ($op:ident, $reg:ident) => {{
                ctx.elapse(INTERNAL_CYCLE);
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
                    let prev_pc = self.regs.pc;
                    self.regs.pc = self.regs.pc.wrapping_add(disp);
                    if self.regs.e && self.regs.pc & 0xFF00 != prev_pc & 0xFF00 {
                        ctx.elapse(INTERNAL_CYCLE);
                    }
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

        macro_rules! rd {
            (imm, u8) => {
                self.fetch8(ctx)
            };
            (imm, u16) => {
                self.fetch16(ctx)
            };
            ($addrmode:ident, u8) => {
                addr!($addrmode, false).read8(ctx)
            };
            ($addrmode:ident, u16) => {
                addr!($addrmode, false).read16(ctx)
            };
        }

        macro_rules! wr {
            ($addrmode:ident, $data:expr, u8) => {
                addr!($addrmode, true).write8(ctx, $data)
            };
            ($addrmode:ident, $data:expr, u16) => {
                addr!($addrmode, true).write16(ctx, $data)
            };
        }

        macro_rules! addr {
            (abs, $_:literal) => {{
                let offset = self.fetch16(ctx);
                Wrap24Addr(((self.regs.db as u32) << 16) | offset as u32)
            }};
            (abx, $wr:literal) => {{
                let addr = addr!(abs, $wr);
                if $wr || !self.regs.p.x() || (self.regs.x as u32 & 0xFF) + (addr.0 & 0xFF) > 0xFF {
                    ctx.elapse(INTERNAL_CYCLE);
                }
                addr.offset(self.regs.x as u32)
            }};
            (aby, $wr:literal) => {{
                let addr = addr!(abs, $wr);
                if $wr || !self.regs.p.x() || (self.regs.y as u32 & 0xFF) + (addr.0 & 0xFF) > 0xFF {
                    ctx.elapse(INTERNAL_CYCLE);
                }
                addr.offset(self.regs.y as u32)
            }};

            (aif, $_:literal) => {{
                let addr = Wrap16Addr(self.fetch16(ctx) as u32);
                Wrap16Addr(addr.read24(ctx))
            }};
            (aix, $_:literal) => {{
                let offset = self.fetch16(ctx).wrapping_add(self.regs.x);
                ctx.elapse(INTERNAL_CYCLE);
                let addr = Wrap16Addr((self.regs.pb as u32) << 16 | offset as u32);
                Wrap16Addr((self.regs.pb as u32) << 16 | addr.read16(ctx) as u32)
            }};

            (zp, $_:literal) => {{
                let offset = self.fetch8(ctx);
                if self.regs.e && self.regs.d & 0xFF == 0 {
                    Wrap16Addr(self.regs.d as u32 | offset as u32)
                } else {
                    if self.regs.d & 0xFF != 0 {
                        ctx.elapse(INTERNAL_CYCLE);
                    }
                    Wrap16Addr(self.regs.d as u32).offset(offset as u16)
                }
            }};
            (zpx, $_:literal) => {{
                let offset = self.fetch8(ctx);
                ctx.elapse(INTERNAL_CYCLE);
                if self.regs.d & 0xFF != 0 {
                    ctx.elapse(INTERNAL_CYCLE);
                }
                if self.regs.e {
                    Wrap16Addr(
                        Wrap8Addr(self.regs.d as u32)
                            .offset(offset)
                            .offset(self.regs.x as u8)
                            .0,
                    )
                } else {
                    Wrap16Addr(self.regs.d as u32)
                        .offset(offset as u16)
                        .offset(self.regs.x)
                }
            }};
            (zpy, $_:literal) => {{
                let offset = self.fetch8(ctx);
                ctx.elapse(INTERNAL_CYCLE);
                if self.regs.d & 0xFF != 0 {
                    ctx.elapse(INTERNAL_CYCLE);
                }
                if self.regs.e {
                    Wrap16Addr(
                        Wrap8Addr(self.regs.d as u32)
                            .offset(offset)
                            .offset(self.regs.y as u8)
                            .0,
                    )
                } else {
                    Wrap16Addr(self.regs.d as u32)
                        .offset(offset as u16)
                        .offset(self.regs.y)
                }
            }};

            (ind, $_:literal) => {{
                let addr = addr!(zp, false).read16(ctx);
                Wrap24Addr((self.regs.db as u32) << 16 | addr as u32)
            }};
            (inf, $_:literal) => {
                Wrap24Addr(addr!(zp, false).read24(ctx))
            };
            (inx, $_:literal) => {{
                let offset = self.fetch8(ctx);
                ctx.elapse(INTERNAL_CYCLE);
                let addr = if self.regs.e {
                    if self.regs.d & 0xFF != 0 {
                        ctx.elapse(INTERNAL_CYCLE);
                    }
                    // Wraps 8 bit to add offset and X,
                    // but does not wrap 8 bit to read indirect address
                    Wrap16Addr(
                        Wrap8Addr(self.regs.d as u32)
                            .offset(offset)
                            .offset(self.regs.x as u8)
                            .unwrap(),
                    )
                    .read16(ctx)
                } else {
                    if self.regs.d & 0xFF != 0 {
                        ctx.elapse(INTERNAL_CYCLE);
                    }
                    Wrap16Addr(self.regs.d as u32)
                        .offset(offset as u16)
                        .offset(self.regs.x)
                        .read16(ctx)
                };
                Wrap24Addr((self.regs.db as u32) << 16 | addr as u32)
            }};
            (iny, $wr:literal) => {{
                let offset = self.fetch8(ctx);
                if self.regs.d & 0xFF != 0 {
                    ctx.elapse(INTERNAL_CYCLE);
                }
                let addr = if self.regs.e && self.regs.d & 0xFF == 0 {
                    Wrap8Addr(self.regs.d as u32 | offset as u32).read16(ctx)
                } else {
                    Wrap16Addr(self.regs.d as u32)
                        .offset(offset as u16)
                        .read16(ctx)
                };
                if $wr || !self.regs.p.x() || (addr & 0xFF) + (self.regs.y & 0xFF) > 0xFF {
                    ctx.elapse(INTERNAL_CYCLE);
                }
                Wrap24Addr((self.regs.db as u32) << 16 | addr as u32).offset(self.regs.y as u32)
            }};
            (ify, $_:literal) => {{
                let offset = self.fetch8(ctx);
                if self.regs.d & 0xFF != 0 {
                    ctx.elapse(INTERNAL_CYCLE);
                }
                let addr = if self.regs.e && self.regs.d & 0xFF == 0 {
                    Wrap8Addr(self.regs.d as u32 | offset as u32).read24(ctx)
                } else {
                    Wrap16Addr(self.regs.d as u32)
                        .offset(offset as u16)
                        .read24(ctx)
                };
                Wrap24Addr(addr).offset(self.regs.y as u32)
            }};

            (far, $_:literal) => {
                Wrap24Addr(self.fetch24(ctx))
            };
            (fax, $_:literal) => {
                Wrap24Addr(self.fetch24(ctx)).offset(self.regs.x as u32)
            };

            (sp, $_:literal) => {{
                let offset = self.fetch8(ctx) as u16;
                ctx.elapse(INTERNAL_CYCLE);
                Wrap16Addr(self.regs.s.wrapping_add(offset) as u32)
            }};
            (siy, $_:literal) => {{
                let addr = addr!(sp, false).read16(ctx);
                ctx.elapse(INTERNAL_CYCLE);
                Wrap24Addr((self.regs.db as u32) << 16 | addr as u32).offset(self.regs.y as u32)
            }};
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
            "{:02X}:{:04X}  {asm:32} A:{:04X} X:{:04X} Y:{:04X} S:{:04X} D:{:04X} DB:{:02X} PB:{:02X} P:{}{}{}{}{}{}{}{}{} @{frame}:{y:03}:{x:03}",
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
            frame = ctx.counter().frame,
            x = ctx.counter().x,
            y = ctx.counter().y,
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
