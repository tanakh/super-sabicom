#![allow(unused_braces)]

use std::fmt::Display;

use log::{debug, trace, warn, Level};
use meru_interface::AudioBuffer;
use modular_bitfield::prelude::*;
use super_sabicom_macro::opcodes;

use crate::context;

pub trait Context: context::Timing {}
impl<T: context::Timing> Context for T {}

pub struct Spc {
    regs: Registers,
    ioregs: IORegisters,
    ram: Vec<u8>,
    counter: u64,

    audio_buffer: AudioBuffer,
}

impl Default for Spc {
    fn default() -> Self {
        Self {
            regs: Registers::default(),
            ioregs: IORegisters::default(),
            ram: vec![0; 0x10000],
            counter: 0,
            audio_buffer: AudioBuffer::default(),
        }
    }
}

struct Registers {
    a: u8,
    x: u8,
    y: u8,
    sp: u8,
    psw: Flags,
    pc: u16,
}

#[bitfield]
#[repr(u8)]
#[derive(Default, Clone, Copy)]
struct Flags {
    c: bool, // 0: Borrow or no-carry, 1: Carry or no-borrow
    z: bool,
    i: bool,
    h: bool,
    b: bool, // 0: Reset, 1: BRK opcode
    p: bool, // Zero page location: 0: 0x00xx, 1: 0x01xx
    v: bool,
    n: bool,
}

impl Default for Registers {
    fn default() -> Self {
        Self {
            a: 0,
            x: 0,
            y: 0,
            sp: 0,
            psw: Flags::default(),
            pc: u16::from_le_bytes(BOOT_ROM[0xFFFE - 0xFFC0..].try_into().unwrap()),
        }
    }
}

impl Registers {
    fn set_nz(&mut self, v: u8) {
        self.psw.set_z(v == 0);
        self.psw.set_n(v & 0x80 != 0);
    }

    fn set_nz16(&mut self, v: u16) {
        self.psw.set_z(v == 0);
        self.psw.set_n(v & 0x8000 != 0);
    }

    fn ya(&self) -> u16 {
        (self.y as u16) << 8 | self.a as u16
    }

    fn set_ya(&mut self, data: u16) {
        self.y = (data >> 8) as u8;
        self.a = data as u8;
    }
}

struct IORegisters {
    timer_enable: bool,
    ram_write_enable: bool,
    crash_spc700: bool, // ???
    ram_wait_cycle: u64,
    io_wait_cycle: u64,
    rom_enable: bool,
    dsp_reg: u8,
    cpuin: [u8; 4],
    cpuout: [u8; 4],
    timer: [Timer; 3],
    counter01: u64,
    counter2: u64,
}

#[derive(Default, Debug)]
struct Timer {
    enable: bool,
    divider: u8,
    counter: u8,
    output: u8,
}

impl Default for IORegisters {
    fn default() -> Self {
        Self {
            timer_enable: true,
            ram_write_enable: true,
            crash_spc700: false,
            ram_wait_cycle: 1,
            io_wait_cycle: 1,
            rom_enable: true,
            dsp_reg: 0,
            cpuin: [0; 4],
            cpuout: [0; 4],
            timer: Default::default(),
            counter01: 0,
            counter2: 0,
        }
    }
}

impl Spc {
    pub fn audio_buffer(&self) -> &AudioBuffer {
        &self.audio_buffer
    }

    pub fn read_port(&self, port: u8) -> u8 {
        self.ioregs.cpuout[port as usize]
    }

    pub fn write_port(&mut self, port: u8, data: u8) {
        self.ioregs.cpuin[port as usize] = data;
    }

    pub fn tick(&mut self, ctx: &mut impl Context) {
        let world = ctx.now() * 10240 / 214772;
        let start = self.counter;

        while self.counter < world {
            self.exec_one();
        }

        self.tick_timer(self.counter - start);
    }

    fn tick_timer(&mut self, elapsed: u64) {
        self.ioregs.counter01 += elapsed;
        self.ioregs.counter2 += elapsed;

        // Timer 0/1 is clocked 8000Hz = 1.024Mhz / 128
        while self.ioregs.counter01 >= 128 {
            self.ioregs.counter01 -= 128;
            for i in 0..2 {
                self.ioregs.timer[i].tick();
            }
        }
        // Timer 2 is clocked 64000Hz = 1.024Mhz / 16
        while self.ioregs.counter2 >= 16 {
            self.ioregs.counter2 -= 16;
            self.ioregs.timer[2].tick();
        }
    }
}

impl Timer {
    fn tick(&mut self) {
        if !self.enable {
            return;
        }

        self.counter = self.counter.wrapping_add(1);
        if self.counter == self.divider {
            self.counter = 0;
            self.output = (self.output + 1) & 0xF;
        }
    }
}

opcodes! {
    match_opcode,
    // 0/8       1/9           2/A            3/B             4/C         5/D         6/E             7/F
    nop        ; tcall 0     ; set1 zp, 0   ; bbs zp.0, dest; or a, zp  ; or a, abs ; or a, (x)     ; or a, zxi    ; // 00
    or a, imm  ; or zp, zp   ; or1 c, aaab  ; asl zp        ; asl abs   ; push psw  ; tset abs, a   ; brk          ; // 08
    bpl dest   ; tcall 1     ; clr1 zp, 0   ; bbc zp.0, dest; or a, zpx ; or a, abx ; or a, aby     ; or a, ziy    ; // 10
    or zp, imm ; or (x), (y) ; decw zp      ; asl zpx       ; asl a     ; dec x     ; cmp x, abs    ; jmp axi      ; // 18
    clr p      ; tcall 2     ; set1 zp, 1   ; bbs zp.1, dest; and a, zp ; and a, abs; and a, (x)    ; and a, zxi   ; // 20
    and a, imm ; and zp, zp  ; or1 c, !aaab ; rol zp        ; rol abs   ; push a    ; cbne zp, dest ; bra dest     ; // 28
    bmi dest   ; tcall 3     ; clr1 zp, 1   ; bbc zp.1, dest; and a, zpx; and a, abx; and a, aby    ; and a, ziy   ; // 30
    and zp, imm; and (x), (y); incw zp      ; rol zpx       ; rol a     ; inc x     ; cmp x, zp     ; call abs     ; // 38
    set p      ; tcall 4     ; set1 zp, 2   ; bbs zp.2, dest; eor a, zp ; eor a, abs; eor a, (x)    ; eor a, zxi   ; // 40
    eor a, imm ; eor zp, zp  ; and1 c, aaab ; lsr zp        ; lsr abs   ; push x    ; tclr abs, a   ; pcall imm    ; // 48
    bvc dest   ; tcall 5     ; clr1 zp, 2   ; bbc zp.2, dest; eor a, zpx; eor a, abx; eor a, aby    ; eor a, ziy   ; // 50
    eor zp, imm; eor (x), (y); cmpw ya, zp  ; lsr zpx       ; lsr a     ; mov x, a  ; cmp y, abs    ; jmp abs      ; // 58
    clr c      ; tcall 6     ; set1 zp, 3   ; bbs zp.3, dest; cmp a, zp ; cmp a, abs; cmp a, (x)    ; cmp a, zxi   ; // 60
    cmp a, imm ; cmp zp, zp  ; and1 c, !aaab; ror zp        ; ror abs   ; push y    ; dbnz zp, dest ; ret          ; // 68
    bvs dest   ; tcall 7     ; clr1 zp, 3   ; bbc zp.3, dest; cmp a, zpx; cmp a, abx; cmp a, aby    ; cmp a, ziy   ; // 70
    cmp zp, imm; cmp (x), (y); addw ya, zp  ; ror zpx       ; ror a     ; mov a, x  ; cmp y, zp     ; reti         ; // 78
    set c      ; tcall 8     ; set1 zp, 4   ; bbs zp.4, dest; adc a, zp ; adc a, abs; adc a, (x)    ; adc a, zxi   ; // 80
    adc a, imm ; adc zp, zp  ; eor1 c, aaab ; dec zp        ; dec abs   ; mov y, imm; pop psw       ; mov zp, imm  ; // 88
    bcc dest   ; tcall 9     ; clr1 zp, 4   ; bbc zp.4, dest; adc a, zpx; adc a, abx; adc a, aby    ; adc a, ziy   ; // 90
    adc zp, imm; adc (x), (y); subw ya, zp  ; dec zpx       ; dec a     ; mov x, sp ; div ya, x     ; xcn a        ; // 98
    ei         ; tcall 10    ; set1 zp, 5   ; bbs zp.5, dest; sbc a, zp ; sbc a, abs; sbc a, (x)    ; sbc a, zxi   ; // A0
    sbc a, imm ; sbc zp, zp  ; mov1 c, aaab ; inc zp        ; inc abs   ; cmp y, imm; pop a         ; mov (x+1), a ; // A8
    bcs dest   ; tcall 11    ; clr1 zp, 5   ; bbc zp.5, dest; sbc a, zpx; sbc a, abx; sbc a, aby    ; sbc a, ziy   ; // B0
    sbc zp, imm; sbc (x), (y); movw ya, zp  ; inc zpx       ; inc a     ; mov sp, x ; das a         ; mov  a, (x+1); // B8
    di         ; tcall 12    ; set1 zp, 6   ; bbs zp.6, dest; mov zp, a ; mov abs, a; mov (x), a    ; mov zxi, a   ; // C0
    cmp x, imm ; mov abs, x  ; mov1 aaab, c ; mov zp, y     ; mov abs, y; mov x, imm; pop x         ; mul ya       ; // C8
    bne dest   ; tcall 13    ; clr1 zp, 6   ; bbc zp.6, dest; mov zpx, a; mov abx, a; mov aby, a    ; mov ziy, a   ; // D0
    mov zp, x  ; mov zpy, x  ; movw zp, ya  ; mov zpx, y    ; dec y     ; mov a, y  ; cbne zpx, dest; daa a        ; // D8
    clr v      ; tcall 14    ; set1 zp, 7   ; bbs zp.7, dest; mov a, zp ; mov a, abs; mov a, (x)    ; mov a, zxi   ; // E0
    mov a, imm ; mov x, abs  ; not1 aaab    ; mov y, zp     ; mov y, abs; not c     ; pop y         ; sleep        ; // E8
    beq dest   ; tcall 15    ; clr1 zp, 7   ; bbc zp.7, dest; mov a, zpx; mov a, abx; mov a, aby    ; mov a, ziy   ; // F0
    mov x, zp  ; mov x, zpy  ; mov zp, zp   ; mov y, zpx    ; inc y     ; mov y, a  ; dbnz y, dest  ; stop         ; // F8
}

impl Spc {
    fn read8(&mut self, addr: u16) -> u8 {
        let data = match addr {
            0x00F0..=0x00FF => {
                self.counter += self.ioregs.io_wait_cycle;
                self.io_read(addr & 0xF)
            }
            0xFFC0..=0xFFFF if self.ioregs.rom_enable => {
                self.counter += self.ioregs.io_wait_cycle;
                BOOT_ROM[(addr - 0xFFC0) as usize]
            }
            _ => {
                self.counter += self.ioregs.ram_wait_cycle;
                self.ram[addr as usize]
            }
        };
        trace!("Read:  {addr:#06X} = {data:#04X}");
        data
    }

    fn read8_pure(&self, addr: u16) -> Option<u8> {
        Some(match addr {
            0x00F0..=0x00FF => None?,
            0xFFC0..=0xFFFF if self.ioregs.rom_enable => BOOT_ROM[(addr - 0xFFC0) as usize],
            _ => self.ram[addr as usize],
        })
    }

    fn write8(&mut self, addr: u16, data: u8) {
        trace!("Write: {addr:#06X} = {data:#04X}");
        if self.ioregs.ram_write_enable {
            self.ram[addr as usize] = data;
        }
        if addr & 0xFFF0 == 0x00F0 {
            self.io_write(addr & 0xF, data);
            self.counter += self.ioregs.io_wait_cycle;
        } else {
            self.counter += self.ioregs.ram_wait_cycle;
        }
    }

    fn read16(&mut self, addr: u16) -> u16 {
        let lo = self.read8(addr);
        let hi = self.read8(addr.wrapping_add(1));
        (hi as u16) << 8 | lo as u16
    }

    fn write16(&mut self, addr: u16, data: u16) {
        self.write8(addr, data as u8);
        self.write8(addr.wrapping_add(1), (data >> 8) as u8);
    }

    fn fetch8(&mut self) -> u8 {
        let ret = self.read8(self.regs.pc);
        self.regs.pc = self.regs.pc.wrapping_add(1);
        ret
    }

    fn fetch16(&mut self) -> u16 {
        let lo = self.fetch8();
        let hi = self.fetch8();
        (hi as u16) << 8 | lo as u16
    }

    fn push8(&mut self, data: u8) {
        self.write8(self.regs.sp as u16 | 0x0100, data);
        self.regs.sp = self.regs.sp.wrapping_sub(1);
    }

    fn push16(&mut self, data: u16) {
        self.push8((data >> 8) as u8);
        self.push8(data as u8);
    }

    fn pop8(&mut self) -> u8 {
        self.regs.sp = self.regs.sp.wrapping_add(1);
        self.read8(self.regs.sp as u16 | 0x0100)
    }

    fn pop16(&mut self) -> u16 {
        let lo = self.pop8();
        let hi = self.pop8();
        (hi as u16) << 8 | lo as u16
    }

    fn io_read(&mut self, addr: u16) -> u8 {
        let data = match addr {
            0 => {
                trace!("Read TEST");
                0
            }
            1 => {
                trace!("Read CONTROL");
                0
            }
            2 => self.ioregs.dsp_reg,
            3 => {
                debug!("Read DSP reg: #{:#04X}", self.ioregs.dsp_reg);
                0
            }
            4..=7 => self.ioregs.cpuin[addr as usize - 4],
            8..=9 => {
                debug!("Read External I/O Port P{}", addr - 4);
                0
            }
            0xA..=0xC => {
                trace!("Read Timer {} Divider", addr - 0xA);
                0
            }
            0xD..=0xF => {
                let data = self.ioregs.timer[addr as usize - 0xD].output;
                self.ioregs.timer[addr as usize - 0xD].output = 0;
                data
            }
            _ => todo!("IO Read: {addr:X}"),
        };

        trace!("IO Read:  {addr:X} = {data:#04X}");
        data
    }

    fn io_write(&mut self, addr: u16, data: u8) {
        trace!("IO Write: {addr:X} = {data:#04X}");

        match addr {
            0 => {
                // FIXME: ???
                // 0: Timer-Enable (0: Normal, 1: Timer don't work)
                // 2: Crash SPC700 (0: Normal, 1: Crashes the CPU)
                // 3: Timer-Disable (0: Timer don't work, 1: Normal)

                const WS_TBL: [u64; 4] = [0, 1, 4, 9];

                self.ioregs.rom_enable = data & 2 != 0;
                self.ioregs.crash_spc700 = data & 4 != 0;
                self.ioregs.ram_wait_cycle = WS_TBL[(data as usize >> 4) & 3] + 1;
                self.ioregs.io_wait_cycle = WS_TBL[(data as usize >> 6) & 3] + 1;
            }
            1 => {
                for i in 0..3 {
                    self.ioregs.timer[i].enable = data & (1 << i) != 0;
                    if !self.ioregs.timer[i].enable {
                        self.ioregs.timer[i].counter = 0;
                        self.ioregs.timer[i].output = 0;
                    }
                }
                for i in 0..2 {
                    if data & (1 << (i + 4)) != 0 {
                        self.ioregs.cpuin[i * 2 + 0] = 0;
                        self.ioregs.cpuin[i * 2 + 1] = 0;
                    }
                }
                self.ioregs.rom_enable = data & 0x80 != 0;
            }
            2 => self.ioregs.dsp_reg = data,
            3 => {
                debug!(
                    "Write DSP reg: #{:#04X} = #{data:#04X}",
                    self.ioregs.dsp_reg
                );
            }
            4..=7 => {
                debug!("CPUIO{} = {data:#04X}", addr as usize - 4);
                self.ioregs.cpuout[addr as usize - 4] = data
            }
            8..=9 => debug!("External I/O Port P{} = {data:#04X}", addr - 4),
            0xA..=0xC => self.ioregs.timer[addr as usize - 0xA].divider = data,
            0xD..=0xF => {
                warn!("Write Timer {} Divider = {data:#04X}", addr - 0xD);
            }
            _ => todo!("IO Write: {addr:X} = {data:#04X}"),
        }
    }
}

impl Spc {
    fn exec_one(&mut self) {
        if log::log_enabled!(Level::Trace) {
            self.trace();
        }

        macro_rules! elapse {
            (ram, $n:expr) => {
                self.counter += self.ioregs.ram_wait_cycle * $n
            };
            (io, $n:expr) => {
                self.counter += self.ioregs.io_wait_cycle * $n
            };
        }

        macro_rules! addr {
            (imm) => {{
                let addr = self.regs.pc;
                self.regs.pc = self.regs.pc.wrapping_add(1);
                addr
            }};

            (zp) => {
                ((self.regs.psw.p() as u16) << 8) | self.fetch8() as u16
            };
            (zpx) => {{
                elapse!(io, 1);
                ((self.regs.psw.p() as u16) << 8) | self.fetch8().wrapping_add(self.regs.x) as u16
            }};
            (zpy) => {{
                elapse!(io, 1);
                ((self.regs.psw.p() as u16) << 8) | self.fetch8().wrapping_add(self.regs.y) as u16
            }};
            ((x)) => {{
                elapse!(ram, 1);
                ((self.regs.psw.p() as u16) << 8) | self.regs.x as u16
            }};
            ((x+1)) => {{
                elapse!(ram, 1);
                elapse!(io, 1);
                let addr = ((self.regs.psw.p() as u16) << 8) | self.regs.x as u16;
                self.regs.x = self.regs.x.wrapping_add(1);
                addr
            }};
            ((y)) => {{
                elapse!(ram, 1);
                ((self.regs.psw.p() as u16) << 8) | self.regs.y as u16
            }};
            (abs) => {
                self.fetch16()
            };
            (abx) => {{
                elapse!(io, 1);
                addr!(abs).wrapping_add(self.regs.x as u16)
            }};
            (aby) => {{
                elapse!(io, 1);
                addr!(abs).wrapping_add(self.regs.y as u16)
            }};
            (abi) => {{
                let abs = addr!(abs);
                self.read16(abs)
            }};
            (axi) => {{
                let addr = addr!(abx);
                self.read16(addr)
            }};
            (ziy) => {{
                let zp = addr!(zp);
                self.read16(zp).wrapping_add(self.regs.y as u16)
            }};
            (zxi) => {{
                let zpx = addr!(zpx);
                self.read16(zpx)
            }};
            (dest) => {{
                let dest = self.fetch8() as i8 as u16;
                self.regs.pc.wrapping_add(dest)
            }};
            (aaab) => {{
                let aaab = self.fetch16();
                (aaab & 0x01FF, aaab >> 13)
            }};
        }

        macro_rules! exec_instr {
            // Annotation
            ($mne:ident a $(, $op2:tt)?) => {
                exec_instr!($mne (reg a) $(, $op2)*)
            };
            ($mne:ident x $(, $op2:tt)?) => {
                exec_instr!($mne (reg x) $(, $op2)*)
            };
            ($mne:ident y $(, $op2:tt)?) => {
                exec_instr!($mne (reg y) $(, $op2)*)
            };
            ($mne:ident sp $(, $op2:tt)?) => {
                exec_instr!($mne (reg sp) $(, $op2)*)
            };
            ($mne:ident $op1:tt, a) => {
                exec_instr!($mne $op1 , (reg a))
            };
            ($mne:ident $op1:tt, x) => {
                exec_instr!($mne $op1 , (reg x))
            };
            ($mne:ident $op1:tt, y) => {
                exec_instr!($mne $op1 , (reg y))
            };
            ($mne:ident $op1:tt, sp) => {
                exec_instr!($mne $op1 , (reg sp))
            };

            // Register to Register
            (mov (reg $dst:ident), (reg $src:ident)) => {{
                let v = self.regs.$src;
                macro_rules! set_nz {
                    (sp) => {};
                    ($_:tt) => {
                        self.regs.set_nz(v)
                    };
                }
                set_nz!($dst);
                self.regs.$dst = v;
                elapse!(ram, 1);
            }};

            // Memory Load
            (mov (reg $dst:tt), $src:tt) => {{
                let addr = addr!($src);
                let v = self.read8(addr);
                self.regs.set_nz(v);
                self.regs.$dst = v;
            }};
            (movw ya, zp) => {{
                let addr = addr!(zp);
                let v = self.read16(addr);
                self.regs.set_nz16(v);
                self.regs.set_ya(v);
                elapse!(io, 1);
            }};

            // Memory Store
            (mov $dst:tt, $src:tt) => {{
                let v = rd!($src);
                let addr = addr!($dst);
                macro_rules! dummy_read {
                    (zp, zp) => {};
                    ((x + 1), a) => {};
                    ($_d:tt, $_s:tt) => {
                        let _ = self.read8(addr);
                    };
                }
                dummy_read!($dst, $src);
                self.write8(addr, v);
            }};

            (movw zp, ya) => {{
                let addr = addr!(zp);
                let _ = self.read8(addr); // dummy read lsb
                self.write16(addr, self.regs.ya());
            }};

            // Push/Pop
            (push $reg:tt) => {{
                let v = rd!($reg);
                self.push8(v);
                elapse!(ram, 1);
                elapse!(io, 1);
            }};
            (pop $reg:tt) => {{
                let v = self.pop8();
                wr!($reg, v);
                elapse!(ram, 1);
                elapse!(io, 1);
            }};

            // 8bit ALU Operations
            (or $op1:tt, $op2:tt) => {
                alu!(or, $op1, $op2, true)
            };
            (and $op1:tt, $op2:tt) => {
                alu!(and, $op1, $op2, true)
            };
            (eor $op1:tt, $op2:tt) => {
                alu!(eor, $op1, $op2, true)
            };
            (cmp $op1:tt, $op2:tt) => {{
                alu!(cmp, $op1, $op2, false)
            }};
            (adc $op1:tt, $op2:tt) => {
                alu!(adc, $op1, $op2, true)
            };
            (sbc $op1:tt, $op2:tt) => {
                alu!(sbc, $op1, $op2, true)
            };

            // 8bit Increment/Decrement and Rotate/Shift Commands
            (asl $op:tt) => {
                rmw!($op, rmw_op, asl)
            };
            (rol $op:tt) => {
                rmw!($op, rmw_op, rol)
            };
            (lsr $op:tt) => {
                rmw!($op, rmw_op, lsr)
            };
            (ror $op:tt) => {
                rmw!($op, rmw_op, ror)
            };
            (dec $op:tt) => {
                rmw!($op, rmw_op, dec)
            };
            (inc $op:tt) => {
                rmw!($op, rmw_op, inc)
            };

            // 16bit ALU Operations
            (addw ya, zp) => {{
                let addr = addr!(zp);
                let op2 = self.read16(addr) as u32;
                let op1 = self.regs.ya() as u32;
                let v = op1 + op2;
                self.regs.psw.set_z(v as u16 == 0);
                self.regs.psw.set_n(v & 0x8000 != 0);
                self.regs.psw.set_c(v > 0xFFFF);
                self.regs.psw.set_v(!(op1 ^ op2) & (op1 ^ v) & 0x8000 != 0);
                let h = (op1 & 0xFFF) + (op2 & 0xFFF);
                self.regs.psw.set_h(h > 0xFFF);
                self.regs.set_ya(v as u16);
                elapse!(io, 1);
            }};
            (subw ya, zp) => {{
                let addr = addr!(zp);
                let op2 = self.read16(addr) as u32;
                let op1 = self.regs.ya() as u32;
                let v = op1.wrapping_sub(op2);
                self.regs.psw.set_z(v as u16 == 0);
                self.regs.psw.set_n(v & 0x8000 != 0);
                self.regs.psw.set_c(!(v > 0xFFFF));
                self.regs.psw.set_v((op1 ^ op2) & (op1 ^ v & 0x8000) != 0);
                let h = (op1 & 0xFFF).wrapping_sub(op2 & 0xFFF);
                self.regs.psw.set_h(!(h > 0xFFF));
                self.regs.set_ya(v as u16);
                elapse!(io, 1);
            }};
            (cmpw ya, zp) => {{
                let addr = addr!(zp);
                let op2 = self.read16(addr);
                let op1 = self.regs.ya();
                let (val, c) = op1.overflowing_sub(op2);
                self.regs.set_nz16(val);
                self.regs.psw.set_c(!c);
            }};
            (incw zp) => {{
                let addr = addr!(zp);
                let op = self.read16(addr);
                let val = op.wrapping_add(1);
                self.regs.set_nz16(val);
                self.write16(addr, val);
            }};
            (decw zp) => {{
                let addr = addr!(zp);
                let op = self.read16(addr);
                let val = op.wrapping_sub(1);
                self.regs.set_nz16(val);
                self.write16(addr, val);
            }};
            (div ya, (reg x)) => {{
                let ya = self.regs.ya();
                if self.regs.x > 0 {
                    let d = ya / self.regs.x as u16;
                    let m = ya % self.regs.x as u16;
                    self.regs.a = d as u8;
                    self.regs.y = m as u8;
                    // FIXME: set H flag
                    self.regs.psw.set_v(d > 0xFF);
                    self.regs.set_nz(d as u8);
                } else {
                    self.regs.a = 0xFF;
                    self.regs.y = 0xFF;
                    // FIXME: I don't know the exact behavior of flags.
                    self.regs.psw.set_z(false);
                    self.regs.psw.set_n(true);
                    self.regs.psw.set_v(true);
                }
                elapse!(ram, 1);
                elapse!(io, 10);
            }};
            (mul ya) => {{
                let val = self.regs.y as u16 * self.regs.a as u16;
                self.regs.set_nz16(val);
                self.regs.set_ya(val);
                elapse!(ram, 1);
                elapse!(io, 7);
            }};

            // 1bit ALU Operations
            (clr1 zp, $i:expr) => {{
                let addr = addr!(zp);
                let op = self.read8(addr);
                let val = op & !(1 << $i);
                self.write8(addr, val);
                elapse!(ram, 1);
            }};
            (set1 zp, $i:expr) => {{
                let addr = addr!(zp);
                let op = self.read8(addr);
                let val = op | (1 << $i);
                self.write8(addr, val);
                elapse!(ram, 1);
            }};
            (not1 aaab) => {{
                let (addr, b) = addr!(aaab);
                let op = self.read8(addr);
                let val = op ^ (1 << b);
                self.write8(addr, val);
                elapse!(ram, 1);
            }};
            (mov1 aaab, c) => {{
                let (addr, b) = addr!(aaab);
                let op = self.read8(addr);
                let val = op & !(1 << b) | ((self.regs.psw.c() as u8) << b);
                self.write8(addr, val);
                elapse!(ram, 1);
                elapse!(io, 1);
            }};
            (mov1 c, aaab) => {{
                let (addr, b) = addr!(aaab);
                let op = self.read8(addr);
                self.regs.psw.set_c(op & (1 << b) != 0);
            }};
            (or1 c, aaab) => {{
                let (addr, b) = addr!(aaab);
                let op = self.read8(addr);
                self.regs.psw.set_c(self.regs.psw.c() || op & (1 << b) != 0);
                elapse!(io, 1);
            }};
            (or1 c, !aaab) => {{
                let (addr, b) = addr!(aaab);
                let op = self.read8(addr);
                self.regs.psw.set_c(self.regs.psw.c() || op & (1 << b) == 0);
                elapse!(io, 1);
            }};
            (and1 c, aaab) => {{
                let (addr, b) = addr!(aaab);
                let op = self.read8(addr);
                self.regs.psw.set_c(self.regs.psw.c() && op & (1 << b) != 0);
            }};
            (and1 c, !aaab) => {{
                let (addr, b) = addr!(aaab);
                let op = self.read8(addr);
                self.regs.psw.set_c(self.regs.psw.c() && op & (1 << b) == 0);
            }};
            (eor1 c, aaab) => {{
                let (addr, b) = addr!(aaab);
                let op = self.read8(addr);
                self.regs.psw.set_c(self.regs.psw.c() ^ (op & (1 << b) != 0));
                elapse!(io, 1);
            }};
            (clr c) => {{
                self.regs.psw.set_c(false);
                elapse!(ram, 1);
            }};
            (set c) => {{
                self.regs.psw.set_c(true);
                elapse!(ram, 1);
            }};
            (not c) => {{
                self.regs.psw.set_c(!self.regs.psw.c());
                elapse!(io, 1);
            }};
            (clr v) => {{
                self.regs.psw.set_v(false);
                self.regs.psw.set_h(false);
                elapse!(ram, 1);
            }};

            // Special ALU Operations
            (daa (reg a)) => {{
                elapse!(ram, 1);
                elapse!(io, 1);
                todo!("DAA")
            }};
            (das (reg a)) => {{
                elapse!(ram, 1);
                elapse!(io, 1);
                todo!("DAS")
            }};
            (xcn (reg a)) => {{
                let v = self.regs.a.rotate_right(4);
                self.regs.set_nz(v);
                self.regs.a = v;
                elapse!(ram, 1);
                elapse!(io, 3);
            }};
            (tclr abs, (reg a)) => {{
                let addr = addr!(abs);
                let v = self.read8(addr);
                self.regs.set_nz(v ^ self.regs.a);
                self.write8(addr, v & !self.regs.a);
                elapse!(ram, 1);
            }};
            (tset abs, (reg a)) => {{
                let addr = addr!(abs);
                let v = self.read8(addr);
                self.regs.set_nz(v ^ self.regs.a);
                self.write8(addr, v | self.regs.a);
                elapse!(ram, 1);
            }};

            // Conditional Jumps
            (bpl dest) => {
                cond_branch!(pl)
            };
            (bmi dest) => {
                cond_branch!(mi)
            };
            (bvc dest) => {
                cond_branch!(vc)
            };
            (bvs dest) => {
                cond_branch!(vs)
            };
            (bcc dest) => {
                cond_branch!(cc)
            };
            (bcs dest) => {
                cond_branch!(cs)
            };
            (bne dest) => {
                cond_branch!(ne)
            };
            (beq dest) => {
                cond_branch!(eq)
            };
            (bbs zp.$i:expr, dest) => {{
                let addr = addr!(zp);
                let dest = addr!(dest);
                let v = self.read8(addr);
                elapse!(io, 1);
                if v & (1 << $i) != 0 {
                    self.regs.pc = dest;
                    elapse!(ram, 2);
                }
            }};
            (bbc zp.$i:expr, dest) => {{
                let addr = addr!(zp);
                let dest = addr!(dest);
                let v = self.read8(addr);
                elapse!(io, 1);
                if v & (1 << $i) == 0 {
                    self.regs.pc = dest;
                    elapse!(io, 2);
                }
            }};
            (cbne $addrmode:tt, dest) => {{
                let addr = addr!($addrmode);
                let dest = addr!(dest);
                elapse!(io, 1);
                if self.regs.a != self.read8(addr) {
                    self.regs.pc = dest;
                    elapse!(io, 2);
                }
            }};
            (dbnz (reg y), dest) => {{
                let dest = addr!(dest);
                self.regs.y = self.regs.y.wrapping_sub(1);
                elapse!(ram, 1);
                elapse!(io, 1);
                if self.regs.y != 0 {
                    self.regs.pc = dest;
                    elapse!(io, 2);
                }
            }};
            (dbnz zp, dest) => {{
                let addr = addr!(zp);
                let dest = addr!(dest);
                let v = self.read8(addr).wrapping_sub(1);
                self.write8(addr, v);
                elapse!(io, 1);
                if v != 0 {
                    self.regs.pc = dest;
                    elapse!(io, 2);
                }
            }};

            // Normal Jumps/Calls
            (bra dest) => {{
                self.regs.pc = addr!(dest);
                elapse!(io, 2);
            }};
            (jmp $addrmode:tt) => {
                self.regs.pc = addr!($addrmode)
            };
            (call $addrmode:tt) => {{
                let addr = addr!($addrmode);
                self.push16(self.regs.pc);
                self.regs.pc = addr;
                elapse!(io, 3);
            }};
            (tcall $i:expr) => {{
                self.push16(self.regs.pc);
                self.regs.pc = 0xFFDE - (2 * $i);
                elapse!(io, 3);
            }};
            (pcall imm) => {{
                let n = self.fetch8();
                self.push16(self.regs.pc);
                self.regs.pc = 0xFF00 | n as u16;
                elapse!(io, 3);
            }};
            (ret) => {{
                self.regs.pc = self.pop16();
                elapse!(ram, 1);
                elapse!(io, 1);
            }};
            (reti) => {{
                self.regs.psw = self.pop8().into();
                self.regs.pc = self.pop16();
                elapse!(ram, 1);
                elapse!(io, 1);
            }};
            (brk) => {{
                self.push16(self.regs.pc);
                self.push8(self.regs.psw.into());
                self.regs.psw.set_i(false);
                self.regs.psw.set_b(true);
                self.regs.pc = self.read16(0xFFDE);
                elapse!(io, 2);
            }};

            // Wait/Delay/Control
            (nop) => {
                elapse!(ram, 1)
            };
            (sleep) => {
                todo!("sleep")
            };
            (stop) => {
                todo!("stop")
            };
            (clr p) => {{
                self.regs.psw.set_p(false);
                elapse!(ram, 1);
            }};
            (set p) => {{
                self.regs.psw.set_p(true);
                elapse!(ram, 1);
            }};
            (ei) => {{
                self.regs.psw.set_i(true);
                elapse!(ram, 1);
                elapse!(io, 1);
            }};
            (di) => {{
                self.regs.psw.set_i(false);
                elapse!(ram, 1);
                elapse!(io, 1);
            }};
        }

        macro_rules! alu {
            ($op:ident, $op1:tt, $op2:tt, $wb:tt) => {{
                // op2 comes first in opcodes
                let op2 = rd!($op2);
                macro_rules! exec {
                    (true) => {
                        rmw_exec!($op1, alu_op, $op, op2)
                    };
                    (false) => {{
                        let op1 = rd!($op1);
                        alu_op!($op, op1, op2)
                    }};
                }
                exec!($wb);
                macro_rules! extra_cycle {
                    ((reg $_:ident), $_2:tt) => {};
                    ($_1:tt, $_2:tt) => {
                        elapse!(io, 1);
                    };
                }
                extra_cycle!($op1, $op2);
            }};
        }

        macro_rules! rmw {
            ($opr:tt, $m:ident, $op:tt) => {{
                rmw_exec!($opr, $m, $op);
                // zp, abs: IOx1, reg, zpx: RAMx1
                macro_rules! rmw_extra_cycle {
                    (zp) => {
                        elapse!(io, 1)
                    };
                    (abs) => {
                        elapse!(io, 1)
                    };
                    ($_:tt) => {
                        elapse!(ram, 1)
                    };
                }
                rmw_extra_cycle!($opr);
            }};
        }

        macro_rules! rmw_exec {
            ((reg $reg:ident), $m:ident, $op:ident $(, $op2:expr)*) => {{
                let op1 = self.regs.$reg;
                let v = $m!($op, op1 $(, $op2)*);
                self.regs.$reg = v;
            }};

            ($addrmode:tt, $m:ident, $op:ident $(, $op2:expr)*) => {{
                let addr = addr!($addrmode);
                let op1 = self.read8(addr);
                let v = $m!($op, op1 $(, $op2)*);
                self.write8(addr, v);
            }};
        }

        macro_rules! alu_op {
            (or, $op1:expr, $op2:expr) => {{
                let v = $op1 | $op2;
                self.regs.set_nz(v);
                v
            }};
            (and, $op1:expr, $op2:expr) => {{
                let v = $op1 & $op2;
                self.regs.set_nz(v);
                v
            }};
            (eor, $op1:expr, $op2:expr) => {{
                let v = $op1 ^ $op2;
                self.regs.set_nz(v);
                v
            }};
            (cmp, $op1:expr, $op2:expr) => {{
                let (v, c) = $op1.overflowing_sub($op2);
                self.regs.set_nz(v);
                self.regs.psw.set_c(!c);
            }};
            (adc, $op1:expr, $op2:expr) => {{
                let op1 = $op1 as u32;
                let op2 = $op2 as u32;
                let v = op1 + op2 + self.regs.psw.c() as u32;
                let h = (op1 & 0xF) + (op2 & 0xF) + self.regs.psw.c() as u32;
                self.regs.set_nz(v as u8);
                self.regs.psw.set_c(v > 0xFF);
                self.regs.psw.set_v(!(op1 ^ op2) & (op1 ^ v) & 0x80 != 0);
                self.regs.psw.set_h(h > 0xF);
                v as u8
            }};
            (sbc, $op1:expr, $op2:expr) => {{
                let op1 = $op1 as u32;
                let op2 = $op2 as u32;
                let v = op1
                    .wrapping_sub(op2)
                    .wrapping_sub(1 - self.regs.psw.c() as u32);
                let h = (op1 & 0xF)
                    .wrapping_sub(op2 & 0xF)
                    .wrapping_sub(1 - self.regs.psw.c() as u32);
                self.regs.set_nz(v as u8);
                self.regs.psw.set_c(!(v > 0xFF));
                self.regs.psw.set_v((op1 ^ op2) & (op1 ^ v) & 0x80 != 0);
                self.regs.psw.set_h(!(h > 0xF));
                v as u8
            }};
        }

        macro_rules! rmw_op {
            (asl, $op:expr) => {{
                let (v, c) = $op.overflowing_shl(1);
                self.regs.set_nz(v);
                self.regs.psw.set_c(c);
                v
            }};
            (rol, $op:expr) => {{
                let (v, c) = $op.overflowing_shl(1);
                let v = v | self.regs.psw.c() as u8;
                self.regs.set_nz(v);
                self.regs.psw.set_c(c);
                v
            }};
            (lsr, $op:expr) => {{
                let v = $op >> 1;
                self.regs.set_nz(v);
                self.regs.psw.set_c($op & 1 != 0);
                v
            }};
            (ror, $op:expr) => {{
                let v = ($op >> 1) | ((self.regs.psw.c() as u8) << 7);
                self.regs.set_nz(v);
                self.regs.psw.set_c($op & 1 != 0);
                v
            }};
            (inc, $op:expr) => {{
                let v = $op.wrapping_add(1);
                self.regs.set_nz(v);
                v
            }};
            (dec, $op:expr) => {{
                let v = $op.wrapping_sub(1);
                self.regs.set_nz(v);
                v
            }};
        }

        #[rustfmt::skip]
        macro_rules! cond_branch {
            (pl) => { cond_exec!(!self.regs.psw.n(), io, 2) };
            (mi) => { cond_exec!(self.regs.psw.n(), ram, 2) };
            (vc) => { cond_exec!(!self.regs.psw.v(), io, 2) };
            (vs) => { cond_exec!(self.regs.psw.v(), ram, 2) };
            (cc) => { cond_exec!(!self.regs.psw.c(), io, 2) };
            (cs) => { cond_exec!(self.regs.psw.c(), ram, 2) };
            (ne) => { cond_exec!(!self.regs.psw.z(), io, 2) };
            (eq) => { cond_exec!(self.regs.psw.z(), ram, 2) };
        }

        macro_rules! cond_exec {
            ($cond:expr, $wait_ty:ident, $wait_count:expr) => {{
                let addr = addr!(dest);
                if $cond {
                    self.regs.pc = addr;
                    elapse!($wait_ty, $wait_count);
                }
            }};
        }

        #[rustfmt::skip]
        macro_rules! rd {
            ((reg a)) => { self.regs.a };
            ((reg x)) => { self.regs.x };
            ((reg y)) => { self.regs.y };
            ((reg sp)) => { self.regs.sp };
            (psw) => { self.regs.psw.into() };

            ($addrmode:tt) => {{
                let addr = addr!($addrmode);
                self.read8(addr)
            }};
        }

        #[rustfmt::skip]
        macro_rules! wr {
            ((reg a), $v:expr) => { self.regs.a = $v };
            ((reg x), $v:expr) => { self.regs.x = $v };
            ((reg y), $v:expr) => { self.regs.y = $v };
            (psw, $v:expr) => { self.regs.psw = $v.into() };

            ($addrmode:tt, $v:expr) => {{
                let addr = addr!($addrmode);
                self.write8(addr, $v);
            }};
        }

        match_opcode!(self.fetch8(), exec_instr);
    }

    fn trace(&self) {
        let pc = self.regs.pc;

        let asm = self.disasm().map_or_else(
            || "          invalid".into(),
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
            "PC:{pc:04X}  {asm:32} A:{:02X} X:{:02X} Y:{:02X} SP:{:02X} P:{}{}{}{}{}{}{}{} CYC:{}",
            self.regs.a,
            self.regs.x,
            self.regs.y,
            self.regs.sp,
            if self.regs.psw.n() { 'N' } else { 'n' },
            if self.regs.psw.v() { 'V' } else { 'v' },
            if self.regs.psw.p() { 'P' } else { 'p' },
            if self.regs.psw.b() { 'B' } else { 'b' },
            if self.regs.psw.h() { 'H' } else { 'h' },
            if self.regs.psw.i() { 'I' } else { 'i' },
            if self.regs.psw.z() { 'Z' } else { 'z' },
            if self.regs.psw.c() { 'C' } else { 'c' },
            self.counter,
        );
    }

    fn disasm(&self) -> Option<(Vec<u8>, String)> {
        let mut pc = self.regs.pc;
        let mut bytes = Vec::new();

        macro_rules! fetch {
            (8) => {{
                let v = self.read8_pure(pc)?;
                bytes.push(v);
                #[allow(unused_assignments)]
                {
                    pc = pc.wrapping_add(1);
                }
                v
            }};
            (16) => {{
                let lo = fetch!(8);
                let hi = fetch!(8);
                (lo as u16) | ((hi as u16) << 8)
            }};
        }

        let opcode = fetch!(8);

        macro_rules! disasm_instr {
            ($mne:ident) => {
                stringify!($mne).to_string()
            };
            ($mne:ident $op:tt) => {
                format!("{} {}", stringify!($mne), opr!($op))
            };
            ($mne:ident $op1:tt, dest) => {{
                let op1 = opr!($op1);
                let op2 = opr!(dest);
                format!("{} {op1}, {op2}", stringify!($mne))
            }};
            ($mne:ident $op1:tt, $op2:tt) => {{
                let op2 = opr!($op2);
                let op1 = opr!($op1);
                format!("{} {op1}, {op2}", stringify!($mne))
            }};
            ($mne:ident $op1:tt.$i:expr, $op2:tt) => {{
                let op1 = opr!($op1);
                let op2 = opr!($op2);
                format!("{} {}.{}, {}", stringify!($mne), op1, $i, op2)
            }};
            ($mne:ident $op1:tt, !aaab) => {{
                let op1 = opr!($op1);
                let op2 = opr!(aaab);
                format!("{} {}, !{}", stringify!($mne), op1, op2)
            }};
        }

        #[rustfmt::skip]
        macro_rules! opr {
            ($i:literal) => { $i };
            (a) => { 'a' };
            (x) => { 'x' };
            (y) => { 'y' };
            (sp) => { "sp" };
            (ya) => { "ya" };
            (c) => { 'c' };
            (p) => { 'p' };
            (v) => { 'v' };
            (psw) => { "psw" };

            ((x)) => { "(x)" };
            ((x+1)) => { "(x)+" };
            ((y)) => { "(y)" };

            (imm) => { format!("#{:#04X}", fetch!(8)) };

            (zp) => { format!("{:#04X}", fetch!(8)) };
            (zpx) => { format!("{:#04X}+x", fetch!(8)) };
            (zpy) => { format!("{:#04X}+y", fetch!(8)) };
            (ziy) => { format!("({:#04X})+y", fetch!(8)) };
            (zxi) => { format!("({:#04X}+x)", fetch!(8)) };

            (abs) => { format!("{:#06X}", fetch!(16)) };
            (abx) => { format!("{:#06X}+x", fetch!(16)) };
            (aby) => { format!("{:#06X}+y", fetch!(16)) };
            (abi) => { format!("({:#06X})", fetch!(16)) };
            (axi) => { format!("({:#06X}+x)", fetch!(16)) };

            (aaab) => {{
                let v = fetch!(16);
                format!("{:06X}.{}", v & 0x01FF, v >> 13)
            }};

            (dest) => {{
                let dest = fetch!(8) as i8 as u16;
                format!("{:#06X}", pc.wrapping_add(dest))
            }}
        }

        let asm = match_opcode!(opcode, disasm_instr);
        Some((bytes, asm))
    }
}

const BOOT_ROM: [u8; 0x40] = [
    0xCD, 0xEF, 0xBD, 0xE8, 0x00, 0xC6, 0x1D, 0xD0, 0xFC, 0x8F, 0xAA, 0xF4, 0x8F, 0xBB, 0xF5, 0x78,
    0xCC, 0xF4, 0xD0, 0xFB, 0x2F, 0x19, 0xEB, 0xF4, 0xD0, 0xFC, 0x7E, 0xF4, 0xD0, 0x0B, 0xE4, 0xF5,
    0xCB, 0xF4, 0xD7, 0x00, 0xFC, 0xD0, 0xF3, 0xAB, 0x01, 0x10, 0xEF, 0x7E, 0xF4, 0x10, 0xEB, 0xBA,
    0xF6, 0xDA, 0x00, 0xBA, 0xF4, 0xC4, 0xF4, 0xDD, 0x5D, 0xD0, 0xDB, 0x1F, 0x00, 0x00, 0xC0, 0xFF,
];

#[derive(Default)]
pub struct SpcFile {
    pub header: Vec<u8>,
    pub has_id666: bool,
    pub version_minor: u8,
    pub pc: u16,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub psw: u8,
    pub sp: u8,
    pub song_title: [u8; 0x20],
    pub game_title: [u8; 0x20],
    pub dumper_name: [u8; 0x10],
    pub comment: [u8; 0x20],
    pub dump_date: (u16, u8, u8),
    pub play_seconds: u32,
    pub fade_milliseconds: u32,
    pub artist_of_song: [u8; 0x20],
    pub default_channel_disable: bool,
    pub emulator_used_to_dump: u8, // 0: Unknown, 1: ZSNES, 2: Snes9x
    pub ram: Vec<u8>,              // 64KB RAM
    pub dsp_registers: Vec<u8>,
    pub extra_ram: Vec<u8>,
}

impl Display for SpcFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Header: {}", String::from_utf8_lossy(&self.header))?;
        writeln!(f, "Has ID666: {}", self.has_id666)?;
        writeln!(f, "Version minor: {}", self.version_minor)?;
        writeln!(f, "PC:  {:04X}", self.pc)?;
        writeln!(f, "A:   {:02X}", self.a)?;
        writeln!(f, "X:   {:02X}", self.x)?;
        writeln!(f, "Y:   {:02X}", self.y)?;
        writeln!(f, "PSW: {:02X}", self.psw)?;
        writeln!(f, "SP:  {:02X}", self.sp)?;
        writeln!(
            f,
            "Song title: {}",
            String::from_utf8_lossy(&self.song_title)
        )?;
        writeln!(
            f,
            "Game title: {}",
            String::from_utf8_lossy(&self.game_title)
        )?;
        writeln!(
            f,
            "Dumper name: {}",
            String::from_utf8_lossy(&self.dumper_name)
        )?;
        writeln!(f, "Comment: {}", String::from_utf8_lossy(&self.comment))?;
        writeln!(
            f,
            "Dump date: {:04}/{:02}/{:02}",
            self.dump_date.0, self.dump_date.1, self.dump_date.2
        )?;
        writeln!(f, "Play seconds: {}", self.play_seconds)?;
        writeln!(f, "Fade milliseconds: {}", self.fade_milliseconds)?;
        writeln!(
            f,
            "Artist of song: {}",
            String::from_utf8_lossy(&self.artist_of_song)
        )?;
        writeln!(
            f,
            "Default channel disable: {}",
            self.default_channel_disable
        )?;
        writeln!(f, "Emulator used to dump: {}", self.emulator_used_to_dump)?;

        Ok(())
    }
}

impl SpcFile {
    pub fn make_spc(&self) -> Spc {
        let mut ret = Spc {
            regs: Registers {
                a: self.a,
                x: self.x,
                y: self.y,
                sp: self.sp,
                psw: self.psw.into(),
                pc: self.pc,
            },
            ioregs: IORegisters::default(),
            ram: self.ram.clone(),
            counter: 0,
            audio_buffer: AudioBuffer::default(),
        };

        for i in 0x00F1..=0x00FC {
            // Exclude DSP write
            if i != 0x00F3 {
                ret.io_write(i & 0xF, self.ram[i as usize]);
            }
        }

        ret
    }
}

#[derive(thiserror::Error, Debug)]
pub enum SpcFileError {
    #[error("Invalid file size")]
    InvalidFileSize,
    #[error("Invalid file header")]
    InvalidHeader,
}

impl SpcFile {
    pub fn from_bytes(bytes: &[u8]) -> Result<SpcFile, SpcFileError> {
        if bytes.len() < 0x10200 {
            Err(SpcFileError::InvalidFileSize)?;
        }

        let header = bytes[0..0x21].try_into().unwrap();

        if &bytes[0x21..=0x22] != [0x1A, 0x1A] {
            Err(SpcFileError::InvalidHeader)?;
        }

        let spc = SpcFile {
            header,
            has_id666: match bytes[0x23] {
                0x1A => true,
                0x1B => false,
                _ => Err(SpcFileError::InvalidHeader)?,
            },
            version_minor: bytes[0x24],

            pc: u16::from_le_bytes(bytes[0x25..=0x26].try_into().unwrap()),
            a: bytes[0x27],
            x: bytes[0x28],
            y: bytes[0x29],
            psw: bytes[0x2A],
            sp: bytes[0x2B],

            song_title: bytes[0x2E..0x4E].try_into().unwrap(),
            game_title: bytes[0x4E..0x6E].try_into().unwrap(),
            dumper_name: bytes[0x6E..0x7E].try_into().unwrap(),
            comment: bytes[0x7E..0x9E].try_into().unwrap(),

            ..Default::default()
        };

        let dump_date_text = parse_date(&bytes[0x9E..0xA9]);

        Ok(if let Some(dump_date) = dump_date_text {
            SpcFile {
                dump_date,
                play_seconds: String::from_utf8_lossy(&bytes[0xA9..0xAC]).parse().unwrap(),
                fade_milliseconds: String::from_utf8_lossy(&bytes[0xAC..0xB1]).parse().unwrap(),
                artist_of_song: bytes[0xB1..0xD1].try_into().unwrap(),
                default_channel_disable: match bytes[0xD1] {
                    0x00 => true,
                    0x01 => false,
                    _ => Err(SpcFileError::InvalidHeader)?,
                },
                emulator_used_to_dump: bytes[0xD2],
                ram: bytes[0x100..0x10100].to_vec(),
                dsp_registers: bytes[0x10100..0x10180].try_into().unwrap(),
                extra_ram: bytes[0x101C0..0x10200].try_into().unwrap(),
                ..spc
            }
        } else {
            SpcFile {
                dump_date: (
                    u16::from_le_bytes(bytes[0xA0..0xA2].try_into().unwrap()),
                    bytes[0x9E],
                    bytes[0x9F],
                ),
                play_seconds: 0,      // FIXME
                fade_milliseconds: 0, // FIXME,
                artist_of_song: bytes[0xB0..0xD0].try_into().unwrap(),
                default_channel_disable: match bytes[0xD0] {
                    0x00 => true,
                    0x01 => false,
                    _ => Err(SpcFileError::InvalidHeader)?,
                },
                emulator_used_to_dump: bytes[0xD1],
                ram: bytes[0x100..0x10100].to_vec(),
                dsp_registers: bytes[0x10100..0x10180].try_into().unwrap(),
                extra_ram: bytes[0x101C0..0x10200].try_into().unwrap(),
                ..spc
            }
        })
    }
}

fn parse_date(b: &[u8]) -> Option<(u16, u8, u8)> {
    if b[2] != b'/' || b[5] != b'/' {
        None?
    }
    let month = String::from_utf8_lossy(&b[0..2]).parse().ok()?;
    let day = String::from_utf8_lossy(&b[3..5]).parse().ok()?;
    let year = String::from_utf8_lossy(&b[6..11]).parse().ok()?;
    Some((year, month, day))
}
