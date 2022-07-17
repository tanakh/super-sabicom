use super_sabicom_macro::{context, delegate};

use crate::{bus, cpu, ppu, rom, spc};

pub trait Cpu {
    fn reset(&mut self);
    fn exec_one(&mut self);
}

#[delegate]
pub trait Bus {
    fn read(&mut self, addr: u32) -> u8;
    fn read_pure(&self, addr: u32) -> Option<u8>;
    fn write(&mut self, addr: u32, data: u8);
}

#[delegate]
pub trait Ppu {
    fn ppu(&self) -> &ppu::Ppu;
    fn ppu_mut(&mut self) -> &mut ppu::Ppu;

    fn ppu_read(&mut self, addr: u16) -> u8;
    fn ppu_write(&mut self, addr: u16, data: u8);
    fn ppu_tick(&mut self);
}

#[delegate]
pub trait Spc {
    fn spc_tick(&mut self);
}

#[delegate]
pub trait Rom {
    fn rom(&self) -> &rom::Rom;
    fn rom_mut(&mut self) -> &mut rom::Rom;
}

#[delegate]
pub trait Interrupt {
    fn set_nmi_enable(&mut self, enable: bool);
    fn nmi_flag(&mut self) -> bool;
    fn set_nmi_flag(&mut self, nmi: bool);
    fn nmi(&mut self) -> bool;

    fn set_irq(&mut self, irq: bool);
    fn irq(&self) -> bool;
}

#[delegate]
pub trait Timing {
    fn now(&self) -> u64;
    fn elapse(&mut self, clock: u64);
}

#[context]
pub struct Context {
    cpu: cpu::Cpu,

    #[split(Inner1: Bus)]
    bus: bus::Bus,

    #[split(Inner2: Ppu + Spc + Rom)]
    ppu: ppu::Ppu,
    spc: spc::Spc,
    rom: rom::Rom,

    #[split(Inner3: Interrupt + Timing)]
    counter: u64,
    nmi_enable: bool,
    nmi_flag: bool,
    nmi: bool,
    irq: bool,
}

impl Context {
    pub fn from_rom(rom: rom::Rom) -> Self {
        let cpu = cpu::Cpu::default();
        let bus = bus::Bus::default();
        let ppu = ppu::Ppu::default();
        let spc = spc::Spc::default();

        let mut ret = Self::new(cpu, bus, ppu, spc, rom, 0, false, false, false, false);
        ret.reset();

        ret
    }
}

impl Cpu for Context {
    fn reset(&mut self) {
        self.cpu.reset(&mut self.inner);
    }

    fn exec_one(&mut self) {
        self.cpu.exec_one(&mut self.inner)
    }
}

impl Bus for Inner1 {
    fn read(&mut self, addr: u32) -> u8 {
        self.bus.read(&mut self.inner, addr)
    }

    fn read_pure(&self, addr: u32) -> Option<u8> {
        self.bus.read_pure(&self.inner, addr)
    }

    fn write(&mut self, addr: u32, data: u8) {
        self.bus.write(&mut self.inner, addr, data)
    }
}

impl Ppu for Inner2 {
    fn ppu(&self) -> &ppu::Ppu {
        &self.ppu
    }
    fn ppu_mut(&mut self) -> &mut ppu::Ppu {
        &mut self.ppu
    }

    fn ppu_read(&mut self, addr: u16) -> u8 {
        self.ppu.read(&mut self.inner, addr)
    }

    fn ppu_write(&mut self, addr: u16, data: u8) {
        self.ppu.write(&mut self.inner, addr, data)
    }

    fn ppu_tick(&mut self) {
        self.ppu.tick(&mut self.inner)
    }
}

impl Spc for Inner2 {
    fn spc_tick(&mut self) {
        self.spc.tick(&mut self.inner)
    }
}

impl Rom for Inner2 {
    fn rom(&self) -> &rom::Rom {
        &self.rom
    }
    fn rom_mut(&mut self) -> &mut rom::Rom {
        &mut self.rom
    }
}

impl Interrupt for Inner3 {
    fn set_nmi_enable(&mut self, enable: bool) {
        let prev = self.nmi_enable && self.nmi_flag;
        self.nmi_enable = enable;
        if !prev && self.nmi_enable && self.nmi_flag {
            self.nmi = true;
        }
    }

    fn nmi_flag(&mut self) -> bool {
        let ret = self.nmi_flag;
        self.nmi_flag = false;
        ret
    }

    fn set_nmi_flag(&mut self, nmi: bool) {
        let prev = self.nmi_enable && self.nmi_flag;
        self.nmi_flag = nmi;
        if !prev && self.nmi_enable && self.nmi_flag {
            self.nmi = true;
        }
    }

    fn nmi(&mut self) -> bool {
        let ret = self.nmi;
        self.nmi = false;
        ret
    }

    fn set_irq(&mut self, irq: bool) {
        self.irq = irq;
    }

    fn irq(&self) -> bool {
        self.irq
    }
}

impl Timing for Inner3 {
    fn now(&self) -> u64 {
        self.counter
    }
    fn elapse(&mut self, clock: u64) {
        self.counter += clock;
    }
}
