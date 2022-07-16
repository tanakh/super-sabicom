use super_sabicom_macro::{context, delegate};

use crate::{bus, cpu, rom, spc};

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
pub trait Spc {
    fn tick_spc(&mut self);
}

#[delegate]
pub trait Rom {
    fn rom(&self) -> &rom::Rom;
    fn rom_mut(&mut self) -> &mut rom::Rom;
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

    #[split(Inner2: Spc + Rom)]
    spc: spc::Spc,
    rom: rom::Rom,

    #[split(Inner3: Timing)]
    counter: u64,
}

impl Context {
    pub fn from_rom(rom: rom::Rom) -> Self {
        let cpu = cpu::Cpu::default();
        let bus = bus::Bus::default();
        let spc = spc::Spc::default();

        let mut ret = Self::new(cpu, bus, spc, rom, 0);
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

impl Spc for Inner2 {
    fn tick_spc(&mut self) {
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

impl Timing for Inner3 {
    fn now(&self) -> u64 {
        self.counter
    }
    fn elapse(&mut self, clock: u64) {
        self.counter += clock;
    }
}
