use serde::{Deserialize, Serialize};
use super_sabicom_macro::{context, delegate};

use crate::{bus, cartridge, cpu, ppu, rom, spc};

pub trait Cpu {
    fn cpu_reset(&mut self);
    fn exec_one(&mut self);
}

#[delegate]
pub trait Bus {
    fn bus(&self) -> &bus::Bus;
    fn bus_mut(&mut self) -> &mut bus::Bus;

    fn read(&mut self, addr: u32) -> u8;
    fn read_pure(&self, addr: u32) -> Option<u8>;
    fn write(&mut self, addr: u32, data: u8);
    fn bus_tick(&mut self);
}

#[delegate]
pub trait Ppu {
    fn ppu(&self) -> &ppu::Ppu;
    fn ppu_mut(&mut self) -> &mut ppu::Ppu;

    fn ppu_read(&mut self, addr: u16, cpu_open_bus: u8) -> u8;
    fn ppu_write(&mut self, addr: u16, data: u8);
    fn ppu_tick(&mut self);
}

#[delegate]
pub trait Spc {
    fn spc(&self) -> &spc::Spc;
    fn spc_mut(&mut self) -> &mut spc::Spc;

    fn spc_reset(&mut self);
    fn spc_tick(&mut self);
}

#[delegate]
pub trait Cartridge {
    fn cartridge(&self) -> &cartridge::Cartridge;
    fn cartridge_mut(&mut self) -> &mut cartridge::Cartridge;
}

#[delegate]
pub trait Interrupt {
    fn interrupt(&self) -> &InterruptCtrl;
    fn interrupt_mut(&mut self) -> &mut InterruptCtrl;
}

#[delegate]
pub trait Timing {
    fn now(&self) -> u64;
    fn elapse(&mut self, clock: u64);
    fn counter(&self) -> &Counter;
    fn counter_mut(&mut self) -> &mut Counter;
}

#[context]
#[derive(Serialize, Deserialize)]
pub struct Context {
    cpu: cpu::Cpu,

    #[split(Inner1: Bus)]
    bus: bus::Bus,

    #[split(Inner2: Ppu + Spc + Cartridge)]
    ppu: ppu::Ppu,
    spc: spc::Spc,
    cartridge: cartridge::Cartridge,

    #[split(Inner3: Interrupt + Timing)]
    interrupt: InterruptCtrl,
    counter: Counter,
}

#[derive(Default, Serialize, Deserialize)]
pub struct InterruptCtrl {
    nmi_enable: bool,
    nmi_flag: bool,
    nmi: bool,
    nmi_raise: bool,
    hvirq_enable: u8,
    h_count: u16,
    v_count: u16,
    irq: bool,
}

#[derive(Default, Serialize, Deserialize)]
pub struct Counter {
    counter: u64,
    pub frame: u64,
    pub x: u32,
    pub y: u32,
}

impl InterruptCtrl {
    pub fn set_nmi_enable(&mut self, enable: bool) {
        // Disabling NMI does not ack NMI
        let prev = self.nmi_enable && self.nmi_flag;
        self.nmi_enable = enable;
        if !prev && self.nmi_enable && self.nmi_flag {
            self.nmi_raise = true;
        }
    }

    pub fn nmi_flag(&mut self) -> bool {
        let ret = self.nmi_flag;
        self.nmi_flag = false;
        ret
    }

    pub fn set_nmi_flag(&mut self, nmi: bool) {
        let prev = self.nmi_enable && self.nmi_flag;
        self.nmi_flag = nmi;
        if self.nmi_enable && !prev && self.nmi_flag {
            self.nmi_raise = true;
        }
    }

    pub fn nmi(&mut self) -> bool {
        let ret = self.nmi;
        self.nmi = self.nmi_raise;
        self.nmi_raise = false;
        ret
    }

    pub fn hvirq_enable(&self) -> u8 {
        self.hvirq_enable
    }

    pub fn set_hvirq_enable(&mut self, enable: u8) {
        self.hvirq_enable = enable;
        if enable == 0 {
            self.irq = false;
        }
    }

    pub fn h_count(&self) -> u16 {
        self.h_count
    }

    pub fn set_h_count(&mut self, h_count: u16) {
        self.h_count = h_count;
    }

    pub fn v_count(&self) -> u16 {
        self.v_count
    }

    pub fn set_v_count(&mut self, v_count: u16) {
        self.v_count = v_count;
    }

    pub fn set_irq(&mut self, irq: bool) {
        self.irq = irq;
    }

    pub fn irq(&self) -> bool {
        self.irq
    }
}

impl Context {
    pub fn from_rom(rom: rom::Rom, backup: Option<&[u8]>) -> Self {
        let cpu = cpu::Cpu::default();
        let bus = bus::Bus::default();
        let ppu = ppu::Ppu::default();
        let spc = spc::Spc::default();
        let cartridge = cartridge::Cartridge::new(rom, backup);

        let mut ret = Self::new(
            cpu,
            bus,
            ppu,
            spc,
            cartridge,
            Default::default(),
            Default::default(),
        );

        ret.cpu_reset();
        ret.spc_reset();

        ret
    }
}

impl Cpu for Context {
    fn cpu_reset(&mut self) {
        self.cpu.reset(&mut self.inner);
    }

    fn exec_one(&mut self) {
        self.cpu.exec_one(&mut self.inner)
    }
}

impl Bus for Inner1 {
    fn bus(&self) -> &bus::Bus {
        &self.bus
    }
    fn bus_mut(&mut self) -> &mut bus::Bus {
        &mut self.bus
    }

    fn read(&mut self, addr: u32) -> u8 {
        self.bus.read::<_, 0>(&mut self.inner, addr)
    }

    fn read_pure(&self, addr: u32) -> Option<u8> {
        self.bus.read_pure(&self.inner, addr)
    }

    fn write(&mut self, addr: u32, data: u8) {
        self.bus.write::<_, 0>(&mut self.inner, addr, data)
    }

    fn bus_tick(&mut self) {
        self.bus.tick(&mut self.inner)
    }
}

impl Ppu for Inner2 {
    fn ppu(&self) -> &ppu::Ppu {
        &self.ppu
    }
    fn ppu_mut(&mut self) -> &mut ppu::Ppu {
        &mut self.ppu
    }

    fn ppu_read(&mut self, addr: u16, cpu_open_bus: u8) -> u8 {
        self.ppu.read(&mut self.inner, addr, cpu_open_bus)
    }

    fn ppu_write(&mut self, addr: u16, data: u8) {
        self.ppu.write(&mut self.inner, addr, data)
    }

    fn ppu_tick(&mut self) {
        self.ppu.tick(&mut self.inner)
    }
}

impl Spc for Inner2 {
    fn spc(&self) -> &spc::Spc {
        &self.spc
    }
    fn spc_mut(&mut self) -> &mut spc::Spc {
        &mut self.spc
    }

    fn spc_reset(&mut self) {
        self.spc.reset();
    }
    fn spc_tick(&mut self) {
        self.spc.tick(&mut self.inner)
    }
}

impl Cartridge for Inner2 {
    fn cartridge(&self) -> &cartridge::Cartridge {
        &self.cartridge
    }
    fn cartridge_mut(&mut self) -> &mut cartridge::Cartridge {
        &mut self.cartridge
    }
}

impl Interrupt for Inner3 {
    fn interrupt(&self) -> &InterruptCtrl {
        &self.interrupt
    }
    fn interrupt_mut(&mut self) -> &mut InterruptCtrl {
        &mut self.interrupt
    }
}

impl Timing for Inner3 {
    fn now(&self) -> u64 {
        self.counter.counter
    }
    fn elapse(&mut self, clock: u64) {
        self.counter.counter += clock;
    }

    fn counter(&self) -> &Counter {
        &self.counter
    }
    fn counter_mut(&mut self) -> &mut Counter {
        &mut self.counter
    }
}
