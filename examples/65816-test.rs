use rayon::prelude::*;
use serde::Deserialize;
use std::collections::BTreeMap;
use std::fmt::Display;
use std::fs::{read_dir, File};
use std::path::PathBuf;

use super_sabicom::{context, cpu::Cpu};

#[derive(Deserialize)]
struct TestCase {
    name: String,
    initial: State,
    #[serde(rename = "final")]
    final_: State,
    cycles: Vec<BusAccess>,
}

#[derive(PartialEq, Eq, Deserialize)]
struct State {
    pc: u16,
    s: u16,
    p: u8,
    a: u16,
    x: u16,
    y: u16,
    dbr: u8,
    d: u16,
    pbr: u8,
    e: u8,
    ram: Vec<(u32, u8)>,
}

impl Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,
            "PC: {:04X}, S: {:04X}, P: {}{}{}{}{}{}{}{}, A: {:04X}, X: {:04X}, Y: {:04X}, DBR: {:02X}, D: {:04X}, PBR: {:02X}, E: {}, ram: [{}]",
            self.pc,
            self.s,
            if self.p & 0x80 != 0 {'N'} else {'n'},
            if self.p & 0x40 != 0 {'V'} else {'v'},
            if self.p & 0x20 != 0 {'M'} else {'m'},
            if self.p & 0x10 != 0 {'X'} else {'x'},
            if self.p & 0x08 != 0 {'D'} else {'d'},
            if self.p & 0x04 != 0 {'I'} else {'i'},
            if self.p & 0x02 != 0 {'Z'} else {'z'},
            if self.p & 0x01 != 0 {'C'} else {'c'},
            self.a, self.x, self.y, self.dbr, self.d, self.pbr, self.e,
            self.ram.iter().map(|(addr, data)| format!("{:06X}:{:02X}", addr, data)).collect::<Vec<_>>().join(", "))
    }
}

impl State {
    fn make_cpu(&self) -> Cpu {
        let mut cpu = Cpu::default();
        cpu.regs.a = self.a;
        cpu.regs.x = self.x;
        cpu.regs.y = self.y;
        cpu.regs.pc = self.pc;
        cpu.regs.s = self.s;
        cpu.regs.p = self.p.into();
        cpu.regs.e = match self.e {
            0 => false,
            1 => true,
            _ => panic!("Invalid E value"),
        };
        cpu.regs.d = self.d;
        cpu.regs.db = self.dbr;
        cpu.regs.pb = self.pbr;
        cpu
    }

    fn from_cpu(cpu: &Cpu, ctx: &Context) -> Self {
        Self {
            pc: cpu.regs.pc,
            s: cpu.regs.s,
            p: cpu.regs.p.into(),
            a: cpu.regs.a,
            x: cpu.regs.x,
            y: cpu.regs.y,
            dbr: cpu.regs.db,
            d: cpu.regs.d,
            pbr: cpu.regs.pb,
            e: cpu.regs.e as u8,
            ram: ctx.ram.iter().map(|(addr, data)| (*addr, *data)).collect(),
        }
    }
}

type BusAccess = (Option<u32>, Option<u8>, String);

type BusLog = Vec<(Option<u32>, Option<u8>, BusAccessAttr)>;

enum TestResult {
    Pass,
    Fail(State, BusLog),
}

struct Context {
    ram: BTreeMap<u32, u8>,
    interrupt_ctrl: context::InterruptCtrl,
    counter: u64,
    bus_log: BusLog,
    invalid_access: bool,
}

#[derive(Debug, Clone, Copy)]
struct BusAccessAttr {
    valid: bool,
    rw: ReadWrite,
}

impl BusAccessAttr {
    fn read(valid: bool) -> Self {
        Self {
            valid,
            rw: ReadWrite::Read,
        }
    }
    fn write(valid: bool) -> Self {
        Self {
            valid,
            rw: ReadWrite::Write,
        }
    }
}

fn test_bus_access_attr(actual: &BusAccessAttr, expect: &str) -> bool {
    if actual.valid && !(expect.contains(|c| c == 'd' || c == 'p' || c == 'v')) {
        return false;
    }

    if !match actual.rw {
        ReadWrite::Read => expect.chars().nth(3) == Some('r'),
        ReadWrite::Write => expect.chars().nth(3) == Some('w'),
    } {
        return false;
    }

    true
}

fn test_bus_log(actual: &BusLog, expect: &[(Option<u32>, Option<u8>, String)]) -> bool {
    if actual.len() != expect.len() {
        return false;
    }

    // for i in 0..actual.len() {
    //     // Bus contents does not matter while internal cycle
    //     if actual[i].2.valid {
    //         if Some(actual[i].0) != expect[i].0 {
    //             return false;
    //         }
    //         if actual[i].1 != expect[i].1 {
    //             return false;
    //         }
    //     }
    //     if !test_bus_access_attr(&actual[i].2, &expect[i].2) {
    //         return false;
    //     }
    // }

    true
}

#[derive(Debug, Clone, Copy)]
enum ReadWrite {
    Read,
    Write,
}

impl Context {
    fn new(ram: &[(u32, u8)]) -> Context {
        Context {
            ram: ram.iter().cloned().collect(),
            interrupt_ctrl: Default::default(),
            counter: 0,
            bus_log: Vec::new(),
            invalid_access: false,
        }
    }
}

impl context::Bus for Context {
    fn read(&mut self, addr: u32) -> u8 {
        let data = if let Some(data) = self.ram.get(&addr).cloned() {
            self.bus_log
                .push((Some(addr), Some(data), BusAccessAttr::read(true)));
            data
        } else {
            self.bus_log
                .push((Some(addr), None, BusAccessAttr::read(true)));
            self.invalid_access = true;
            0xAA
        };
        self.counter += 8;
        data
    }

    fn write(&mut self, addr: u32, data: u8) {
        self.counter += 8;
        self.ram.insert(addr, data);
        self.bus_log
            .push((Some(addr), Some(data), BusAccessAttr::write(true)));
    }

    fn bus_locked(&self) -> bool {
        false
    }

    fn bus(&self) -> &super_sabicom::bus::Bus {
        panic!()
    }

    fn bus_mut(&mut self) -> &mut super_sabicom::bus::Bus {
        panic!()
    }

    fn read_pure(&self, _addr: u32) -> Option<u8> {
        panic!()
    }

    fn bus_tick(&mut self) {
        panic!()
    }
}

impl context::Interrupt for Context {
    fn interrupt(&self) -> &context::InterruptCtrl {
        &self.interrupt_ctrl
    }

    fn interrupt_mut(&mut self) -> &mut context::InterruptCtrl {
        &mut self.interrupt_ctrl
    }
}

impl context::Timing for Context {
    fn now(&self) -> u64 {
        self.counter
    }

    fn elapse(&mut self, clock: u64) {
        for _ in 0..clock / 6 {
            let (addr, _, attr) = self.bus_log[self.bus_log.len() - 1];
            self.bus_log.push((
                addr,
                None,
                BusAccessAttr {
                    valid: false,
                    ..attr
                },
            ));
        }
        self.counter += clock;
    }
}

fn test_cpu(test_case: &TestCase) -> TestResult {
    let mut cpu = test_case.initial.make_cpu();
    let mut ctx = Context::new(&test_case.initial.ram);

    cpu.exec_one(&mut ctx);
    if cpu.halt || cpu.stop {
        ctx.bus_log.push((None, None, BusAccessAttr::read(false)));
    }

    let result = State::from_cpu(&cpu, &ctx);

    if result == test_case.final_ && test_bus_log(&ctx.bus_log, &test_case.cycles) {
        TestResult::Pass
    } else {
        TestResult::Fail(result, ctx.bus_log)
    }
}

fn report_failed(state: &State, bus_log: &BusLog, test_case: &TestCase) {
    println!("Test case: {}", test_case.name);
    println!("  initial:  {}", test_case.initial);

    let exp = format!("  expected: {}", test_case.final_);
    let act = format!("  actual:   {}", state);

    println!("{exp}");
    println!("{act}");

    print_diff(&exp, &act);

    let ref_log = format!(
        "  ref log:  {}",
        test_case
            .cycles
            .iter()
            .map(|(addr, data, attr)| format!(
                "{}:{} {attr}",
                addr.map_or_else(|| "------".to_string(), |addr| format!("{addr:06X}")),
                data.map_or_else(|| "--".to_string(), |addr| format!("{addr:02X}"))
            ))
            .collect::<Vec<_>>()
            .join(", ")
    );
    let act_log = format!(
        "  act log:  {}",
        bus_log
            .iter()
            .map(|(addr, data, attr)| format!(
                "{}:{} {d}..{rw}....",
                addr.map_or_else(|| "------".to_string(), |addr| format!("{addr:06X}")),
                data.map_or_else(|| "--".to_string(), |addr| format!("{addr:02X}")),
                d = if attr.valid { 'd' } else { '-' },
                rw = match attr.rw {
                    ReadWrite::Read => 'r',
                    ReadWrite::Write => 'w',
                }
            ))
            .collect::<Vec<_>>()
            .join(", ")
    );

    println!("{ref_log}");
    println!("{act_log}");
    print_diff(&ref_log, &act_log);

    println!();
}

fn print_diff(expect: &str, actual: &str) {
    let len = expect.len().max(actual.len());
    let mut diff = " ".repeat(12);
    for i in 12..len {
        if actual.get(i..i + 1) != Some(".") && expect.get(i..i + 1) != actual.get(i..i + 1) {
            diff.push('^');
        } else {
            diff.push(' ');
        }
    }

    if diff.chars().any(|c| c != ' ') {
        println!("{diff}");
    }
}

#[argopt::cmd]
fn main(data_dir: PathBuf, opcode: Option<String>) -> anyhow::Result<()> {
    let mut files = vec![];
    for entry in read_dir(data_dir)? {
        let entry = entry?;
        let path = entry.path();
        files.push(path);
    }

    files.sort();

    let results = files
        .par_iter()
        .map(
            |file| -> anyhow::Result<Option<Vec<(TestCase, TestResult)>>> {
                if let Some(opcode) = &opcode {
                    let opcode = opcode.to_lowercase();
                    if !(file.ends_with(format!("{opcode}.e.json"))
                        || file.ends_with(format!("{opcode}.n.json")))
                    {
                        return Ok(None);
                    }
                }

                eprintln!("Testing {}...", file.display());
                let mut tests: Vec<TestCase> = serde_json::from_reader(File::open(&file)?)?;

                // Why test case's RAMs are not sorted?
                for test in tests.iter_mut() {
                    test.initial.ram.sort();
                    test.final_.ram.sort();
                }

                let mut ret = vec![];

                for test in tests {
                    let result = test_cpu(&test);
                    ret.push((test, result));
                }

                Ok(Some(ret))
            },
        )
        .collect::<Vec<_>>();

    let mut summary = (0, 0);

    for (file, results) in files.iter().zip(results) {
        if let Err(err) = &results {
            println!("Test filed: {}: {:?}", file.display(), err);
        }

        let results = results.unwrap();
        if results.is_none() {
            continue;
        }

        let results = results.unwrap();
        let mut failed = vec![];

        for (test, result) in results.iter() {
            match result {
                TestResult::Pass => {}
                TestResult::Fail(state, bus_log) => failed.push((test, state, bus_log)),
            }
        }

        if failed.len() != 0 {
            println!(
                "Test {}: {} / {} Failed",
                file.display(),
                failed.len(),
                results.len()
            );
        }

        summary.0 += failed.len();
        summary.1 += results.len();

        if opcode.is_some() && !failed.is_empty() {
            println!("Failed tests:");
            for (test, state, bus_log) in failed.iter().take(10) {
                report_failed(&state, &bus_log, &test);
            }
        }
    }

    println!("Summary: {} / {} Failed", summary.0, summary.1);

    Ok(())
}
