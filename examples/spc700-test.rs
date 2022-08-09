use rayon::prelude::*;
use serde::Deserialize;
use std::collections::BTreeSet;
use std::fmt::Display;
use std::fs::{read_dir, File};
use std::path::PathBuf;

use super_sabicom::spc::{self, Spc};

#[derive(Deserialize)]
struct TestCase {
    name: String,
    initial: State,
    #[serde(rename = "final")]
    final_: State,
    cycles: BusLog,
}

#[derive(PartialEq, Eq, Deserialize, Debug)]
struct State {
    pc: u16,
    a: u8,
    x: u8,
    y: u8,
    sp: u8,
    psw: u8,
    ram: Vec<(u16, u8)>,
}

impl Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,
            "PC: {:04X}, SP: {:04X}, P: {}{}{}{}{}{}{}{}, A: {:02X}, X: {:02X}, Y: {:02X}, ram: [{}]",
            self.pc,
            self.sp,
            if self.psw & 0x80 != 0 {'N'} else {'n'},
            if self.psw & 0x40 != 0 {'V'} else {'v'},
            if self.psw & 0x20 != 0 {'P'} else {'p'},
            if self.psw & 0x10 != 0 {'B'} else {'b'},
            if self.psw & 0x08 != 0 {'H'} else {'h'},
            if self.psw & 0x04 != 0 {'I'} else {'i'},
            if self.psw & 0x02 != 0 {'Z'} else {'z'},
            if self.psw & 0x01 != 0 {'C'} else {'c'},
            self.a, self.x, self.y, self.ram.iter().map(|(addr, data)| format!("{:06X}:{:02X}", addr, data)).collect::<Vec<_>>().join(", "))
    }
}

impl State {
    fn make_spc(&self) -> Spc {
        let mut spc = Spc::default();
        spc.regs.a = self.a;
        spc.regs.x = self.x;
        spc.regs.y = self.y;
        spc.regs.pc = self.pc;
        spc.regs.sp = self.sp;
        spc.regs.psw = self.psw.into();
        spc.ioregs.rom_enable = false;

        for (addr, data) in self.ram.iter() {
            spc.dsp.ram[*addr as usize] = *data;
        }

        spc
    }

    fn from_cpu(spc: &Spc, init_ram: &[(u16, u8)]) -> Self {
        let valid_addrs = spc
            .bus_log
            .iter()
            .filter_map(|e| {
                if matches!(e.2, spc::BusAccessType::Write) {
                    e.0
                } else {
                    None
                }
            })
            .chain(init_ram.iter().map(|e| e.0))
            .collect::<BTreeSet<_>>();

        Self {
            pc: spc.regs.pc,
            sp: spc.regs.sp,
            psw: spc.regs.psw.into(),
            a: spc.regs.a,
            x: spc.regs.x,
            y: spc.regs.y,
            ram: valid_addrs
                .into_iter()
                .map(|addr| (addr, spc.dsp.ram[addr as usize]))
                .collect(),
        }
    }
}

type BusAccess = (Option<u32>, Option<u8>, BusAccessType);

#[derive(Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
enum BusAccessType {
    Read,
    Write,
    Wait,
}

type BusLog = Vec<BusAccess>;
type SpcBusLog = Vec<(Option<u16>, Option<u8>, spc::BusAccessType)>;

enum TestResult {
    Pass,
    Fail(State, SpcBusLog),
}

fn test_bus_access_attr(actual: &spc::BusAccessType, expect: &BusAccessType) -> bool {
    match (actual, expect) {
        (spc::BusAccessType::Read, BusAccessType::Read) => true,
        (spc::BusAccessType::Write, BusAccessType::Write) => true,
        (spc::BusAccessType::Wait, BusAccessType::Wait) => true,
        _ => false,
    }
}

fn test_bus_log(actual: &SpcBusLog, expect: &BusLog) -> bool {
    if actual.len() != expect.len() {
        return false;
    }

    for i in 0..actual.len() {
        if !test_bus_access_attr(&actual[i].2, &expect[i].2) {
            return false;
        }
    }

    true
}

fn test_cpu(test_case: &TestCase) -> TestResult {
    let mut spc = test_case.initial.make_spc();

    spc.exec_one();

    let result = State::from_cpu(&spc, &test_case.initial.ram);

    if result == test_case.final_ && test_bus_log(&spc.bus_log, &test_case.cycles) {
        TestResult::Pass
    } else {
        TestResult::Fail(result, spc.bus_log.clone())
    }
}

fn report_failed(state: &State, bus_log: &SpcBusLog, test_case: &TestCase) {
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
                "{}:{} {attr:5?}",
                addr.map_or_else(|| "----".to_string(), |addr| format!("{addr:04X}")),
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
                "{}:{} {attr:5?}",
                addr.map_or_else(|| "----".to_string(), |addr| format!("{addr:04X}")),
                data.map_or_else(|| "--".to_string(), |addr| format!("{addr:02X}")),
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
                    if !file.ends_with(format!("{opcode}.json")) {
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
