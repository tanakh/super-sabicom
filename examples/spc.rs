use std::path::PathBuf;

use super_sabicom::{
    context,
    spc::{Spc, SpcFile},
};

#[derive(Default)]
struct Timing {
    counter: u64,
}

impl context::Timing for Timing {
    fn now(&self) -> u64 {
        self.counter
    }

    fn elapse(&mut self, clock: u64) {
        self.counter += clock;
    }
}

#[argopt::cmd]
fn main(spc_file: PathBuf) -> anyhow::Result<()> {
    use std::io::Write;
    env_logger::builder()
        .format(|buf, record| {
            writeln!(
                buf,
                "[{:5} {}] {}",
                record.level(),
                record
                    .module_path()
                    .map_or("", |path| path.trim_start_matches("super_sabicom::")),
                record.args()
            )
        })
        .init();

    let bytes = std::fs::read(spc_file)?;
    let spc_file = SpcFile::from_bytes(&bytes)?;

    println!("SPC File Info:");
    println!("{spc_file}");

    let mut spc = spc_file.make_spc();
    let mut ctx = Timing::default();

    loop {
        use context::Timing;
        ctx.elapse(8);
        spc.tick(&mut ctx);
    }

    Ok(())
}
