// Test helper: mock snapshot-converter that writes far more than a pipe buffer
// holds to stdout and stderr before it finishes, as snapshot-converter does
// when it animates its progress bar. Exits 0 only if every write completes,
// so a watchdog that does not read both streams while it runs never sees it
// exit.

use std::env;
use std::fs;
use std::io::Write;

const OUTPUT_BYTES: usize = 1024 * 1024;

fn get_flag(args: &[String], flag: &str) -> Option<String> {
    args.windows(2).find(|w| w[0] == flag).map(|w| w[1].clone())
}

fn chatter() {
    let frame = "InMemory@[1] [=====>    ] LSM (exported)@[mock]\r";
    let mut out = std::io::stdout().lock();
    let mut err = std::io::stderr().lock();
    let mut written = 0;
    while written < OUTPUT_BYTES {
        out.write_all(frame.as_bytes()).expect("write stdout");
        err.write_all(frame.as_bytes()).expect("write stderr");
        written += frame.len();
    }
    out.flush().expect("flush stdout");
    err.write_all(b"\nmock-snapshot-converter-chatty: done\n")
        .expect("write stderr");
}

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.get(1).map(|s| s.as_str()) {
        Some("convert") => {
            chatter();
            if let Some(p) = get_flag(&args, "--snapshot-out") {
                fs::create_dir_all(&p).unwrap_or_else(|e| panic!("create {p}: {e}"));
            }
            if let Some(p) = get_flag(&args, "--lsm-export-to") {
                fs::create_dir_all(&p).unwrap_or_else(|e| panic!("create {p}: {e}"));
            }
        }
        Some("lsm") if args.get(2).map(|s| s.as_str()) == Some("import") => {
            chatter();
            if let Some(p) = get_flag(&args, "--lsm-database") {
                fs::create_dir_all(&p).unwrap_or_else(|e| panic!("create {p}: {e}"));
            }
        }
        _ => {
            eprintln!(
                "mock-snapshot-converter-chatty: unknown subcommand {:?}",
                args.get(1)
            );
            std::process::exit(1);
        }
    }
}
