// Test helper: mock snapshot-converter that creates output dirs, writes a
// sentinel file (lsm/CONVERTER_DONE) after the lsm import step to signal
// natural completion, then exits 0.  Used by cancel_before_cutover_gate to
// synchronize precisely on both converter steps completing — the test polls
// for the sentinel, then sends cancel into the still-open cutover gate window.

use std::env;
use std::fs;

fn get_flag(args: &[String], flag: &str) -> Option<String> {
    args.windows(2).find(|w| w[0] == flag).map(|w| w[1].clone())
}

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.get(1).map(|s| s.as_str()) {
        Some("convert") => {
            if let Some(p) = get_flag(&args, "--snapshot-out") {
                fs::create_dir_all(&p).unwrap_or_else(|e| panic!("create {p}: {e}"));
            }
            if let Some(p) = get_flag(&args, "--lsm-export-to") {
                fs::create_dir_all(&p).unwrap_or_else(|e| panic!("create {p}: {e}"));
            }
        }
        Some("lsm") if args.get(2).map(|s| s.as_str()) == Some("import") => {
            let lsm_db = get_flag(&args, "--lsm-database").expect("--lsm-database required");
            fs::create_dir_all(&lsm_db).unwrap_or_else(|e| panic!("create {lsm_db}: {e}"));
            // Sentinel written last so the test sees it only after all output is ready.
            fs::write(format!("{lsm_db}/CONVERTER_DONE"), b"").unwrap();
        }
        _ => {
            eprintln!(
                "mock-snapshot-converter-sentinel: unknown subcommand {:?}",
                args.get(1)
            );
            std::process::exit(1);
        }
    }
}
