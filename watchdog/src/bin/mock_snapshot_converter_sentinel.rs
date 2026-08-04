// Test helper: mock snapshot-converter that creates output dirs, writes a
// sentinel file (lsm/CONVERTER_DONE) to signal natural completion, then
// exits 0.  Used by cancel_before_cutover_gate to synchronize precisely
// on converter exit — the test polls for the sentinel, then sends cancel
// into the still-open cutover gate window.

use std::env;
use std::fs;

fn get_flag(args: &[String], flag: &str) -> Option<String> {
    args.windows(2).find(|w| w[0] == flag).map(|w| w[1].clone())
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if let Some(p) = get_flag(&args, "--output-lsm-snapshot") {
        fs::create_dir_all(&p).unwrap_or_else(|e| panic!("create {p}: {e}"));
    }
    let lsm_db = get_flag(&args, "--output-lsm-database").expect("--output-lsm-database required");
    fs::create_dir_all(&lsm_db).unwrap_or_else(|e| panic!("create {lsm_db}: {e}"));
    // Sentinel written last so the test sees it only after all output is ready.
    fs::write(format!("{lsm_db}/CONVERTER_DONE"), b"").unwrap();
}
