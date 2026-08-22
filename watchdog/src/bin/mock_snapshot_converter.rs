// Test helper: mock snapshot-converter.
//
// Usage (mirrors the real binary):
//   mock-snapshot-converter \
//     --input-mem     <slot_dir>      \
//     --output-lsm-snapshot <dir>    \
//     --output-lsm-database <dir>    \
//     --config        <file>
//
// Creates the two output directories and exits 0, which is all run_converter()
// needs to continue.

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
    if let Some(p) = get_flag(&args, "--output-lsm-database") {
        fs::create_dir_all(&p).unwrap_or_else(|e| panic!("create {p}: {e}"));
    }
}
