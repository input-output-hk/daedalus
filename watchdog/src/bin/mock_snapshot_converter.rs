// Test helper: mock snapshot-converter.
//
// Handles the two-step pipeline:
//   convert --snapshot-in X --snapshot-out Y --lsm-export-to Z --config C
//   lsm import --lsm-database D --lsm-import-from E --snapshot S
//
// Creates the required output directories and exits 0.

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
            if let Some(p) = get_flag(&args, "--lsm-database") {
                fs::create_dir_all(&p).unwrap_or_else(|e| panic!("create {p}: {e}"));
            }
        }
        _ => {
            eprintln!(
                "mock-snapshot-converter: unknown subcommand {:?}",
                args.get(1)
            );
            std::process::exit(1);
        }
    }
}
