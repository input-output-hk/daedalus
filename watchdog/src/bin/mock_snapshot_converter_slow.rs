// Test helper: mock snapshot-converter that creates output dirs then sleeps
// until killed. Used to test cancellation during the conversion phase.

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
            // Sleep indefinitely; the watchdog kills this process on cancellation.
            std::thread::sleep(std::time::Duration::from_secs(300));
        }
        Some("lsm") if args.get(2).map(|s| s.as_str()) == Some("import") => {
            if let Some(p) = get_flag(&args, "--lsm-database") {
                fs::create_dir_all(&p).unwrap_or_else(|e| panic!("create {p}: {e}"));
            }
            std::thread::sleep(std::time::Duration::from_secs(300));
        }
        _ => {
            eprintln!(
                "mock-snapshot-converter-slow: unknown subcommand {:?}",
                args.get(1)
            );
            std::process::exit(1);
        }
    }
}
