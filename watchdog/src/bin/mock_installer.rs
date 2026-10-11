// Test helper: mock update installer.
// Usage: mock-installer <out_file> [pid...]
// Writes one line per PID to out_file, "<pid> running" or "<pid> exited",
// as it finds them when it starts, so a test can tell whether the installer
// started while cardano-node or cardano-wallet was still running.
fn main() {
    let mut args = std::env::args().skip(1);
    let out = args.next().expect("output file required");
    let mut report = String::new();
    for pid in args {
        let running = is_running(pid.parse().expect("numeric PID"));
        let state = if running { "running" } else { "exited" };
        report.push_str(&format!("{pid} {state}\n"));
    }
    std::fs::write(out, report).expect("write report");
}

#[cfg(unix)]
fn is_running(pid: i32) -> bool {
    // Signal 0 only checks that the process exists.
    unsafe { libc::kill(pid, 0) == 0 }
}

#[cfg(not(unix))]
fn is_running(_pid: i32) -> bool {
    false
}
