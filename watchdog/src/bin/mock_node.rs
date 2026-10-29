// Test helper: mock cardano-node.
// Usage: mock-node <socket_path> [--shutdown-ipc 3] (watchdog always appends --shutdown-ipc 3)
// Creates socket_path then blocks on fd 3 until EOF (watchdog closing the shutdown pipe).
//
// With MOCK_NODE_IGNORE_SHUTDOWN=1 in the environment it keeps running after
// that EOF, like a node that does not act on the stop request, until killed.
// With MOCK_NODE_EXIT_DELAY_MS=<n> it exits n milliseconds after the EOF, like
// a node that takes a while to close its database.
// With MOCK_NODE_CRASH_AFTER_READY_MS=<n> it exits with code 1 n milliseconds
// after reporting chainDbReady, like a node that crashes once running.
// With MOCK_NODE_EXTRA_LINES=<file> it prints that file's contents to stdout
// after the startup phases, so a test can inject log lines.
// With MOCK_NODE_UNTERMINATED_LINE=<file> it stops after opening the ledger
// database begins, writes that file's contents to stderr without the final
// newline, and hangs there until the shutdown pipe closes, so a test can
// leave a line unterminated while the node runs. Once a file named
// <socket_path>.finish exists, it writes the newline and keeps hanging.
//
// Given --topology <file>, it prints the path and, on one line, the contents of
// that file, so a test can assert on the topology the node was started with.
use std::io::Write as _;

fn main() {
    let socket_path = std::env::args().nth(1).expect("socket path required");
    if let Some(parent) = std::path::Path::new(&socket_path).parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    // Emit the full startup phase sequence so the state machine reaches chainDbReady.
    // Echo the host name the watchdog gave the node, so tests can assert on it.
    println!(
        "TRACE_DISPATCHER_LOGGING_HOSTNAME={}",
        std::env::var("TRACE_DISPATCHER_LOGGING_HOSTNAME").unwrap_or_default()
    );
    echo_topology();
    println!("StartedOpeningDB");
    println!("StartedOpeningImmutableDB");
    println!("OpenedImmutableDB");
    println!("StartedOpeningVolatileDB");
    println!("OpenedVolatileDB");
    println!("StartedOpeningLgrDB");
    if let Some(file) = std::env::var_os("MOCK_NODE_UNTERMINATED_LINE") {
        std::io::stdout().flush().unwrap();
        hang_on_unterminated_line(&socket_path, &file);
        return;
    }
    println!("OpenedLgrDB");
    println!("OpenedDB");
    if let Some(extra) = std::env::var_os("MOCK_NODE_EXTRA_LINES") {
        print!(
            "{}",
            std::fs::read_to_string(extra).expect("read MOCK_NODE_EXTRA_LINES")
        );
    }
    std::io::stdout().flush().unwrap();
    std::fs::File::create(&socket_path).expect("create socket file");

    if let Some(ms) = std::env::var("MOCK_NODE_CRASH_AFTER_READY_MS")
        .ok()
        .and_then(|v| v.parse::<u64>().ok())
    {
        std::thread::sleep(std::time::Duration::from_millis(ms));
        std::process::exit(1);
    }

    wait_for_shutdown();

    if let Some(ms) = std::env::var("MOCK_NODE_EXIT_DELAY_MS")
        .ok()
        .and_then(|v| v.parse::<u64>().ok())
    {
        std::thread::sleep(std::time::Duration::from_millis(ms));
    }
    if std::env::var("MOCK_NODE_IGNORE_SHUTDOWN").as_deref() == Ok("1") {
        loop {
            std::thread::sleep(std::time::Duration::from_secs(3600));
        }
    }
}

/// Print the path given as `--topology` and the compact JSON in that file.
fn echo_topology() {
    let args: Vec<String> = std::env::args().collect();
    let Some(path) = args
        .windows(2)
        .find(|w| w[0] == "--topology")
        .map(|w| &w[1])
    else {
        return;
    };
    println!("MOCK_NODE_TOPOLOGY_PATH={path}");
    let contents = std::fs::read_to_string(path).expect("read the topology file");
    let value: serde_json::Value = serde_json::from_str(&contents).expect("parse the topology");
    println!("MOCK_NODE_TOPOLOGY={value}");
}

/// Write `file` to stderr without its final newline, write the newline once
/// `<socket_path>.finish` exists, and return when the shutdown pipe closes.
fn hang_on_unterminated_line(socket_path: &str, file: &std::ffi::OsStr) {
    let text = std::fs::read_to_string(file).expect("read MOCK_NODE_UNTERMINATED_LINE");
    let mut err = std::io::stderr();
    err.write_all(text.trim_end_matches('\n').as_bytes())
        .unwrap();
    err.flush().unwrap();
    let finish = std::path::PathBuf::from(format!("{socket_path}.finish"));
    std::thread::spawn(move || {
        while !finish.exists() {
            std::thread::sleep(std::time::Duration::from_millis(20));
        }
        let mut err = std::io::stderr();
        err.write_all(b"\n").unwrap();
        err.flush().unwrap();
    });
    wait_for_shutdown();
}

/// Block until the watchdog closes the shutdown pipe.
fn wait_for_shutdown() {
    #[cfg(unix)]
    {
        use std::io::Read;
        use std::os::unix::io::FromRawFd;
        let mut pipe = unsafe { std::fs::File::from_raw_fd(3) };
        let mut buf = [0u8; 64];
        while pipe.read(&mut buf).unwrap_or(0) > 0 {}
    }
    #[cfg(not(unix))]
    {
        // On Windows the watchdog passes --shutdown-ipc 0 and binds the read
        // end of an anonymous pipe as stdin.  Reading until EOF mirrors the
        // Unix fd-3 behaviour: we exit when the watchdog closes the write end.
        use std::io::Read;
        let mut buf = [0u8; 64];
        while std::io::stdin().read(&mut buf).unwrap_or(0) > 0 {}
    }
}
