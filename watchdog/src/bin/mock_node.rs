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
fn main() {
    let socket_path = std::env::args().nth(1).expect("socket path required");
    if let Some(parent) = std::path::Path::new(&socket_path).parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    // Emit the full startup phase sequence so the state machine reaches chainDbReady.
    use std::io::Write as _;
    // Echo the host name the watchdog gave the node, so tests can assert on it.
    println!(
        "TRACE_DISPATCHER_LOGGING_HOSTNAME={}",
        std::env::var("TRACE_DISPATCHER_LOGGING_HOSTNAME").unwrap_or_default()
    );
    println!("StartedOpeningDB");
    println!("StartedOpeningImmutableDB");
    println!("OpenedImmutableDB");
    println!("StartedOpeningVolatileDB");
    println!("OpenedVolatileDB");
    println!("StartedOpeningLgrDB");
    println!("OpenedLgrDB");
    println!("OpenedDB");
    std::io::stdout().flush().unwrap();
    std::fs::File::create(&socket_path).expect("create socket file");

    if let Some(ms) = std::env::var("MOCK_NODE_CRASH_AFTER_READY_MS")
        .ok()
        .and_then(|v| v.parse::<u64>().ok())
    {
        std::thread::sleep(std::time::Duration::from_millis(ms));
        std::process::exit(1);
    }

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
