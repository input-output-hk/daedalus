// Test helper: mock cardano-node that emits startup-phase log lines before
// (and after) creating the socket file, so integration tests can verify that
// the watchdog emits the correct node_startup_status and
// node_block_sync_progress events.
//
// Usage: mock-node-startup-log <socket_path>
//   (same positional-arg convention as mock-node)

use std::io::Read;

fn main() {
    let socket_path = std::env::args().nth(1).expect("socket path required");

    // Emit startup phases to stdout BEFORE the socket exists, so the watchdog
    // can parse them from the log pipe while it is waiting for the socket.
    println!("StartedOpeningDB");
    println!("StartedOpeningImmutableDB");
    println!("OpenedImmutableDB");
    println!("StartedOpeningVolatileDB");
    println!("OpenedVolatileDB");
    println!("StartedOpeningLgrDB");
    println!("ReplayFromGenesis");
    println!("Replayed block Progress: 25.00%");
    println!("Replayed block Progress: 50.00%");
    println!("Replayed block Progress: 75.00%");
    println!("Replayed block Progress: 100.00%");
    println!("OpenedLgrDB");
    println!("OpenedDB");

    // Now create the socket so wait_for_socket() unblocks.
    if let Some(parent) = std::path::Path::new(&socket_path).parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    std::fs::File::create(&socket_path).expect("create socket file");

    // Block on shutdown pipe (fd 3) until the watchdog closes the write end.
    #[cfg(unix)]
    {
        use std::os::unix::io::FromRawFd;
        let mut pipe = unsafe { std::fs::File::from_raw_fd(3) };
        let mut buf = [0u8; 64];
        while pipe.read(&mut buf).unwrap_or(0) > 0 {}
    }
    #[cfg(not(unix))]
    std::thread::sleep(std::time::Duration::from_secs(9999));
}
