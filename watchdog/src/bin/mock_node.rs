// Test helper: mock cardano-node.
// Usage: mock-node <socket_path> [--shutdown-ipc 3] (watchdog always appends --shutdown-ipc 3)
// Creates socket_path then blocks on fd 3 until EOF (watchdog closing the shutdown pipe).
fn main() {
    let socket_path = std::env::args().nth(1).expect("socket path required");
    if let Some(parent) = std::path::Path::new(&socket_path).parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    std::fs::File::create(&socket_path).expect("create socket file");

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
