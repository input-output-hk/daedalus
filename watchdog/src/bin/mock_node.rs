// Test helper: mock cardano-node.
// Usage: mock-node <socket_path> [--shutdown-ipc 3] (watchdog always appends --shutdown-ipc 3)
// Creates socket_path then blocks on fd 3 until EOF (watchdog closing the shutdown pipe).
use std::io::Read;
use std::os::unix::io::FromRawFd;

fn main() {
    let socket_path = std::env::args().nth(1).expect("socket path required");
    if let Some(parent) = std::path::Path::new(&socket_path).parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    std::fs::File::create(&socket_path).expect("create socket file");

    let mut pipe = unsafe { std::fs::File::from_raw_fd(3) };
    let mut buf = [0u8; 64];
    while pipe.read(&mut buf).unwrap_or(0) > 0 {}
}
