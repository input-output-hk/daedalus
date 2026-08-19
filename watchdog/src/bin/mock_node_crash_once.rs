// Test helper: mock cardano-node that crashes (exit 1) on the first invocation,
// then behaves like mock-node (creates socket, waits for EOF on fd 3) on every
// subsequent invocation.  State is tracked via a sentinel file written next to
// the socket path.
fn main() {
    let socket_path = std::env::args().nth(1).expect("socket path required");
    let sentinel = format!("{socket_path}.crash_once_done");

    if std::path::Path::new(&sentinel).exists() {
        // Second+ invocation: behave like mock-node.
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
            use std::io::Read;
            let mut buf = [0u8; 64];
            while std::io::stdin().read(&mut buf).unwrap_or(0) > 0 {}
        }
    } else {
        // First invocation: write sentinel then exit 1.
        std::fs::write(&sentinel, b"").expect("write sentinel");
        std::process::exit(1);
    }
}
