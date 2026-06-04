// Test helper: mock cardano-node that never creates the socket file.
// Blocks on the shutdown pipe (fd 3) so it exits cleanly when the watchdog
// closes the write end. Used to test the stop-during-socket-wait path.

fn main() {
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
        // On non-Unix, just sleep until killed.
        std::thread::sleep(std::time::Duration::from_secs(300));
    }
}
