// Test helper: mock cardano-node that writes a line that is not valid UTF-8
// (a Latin-1 "ü", as a Windows user name in the active code page would appear)
// and then keeps writing. Every write result is recorded in
// `<socket_path>.writes` so tests can verify the watchdog kept its end of the
// stdout pipe open.
//
// Usage: mock-node-non-utf8-output <socket_path> [before-startup]
//   With `before-startup`, the bad line is written ahead of the startup phase
//   lines; otherwise it follows them.
use std::io::Write as _;

fn main() {
    let mut args = std::env::args().skip(1);
    let socket_path = args.next().expect("socket path required");
    let bad_line_first = args.next().as_deref() == Some("before-startup");
    let mut record = std::fs::File::create(format!("{socket_path}.writes")).unwrap();
    let mut out = std::io::stdout();

    let mut write = |bytes: &[u8]| {
        let result = out.write_all(bytes).and_then(|_| out.flush());
        match result {
            Ok(()) => writeln!(record, "ok"),
            Err(e) => writeln!(record, "err {e}"),
        }
        .unwrap();
    };

    let bad_line: &[u8] = b"ChainDB path C:\\Users\\J\xFCrgen\\chain\n";
    let startup: [&[u8]; 8] = [
        b"StartedOpeningDB\n",
        b"StartedOpeningImmutableDB\n",
        b"OpenedImmutableDB\n",
        b"StartedOpeningVolatileDB\n",
        b"OpenedVolatileDB\n",
        b"StartedOpeningLgrDB\n",
        b"OpenedLgrDB\n",
        b"OpenedDB\n",
    ];

    if bad_line_first {
        write(bad_line);
    }
    for line in startup {
        write(line);
    }
    if !bad_line_first {
        write(bad_line);
    }
    std::fs::File::create(&socket_path).expect("create socket file");

    // Keep writing after the bad line; a closed pipe shows up as an error.
    for i in 0..20 {
        std::thread::sleep(std::time::Duration::from_millis(50));
        write(format!("after bad line {i}\n").as_bytes());
    }
    writeln!(record, "done").unwrap();

    // Block on shutdown pipe (fd 3) until the watchdog closes the write end.
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
}
