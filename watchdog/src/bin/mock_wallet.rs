// Test helper: mock cardano-wallet.
// Usage: mock-wallet <port> [other args...]
// Binds the TCP port (signals readiness to the watchdog's wait_for_port) and hangs.
//
// Like `cardano-wallet serve --shutdown-handler`, when that flag is among its
// arguments it exits with code 0 once its stdin reaches end-of-file. When
// MOCK_WALLET_EXIT_FILE is set, it writes "stdin-eof" to that file before
// exiting that way, so a test can tell a clean stop from a kill.
// With MOCK_WALLET_CRASH_AFTER_MS=<n> it exits with code 1 n milliseconds
// after binding its port, like a wallet that crashes once running.
// With MOCK_WALLET_EXTRA_LINES=<file> it prints that file's contents to stdout
// after binding its port, so a test can inject log lines.
//
// Modes for the health check, all off unless set:
// MOCK_WALLET_HTTP=1                answers each connection with "HTTP/1.1 200 OK"
//                                   and closes it, like a healthy wallet.
// MOCK_WALLET_CLOSE_AFTER_MS=<n>    n ms after binding it closes its listener and
//                                   stays alive: the server is gone, the process
//                                   is not.
// MOCK_WALLET_MUTE_AFTER_MS=<n>     n ms after binding it keeps its listener but
//                                   reads and answers nothing: a hung server.
// MOCK_WALLET_IGNORE_STDIN=1        does not exit when stdin reaches end-of-file,
//                                   so only a kill stops it.
// MOCK_WALLET_HEALTHY_SENTINEL=<f>  the faults above apply only when <f> does not
//                                   exist; the mock creates it at start. A second
//                                   process then runs healthy.
use std::io::{Read, Write};
use std::net::TcpStream;
use std::time::{Duration, Instant};

fn env_ms(name: &str) -> Option<Duration> {
    std::env::var(name)
        .ok()
        .and_then(|v| v.parse::<u64>().ok())
        .map(Duration::from_millis)
}

fn answer(mut stream: TcpStream) {
    std::thread::spawn(move || {
        let mut buf = [0u8; 1024];
        let _ = stream.read(&mut buf);
        let _ =
            stream.write_all(b"HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: close\r\n\r\n");
    });
}

fn main() {
    let port: u16 = std::env::args()
        .nth(1)
        .expect("port required")
        .parse()
        .expect("valid port number");
    let listener = std::net::TcpListener::bind(("127.0.0.1", port)).expect("bind port");

    if let Some(extra) = std::env::var_os("MOCK_WALLET_EXTRA_LINES") {
        use std::io::Write as _;
        print!(
            "{}",
            std::fs::read_to_string(extra).expect("read MOCK_WALLET_EXTRA_LINES")
        );
        std::io::stdout().flush().unwrap();
    }

    if let Some(ms) = std::env::var("MOCK_WALLET_CRASH_AFTER_MS")
        .ok()
        .and_then(|v| v.parse::<u64>().ok())
    {
        std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(ms));
            std::process::exit(1);
        });
    }

    if std::env::args().any(|a| a == "--shutdown-handler")
        && std::env::var_os("MOCK_WALLET_IGNORE_STDIN").is_none()
    {
        std::thread::spawn(|| {
            use std::io::Read;
            let mut buf = [0u8; 64];
            while std::io::stdin().read(&mut buf).unwrap_or(0) > 0 {}
            if let Ok(path) = std::env::var("MOCK_WALLET_EXIT_FILE") {
                let _ = std::fs::write(path, "stdin-eof");
            }
            std::process::exit(0);
        });
    }

    let http = std::env::var_os("MOCK_WALLET_HTTP").is_some();
    let faulty = match std::env::var_os("MOCK_WALLET_HEALTHY_SENTINEL") {
        Some(path) => {
            let first = !std::path::Path::new(&path).exists();
            let _ = std::fs::write(&path, b"");
            first
        }
        None => true,
    };
    let close_at = env_ms("MOCK_WALLET_CLOSE_AFTER_MS")
        .filter(|_| faulty)
        .map(|d| Instant::now() + d);
    let mute_at = env_ms("MOCK_WALLET_MUTE_AFTER_MS")
        .filter(|_| faulty)
        .map(|d| Instant::now() + d);

    if close_at.is_none() && mute_at.is_none() {
        for stream in listener.incoming() {
            match stream {
                Ok(s) if http => answer(s),
                other => drop(other),
            }
        }
        return;
    }

    // A fault is scheduled: poll, so the listener can be dropped from here.
    listener
        .set_nonblocking(true)
        .expect("nonblocking listener");
    let mut held = Vec::new();
    loop {
        let now = Instant::now();
        if close_at.is_some_and(|t| now >= t) {
            drop(listener);
            drop(held);
            loop {
                std::thread::sleep(Duration::from_secs(3600));
            }
        }
        match listener.accept() {
            // Muted: keep the connection open and say nothing.
            Ok((s, _)) if mute_at.is_some_and(|t| now >= t) => held.push(s),
            Ok((s, _)) if http => {
                let _ = s.set_nonblocking(false);
                answer(s);
            }
            Ok((s, _)) => drop(s),
            Err(_) => std::thread::sleep(Duration::from_millis(5)),
        }
    }
}
