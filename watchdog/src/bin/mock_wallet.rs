// Test helper: mock cardano-wallet.
// Usage: mock-wallet <port> [other args...]
// Binds the TCP port (signals readiness to the watchdog's wait_for_port) and hangs.
//
// Like `cardano-wallet serve --shutdown-handler`, when that flag is among its
// arguments it exits with code 0 once its stdin reaches end-of-file. When
// MOCK_WALLET_EXIT_FILE is set, it writes "stdin-eof" to that file before
// exiting that way, so a test can tell a clean stop from a kill.
fn main() {
    let port: u16 = std::env::args()
        .nth(1)
        .expect("port required")
        .parse()
        .expect("valid port number");
    let listener = std::net::TcpListener::bind(("127.0.0.1", port)).expect("bind port");

    if std::env::args().any(|a| a == "--shutdown-handler") {
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

    for stream in listener.incoming() {
        drop(stream);
    }
}
