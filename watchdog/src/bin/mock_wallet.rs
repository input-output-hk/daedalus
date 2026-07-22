// Test helper: mock cardano-wallet.
// Usage: mock-wallet <port> [other args...]
// Binds the TCP port (signals readiness to the watchdog's wait_for_port) and hangs.
fn main() {
    let port: u16 = std::env::args()
        .nth(1)
        .expect("port required")
        .parse()
        .expect("valid port number");
    let listener =
        std::net::TcpListener::bind(("127.0.0.1", port)).expect("bind port");
    for stream in listener.incoming() {
        drop(stream);
    }
}
