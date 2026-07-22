// Integration tests for IPC communication with cardano-watchdog.
//
// Each test spawns the real watchdog binary with mock node/wallet helpers,
// exchanges JSON events over stdout, and sends commands over stdin.

use std::io::{BufRead, BufReader, Write};
use std::net::TcpListener;
use std::process::{Command, Stdio};
use std::sync::mpsc;
use std::time::Duration;

use serde_json::Value;

const EVENT_TIMEOUT: Duration = Duration::from_secs(15);

// ── helpers ──────────────────────────────────────────────────────────────────

fn free_port() -> u16 {
    // Bind port 0 to let the OS assign a free ephemeral port, then release it.
    TcpListener::bind("127.0.0.1:0")
        .unwrap()
        .local_addr()
        .unwrap()
        .port()
}

fn test_dir(name: &str) -> std::path::PathBuf {
    // Include process ID so concurrent test runs don't collide.
    let dir = std::env::temp_dir()
        .join(format!("watchdog-test-{}-{}", name, std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn config_json(
    socket_path: &str,
    state_dir: &str,
    wallet_port: u16,
    node_log: &str,
    wallet_log: &str,
) -> String {
    serde_json::json!({
        "node": {
            "exe":         env!("CARGO_BIN_EXE_mock-node"),
            "args":        [socket_path],
            "state_dir":   state_dir,
            "socket_path": socket_path
        },
        "wallet": {
            "exe":              env!("CARGO_BIN_EXE_mock-wallet"),
            "args":             [wallet_port.to_string()],
            "state_dir":        state_dir,
            "api_port":         wallet_port,
            "restart_delay_ms": 100
        },
        "node_log_file":   node_log,
        "wallet_log_file": wallet_log
    })
    .to_string()
}

// Wraps a running watchdog process with a background thread that forwards
// stdout events to a channel so tests can receive them with a timeout.
struct Watchdog {
    // Wrapped in Option so send_stop can take and drop it, sending EOF to the
    // watchdog's async stdin reader and letting the tokio runtime exit cleanly.
    stdin: Option<std::process::ChildStdin>,
    rx: mpsc::Receiver<Value>,
    child: std::process::Child,
}

impl Watchdog {
    fn start(config: &str) -> Self {
        let mut child = Command::new(env!("CARGO_BIN_EXE_cardano-watchdog"))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .expect("spawn cardano-watchdog");

        let mut stdin_pipe = child.stdin.take().unwrap();
        let stdout = child.stdout.take().unwrap();

        writeln!(stdin_pipe, "{}", config).unwrap();

        let (tx, rx) = mpsc::channel();
        std::thread::spawn(move || {
            for line in BufReader::new(stdout).lines().flatten() {
                if line.is_empty() {
                    continue;
                }
                if let Ok(v) = serde_json::from_str::<Value>(&line) {
                    if tx.send(v).is_err() {
                        break;
                    }
                }
            }
        });

        Self { stdin: Some(stdin_pipe), rx, child }
    }

    fn next(&self) -> Value {
        self.rx
            .recv_timeout(EVENT_TIMEOUT)
            .expect("timed out waiting for watchdog event")
    }

    fn send_stop(&mut self) {
        if let Some(mut stdin) = self.stdin.take() {
            writeln!(stdin, r#"{{"cmd":"stop"}}"#).unwrap();
            // Dropping stdin here closes the pipe, sending EOF to the watchdog's async
            // stdin reader so the tokio runtime can shut down cleanly after stop.
        }
    }

    fn wait(&mut self) {
        self.child.wait().unwrap();
    }
}

// ── tests ─────────────────────────────────────────────────────────────────────

/// Happy path: node starts → wallet starts → wallet ready → stop → stopped.
#[test]
fn lifecycle_stop() {
    let dir = test_dir("lifecycle");
    let socket = dir.join("node.socket");
    let port = free_port();

    let mut wdog = Watchdog::start(&config_json(
        socket.to_str().unwrap(),
        dir.to_str().unwrap(),
        port,
        dir.join("node.log").to_str().unwrap(),
        dir.join("wallet.log").to_str().unwrap(),
    ));

    let ev = wdog.next();
    assert_eq!(ev["event"], "node_started");
    assert!(ev["pid"].as_u64().unwrap() > 0);
    assert!(ev["started_at_unix_ms"].as_u64().unwrap() > 0);

    let ev = wdog.next();
    assert_eq!(ev["event"], "wallet_started");
    assert!(ev["pid"].as_u64().unwrap() > 0);
    assert!(ev["started_at_unix_ms"].as_u64().unwrap() > 0);

    let ev = wdog.next();
    assert_eq!(ev["event"], "wallet_ready");
    assert_eq!(ev["port"], port);

    wdog.send_stop();

    let ev = wdog.next();
    assert_eq!(ev["event"], "stopped");

    wdog.wait();
    let _ = std::fs::remove_dir_all(&dir);
}

/// Crash the wallet process after it becomes ready; watchdog should restart it.
#[test]
fn wallet_restarts_on_crash() {
    let dir = test_dir("restart");
    let socket = dir.join("node.socket");
    let port = free_port();

    let mut wdog = Watchdog::start(&config_json(
        socket.to_str().unwrap(),
        dir.to_str().unwrap(),
        port,
        dir.join("node.log").to_str().unwrap(),
        dir.join("wallet.log").to_str().unwrap(),
    ));

    let ev = wdog.next();
    assert_eq!(ev["event"], "node_started");
    assert!(ev["started_at_unix_ms"].as_u64().unwrap() > 0);

    let ev = wdog.next();
    assert_eq!(ev["event"], "wallet_started");
    let wallet_pid = ev["pid"].as_u64().unwrap() as i32;
    assert!(ev["started_at_unix_ms"].as_u64().unwrap() > 0);

    let ev = wdog.next();
    assert_eq!(ev["event"], "wallet_ready");

    // Kill the wallet to simulate a crash.
    nix::sys::signal::kill(
        nix::unistd::Pid::from_raw(wallet_pid),
        nix::sys::signal::Signal::SIGKILL,
    )
    .unwrap();

    let ev = wdog.next();
    assert_eq!(ev["event"], "wallet_exited");

    let ev = wdog.next();
    assert_eq!(ev["event"], "wallet_restarting");
    assert_eq!(ev["attempt"], 1);

    // Watchdog restarts the wallet; wait for it to become ready again.
    let ev = wdog.next();
    assert_eq!(ev["event"], "wallet_started");
    assert!(ev["started_at_unix_ms"].as_u64().unwrap() > 0);

    let ev = wdog.next();
    assert_eq!(ev["event"], "wallet_ready");

    wdog.send_stop();

    let ev = wdog.next();
    assert_eq!(ev["event"], "stopped");

    wdog.wait();
    let _ = std::fs::remove_dir_all(&dir);
}
