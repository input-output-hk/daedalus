// Two watchdogs started for one state directory yield one backend.
//
// Each test runs real watchdog processes against the mock node and wallet.
// Unix-only, like the other process tests: the mocks read the shutdown pipe
// on fd 3.
#![cfg(unix)]

use serde_json::{Value, json};
use std::io::{BufRead, BufReader, Read};
use std::net::TcpListener;
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc;
use std::time::{Duration, Instant};

const WATCHDOG: &str = env!("CARGO_BIN_EXE_cardano-watchdog");
const MOCK_NODE: &str = env!("CARGO_BIN_EXE_mock-node");
const MOCK_WALLET: &str = env!("CARGO_BIN_EXE_mock-wallet");

/// A node that behaves like mock-node but takes two seconds to exit after the
/// shutdown pipe closes, so a stopping instance holds the lock for a while.
/// `$1` is the socket path; the watchdog appends `--shutdown-ipc 3`.
const SLOW_EXIT_NODE: &str = r#"
for phase in StartedOpeningDB StartedOpeningImmutableDB OpenedImmutableDB \
             StartedOpeningVolatileDB OpenedVolatileDB StartedOpeningLgrDB \
             OpenedLgrDB OpenedDB; do
  echo "$phase"
done
: > "$1"
cat <&3 >/dev/null
sleep 2
"#;

struct TempDir(PathBuf);

impl TempDir {
    fn new(label: &str) -> Self {
        let path = std::env::temp_dir().join(format!(
            "wdg-single-{}-{}-{}",
            label,
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .subsec_nanos()
        ));
        std::fs::create_dir_all(path.join("chain")).unwrap();
        std::fs::write(path.join("chain").join("protocolMagicId"), b"1").unwrap();
        // An existing state file skips the migration request, which no test
        // client here answers.
        std::fs::write(path.join("watchdog-state.json"), b"{}").unwrap();
        std::fs::create_dir_all(path.join("logs")).unwrap();
        TempDir(path)
    }

    fn path(&self) -> &Path {
        &self.0
    }

    fn log(&self) -> String {
        std::fs::read_to_string(self.0.join("logs").join("watchdog.log")).unwrap_or_default()
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn free_port() -> u16 {
    TcpListener::bind("127.0.0.1:0")
        .unwrap()
        .local_addr()
        .unwrap()
        .port()
}

fn config(dir: &TempDir, node_exe: &str, node_args: Vec<String>) -> Value {
    let state = dir.path().to_str().unwrap();
    let socket = dir.path().join("node.socket");
    let port = free_port();
    json!({
        "node": {
            "exe": node_exe,
            "args": node_args,
            "state_dir": state,
            "socket_path": socket.to_str().unwrap(),
        },
        "wallet": {
            "exe": MOCK_WALLET,
            "args": [port.to_string()],
            "state_dir": state,
            "api_port": port,
            "restart_delay_ms": 50,
        },
        "pub_logs_dir": dir.path().join("logs").to_str().unwrap(),
        "tls_dir": dir.path().join("tls").to_str().unwrap(),
    })
}

fn mock_node_config(dir: &TempDir) -> Value {
    let socket = dir.path().join("node.socket");
    config(dir, MOCK_NODE, vec![socket.to_str().unwrap().to_string()])
}

fn spawn(dir: &TempDir, cfg: &Value) -> (Child, ChildStdin, mpsc::Receiver<Value>) {
    let path = dir.path().join("daedalus-config.json");
    std::fs::write(&path, serde_json::to_string_pretty(cfg).unwrap()).unwrap();
    let mut child = Command::new(WATCHDOG)
        .arg("--config")
        .arg(&path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn watchdog");
    let stdin = child.stdin.take().unwrap();
    let stdout = child.stdout.take().unwrap();
    let (tx, rx) = mpsc::channel();
    std::thread::spawn(move || {
        for line in BufReader::new(stdout).lines().map_while(Result::ok) {
            if let Ok(v) = serde_json::from_str::<Value>(&line) {
                if tx.send(v).is_err() {
                    break;
                }
            }
        }
    });
    (child, stdin, rx)
}

fn expect(rx: &mpsc::Receiver<Value>, name: &str) -> Value {
    loop {
        let v = rx
            .recv_timeout(Duration::from_secs(15))
            .unwrap_or_else(|_| panic!("timeout waiting for '{name}'"));
        if v["event"] == name {
            return v;
        }
    }
}

fn stop(stdin: &mut ChildStdin) {
    use std::io::Write;
    writeln!(stdin, r#"{{"cmd":"stop"}}"#).unwrap();
    stdin.flush().unwrap();
}

fn wait_for_exit(child: &mut Child, limit: Duration) -> std::process::ExitStatus {
    let start = Instant::now();
    loop {
        if let Some(status) = child.try_wait().unwrap() {
            return status;
        }
        assert!(
            start.elapsed() < limit,
            "process did not exit within {limit:?}"
        );
        std::thread::sleep(Duration::from_millis(50));
    }
}

fn wait_until(what: &str, limit: Duration, mut done: impl FnMut() -> bool) {
    let start = Instant::now();
    while !done() {
        assert!(start.elapsed() < limit, "timed out waiting for {what}");
        std::thread::sleep(Duration::from_millis(50));
    }
}

/// Run a second watchdog to completion and return its exit status and every
/// event it printed.
fn run_second(dir: &TempDir, cfg: &Value, limit: Duration) -> (std::process::ExitStatus, String) {
    let path = dir.path().join("daedalus-config.json");
    std::fs::write(&path, serde_json::to_string_pretty(cfg).unwrap()).unwrap();
    let mut child = Command::new(WATCHDOG)
        .arg("--config")
        .arg(&path)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn second watchdog");
    let start = Instant::now();
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break status;
        }
        if start.elapsed() > limit {
            let _ = child.kill();
            let _ = child.wait();
            panic!("second launch still running after {limit:?}");
        }
        std::thread::sleep(Duration::from_millis(50));
    };
    let mut out = String::new();
    child
        .stdout
        .take()
        .unwrap()
        .read_to_string(&mut out)
        .unwrap();
    (status, out)
}

/// A second launch while the first runs asks the first to bring its window
/// forward and exits. It starts no node, emits nothing, and leaves the first
/// instance's TLS certificates alone.
#[test]
fn second_launch_activates_running_instance() {
    let dir = TempDir::new("activate");
    let cfg = mock_node_config(&dir);
    let (mut first, mut stdin, rx) = spawn(&dir, &cfg);
    expect(&rx, "wallet_ready");
    let ca = dir.path().join("tls").join("server").join("ca.crt");
    let ca_before = std::fs::read(&ca).unwrap();

    let (status, out) = run_second(&dir, &cfg, Duration::from_secs(15));

    assert!(status.success(), "second launch exited with {status}");
    assert!(out.trim().is_empty(), "second launch emitted events: {out}");
    expect(&rx, "activate_window");
    assert_eq!(
        std::fs::read(&ca).unwrap(),
        ca_before,
        "second launch regenerated TLS certificates"
    );
    let log = dir.log();
    assert!(log.contains("brought its window to the front"), "{log}");
    assert_eq!(
        log.matches("watchdog started").count(),
        1,
        "a second instance started: {log}"
    );

    // The first instance is undisturbed.
    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = first.wait();
}

/// A second launch while the first is stopping with no window waits for the
/// first to exit, then starts normally. The two backends never overlap.
#[test]
fn second_launch_waits_for_stopping_instance_then_starts() {
    let dir = TempDir::new("wait");
    let socket = dir.path().join("node.socket");
    let cfg = config(
        &dir,
        "/bin/sh",
        vec![
            "-c".to_string(),
            SLOW_EXIT_NODE.to_string(),
            "slow-exit-node".to_string(),
            socket.to_str().unwrap().to_string(),
        ],
    );
    let (mut first, stdin, rx) = spawn(&dir, &cfg);
    expect(&rx, "wallet_ready");

    // The client hanging up leaves the first instance without a window and
    // stopping; its node takes two seconds to exit.
    drop(stdin);
    wait_until(
        "the first instance to start stopping",
        Duration::from_secs(10),
        || dir.log().contains("stopping wallet (shutdown requested)"),
    );

    let launched = Instant::now();
    let (mut second, mut second_stdin, second_rx) = spawn(&dir, &cfg);
    expect(&second_rx, "watchdog_started");
    let waited = launched.elapsed();

    // The lock is released as the first process exits; allow a moment for it
    // to be reaped.
    wait_for_exit(&mut first, Duration::from_secs(5));
    assert!(
        waited >= Duration::from_millis(1500),
        "second instance started after {waited:?}, before the first node had exited"
    );
    let log = dir.log();
    assert!(log.contains("has no window and is stopping"), "{log}");
    assert!(log.contains("previous instance exited"), "{log}");

    expect(&second_rx, "node_started");
    expect(&second_rx, "wallet_ready");
    stop(&mut second_stdin);
    expect(&second_rx, "stopped");
    drop(second_stdin);
    let _ = second.wait();
}

/// A launch that finds the lock held by a process that never answers gives up
/// and exits without starting anything.
#[test]
fn second_launch_gives_up_when_holder_does_not_answer() {
    use std::os::unix::io::AsRawFd;
    let dir = TempDir::new("silent");
    let cfg = mock_node_config(&dir);
    let lock = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(dir.path().join("watchdog.lock"))
        .unwrap();
    assert_eq!(
        unsafe { libc::flock(lock.as_raw_fd(), libc::LOCK_EX | libc::LOCK_NB) },
        0
    );

    let (status, out) = run_second(&dir, &cfg, Duration::from_secs(20));

    assert!(!status.success(), "second launch should report failure");
    assert!(out.trim().is_empty(), "second launch emitted events: {out}");
    assert!(
        !dir.path().join("tls").exists(),
        "TLS certificates were generated"
    );
    assert!(
        !dir.path().join("node.socket").exists(),
        "a node was started"
    );
    assert!(dir.log().contains("did not answer"), "{}", dir.log());
    drop(lock);
}

/// In parent mode the activation reaches Electron over its stdin, the same
/// channel as every other watchdog event.
#[test]
fn activation_reaches_electron_in_parent_mode() {
    let dir = TempDir::new("parent");
    let events = dir.path().join("electron-events.jsonl");
    let mut cfg = mock_node_config(&dir);
    cfg["electron"] = json!({
        "exe": "/bin/sh",
        // A read loop rather than a lone `cat`: bash execs a single -c
        // command, which would close the stdout pipe Electron holds open.
        // It exits on stopped, as Electron does.
        "args": ["-c", r#"while IFS= read -r line; do printf '%s\n' "$line" >> "$EVENTS_FILE"; case "$line" in *'"event":"stopped"'*) exit 0 ;; esac; done"#],
        "env": { "EVENTS_FILE": events.to_str().unwrap() },
    });
    let (mut first, _stdin, _rx) = spawn(&dir, &cfg);
    let read_events = || std::fs::read_to_string(&events).unwrap_or_default();
    wait_until("wallet_ready at Electron", Duration::from_secs(15), || {
        read_events().contains("\"wallet_ready\"")
    });

    let (status, _) = run_second(&dir, &cfg, Duration::from_secs(15));

    assert!(status.success(), "second launch exited with {status}");
    wait_until(
        "activate_window at Electron",
        Duration::from_secs(5),
        || read_events().contains("\"activate_window\""),
    );

    unsafe { libc::kill(first.id() as i32, libc::SIGTERM) };
    wait_for_exit(&mut first, Duration::from_secs(15));
}
