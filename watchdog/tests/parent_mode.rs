// Parent mode: the watchdog spawns Electron and exchanges events and commands
// with it over Electron's stdin and stdout, and Electron outlives the backend
// long enough to act on `stopped`.
//
// Unix-only, like the other process tests: the mocks read the shutdown pipe
// on fd 3, and the mock Electron is a shell script.
#![cfg(unix)]

use serde_json::{Value, json};
use std::net::TcpListener;
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

const WATCHDOG: &str = env!("CARGO_BIN_EXE_cardano-watchdog");
const MOCK_NODE: &str = env!("CARGO_BIN_EXE_mock-node");
const MOCK_WALLET: &str = env!("CARGO_BIN_EXE_mock-wallet");

/// How long the watchdog gives Electron to exit after the backend stopped.
const EXIT_GRACE: Duration = Duration::from_secs(15);

/// Mock Electron that quits once the wallet is ready, as the user does, and
/// on `stopped` takes a second to close before it records that it got there
/// and exits.
const QUITTING_ELECTRON: &str = r#"
while IFS= read -r line; do
  printf '%s\n' "$line" >> "$EVENTS_FILE"
  case "$line" in
    *'"wallet_ready"'*) printf '{"cmd":"stop"}\n' ;;
    *'"event":"stopped"'*) sleep 1; echo exited-after-stopped > "$MARKER"; exit 0 ;;
  esac
done
"#;

/// Mock Electron that quits once the wallet is ready and on `stopped` records
/// its PID, then keeps running.
const LINGERING_ELECTRON: &str = r#"
while IFS= read -r line; do
  printf '%s\n' "$line" >> "$EVENTS_FILE"
  case "$line" in
    *'"wallet_ready"'*) printf '{"cmd":"stop"}\n' ;;
    *'"event":"stopped"'*) echo $$ > "$MARKER"; exec sleep 120 ;;
  esac
done
"#;

/// Mock Electron that asks for the blank-screen flag once the wallet is
/// ready, and quits once it runs with that flag.
const RESTARTING_ELECTRON: &str = r#"
echo "electron-args: $*" >> "$EVENTS_FILE"
while IFS= read -r line; do
  printf '%s\n' "$line" >> "$EVENTS_FILE"
  case "$line" in
    *'"wallet_ready"'*)
      case " $* " in
        *' --test-flag '*) printf '{"cmd":"stop"}\n' ;;
        *) printf '{"cmd":"set_electron_flags","flags":["--test-flag"]}\n' ;;
      esac
      ;;
    *'"event":"stopped"'*) echo exited-after-stopped > "$MARKER"; exit 0 ;;
  esac
done
"#;

struct TempDir(PathBuf);

impl TempDir {
    fn new(label: &str) -> Self {
        let path = std::env::temp_dir().join(format!(
            "wdg-parent-{}-{}-{}",
            label,
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .subsec_nanos()
        ));
        let state = path.join("state");
        std::fs::create_dir_all(state.join("logs")).unwrap();
        // Settings already migrated, and a database to start the node on.
        std::fs::write(state.join("watchdog-state.json"), b"{}").unwrap();
        std::fs::create_dir_all(state.join("chain")).unwrap();
        std::fs::write(state.join("chain").join("protocolMagicId"), b"1").unwrap();
        TempDir(path)
    }

    fn state(&self) -> PathBuf {
        self.0.join("state")
    }

    fn events_file(&self) -> PathBuf {
        self.0.join("electron-events.jsonl")
    }

    fn marker(&self) -> PathBuf {
        self.0.join("electron-marker")
    }

    fn events(&self) -> String {
        std::fs::read_to_string(self.events_file()).unwrap_or_default()
    }

    fn marker_text(&self) -> Option<String> {
        std::fs::read_to_string(self.marker())
            .ok()
            .map(|s| s.trim().to_string())
    }

    fn log(&self) -> String {
        std::fs::read_to_string(self.state().join("logs").join("watchdog.log")).unwrap_or_default()
    }

    fn saved_state(&self) -> Value {
        let text = std::fs::read_to_string(self.state().join("watchdog-state.json")).unwrap();
        serde_json::from_str(&text).unwrap()
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// Runs the watchdog in parent mode with `script` as Electron.
fn spawn(dir: &TempDir, script: &str) -> Child {
    let state = dir.state();
    let socket = state.join("node.socket");
    let port = TcpListener::bind("127.0.0.1:0")
        .unwrap()
        .local_addr()
        .unwrap()
        .port();
    let cfg = json!({
        "node": {
            "exe": MOCK_NODE,
            "args": [socket.to_str().unwrap()],
            "state_dir": state.to_str().unwrap(),
            "socket_path": socket.to_str().unwrap(),
        },
        "wallet": {
            "exe": MOCK_WALLET,
            "args": [port.to_string()],
            "state_dir": state.to_str().unwrap(),
            "api_port": port,
        },
        "pub_logs_dir": state.join("logs").to_str().unwrap(),
        "electron": {
            "exe": "/bin/sh",
            // `$0` is "electron"; flags set at runtime follow as `$1`...
            "args": ["-c", script, "electron"],
            "env": {
                "EVENTS_FILE": dir.events_file().to_str().unwrap(),
                "MARKER": dir.marker().to_str().unwrap(),
            },
        },
    });
    let path = state.join("daedalus-config.json");
    std::fs::write(&path, serde_json::to_string_pretty(&cfg).unwrap()).unwrap();
    Command::new(WATCHDOG)
        .arg("--config")
        .arg(&path)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn watchdog")
}

fn wait_until(what: &str, limit: Duration, mut done: impl FnMut() -> bool) {
    let start = Instant::now();
    while !done() {
        assert!(start.elapsed() < limit, "timed out waiting for {what}");
        std::thread::sleep(Duration::from_millis(50));
    }
}

fn wait_for_exit(child: &mut Child, limit: Duration) {
    let start = Instant::now();
    while child.try_wait().unwrap().is_none() {
        if start.elapsed() > limit {
            let _ = child.kill();
            panic!("watchdog did not exit within {limit:?}");
        }
        std::thread::sleep(Duration::from_millis(50));
    }
}

/// Whether a process is running. A killed process whose parent has exited can
/// remain a zombie until it is reaped, which counts as not running.
fn is_running(pid: i32) -> bool {
    if unsafe { libc::kill(pid, 0) } != 0 {
        return false;
    }
    match std::fs::read_to_string(format!("/proc/{pid}/stat")) {
        Ok(stat) => !stat
            .rsplit(')')
            .next()
            .is_some_and(|rest| rest.trim_start().starts_with('Z')),
        Err(_) => true,
    }
}

/// Electron gets `stopped` and is left to exit by itself; the watchdog waits
/// for it instead of killing it as the supervisor returns.
#[test]
fn electron_exits_by_itself_after_the_backend_stops() {
    let dir = TempDir::new("quit");
    let mut watchdog = spawn(&dir, QUITTING_ELECTRON);

    wait_for_exit(&mut watchdog, EXIT_GRACE);

    assert!(
        dir.events().contains("\"event\":\"stopped\""),
        "{}",
        dir.events()
    );
    assert_eq!(
        dir.marker_text().as_deref(),
        Some("exited-after-stopped"),
        "Electron was closed before it exited by itself"
    );
    let log = dir.log();
    assert!(
        log.contains("Electron exited after the backend stopped"),
        "{log}"
    );
    assert!(!log.contains("closing it"), "{log}");
}

/// An Electron still running when the grace expires is closed, and the
/// watchdog says so rather than reporting that it exited.
#[test]
fn electron_still_running_after_the_grace_is_closed() {
    let dir = TempDir::new("linger");
    let mut watchdog = spawn(&dir, LINGERING_ELECTRON);

    wait_until("stopped at Electron", Duration::from_secs(15), || {
        dir.marker_text().is_some_and(|pid| !pid.is_empty())
    });
    let stopped_at = Instant::now();
    let pid: i32 = dir.marker_text().unwrap().parse().unwrap();
    assert!(is_running(pid), "Electron exited on its own");

    wait_for_exit(&mut watchdog, EXIT_GRACE + Duration::from_secs(10));

    assert!(
        stopped_at.elapsed() >= EXIT_GRACE - Duration::from_secs(1),
        "watchdog exited {:?} after stopped",
        stopped_at.elapsed()
    );
    wait_until("Electron to be closed", Duration::from_secs(5), || {
        !is_running(pid)
    });
    let log = dir.log();
    assert!(log.contains("closing it"), "{log}");
    assert!(
        !log.contains("Electron exited after the backend stopped"),
        "{log}"
    );
}

/// A blank-screen flag change restarts Electron with the flag while the node
/// and wallet keep running, and the restarted Electron still sees the backend
/// stop and exits by itself.
#[test]
fn electron_restarts_with_new_flags_while_the_backend_runs() {
    let dir = TempDir::new("restart");
    let mut watchdog = spawn(&dir, RESTARTING_ELECTRON);

    wait_for_exit(&mut watchdog, Duration::from_secs(30));

    let events = dir.events();
    let first = events
        .find("electron-args: \n")
        .expect("first Electron started without flags");
    let second = events
        .find("electron-args: --test-flag")
        .expect("Electron restarted with the new flag");
    assert!(first < second, "{events}");
    assert_eq!(
        events.matches("\"event\":\"node_started\"").count(),
        1,
        "the node was restarted: {events}"
    );
    // The restarted Electron is told the wallet is ready again.
    assert!(
        events[second..].contains("\"event\":\"wallet_ready\""),
        "{events}"
    );
    assert_eq!(dir.marker_text().as_deref(), Some("exited-after-stopped"));
    assert_eq!(dir.saved_state()["electron_flags"], json!(["--test-flag"]));
    let log = dir.log();
    assert!(
        log.contains("Electron exited after the backend stopped"),
        "{log}"
    );
}
