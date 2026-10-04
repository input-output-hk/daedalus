// First start after an upgrade: the watchdog asks Electron for the settings
// earlier versions kept in electron-store and seeds watchdog-state.json.
//
// Unix-only, like the other process tests: the mocks read the shutdown pipe
// on fd 3, and the mock Electron is a shell script.
#![cfg(unix)]

use serde_json::{Value, json};
use std::io::{BufRead, BufReader, Write};
use std::net::TcpListener;
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc;
use std::time::{Duration, Instant};

const WATCHDOG: &str = env!("CARGO_BIN_EXE_cardano-watchdog");
const MOCK_NODE: &str = env!("CARGO_BIN_EXE_mock-node");
const MOCK_WALLET: &str = env!("CARGO_BIN_EXE_mock-wallet");

/// Mock Electron: records every event in $EVENTS_FILE and answers
/// migrate_state_request with $REPLY after $REPLY_DELAY seconds.
const ANSWERING_ELECTRON: &str = r#"
while IFS= read -r line; do
  printf '%s\n' "$line" >> "$EVENTS_FILE"
  case "$line" in
    *'"migrate_state_request"'*)
      sleep "$REPLY_DELAY"
      printf '%s\n' "$REPLY"
      ;;
  esac
done
"#;

/// Mock Electron that exits after its first event without answering.
const EXITING_ELECTRON: &str = r#"
IFS= read -r line
printf '%s\n' "$line" >> "$EVENTS_FILE"
"#;

struct TempDir(PathBuf);

impl TempDir {
    fn new(label: &str) -> Self {
        let path = std::env::temp_dir().join(format!(
            "wdg-migration-{}-{}-{}",
            label,
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .subsec_nanos()
        ));
        std::fs::create_dir_all(path.join("state").join("logs")).unwrap();
        TempDir(path)
    }

    fn state(&self) -> PathBuf {
        self.0.join("state")
    }

    fn log(&self) -> String {
        std::fs::read_to_string(self.state().join("logs").join("watchdog.log")).unwrap_or_default()
    }

    fn saved_state(&self) -> Option<Value> {
        let text = std::fs::read_to_string(self.state().join("watchdog-state.json")).ok()?;
        serde_json::from_str(&text).ok()
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// A cardano-node database as the node or Mithril leaves it.
fn make_db(dir: &Path) {
    std::fs::create_dir_all(dir.join("immutable")).unwrap();
    std::fs::write(dir.join("protocolMagicId"), b"1").unwrap();
}

fn config(dir: &TempDir) -> Value {
    let state = dir.state();
    let socket = state.join("node.socket");
    let port = TcpListener::bind("127.0.0.1:0")
        .unwrap()
        .local_addr()
        .unwrap()
        .port();
    json!({
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
    })
}

fn with_electron(mut cfg: Value, script: &str, env: Value) -> Value {
    cfg["electron"] = json!({ "exe": "/bin/sh", "args": ["-c", script], "env": env });
    cfg
}

fn spawn(dir: &TempDir, cfg: &Value) -> (Child, ChildStdin, mpsc::Receiver<Value>) {
    let path = dir.state().join("daedalus-config.json");
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

fn send(stdin: &mut ChildStdin, cmd: Value) {
    writeln!(stdin, "{cmd}").unwrap();
    stdin.flush().unwrap();
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

/// Electron answering after 12 s, as a slow first start after an upgrade
/// does, still has its settings applied, and nothing starts before them.
#[test]
fn slow_electron_reply_is_applied_before_the_node_starts() {
    let dir = TempDir::new("slow");
    make_db(&dir.state().join("chain"));
    let events = dir.0.join("electron-events.jsonl");
    let reply = json!({
        "cmd": "migrate_state",
        "chain_path": null,
        "electron_flags": [],
        "node_extra_args": ["+RTS", "-c", "-RTS"],
    });
    let cfg = with_electron(
        config(&dir),
        ANSWERING_ELECTRON,
        json!({
            "EVENTS_FILE": events.to_str().unwrap(),
            "REPLY_DELAY": "12",
            "REPLY": reply.to_string(),
        }),
    );
    let started = Instant::now();
    let (mut watchdog, _stdin, _rx) = spawn(&dir, &cfg);
    let read_events = || std::fs::read_to_string(&events).unwrap_or_default();

    wait_until("wallet_ready", Duration::from_secs(40), || {
        read_events().contains("\"wallet_ready\"")
    });

    assert!(started.elapsed() >= Duration::from_secs(12));
    let seen = read_events();
    let saved_at = seen
        .find("\"migrate_state_saved\"")
        .expect("migrate_state_saved reaches Electron");
    let node_at = seen.find("\"node_started\"").unwrap();
    assert!(saved_at < node_at, "node started before migration: {seen}");
    let state = dir.saved_state().expect("watchdog-state.json written");
    assert_eq!(state["node_extra_args"], json!(["+RTS", "-c", "-RTS"]));
    assert!(!dir.log().contains("proceeding with defaults"));

    unsafe { libc::kill(watchdog.id() as i32, libc::SIGTERM) };
    wait_for_exit(&mut watchdog, Duration::from_secs(15));
}

/// Electron exiting before it answers stops the watchdog without starting a
/// node, and leaves watchdog-state.json absent so the next launch asks again.
#[test]
fn electron_exit_before_reply_starts_nothing() {
    let dir = TempDir::new("exit");
    make_db(&dir.state().join("chain"));
    let events = dir.0.join("electron-events.jsonl");
    let cfg = with_electron(
        config(&dir),
        EXITING_ELECTRON,
        json!({ "EVENTS_FILE": events.to_str().unwrap() }),
    );
    let (mut watchdog, _stdin, _rx) = spawn(&dir, &cfg);

    wait_for_exit(&mut watchdog, Duration::from_secs(15));

    assert!(
        dir.saved_state().is_none(),
        "watchdog-state.json was written"
    );
    assert!(!dir.log().contains("cardano-node started"), "{}", dir.log());
}

/// An 11.3 or 11.4 storage folder holds at most a Mithril download in a
/// `chain` subdirectory, while the node ran on <state>/chain. Migration keeps
/// the node on that database and leaves the folder untouched.
#[test]
fn migrated_folder_without_a_database_keeps_the_default() {
    let dir = TempDir::new("folder");
    make_db(&dir.state().join("chain"));
    let folder = dir.0.join("chosen-folder");
    make_db(&folder.join("chain"));
    let cfg = config(&dir);
    let (mut watchdog, mut stdin, rx) = spawn(&dir, &cfg);

    expect(&rx, "migrate_state_request");
    send(
        &mut stdin,
        json!({
            "cmd": "migrate_state",
            "chain_path": folder.to_str().unwrap(),
            "electron_flags": [],
            "node_extra_args": [],
        }),
    );
    expect(&rx, "migrate_state_saved");
    let status = expect(&rx, "chain_status");
    assert_eq!(status["has_chain"], true);
    expect(&rx, "node_started");

    let state = dir.saved_state().expect("watchdog-state.json written");
    assert!(state["chain_path"].is_null(), "{state}");
    let log = dir.log();
    assert!(log.contains("keeping"), "{log}");
    assert!(
        log.contains(dir.state().join("chain").to_str().unwrap()),
        "{log}"
    );
    assert!(log.contains(folder.to_str().unwrap()), "{log}");
    assert!(folder.join("chain").join("protocolMagicId").exists());
    assert!(dir.state().join("chain").join("protocolMagicId").exists());

    send(&mut stdin, json!({"cmd": "stop"}));
    expect(&rx, "stopped");
    drop(stdin);
    let _ = watchdog.wait();
}
