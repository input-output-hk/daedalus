// Integration tests for cardano-watchdog.
//
// Each test spawns the compiled watchdog binary with mock node/wallet/mithril
// binaries and validates the JSON-line event protocol over stdin/stdout.
//
// Run with:
//   cargo test --test integration
//
// These are intentionally excluded from the nix build because they spawn real
// OS processes and require a pre-built watchdog binary.  The mock binaries
// use raw fd 3 for the shutdown pipe, so the tests are Unix-only.
#![cfg(unix)]

use serde_json::{Value, json};
use std::io::{BufRead, BufReader, Write};
use std::net::TcpListener;
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc;
use std::time::Duration;

// ── Binary paths ──────────────────────────────────────────────────────────────

const WATCHDOG: &str = env!("CARGO_BIN_EXE_cardano-watchdog");
const MOCK_NODE: &str = env!("CARGO_BIN_EXE_mock-node");
const MOCK_NODE_CRASH: &str = env!("CARGO_BIN_EXE_mock-node-crash");
const MOCK_NODE_NO_SOCKET: &str = env!("CARGO_BIN_EXE_mock-node-no-socket");
const MOCK_NODE_STARTUP_LOG: &str = env!("CARGO_BIN_EXE_mock-node-startup-log");
const MOCK_WALLET: &str = env!("CARGO_BIN_EXE_mock-wallet");
const MOCK_WALLET_CRASH: &str = env!("CARGO_BIN_EXE_mock-wallet-crash");
const MOCK_MITHRIL: &str = env!("CARGO_BIN_EXE_mock-mithril-client");
const MOCK_MITHRIL_FAIL: &str = env!("CARGO_BIN_EXE_mock-mithril-client-fail");
const MOCK_CONVERTER: &str = env!("CARGO_BIN_EXE_mock-snapshot-converter");
const MOCK_CONVERTER_FAIL: &str = env!("CARGO_BIN_EXE_mock-snapshot-converter-fail");
const MOCK_CONVERTER_SLOW: &str = env!("CARGO_BIN_EXE_mock-snapshot-converter-slow");
const MOCK_CONVERTER_SENTINEL: &str = env!("CARGO_BIN_EXE_mock-snapshot-converter-sentinel");

// ── Utilities ─────────────────────────────────────────────────────────────────

fn free_port() -> u16 {
    TcpListener::bind("127.0.0.1:0")
        .unwrap()
        .local_addr()
        .unwrap()
        .port()
}

struct TempDir(PathBuf);

impl TempDir {
    fn new(label: &str) -> Self {
        let path = std::env::temp_dir().join(format!(
            "wdg-test-{}-{}-{}",
            label,
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .subsec_nanos()
        ));
        std::fs::create_dir_all(&path).unwrap();
        TempDir(path)
    }

    fn path(&self) -> &Path {
        &self.0
    }

    /// Create `<state_dir>/chain/` with a sentinel file so chain_has_data() → true.
    fn populate_chain(&self) {
        let chain = self.0.join("chain");
        std::fs::create_dir_all(&chain).unwrap();
        std::fs::write(chain.join(".sentinel"), b"exists").unwrap();
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn event_reader(stdout: std::process::ChildStdout) -> mpsc::Receiver<Value> {
    let (tx, rx) = mpsc::channel();
    std::thread::spawn(move || {
        for line in BufReader::new(stdout).lines().flatten() {
            if let Ok(v) = serde_json::from_str::<Value>(&line) {
                if tx.send(v).is_err() {
                    break;
                }
            }
        }
    });
    rx
}

/// Drain events until one with `event == name` arrives; panic after 15 s.
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

/// Like `expect` but also applies a predicate.
fn expect_with(rx: &mpsc::Receiver<Value>, name: &str, pred: impl Fn(&Value) -> bool) -> Value {
    loop {
        let v = rx
            .recv_timeout(Duration::from_secs(15))
            .unwrap_or_else(|_| panic!("timeout waiting for '{name}' matching predicate"));
        if v["event"] == name && pred(&v) {
            return v;
        }
    }
}

/// Send a JSON command line to the watchdog's stdin.
fn send(stdin: &mut ChildStdin, cmd: Value) {
    writeln!(stdin, "{}", serde_json::to_string(&cmd).unwrap()).unwrap();
    stdin.flush().unwrap();
}

fn stop(stdin: &mut ChildStdin) {
    send(stdin, json!({"cmd": "stop"}));
}

// ── Config builder ────────────────────────────────────────────────────────────

struct Cfg<'a> {
    dir: &'a TempDir,
    node_exe: &'a str,
    wallet_exe: &'a str,
    node_args: Vec<String>,
    wallet_args: Vec<String>,
    wallet_port: u16,
    socket_path: PathBuf,
    max_restarts: u32,
    restart_delay_ms: u64,
    with_mithril: bool,
    mithril_bin: Option<&'a str>,
    converter_bin: Option<&'a str>,
}

impl<'a> Cfg<'a> {
    fn new(dir: &'a TempDir, node_exe: &'a str, wallet_exe: &'a str) -> Self {
        let socket_path = dir.path().join("node.socket");
        let wallet_port = free_port();
        Cfg {
            dir,
            node_exe,
            wallet_exe,
            node_args: vec![socket_path.to_str().unwrap().to_string()],
            wallet_args: vec![wallet_port.to_string()],
            wallet_port,
            socket_path,
            max_restarts: 10,
            restart_delay_ms: 50,
            with_mithril: false,
            mithril_bin: None,
            converter_bin: None,
        }
    }

    fn max_restarts(mut self, n: u32) -> Self {
        self.max_restarts = n;
        self
    }

    fn restart_delay(mut self, ms: u64) -> Self {
        self.restart_delay_ms = ms;
        self
    }

    fn mithril(mut self) -> Self {
        self.with_mithril = true;
        self
    }

    fn mithril_bin(mut self, bin: &'a str) -> Self {
        self.with_mithril = true;
        self.mithril_bin = Some(bin);
        self
    }

    fn converter_bin(mut self, bin: &'a str) -> Self {
        self.with_mithril = true;
        self.converter_bin = Some(bin);
        self
    }

    fn build(self) -> (Value, u16) {
        let state = self.dir.path().to_str().unwrap();
        let logs = self.dir.path().join("logs");
        std::fs::create_dir_all(&logs).unwrap();

        let mut cfg = json!({
            "node": {
                "exe": self.node_exe,
                "args": self.node_args,
                "state_dir": state,
                "socket_path": self.socket_path.to_str().unwrap()
            },
            "wallet": {
                "exe": self.wallet_exe,
                "args": self.wallet_args,
                "state_dir": state,
                "api_port": self.wallet_port,
                "restart_delay_ms": self.restart_delay_ms,
                "max_restart_attempts": self.max_restarts
            },
            "node_log_file": logs.join("node.log").to_str().unwrap(),
            "wallet_log_file": logs.join("wallet.log").to_str().unwrap()
        });

        if self.with_mithril {
            let mithril_bin = self.mithril_bin.unwrap_or(MOCK_MITHRIL);
            let converter_bin = self.converter_bin.unwrap_or(MOCK_CONVERTER);
            cfg["mithril"] = json!({
                "mithril_bin": mithril_bin,
                "snapshot_converter_bin": converter_bin,
                "converter_config": "/dev/null",
                "aggregator_url": "http://localhost:0",
                "genesis_vkey": "test",
                "state_dir": state,
                "chain_path": self.dir.path().join("chain").to_str().unwrap(),
                "behind_threshold": 20
            });
        }

        (cfg, self.wallet_port)
    }
}

fn spawn_watchdog(config: &Value) -> (Child, ChildStdin, mpsc::Receiver<Value>) {
    let mut child = Command::new(WATCHDOG)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn watchdog");

    let mut stdin = child.stdin.take().unwrap();
    let stdout = child.stdout.take().unwrap();
    let rx = event_reader(stdout);

    writeln!(stdin, "{}", serde_json::to_string(config).unwrap()).unwrap();
    stdin.flush().unwrap();

    (child, stdin, rx)
}

// ── Tests: chain-state gating ─────────────────────────────────────────────────

/// Empty chain → chain_status{has_chain:false} → start_node → full startup.
#[test]
fn empty_chain_start_node() {
    let dir = TempDir::new("empty-start-node");
    // Do NOT populate chain → has_chain=false.
    let (cfg, port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");

    let cs = expect(&rx, "chain_status");
    assert_eq!(cs["has_chain"], false);

    send(&mut stdin, json!({"cmd": "start_node"}));
    expect(&rx, "node_started");
    expect(&rx, "node_socket_ready");
    expect(&rx, "wallet_started");
    let ready = expect(&rx, "wallet_ready");
    assert_eq!(ready["port"], port);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Populated chain → chain_status{has_chain:true} → node starts immediately.
#[test]
fn existing_chain_starts_immediately() {
    let dir = TempDir::new("existing-chain");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");

    let cs = expect(&rx, "chain_status");
    assert_eq!(cs["has_chain"], true);

    // No start_node needed — watchdog proceeds on its own.
    expect(&rx, "node_started");
    expect(&rx, "node_socket_ready");
    expect(&rx, "wallet_started");
    expect(&rx, "wallet_ready");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Stop while waiting for chain decision (before any start_node / start_mithril).
#[test]
fn stop_during_chain_decision() {
    let dir = TempDir::new("stop-during-decision");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect_with(&rx, "chain_status", |v| v["has_chain"] == false);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── Tests: Mithril bootstrap ──────────────────────────────────────────────────

/// Empty chain + Mithril → user picks mithril → full pipeline → node+wallet restart.
#[test]
fn empty_chain_mithril_bootstrap() {
    let dir = TempDir::new("mithril-bootstrap");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect_with(&rx, "chain_status", |v| v["has_chain"] == false);

    send(&mut stdin, json!({"cmd": "start_mithril"}));

    expect_with(&rx, "mithril_status", |v| v["phase"] == "preparing");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "downloading");
    expect(&rx, "mithril_progress"); // at least one progress event
    expect_with(&rx, "mithril_status", |v| v["phase"] == "converting");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "installing");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "finalizing");

    // Node and wallet restart after install.
    expect(&rx, "node_started");
    expect(&rx, "node_socket_ready");
    expect(&rx, "wallet_started");
    expect(&rx, "wallet_ready");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "completed");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Cancel Mithril mid-download → re-prompts with chain_status{false} → user picks genesis.
#[test]
fn cancel_mithril_then_start_node() {
    let dir = TempDir::new("cancel-mithril");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect_with(&rx, "chain_status", |v| v["has_chain"] == false);

    send(&mut stdin, json!({"cmd": "start_mithril"}));
    expect_with(&rx, "mithril_status", |v| v["phase"] == "preparing");

    // Cancel before download completes.
    send(&mut stdin, json!({"cmd": "cancel_mithril"}));
    expect_with(&rx, "mithril_status", |v| v["phase"] == "cancelled");

    // Chain is still empty → watchdog re-prompts.
    expect_with(&rx, "chain_status", |v| v["has_chain"] == false);

    send(&mut stdin, json!({"cmd": "start_node"}));
    expect(&rx, "node_started");
    expect(&rx, "node_socket_ready");
    expect(&rx, "wallet_started");
    expect(&rx, "wallet_ready");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── Tests: partial sync while running ────────────────────────────────────────

/// Running system → start_mithril(force) → node/wallet stop → pipeline → restart.
#[test]
fn partial_sync_while_running() {
    let dir = TempDir::new("partial-sync");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect(&rx, "chain_status");
    expect(&rx, "node_started");
    expect(&rx, "node_socket_ready");
    expect(&rx, "wallet_started");
    expect(&rx, "wallet_ready");

    // User triggers partial sync.
    send(&mut stdin, json!({"cmd": "start_mithril", "force": true}));

    // Wallet stops, then node shuts down gracefully.
    expect(&rx, "node_shutdown_ms");

    expect_with(&rx, "mithril_status", |v| v["phase"] == "preparing");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "downloading");
    expect(&rx, "mithril_progress");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "installing");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "finalizing");

    // Comes back up.
    expect(&rx, "node_started");
    expect(&rx, "node_socket_ready");
    expect(&rx, "wallet_started");
    expect(&rx, "wallet_ready");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "completed");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── Tests: wallet supervision ────────────────────────────────────────────────

/// Wallet crashes on every attempt → circuit breaker fires after max_restarts.
#[test]
fn wallet_circuit_breaker() {
    let dir = TempDir::new("circuit");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET_CRASH)
        .max_restarts(3)
        .restart_delay(0)
        .build();
    let (mut child, stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "node_socket_ready");

    for i in 0..3u32 {
        expect(&rx, "wallet_started");
        let exited = expect(&rx, "wallet_exited");
        assert_eq!(exited["phase"], "pre_ready", "cycle {i}: wrong phase");
        assert_eq!(exited["code"], 1, "cycle {i}: wrong code");
    }

    let unrec = expect(&rx, "wallet_unrecoverable");
    assert_eq!(unrec["attempt"], 3);

    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── Tests: node failures ──────────────────────────────────────────────────────

/// Node crashes before socket → node_exited emitted → stopped (no wallet).
#[test]
fn node_crash_before_socket() {
    let dir = TempDir::new("node-crash");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE_CRASH, MOCK_WALLET).build();
    let (mut child, stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect(&rx, "chain_status");
    expect(&rx, "node_started");

    let exited = expect(&rx, "node_exited");
    assert_eq!(exited["code"], 1);

    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Stop sent while waiting for socket → clean shutdown with node_shutdown_ms.
#[test]
fn stop_during_socket_wait() {
    let dir = TempDir::new("stop-socket-wait");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE_NO_SOCKET, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect(&rx, "chain_status");
    expect(&rx, "node_started");

    // Give watchdog a moment to enter the socket-wait select.
    std::thread::sleep(Duration::from_millis(50));

    stop(&mut stdin);
    expect(&rx, "node_shutdown_ms");
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── Tests: stdin EOF ──────────────────────────────────────────────────────────

/// Dropping stdin (EOF) is treated as an implicit stop.
#[test]
fn stdin_eof_stops_watchdog() {
    let dir = TempDir::new("eof");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "wallet_ready");
    drop(stdin); // EOF → implicit stop

    expect(&rx, "stopped");
    let _ = child.wait();
}

// ── Tests: startup log parsing ────────────────────────────────────────────────

/// Node stdout emits startup log lines → watchdog parses and re-emits as
/// node_startup_status and node_block_sync_progress events.
#[test]
fn startup_log_phase_parsing() {
    let dir = TempDir::new("startup-log");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE_STARTUP_LOG, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect(&rx, "chain_status");
    expect(&rx, "node_started");

    // Startup phases arrive while socket is not yet ready.
    expect_with(&rx, "node_startup_status", |v| {
        v["phase"] == "openingImmutableDb"
    });
    expect_with(&rx, "node_startup_status", |v| {
        v["phase"] == "replayingLedger"
    });

    // Block sync progress from "Replayed block Progress: N%" lines.
    let prog = expect_with(&rx, "node_block_sync_progress", |v| {
        v["kind"] == "replayedBlock"
    });
    let pct = prog["progress"].as_f64().unwrap();
    assert!(pct > 0.0 && pct <= 100.0, "unexpected progress {pct}");

    expect_with(&rx, "node_startup_status", |v| v["phase"] == "chainDbReady");

    // Socket created after log lines in this mock.
    expect(&rx, "node_socket_ready");
    expect(&rx, "wallet_started");
    expect(&rx, "wallet_ready");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── Tests: protocol robustness ────────────────────────────────────────────────

/// Malformed stdin lines are silently discarded; watchdog keeps running.
#[test]
fn malformed_stdin_ignored() {
    let dir = TempDir::new("malformed");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "wallet_ready");

    writeln!(stdin, "not json at all {{{{").unwrap();
    writeln!(stdin, r#"{{"cmd":"unknown_future_command"}}"#).unwrap();
    stop(&mut stdin);

    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// watchdog_started carries the actual watchdog PID.
#[test]
fn watchdog_started_pid() {
    let dir = TempDir::new("pid");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    let started = expect(&rx, "watchdog_started");
    let pid = started["pid"].as_u64().unwrap();
    assert!(pid > 1, "pid {pid} looks wrong");
    assert_ne!(
        pid,
        std::process::id() as u64,
        "pid should be watchdog, not test"
    );

    expect(&rx, "wallet_ready");
    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// wallet_ready port matches the port in the config.
#[test]
fn wallet_ready_port_matches_config() {
    let dir = TempDir::new("port-check");
    dir.populate_chain();
    let (cfg, expected_port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "chain_status");
    expect(&rx, "node_started");
    expect(&rx, "node_socket_ready");
    expect(&rx, "wallet_started");
    let ready = expect(&rx, "wallet_ready");

    assert_eq!(
        ready["port"].as_u64().unwrap(),
        expected_port as u64,
        "port mismatch"
    );

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── T5: probe_mithril ─────────────────────────────────────────────────────────

/// probe_mithril while running → certified >> local → mithril_significantly_behind.
#[test]
fn probe_mithril_significantly_behind() {
    let dir = TempDir::new("probe-behind");
    dir.populate_chain();
    // Default mock returns certified=999999; local=0 immutables in chain/immutable/
    // → behind=999999 >= threshold(20) → MithrilSignificantlyBehind
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "wallet_ready");

    send(&mut stdin, json!({"cmd": "probe_mithril"}));
    let ev = expect(&rx, "mithril_significantly_behind");
    assert_eq!(ev["local_immutable_count"], 0);
    assert_eq!(ev["latest_certified_immutable"], 999_999);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// probe_mithril while running → certified close to local → mithril_not_needed.
#[test]
fn probe_mithril_not_needed() {
    let dir = TempDir::new("probe-not-needed");
    dir.populate_chain();
    // not-needed mock returns certified=10; local=0 → behind=10 < threshold(20) → NotNeeded
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .mithril_bin(env!("CARGO_BIN_EXE_mock-mithril-client-not-needed"))
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "wallet_ready");

    send(&mut stdin, json!({"cmd": "probe_mithril"}));
    let ev = expect(&rx, "mithril_not_needed");
    assert_eq!(ev["latest_certified_immutable"], 10);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── T6: wipe_chain: true ─────────────────────────────────────────────────────

/// start_mithril with wipe_chain:true → chain dir deleted, full bootstrap install, restart.
#[test]
fn wipe_chain_full_bootstrap() {
    let dir = TempDir::new("wipe-chain");
    dir.populate_chain();
    let chain_path = dir.path().join("chain");

    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect(&rx, "chain_status");
    expect(&rx, "wallet_ready");

    send(
        &mut stdin,
        json!({"cmd": "start_mithril", "force": true, "wipe_chain": true}),
    );

    expect(&rx, "node_shutdown_ms");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "preparing");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "downloading");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "installing");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "finalizing");

    expect(&rx, "node_started");
    expect(&rx, "wallet_ready");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "completed");

    assert!(
        chain_path.exists(),
        "chain dir should be recreated after install"
    );

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── T7: node crash after wallet ready ────────────────────────────────────────

/// Node exits after wallet is ready → node_exited emitted → wallet stops → stopped.
#[test]
fn node_crash_after_wallet_ready() {
    let dir = TempDir::new("node-crash-post-ready");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect(&rx, "chain_status");

    let node_ev = expect(&rx, "node_started");
    let node_pid = node_ev["pid"].as_u64().unwrap() as i32;

    expect(&rx, "node_socket_ready");
    expect(&rx, "wallet_started");
    expect(&rx, "wallet_ready");

    nix::sys::signal::kill(
        nix::unistd::Pid::from_raw(node_pid),
        nix::sys::signal::Signal::SIGKILL,
    )
    .unwrap();

    expect(&rx, "node_exited");
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── T8: Mithril download failure ─────────────────────────────────────────────

/// Mithril download fails → PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED → re-prompt.
#[test]
fn mithril_download_failure() {
    let dir = TempDir::new("mithril-dl-fail");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .mithril_bin(MOCK_MITHRIL_FAIL)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect_with(&rx, "chain_status", |v| v["has_chain"] == false);

    send(&mut stdin, json!({"cmd": "start_mithril"}));

    expect_with(&rx, "mithril_status", |v| v["phase"] == "preparing");
    let err = expect(&rx, "mithril_error");
    assert_eq!(err["code"], "PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED");

    expect_with(&rx, "chain_status", |v| v["has_chain"] == false);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── T9: Mithril conversion failure ───────────────────────────────────────────

/// Download succeeds but converter fails → PARTIAL_SYNC_CONVERSION_FAILED → re-prompt.
#[test]
fn mithril_conversion_failure() {
    let dir = TempDir::new("mithril-conv-fail");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .converter_bin(MOCK_CONVERTER_FAIL)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect_with(&rx, "chain_status", |v| v["has_chain"] == false);

    send(&mut stdin, json!({"cmd": "start_mithril"}));

    expect_with(&rx, "mithril_status", |v| v["phase"] == "preparing");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "downloading");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "converting");
    let err = expect(&rx, "mithril_error");
    assert_eq!(err["code"], "PARTIAL_SYNC_CONVERSION_FAILED");

    expect_with(&rx, "chain_status", |v| v["has_chain"] == false);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── T10: Marker recovery on startup ──────────────────────────────────────────

/// Stale cutover-in-progress marker + staging dir → watchdog cleans up and starts normally.
#[test]
fn marker_recovery_cutover_in_progress() {
    let dir = TempDir::new("marker-cutover");
    dir.populate_chain();

    let logs = dir.path().join("Logs");
    std::fs::create_dir_all(&logs).unwrap();
    std::fs::write(
        logs.join("mithril-partial-sync.lock"),
        r#"{"state":"cutover-in-progress"}"#,
    )
    .unwrap();
    let staging = dir.path().join("mithril-partial-sync");
    std::fs::create_dir_all(&staging).unwrap();
    std::fs::write(staging.join("sentinel"), b"").unwrap();

    // Mithril must be configured for the watchdog to read the marker at startup.
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect(&rx, "chain_status");
    expect(&rx, "node_started");
    expect(&rx, "wallet_ready");

    assert!(
        !staging.exists(),
        "staging dir should be cleaned up on startup"
    );

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Stale installed-awaiting-node-start marker → cleared on startup, node starts normally.
#[test]
fn marker_recovery_installed_awaiting_node_start() {
    let dir = TempDir::new("marker-awaiting");
    dir.populate_chain();

    let logs = dir.path().join("Logs");
    std::fs::create_dir_all(&logs).unwrap();
    std::fs::write(
        logs.join("mithril-partial-sync.lock"),
        r#"{"state":"installed-awaiting-node-start"}"#,
    )
    .unwrap();

    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    let cs = expect(&rx, "chain_status");
    assert_eq!(cs["has_chain"], true);
    expect(&rx, "node_started");
    expect(&rx, "wallet_ready");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── T11: max_restart_attempts = 0 ────────────────────────────────────────────

/// max_restart_attempts=0 → first wallet crash immediately triggers unrecoverable.
#[test]
fn max_restart_attempts_zero() {
    let dir = TempDir::new("max-restart-zero");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET_CRASH)
        .max_restarts(0)
        .restart_delay(0)
        .build();
    let (mut child, stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "node_socket_ready");
    expect(&rx, "wallet_started");
    let exited = expect(&rx, "wallet_exited");
    assert_eq!(exited["code"], 1);

    let unrec = expect(&rx, "wallet_unrecoverable");
    assert_eq!(unrec["attempt"], 1);

    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── T12: Cancellation during conversion ─────────────────────────────────────

/// Cancel while the converter process is running → converter is killed,
/// "cancelled" status emitted, cutover marker is NOT written.
#[test]
fn cancel_mithril_during_conversion() {
    let dir = TempDir::new("cancel-converting");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .converter_bin(MOCK_CONVERTER_SLOW)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect_with(&rx, "chain_status", |v| v["has_chain"] == false);

    send(&mut stdin, json!({"cmd": "start_mithril"}));
    expect_with(&rx, "mithril_status", |v| v["phase"] == "preparing");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "downloading");
    // Converter is now running (sleeping indefinitely); send cancel.
    expect_with(&rx, "mithril_status", |v| v["phase"] == "converting");
    send(&mut stdin, json!({"cmd": "cancel_mithril"}));

    expect_with(&rx, "mithril_status", |v| v["phase"] == "cancelled");

    // Cutover marker must not be written when cancellation occurs before cutover.
    let marker = dir.path().join("Logs").join("mithril-partial-sync.lock");
    assert!(
        !marker.exists(),
        "cutover marker must not exist after cancel"
    );

    // Watchdog re-prompts since chain is still empty.
    expect_with(&rx, "chain_status", |v| v["has_chain"] == false);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Converter completes naturally then cancel arrives inside the cutover gate
/// window — verifies the gate's blocking recv catches it before the cutover
/// marker is written or install_staged runs.
///
/// The sentinel converter writes `lsm/CONVERTER_DONE` as its last act before
/// exiting 0.  The test polls for that file (proving the converter has exited
/// and the gate is now the only thing standing between cancel and chain
/// mutation), then sends cancel into the still-open window.  This exercises a
/// code path that neither the slow-converter tests nor the old try_recv gate
/// could exercise.
#[test]
fn cancel_before_cutover_gate() {
    let dir = TempDir::new("cancel-gate");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .converter_bin(MOCK_CONVERTER_SENTINEL)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect_with(&rx, "chain_status", |v| v["has_chain"] == false);

    send(&mut stdin, json!({"cmd": "start_mithril"}));
    expect_with(&rx, "mithril_status", |v| v["phase"] == "converting");

    // Poll for the sentinel written by the converter just before it exits 0.
    // Visibility of the sentinel means the converter has exited and the
    // pipeline is now inside the CUTOVER_GATE_MS blocking recv window.
    let sentinel = dir
        .path()
        .join("mithril-partial-sync")
        .join("download")
        .join("db")
        .join("lsm")
        .join("CONVERTER_DONE");
    let deadline = std::time::Instant::now() + Duration::from_secs(10);
    while !sentinel.exists() {
        assert!(
            std::time::Instant::now() < deadline,
            "timeout waiting for converter sentinel"
        );
        std::thread::sleep(Duration::from_millis(5));
    }

    // Converter has exited; send cancel into the open gate window.
    send(&mut stdin, json!({"cmd": "cancel_mithril"}));
    expect_with(&rx, "mithril_status", |v| v["phase"] == "cancelled");

    let marker = dir.path().join("Logs").join("mithril-partial-sync.lock");
    assert!(
        !marker.exists(),
        "cutover marker must not be written when gate catches cancel after natural converter exit"
    );

    expect_with(&rx, "chain_status", |v| v["has_chain"] == false);
    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── T13: TOCTOU note ─────────────────────────────────────────────────────────
// free_port() binds port 0, records the port, drops the listener, then passes
// the port number to the watchdog mock. There is a TOCTOU race window between
// the listener drop and the mock's bind. The probability of collision is
// extremely low (linear PID allocation on Linux) but can cause spurious
// failures on heavily loaded CI machines. Use SO_REUSEPORT if this ever
// becomes flaky.
