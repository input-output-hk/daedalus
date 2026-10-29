// Integration tests for cardano-watchdog.
//
// Each test spawns the compiled watchdog binary with mock node/wallet/mithril
// binaries and validates the JSON-line event protocol over stdin/stdout.
//
// Run with:
//   cargo test --test integration
//
// The nix `watchdog-test` check runs them: perSystem/watchdog.nix includes
// this directory in the source and cargo builds the watchdog and mock binaries
// before the tests spawn them.  The mock binaries use raw fd 3 for the
// shutdown pipe, so the tests are Unix-only.
#![cfg(unix)]

use serde_json::{Value, json};
use std::collections::BTreeMap;
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
const MOCK_NODE_NON_UTF8: &str = env!("CARGO_BIN_EXE_mock-node-non-utf8-output");
const MOCK_WALLET: &str = env!("CARGO_BIN_EXE_mock-wallet");
const MOCK_WALLET_CRASH: &str = env!("CARGO_BIN_EXE_mock-wallet-crash");
const MOCK_INSTALLER: &str = env!("CARGO_BIN_EXE_mock-installer");
const MOCK_MITHRIL: &str = env!("CARGO_BIN_EXE_mock-mithril-client");
const MOCK_MITHRIL_FAIL: &str = env!("CARGO_BIN_EXE_mock-mithril-client-fail");
const MOCK_CONVERTER: &str = env!("CARGO_BIN_EXE_mock-snapshot-converter");
const MOCK_CONVERTER_FAIL: &str = env!("CARGO_BIN_EXE_mock-snapshot-converter-fail");
const MOCK_CONVERTER_SLOW: &str = env!("CARGO_BIN_EXE_mock-snapshot-converter-slow");
const MOCK_CONVERTER_SENTINEL: &str = env!("CARGO_BIN_EXE_mock-snapshot-converter-sentinel");
const MOCK_CONVERTER_CHATTY: &str = env!("CARGO_BIN_EXE_mock-snapshot-converter-chatty");

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

    /// Create `<state_dir>/chain/` holding a database marker, so
    /// chain_has_data() → true and a Mithril install may replace it.
    fn populate_chain(&self) {
        let chain = self.0.join("chain");
        std::fs::create_dir_all(&chain).unwrap();
        std::fs::write(chain.join("protocolMagicId"), b"1").unwrap();
    }

    /// Create `<state_dir>/chain/` holding a whole database: chunks 0 to 3, a
    /// volatile block file, a ledger snapshot, an LSM directory and the
    /// `clean` and `protocolMagicId` markers. Returns its path.
    fn populate_database(&self) -> PathBuf {
        let chain = self.0.join("chain");
        let immutable = chain.join("immutable");
        std::fs::create_dir_all(&immutable).unwrap();
        for n in 0..4 {
            for ext in ["chunk", "primary", "secondary"] {
                std::fs::write(
                    immutable.join(format!("{n:05}.{ext}")),
                    format!("local {n}"),
                )
                .unwrap();
            }
        }
        std::fs::create_dir_all(chain.join("volatile")).unwrap();
        std::fs::write(chain.join("volatile").join("blocks-0.dat"), b"blocks").unwrap();
        std::fs::create_dir_all(chain.join("ledger").join("999")).unwrap();
        std::fs::write(chain.join("ledger").join("999").join("state"), b"ledger").unwrap();
        std::fs::create_dir_all(chain.join("lsm")).unwrap();
        std::fs::write(chain.join("lsm").join("table"), b"lsm").unwrap();
        std::fs::write(chain.join("clean"), b"").unwrap();
        std::fs::write(chain.join("protocolMagicId"), b"1").unwrap();
        chain
    }
}

/// Every entry under `dir` with the contents of each file, to show that a
/// directory was left exactly as it was.
fn tree(dir: &Path) -> BTreeMap<PathBuf, Option<Vec<u8>>> {
    let mut entries = BTreeMap::new();
    let mut stack = vec![dir.to_path_buf()];
    while let Some(d) = stack.pop() {
        for entry in std::fs::read_dir(&d).unwrap() {
            let path = entry.unwrap().path();
            let rel = path.strip_prefix(dir).unwrap().to_path_buf();
            if path.is_dir() {
                entries.insert(rel, None);
                stack.push(path);
            } else {
                entries.insert(rel, Some(std::fs::read(&path).unwrap()));
            }
        }
    }
    entries
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// The `--download-dir` mock-mithril-client was given, from the arguments it
/// recorded in `args_file`.
fn download_dir(args_file: &Path) -> PathBuf {
    let args: Vec<String> =
        serde_json::from_str(&std::fs::read_to_string(args_file).unwrap()).unwrap();
    let i = args.iter().position(|a| a == "--download-dir").unwrap();
    PathBuf::from(&args[i + 1])
}

fn event_reader(stdout: std::process::ChildStdout) -> mpsc::Receiver<Value> {
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

/// Like `expect`, but with one deadline for the whole wait: a process stuck in
/// a restart loop keeps events flowing, which `expect` would wait on forever.
fn expect_within(rx: &mpsc::Receiver<Value>, name: &str, total: Duration) -> Value {
    let deadline = std::time::Instant::now() + total;
    loop {
        let left = deadline
            .checked_duration_since(std::time::Instant::now())
            .unwrap_or_else(|| panic!("'{name}' did not arrive within {total:?}"));
        let v = rx
            .recv_timeout(left)
            .unwrap_or_else(|_| panic!("'{name}' did not arrive within {total:?}"));
        if v["event"] == name {
            return v;
        }
    }
}

/// Assert that no event named in `names` arrives within `ms` milliseconds.
fn expect_none_of(rx: &mpsc::Receiver<Value>, names: &[&str], ms: u64) {
    let deadline = std::time::Instant::now() + Duration::from_millis(ms);
    while let Some(left) = deadline.checked_duration_since(std::time::Instant::now()) {
        match rx.recv_timeout(left) {
            Ok(v) => assert!(
                !names.iter().any(|n| v["event"] == *n),
                "unexpected event: {v}"
            ),
            Err(_) => return,
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

const MOCK_NODE_CRASH_ONCE: &str = env!("CARGO_BIN_EXE_mock-node-crash-once");

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
    node_max_crash_attempts: u32,
    node_crash_restart_delay_ms: u64,
    node_stop_timeout_secs: Option<u64>,
    wallet_stop_timeout_secs: Option<u64>,
    node_crash_window: Option<(u32, u64)>,
    wallet_crash_window: Option<(u32, u64)>,
    wallet_health: Option<Value>,
    startup_silence_secs: Option<u64>,
    with_mithril: bool,
    mithril_stall: Option<(u64, u64)>,
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
            node_max_crash_attempts: 10,
            node_crash_restart_delay_ms: 0,
            node_stop_timeout_secs: None,
            wallet_stop_timeout_secs: None,
            node_crash_window: None,
            wallet_crash_window: None,
            wallet_health: None,
            startup_silence_secs: None,
            with_mithril: false,
            mithril_stall: None,
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

    fn node_max_crash_attempts(mut self, n: u32) -> Self {
        self.node_max_crash_attempts = n;
        self
    }

    fn node_crash_restart_delay_ms(mut self, ms: u64) -> Self {
        self.node_crash_restart_delay_ms = ms;
        self
    }

    fn node_stop_timeout_secs(mut self, secs: u64) -> Self {
        self.node_stop_timeout_secs = Some(secs);
        self
    }

    fn wallet_stop_timeout_secs(mut self, secs: u64) -> Self {
        self.wallet_stop_timeout_secs = Some(secs);
        self
    }

    fn node_crash_window(mut self, max: u32, secs: u64) -> Self {
        self.node_crash_window = Some((max, secs));
        self
    }

    fn wallet_crash_window(mut self, max: u32, secs: u64) -> Self {
        self.wallet_crash_window = Some((max, secs));
        self
    }

    /// Turns the wallet health check on, with `overrides` laid over thresholds
    /// of tens of milliseconds, so a test sees a verdict in a second or two.
    fn wallet_health(mut self, overrides: Value) -> Self {
        let mut health = json!({
            "probe_interval_ms": 50,
            "probe_timeout_ms": 200,
            "failure_threshold": 3,
            "startup_grace_ms": 60_000,
            "max_restarts": 5,
            "restart_window_ms": 600_000,
            "stop_timeout_secs": 5
        });
        for (k, v) in overrides.as_object().expect("an object") {
            health[k] = v.clone();
        }
        self.wallet_health = Some(health);
        self
    }

    fn startup_silence_secs(mut self, secs: u64) -> Self {
        self.startup_silence_secs = Some(secs);
        self
    }

    fn mithril(mut self) -> Self {
        self.with_mithril = true;
        self
    }

    /// Mithril with a download stall noticed after `stall_secs` and
    /// rechecked every `recheck_secs`.
    fn mithril_stall(mut self, stall_secs: u64, recheck_secs: u64) -> Self {
        self.with_mithril = true;
        self.mithril_stall = Some((stall_secs, recheck_secs));
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
                "socket_path": self.socket_path.to_str().unwrap(),
                "max_crash_attempts": self.node_max_crash_attempts,
                "crash_restart_delay_ms": self.node_crash_restart_delay_ms
            },
            "wallet": {
                "exe": self.wallet_exe,
                "args": self.wallet_args,
                "state_dir": state,
                "api_port": self.wallet_port,
                "restart_delay_ms": self.restart_delay_ms,
                "max_restart_attempts": self.max_restarts
            },
            "pub_logs_dir": logs.to_str().unwrap()
        });
        if let Some(secs) = self.node_stop_timeout_secs {
            cfg["node"]["stop_timeout_secs"] = json!(secs);
        }
        if let Some(secs) = self.wallet_stop_timeout_secs {
            cfg["wallet"]["stop_timeout_secs"] = json!(secs);
        }
        if let Some(secs) = self.startup_silence_secs {
            cfg["node"]["startup_silence_secs"] = json!(secs);
        }
        if let Some(health) = self.wallet_health {
            cfg["wallet"]["health"] = health;
        }
        if let Some((max, secs)) = self.node_crash_window {
            cfg["node"]["max_crashes_in_window"] = json!(max);
            cfg["node"]["crash_window_secs"] = json!(secs);
        }
        if let Some((max, secs)) = self.wallet_crash_window {
            cfg["wallet"]["max_crashes_in_window"] = json!(max);
            cfg["wallet"]["crash_window_secs"] = json!(secs);
        }

        if self.with_mithril {
            let mithril_bin = self.mithril_bin.unwrap_or(MOCK_MITHRIL);
            let converter_bin = self.converter_bin.unwrap_or(MOCK_CONVERTER);
            cfg["mithril"] = json!({
                "mithril_bin": mithril_bin,
                "snapshot_converter_bin": converter_bin,
                "converter_config": "/dev/null",
                "aggregator_url": "http://localhost:0",
                "genesis_vkey": "test",
                "ancillary_vkey": "test",
                "state_dir": state,
                "chain_path": self.dir.path().join("chain").to_str().unwrap(),
                "behind_threshold": 20,
                // An install whose rename fails, as the tests make it fail,
                // fails at once instead of trying again for a minute.
                "rename_retry_secs": 0
            });
            if let Some((stall_secs, recheck_secs)) = self.mithril_stall {
                cfg["mithril"]["download_stall_secs"] = json!(stall_secs);
                cfg["mithril"]["network_recheck_secs"] = json!(recheck_secs);
            }
        }

        (cfg, self.wallet_port)
    }
}

fn spawn_watchdog(config: &Value) -> (Child, ChildStdin, mpsc::Receiver<Value>) {
    spawn_watchdog_with_env(config, &[])
}

/// Like `spawn_watchdog`, with extra environment variables that the watchdog
/// passes on to the mock binaries it spawns.
fn spawn_watchdog_with_env(
    config: &Value,
    envs: &[(&str, &str)],
) -> (Child, ChildStdin, mpsc::Receiver<Value>) {
    let state_dir = config["node"]["state_dir"]
        .as_str()
        .expect("node.state_dir");
    let config_path = std::path::Path::new(state_dir).join("daedalus-config.json");
    std::fs::write(&config_path, serde_json::to_string_pretty(config).unwrap()).unwrap();

    let mut child = Command::new(WATCHDOG)
        .arg("--config")
        .arg(&config_path)
        .envs(envs.iter().copied())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn watchdog");

    let stdin = child.stdin.take().unwrap();
    let stdout = child.stdout.take().unwrap();
    let rx = event_reader(stdout);

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

    let preparing = expect_with(&rx, "mithril_status", |v| v["phase"] == "preparing");
    assert_eq!(preparing["rebuild"], false, "{preparing}");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "downloading");
    expect(&rx, "mithril_progress"); // at least one progress event
    expect_with(&rx, "mithril_status", |v| v["phase"] == "converting");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "installing");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "finalizing");

    // Node and wallet restart after install.
    expect(&rx, "node_started");
    // completed fires right after node_started (before wallet is up) so the
    // renderer can leave the Mithril overlay and show normal sync progress.
    let completed = expect_with(&rx, "mithril_status", |v| v["phase"] == "completed");
    assert_eq!(completed["rebuild"], false, "{completed}");
    expect(&rx, "node_socket_ready");
    expect(&rx, "wallet_started");
    expect(&rx, "wallet_ready");
    assert!(!dir.path().join("chain-rebuild.json").exists());

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

    let preparing = expect_with(&rx, "mithril_status", |v| v["phase"] == "preparing");
    assert_eq!(preparing["rebuild"], false, "{preparing}");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "downloading");
    expect(&rx, "mithril_progress");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "installing");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "finalizing");

    // Comes back up.
    expect(&rx, "node_started");
    let completed = expect_with(&rx, "mithril_status", |v| v["phase"] == "completed");
    assert_eq!(completed["rebuild"], false, "{completed}");
    expect(&rx, "node_socket_ready");
    expect(&rx, "wallet_started");
    expect(&rx, "wallet_ready");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A network without Mithril, such as a local test cluster: a Mithril sync or
/// a rebuild stops the node, starts it again on the chain it had and keeps
/// the watchdog running.
#[test]
fn mithril_request_without_mithril_restarts_the_node() {
    let dir = TempDir::new("no-mithril");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect(&rx, "chain_status");
    expect(&rx, "node_started");
    expect(&rx, "wallet_ready");

    for cmd in [
        json!({"cmd": "start_mithril", "force": true}),
        json!({"cmd": "start_mithril", "wipe_chain": true}),
    ] {
        send(&mut stdin, cmd);
        expect(&rx, "node_shutdown_ms");
        expect(&rx, "node_started");
        expect(&rx, "wallet_ready");
    }
    assert!(dir.path().join("chain").join("protocolMagicId").exists());

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Partial sync with the local chain behind the certified tip: the highest
/// local chunk (the one the node was still appending to) is downloaded again
/// and replaced, so no blocks are missing between it and the next chunk.
#[test]
fn partial_sync_replaces_incomplete_highest_local_chunk() {
    let dir = TempDir::new("partial-sync-gap");
    let immutable = dir.path().join("chain").join("immutable");
    std::fs::create_dir_all(&immutable).unwrap();
    let trio = ["chunk", "primary", "secondary"];
    for n in 0..=5u64 {
        let content = if n == 5 {
            "local 5 prefix".to_string()
        } else {
            format!("local {n}")
        };
        for ext in trio {
            std::fs::write(immutable.join(format!("{n:05}.{ext}")), &content).unwrap();
        }
    }
    let args_file = dir.path().join("mithril-download-args.json");

    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[
            ("MOCK_CERTIFIED_IMMUTABLE", "30"),
            ("MOCK_MITHRIL_ARGS_FILE", args_file.to_str().unwrap()),
        ],
    );

    expect(&rx, "wallet_ready");
    send(&mut stdin, json!({"cmd": "start_mithril", "force": true}));
    expect_with(&rx, "mithril_status", |v| v["phase"] == "finalizing");
    expect(&rx, "node_started");

    let args: Vec<String> =
        serde_json::from_str(&std::fs::read_to_string(&args_file).unwrap()).unwrap();
    let flag = |name: &str| args.windows(2).find(|w| w[0] == name).map(|w| w[1].clone());
    assert_eq!(flag("--start").as_deref(), Some("5"), "args: {args:?}");
    assert_eq!(flag("--end").as_deref(), Some("30"), "args: {args:?}");
    assert!(
        args.iter().any(|a| a == "--allow-override"),
        "args: {args:?}"
    );
    // The download is staged in the chain directory, on the chain's volume,
    // and nothing of it is left once installed.
    let chain = dir.path().join("chain");
    assert_eq!(
        download_dir(&args_file),
        chain.join("mithril-staging").join("download")
    );
    assert!(!chain.join("mithril-staging").exists());

    let read = |n: u64, ext: &str| {
        std::fs::read_to_string(immutable.join(format!("{n:05}.{ext}"))).unwrap()
    };
    for ext in trio {
        assert_eq!(read(4, ext), "local 4");
        assert_eq!(read(5, ext), "certified 5");
        assert_eq!(read(30, ext), "certified 30");
        assert_eq!(read(31, ext), "ancillary");
    }

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── Tests: wallet supervision ────────────────────────────────────────────────

/// Wallet crashes on every attempt → circuit breaker fires after max_restarts.
/// The watchdog then keeps the node running and waits: restart_wallet retries.
#[test]
fn wallet_circuit_breaker() {
    let dir = TempDir::new("circuit");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET_CRASH)
        .max_restarts(3)
        .restart_delay(0)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "node_socket_ready");

    for i in 0..3u32 {
        expect(&rx, "wallet_started");
        let exited = expect(&rx, "wallet_exited");
        assert_eq!(exited["phase"], "pre_ready", "cycle {i}: wrong phase");
        assert_eq!(exited["code"], 1, "cycle {i}: wrong code");
    }

    let unrec = expect(&rx, "wallet_unrecoverable");
    assert_eq!(unrec["attempt"], 3);

    // No further wallet start, and the node is not stopped.
    expect_none_of(&rx, &["wallet_started", "node_shutdown_ms", "stopped"], 500);

    // Retry starts the count again: three more failed starts, not one.
    send(&mut stdin, json!({"cmd": "restart_wallet"}));
    for i in 0..3u32 {
        expect(&rx, "wallet_started");
        let exited = expect(&rx, "wallet_exited");
        assert_eq!(exited["phase"], "pre_ready", "retry cycle {i}");
    }
    let unrec = expect(&rx, "wallet_unrecoverable");
    assert_eq!(unrec["attempt"], 3);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A wallet that crashes each time after becoming ready restarts until the
/// crash window is exhausted, then stays stopped while the node keeps running.
#[test]
fn wallet_crashing_after_ready_exhausts_the_crash_window() {
    let dir = TempDir::new("wallet-crash-window");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .restart_delay(0)
        .wallet_crash_window(3, 600)
        .build();
    // Longer than the watchdog's 500 ms port poll, so each crash follows ready.
    let (mut child, mut stdin, rx) =
        spawn_watchdog_with_env(&cfg, &[("MOCK_WALLET_CRASH_AFTER_MS", "1500")]);

    let node = expect(&rx, "node_started");
    for i in 0..3 {
        expect(&rx, "wallet_ready");
        let exited = expect(&rx, "wallet_exited");
        assert_eq!(exited["phase"], "post_ready", "crash {i}");
    }
    let unrec = expect_within(&rx, "wallet_unrecoverable", Duration::from_secs(10));
    // Each crash followed a successful start, so the consecutive count is 1.
    assert_eq!(unrec["attempt"], 1);

    expect_none_of(&rx, &["wallet_started", "node_shutdown_ms", "stopped"], 700);
    let node_pid = node["pid"].as_i64().unwrap() as i32;
    assert!(
        nix::sys::signal::kill(nix::unistd::Pid::from_raw(node_pid), None).is_ok(),
        "the node must keep running"
    );

    // Retry clears the crash window: three more crashes before the next stop.
    send(&mut stdin, json!({"cmd": "restart_wallet"}));
    for i in 0..3 {
        expect(&rx, "wallet_ready");
        let exited = expect(&rx, "wallet_exited");
        assert_eq!(exited["phase"], "post_ready", "retry crash {i}");
    }
    expect_within(&rx, "wallet_unrecoverable", Duration::from_secs(10));

    stop(&mut stdin);
    expect(&rx, "node_shutdown_ms");
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── Tests: node failures ──────────────────────────────────────────────────────

/// Node crashes before socket repeatedly → watchdog restarts until
/// max_crash_attempts, then reports it unrecoverable and waits.
#[test]
fn node_crash_before_socket() {
    let dir = TempDir::new("node-crash");
    dir.populate_chain();
    // Use max_crash_attempts=2 so the test finishes quickly without hitting the default 10.
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE_CRASH, MOCK_WALLET)
        .node_max_crash_attempts(2)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect(&rx, "chain_status");

    // Two crash attempts before giving up.
    for _ in 0..2 {
        expect(&rx, "node_started");
        let exited = expect(&rx, "node_exited");
        assert_eq!(exited["code"], 1);
    }

    // After max_crash_attempts the watchdog emits an error and waits.
    expect(&rx, "error");
    let unrec = expect(&rx, "node_unrecoverable");
    assert_eq!(unrec["crashes"], 2);
    expect_none_of(&rx, &["node_started", "stopped"], 500);

    stop(&mut stdin);
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

/// Poll try_wait() until the child exits; panic after the deadline.
fn wait_for_exit(child: &mut Child, deadline: Duration) {
    let start = std::time::Instant::now();
    loop {
        if child.try_wait().unwrap().is_some() {
            return;
        }
        assert!(start.elapsed() < deadline, "timeout waiting for exit");
        std::thread::sleep(Duration::from_millis(50));
    }
}

/// A SIGKILLed watchdog cannot run any shutdown path; the PDEATHSIG tether
/// set at spawn time must reap the wallet (and node) anyway.
#[cfg(target_os = "linux")]
#[test]
fn sigkill_watchdog_kills_children() {
    let dir = TempDir::new("sigkill");
    dir.populate_chain();
    let (cfg, wallet_port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "wallet_ready");
    assert!(std::net::TcpStream::connect(("127.0.0.1", wallet_port)).is_ok());

    child.kill().unwrap(); // SIGKILL — no Drop handlers, no Stop path
    let _ = child.wait();
    drop(stdin);

    // PDEATHSIG delivery is immediate; poll briefly to absorb scheduler lag.
    let start = std::time::Instant::now();
    loop {
        if std::net::TcpStream::connect(("127.0.0.1", wallet_port)).is_err() {
            break;
        }
        assert!(
            start.elapsed() < Duration::from_secs(5),
            "wallet survived watchdog SIGKILL"
        );
        std::thread::sleep(Duration::from_millis(50));
    }
}

/// A closed stdout (crashed parent) must not abort the watchdog: the EPIPE'd
/// event is dropped, and the stdin EOF that follows still runs the orderly
/// shutdown, so the wallet is stopped rather than orphaned.
#[test]
fn stdout_close_does_not_orphan_children() {
    let dir = TempDir::new("epipe");
    dir.populate_chain();
    let (cfg, wallet_port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();

    let state_dir = cfg["node"]["state_dir"].as_str().expect("node.state_dir");
    let config_path = std::path::Path::new(state_dir).join("daedalus-config.json");
    std::fs::write(&config_path, serde_json::to_string_pretty(&cfg).unwrap()).unwrap();

    let mut child = Command::new(WATCHDOG)
        .arg("--config")
        .arg(&config_path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn watchdog");
    let mut stdin = child.stdin.take().unwrap();
    let stdout = child.stdout.take().unwrap();

    // Read events inline (not via event_reader) so this test owns stdout and
    // can close it deterministically.
    let mut reader = BufReader::new(stdout);
    loop {
        let mut line = String::new();
        assert!(reader.read_line(&mut line).unwrap() > 0, "unexpected EOF");
        if line.contains("wallet_ready") {
            break;
        }
    }
    drop(reader); // close the parent's read end of the watchdog's stdout

    // Trigger an event emission: the probe result's write hits EPIPE.
    send(&mut stdin, json!({"cmd": "probe_mithril"}));
    std::thread::sleep(Duration::from_millis(500));
    assert!(
        child.try_wait().unwrap().is_none(),
        "watchdog must survive a broken stdout pipe"
    );
    assert!(
        std::net::TcpStream::connect(("127.0.0.1", wallet_port)).is_ok(),
        "wallet should still be running"
    );

    drop(stdin); // EOF → implicit stop → orderly shutdown
    wait_for_exit(&mut child, Duration::from_secs(15));
    assert!(
        std::net::TcpStream::connect(("127.0.0.1", wallet_port)).is_err(),
        "wallet must not outlive the watchdog"
    );
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

// ── Tests: chain damage detection ─────────────────────────────────────────────

fn fixture(name: &str) -> String {
    format!("{}/tests/fixtures/{name}", env!("CARGO_MANIFEST_DIR"))
}

/// Run a full start with `env` set, wait for the wallet, and return the
/// `chain_damage` events seen up to a short while after the stack is ready.
fn chain_damage_events(label: &str, env: &[(&str, &str)]) -> Vec<Value> {
    let dir = TempDir::new(label);
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(&cfg, env);

    let mut seen = Vec::new();
    let mut ready = false;
    let deadline = std::time::Instant::now() + Duration::from_secs(15);
    while let Some(left) = deadline.checked_duration_since(std::time::Instant::now()) {
        let Ok(v) = rx.recv_timeout(left) else { break };
        if v["event"] == "chain_damage" {
            seen.push(v);
        } else if v["event"] == "wallet_ready" {
            ready = true;
        }
        if ready {
            // The mock prints its lines before the node or wallet is ready,
            // so anything they cause has arrived by now.
            std::thread::sleep(Duration::from_millis(300));
            while let Ok(v) = rx.try_recv() {
                if v["event"] == "chain_damage" {
                    seen.push(v);
                }
            }
            break;
        }
    }
    assert!(ready, "wallet never became ready");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
    seen
}

/// The missing ledger point line on node output produces one event.
#[test]
fn chain_damage_ledger_point_missing() {
    let lines = fixture("node_ledger_point_missing.txt");
    let events = chain_damage_events("damage-ledger", &[("MOCK_NODE_EXTRA_LINES", &lines)]);
    assert_eq!(events.len(), 1, "{events:?}");
    assert_eq!(events[0]["kind"], "ledger_point_missing");
    assert_eq!(events[0]["slot"], 200_102_375u64);
    assert!(events[0]["last_good_slot"].is_null());
}

/// Rejections of one slot from three distinct peers produce one event.
#[test]
fn chain_damage_block_rejected_by_three_peers() {
    let lines = fixture("node_bad_inputs_three_peers.txt");
    let events = chain_damage_events("damage-three-peers", &[("MOCK_NODE_EXTRA_LINES", &lines)]);
    assert_eq!(events.len(), 1, "{events:?}");
    assert_eq!(events[0]["kind"], "node_rejects_canonical_block");
    assert_eq!(events[0]["slot"], 200_361_988u64);
}

/// Repeated rejections from a single peer produce no event.
#[test]
fn chain_damage_block_rejected_by_one_peer() {
    let lines = fixture("node_bad_inputs_one_peer.txt");
    let events = chain_damage_events("damage-one-peer", &[("MOCK_NODE_EXTRA_LINES", &lines)]);
    assert!(events.is_empty(), "{events:?}");
}

/// The wallet discontinuity line, logged once per wallet worker, produces one
/// event carrying both edges of the gap.
#[test]
fn chain_damage_wallet_discontinuity() {
    let lines = fixture("wallet_discontinuity.txt");
    let events = chain_damage_events("damage-wallet", &[("MOCK_WALLET_EXTRA_LINES", &lines)]);
    assert_eq!(events.len(), 1, "{events:?}");
    assert_eq!(events[0]["kind"], "wallet_chain_discontinuity");
    assert_eq!(events[0]["slot"], 200_102_408u64);
    assert_eq!(events[0]["last_good_slot"], 200_080_892u64);
}

/// Node signatures on wallet output and the wallet signature on node output
/// are not recognised.
#[test]
fn chain_damage_signatures_are_read_from_their_own_process() {
    let node_lines = fixture("wallet_discontinuity.txt");
    let wallet_lines = fixture("node_ledger_point_missing.txt");
    let events = chain_damage_events(
        "damage-crossed",
        &[
            ("MOCK_NODE_EXTRA_LINES", &node_lines),
            ("MOCK_WALLET_EXTRA_LINES", &wallet_lines),
        ],
    );
    assert!(events.is_empty(), "{events:?}");
}

// ── Tests: Mithril download waiting for the network ──────────────────────────

/// A download that stalls while mithril-client cannot reach the aggregator
/// is reported as waiting for the network, and the wait ends when it goes
/// on. The client is left running, and the install finishes.
#[test]
fn mithril_download_waits_for_the_network() {
    let dir = TempDir::new("mithril-network-wait");
    // The mock client is offline while its download stalls, so its snapshot
    // query fails then.
    let offline = dir.path().join("offline");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .mithril_stall(1, 1)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[
            ("MOCK_MITHRIL_DOWNLOAD_STALL_MS", "4000"),
            ("MOCK_MITHRIL_OFFLINE_FILE", offline.to_str().unwrap()),
        ],
    );

    let status = expect(&rx, "chain_status");
    assert_eq!(status["has_chain"], false);
    send(&mut stdin, json!({"cmd": "start_mithril"}));
    let waiting = expect(&rx, "mithril_waiting_for_network");
    assert_eq!(waiting["waiting"], true, "{waiting}");
    let resumed = expect(&rx, "mithril_waiting_for_network");
    assert_eq!(resumed["waiting"], false, "{resumed}");
    expect(&rx, "node_started");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "completed");

    let log =
        std::fs::read_to_string(dir.path().join("logs").join("watchdog.log")).unwrap_or_default();
    assert_eq!(
        log.matches("cannot be reached; waiting for the network")
            .count(),
        1,
        "{log}"
    );
    assert!(log.contains("no longer waiting for the network"), "{log}");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A download that stalls while mithril-client can reach the aggregator is
/// not reported as waiting for the network, whatever a direct connection
/// would find: the configured aggregator URL here accepts no connections.
#[test]
fn mithril_download_stall_with_a_reachable_aggregator_reports_nothing() {
    let dir = TempDir::new("mithril-network-reachable");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .mithril_stall(1, 1)
        .build();
    let (mut child, mut stdin, rx) =
        spawn_watchdog_with_env(&cfg, &[("MOCK_MITHRIL_DOWNLOAD_STALL_MS", "3000")]);

    expect(&rx, "chain_status");
    send(&mut stdin, json!({"cmd": "start_mithril"}));
    loop {
        let v = rx
            .recv_timeout(Duration::from_secs(15))
            .expect("timeout waiting for node_started");
        assert_ne!(v["event"], "mithril_waiting_for_network", "{v}");
        if v["event"] == "node_started" {
            break;
        }
    }

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── Tests: startup silence ───────────────────────────────────────────────────

/// A node that writes nothing while it starts is reported once per silence,
/// output that arrives afterwards ends the silence, and a later silence is
/// reported again. restart_node restarts a node that has not reached
/// chainDbReady.
#[test]
fn startup_silence_is_reported_once_and_ends_with_output() {
    let dir = TempDir::new("startup-silence");
    dir.populate_chain();
    let cfg = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).startup_silence_secs(1);
    let finish = PathBuf::from(format!("{}.finish", cfg.socket_path.display()));
    let (cfg, _port) = cfg.build();
    let line = dir.path().join("partial-line.txt");
    std::fs::write(&line, "Replaying ledger from snapshot\n").unwrap();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[("MOCK_NODE_UNTERMINATED_LINE", line.to_str().unwrap())],
    );

    expect(&rx, "node_started");
    let silent = expect(&rx, "node_silent");
    assert_eq!(silent["silent_secs"], 1);
    expect_none_of(&rx, &["node_silent", "node_output_resumed"], 1500);

    std::fs::write(&finish, b"").unwrap();
    expect(&rx, "node_output_resumed");
    expect(&rx, "node_silent");

    let log =
        std::fs::read_to_string(dir.path().join("logs").join("watchdog.log")).unwrap_or_default();
    assert!(
        log.contains("node: no output for 1 s while starting"),
        "{log}"
    );

    send(&mut stdin, json!({"cmd": "restart_node"}));
    expect(&rx, "node_shutdown_ms");
    expect(&rx, "node_started");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── Tests: ledger snapshot check before a node start ─────────────────────────

/// CRC-32 as zlib computes it, which the node's chunk and snapshot checksums
/// are.
fn crc32(bytes: &[u8]) -> u32 {
    let mut crc = 0xFFFF_FFFFu32;
    for &b in bytes {
        crc ^= u32::from(b);
        for _ in 0..8 {
            crc = if crc & 1 != 0 {
                0xEDB8_8320 ^ (crc >> 1)
            } else {
                crc >> 1
            };
        }
    }
    !crc
}

/// Slots per chunk in the test databases: ten times the k the test Byron
/// genesis gives.
const TEST_CHUNK_SLOTS: u64 = 100;

/// Writes ImmutableDB chunk `chunk` into `immutable` with a block at each of
/// `slots`, its secondary index carrying each block's CRC32, and a whole
/// primary index.
fn write_chunk(immutable: &Path, chunk: u64, slots: &[u64]) {
    let mut data = Vec::new();
    let mut secondary = Vec::new();
    let mut filled = vec![false; TEST_CHUNK_SLOTS as usize + 1];
    for &slot in slots {
        let block = format!("block at slot {slot}").into_bytes();
        secondary.extend_from_slice(&(data.len() as u64).to_be_bytes());
        secondary.extend_from_slice(&[0, 0, 0, 0]);
        secondary.extend_from_slice(&crc32(&block).to_be_bytes());
        secondary.extend_from_slice(&[0xAB; 32]);
        secondary.extend_from_slice(&slot.to_be_bytes());
        data.extend_from_slice(&block);
        filled[(slot % TEST_CHUNK_SLOTS) as usize + 1] = true;
    }
    let mut primary = vec![1u8];
    let mut offset = 0u32;
    primary.extend_from_slice(&offset.to_be_bytes());
    for f in filled {
        if f {
            offset += 56;
        }
        primary.extend_from_slice(&offset.to_be_bytes());
    }
    std::fs::write(immutable.join(format!("{chunk:05}.chunk")), data).unwrap();
    std::fs::write(immutable.join(format!("{chunk:05}.secondary")), secondary).unwrap();
    std::fs::write(immutable.join(format!("{chunk:05}.primary")), primary).unwrap();
}

/// Creates `<state_dir>/chain` as a cleanly stopped database whose chunk 0
/// ends at slot 20, followed by chunks 1 and 2, with one LSM ledger snapshot
/// at `snapshot_slot`. Returns the node arguments that point the node at it,
/// with a node configuration that selects the LSM backend and a Byron
/// genesis with k = 10.
fn populate_checked_database(dir: &TempDir, socket_path: &Path, snapshot_slot: u64) -> Vec<String> {
    let chain = dir.path().join("chain");
    let immutable = chain.join("immutable");
    std::fs::create_dir_all(&immutable).unwrap();
    write_chunk(&immutable, 0, &[10, 20]);
    write_chunk(&immutable, 1, &[110, 150]);
    write_chunk(&immutable, 2, &[210, 250]);
    let name = snapshot_slot.to_string();
    let snapshot = chain.join("ledger").join(&name);
    std::fs::create_dir_all(&snapshot).unwrap();
    let state = b"ledger state".to_vec();
    std::fs::write(snapshot.join("state"), &state).unwrap();
    std::fs::write(
        snapshot.join("meta"),
        json!({"backend": "utxohd-lsm", "checksum": crc32(&state), "tablesCodecVersion": 1})
            .to_string(),
    )
    .unwrap();
    std::fs::write(snapshot.join("utxoSize"), b"3").unwrap();
    std::fs::create_dir_all(chain.join("lsm").join("snapshots").join(&name)).unwrap();
    std::fs::create_dir_all(chain.join("volatile")).unwrap();
    std::fs::write(chain.join("clean"), b"").unwrap();
    // The network magic mock-mithril-client writes into what it installs.
    std::fs::write(chain.join("protocolMagicId"), b"764824073").unwrap();

    let config = dir.path().join("node-config.json");
    std::fs::write(
        &config,
        json!({
            "ByronGenesisFile": "genesis-byron.json",
            "LedgerDB": {"Backend": "V2LSM", "LSMDatabasePath": "lsm/"}
        })
        .to_string(),
    )
    .unwrap();
    std::fs::write(
        dir.path().join("genesis-byron.json"),
        json!({"protocolConsts": {"k": 10, "protocolMagic": 764824073}}).to_string(),
    )
    .unwrap();
    vec![
        socket_path.to_str().unwrap().to_string(),
        "--config".to_string(),
        config.to_str().unwrap().to_string(),
        "--database-path".to_string(),
        chain.to_str().unwrap().to_string(),
    ]
}

/// A database whose newest ledger snapshot names a slot missing from the
/// ImmutableDB is reported as ledger_point_missing before the node starts,
/// and the node is not started. A retry checks again and reports it again.
#[test]
fn ledger_point_missing_before_start_holds_the_node() {
    let dir = TempDir::new("ledger-point-held");
    let mut cfg = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET);
    cfg.node_args = populate_checked_database(&dir, &cfg.socket_path, 50);
    let (cfg, _port) = cfg.build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    let status = expect(&rx, "chain_status");
    assert_eq!(status["has_chain"], true);
    let damage = expect(&rx, "chain_damage");
    assert_eq!(damage["kind"], "ledger_point_missing");
    assert_eq!(damage["slot"], 50);
    assert!(damage["last_good_slot"].is_null());
    assert_eq!(damage["node_held"], true);
    expect_none_of(&rx, &["node_started"], 1000);

    send(&mut stdin, json!({"cmd": "restart_node"}));
    let again = expect(&rx, "chain_damage");
    assert_eq!(again["node_held"], true);
    expect_none_of(&rx, &["node_started"], 1000);

    let log =
        std::fs::read_to_string(dir.path().join("logs").join("watchdog.log")).unwrap_or_default();
    assert!(
        log.contains("chain damage detected: kind=ledger_point_missing, slot=50"),
        "{log}"
    );

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A database whose newest ledger snapshot names a block that is there starts
/// the node as before.
#[test]
fn ledger_point_present_starts_the_node() {
    let dir = TempDir::new("ledger-point-present");
    let mut cfg = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET);
    cfg.node_args = populate_checked_database(&dir, &cfg.socket_path, 20);
    let (cfg, _port) = cfg.build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    loop {
        let v = rx
            .recv_timeout(Duration::from_secs(15))
            .expect("timeout waiting for node_started");
        assert_ne!(v["event"], "chain_damage", "{v}");
        if v["event"] == "node_started" {
            break;
        }
    }
    expect(&rx, "wallet_ready");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A rebuild sent while the node is held runs without a node to stop, and the
/// node starts on the database it installs.
#[test]
fn ledger_point_missing_before_start_rebuilds() {
    let dir = TempDir::new("ledger-point-rebuild");
    let mut cfg = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril();
    cfg.node_args = populate_checked_database(&dir, &cfg.socket_path, 50);
    let (cfg, _port) = cfg.build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    let damage = expect(&rx, "chain_damage");
    assert_eq!(damage["node_held"], true);

    send(
        &mut stdin,
        json!({"cmd": "start_mithril", "wipe_chain": true}),
    );
    let preparing = expect_with(&rx, "mithril_status", |v| v["phase"] == "preparing");
    assert_eq!(preparing["rebuild"], true, "{preparing}");
    expect(&rx, "node_started");
    let completed = expect_with(&rx, "mithril_status", |v| v["phase"] == "completed");
    assert_eq!(completed["rebuild"], true, "{completed}");
    expect(&rx, "wallet_ready");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// cardano-node writes the missing ledger point error without a newline. When
/// the node leaves it unterminated, the event arrives while the node is still
/// running, and when the rest of the line arrives it is logged once, whole,
/// and reported no more.
#[test]
fn chain_damage_from_an_unterminated_line_while_the_node_hangs() {
    let dir = TempDir::new("damage-unterminated");
    dir.populate_chain();
    let cfg = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET);
    let finish = PathBuf::from(format!("{}.finish", cfg.socket_path.display()));
    let (cfg, _port) = cfg.build();
    let line = fixture("node_ledger_point_missing.txt");
    let (mut child, mut stdin, rx) =
        spawn_watchdog_with_env(&cfg, &[("MOCK_NODE_UNTERMINATED_LINE", line.as_str())]);

    expect(&rx, "node_started");
    let damage = loop {
        let v = rx
            .recv_timeout(Duration::from_secs(15))
            .expect("no chain_damage while the node hangs");
        assert_ne!(v["event"], "node_exited", "{v}");
        if v["event"] == "chain_damage" {
            break v;
        }
    };
    assert_eq!(damage["kind"], "ledger_point_missing");
    assert_eq!(damage["slot"], 200_102_375u64);

    std::fs::write(&finish, b"").unwrap();
    let node_log = dir.path().join("logs").join("node.log");
    let deadline = std::time::Instant::now() + Duration::from_secs(15);
    loop {
        let log = std::fs::read_to_string(&node_log).unwrap_or_default();
        if log.contains("open ledgerdbNode configuration: NodeConfiguration {...}\n") {
            assert_eq!(
                log.matches("Critical invariant violation").count(),
                1,
                "{log}"
            );
            break;
        }
        assert!(
            std::time::Instant::now() < deadline,
            "the line was not logged: {log}"
        );
        std::thread::sleep(Duration::from_millis(50));
    }
    expect_none_of(&rx, &["chain_damage", "node_exited"], 1000);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── Tests: non-UTF-8 child output ─────────────────────────────────────────────

/// Poll until the mock has recorded `done`, then return its write results.
fn mock_write_results(socket_path: &Path) -> Vec<String> {
    let path = format!("{}.writes", socket_path.display());
    let deadline = std::time::Instant::now() + Duration::from_secs(10);
    loop {
        let text = std::fs::read_to_string(&path).unwrap_or_default();
        if text.lines().last() == Some("done") {
            return text.lines().map(String::from).collect();
        }
        assert!(
            std::time::Instant::now() < deadline,
            "mock node did not finish writing"
        );
        std::thread::sleep(Duration::from_millis(50));
    }
}

/// Run the non-UTF-8 mock node, optionally writing the bad line before the
/// startup phases, and assert the watchdog keeps draining its output.
fn assert_non_utf8_output_drained(label: &str, node_args: &[&str]) {
    let dir = TempDir::new(label);
    dir.populate_chain();
    let mut cfg = Cfg::new(&dir, MOCK_NODE_NON_UTF8, MOCK_WALLET);
    cfg.node_args
        .extend(node_args.iter().map(|a| a.to_string()));
    let socket_path = cfg.socket_path.clone();
    let (cfg, _port) = cfg.build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect(&rx, "node_started");
    expect_with(&rx, "node_startup_status", |v| v["phase"] == "chainDbReady");
    expect(&rx, "node_socket_ready");

    let writes = mock_write_results(&socket_path);
    assert!(
        writes.iter().all(|w| w == "ok" || w == "done"),
        "node saw a failed write to stdout: {writes:?}"
    );

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();

    let log = std::fs::read(dir.path().join("logs").join("node.log")).unwrap();
    let log = String::from_utf8(log).expect("node.log must be valid UTF-8");
    assert!(log.contains("J\u{FFFD}rgen"), "bad line missing: {log}");
    assert!(
        log.contains("after bad line 0\n"),
        "first later line missing"
    );
    assert!(
        log.contains("after bad line 19\n"),
        "last later line missing"
    );
}

/// A line that is not valid UTF-8 must not stop the watchdog reading node
/// output: later lines are logged and the node's writes keep succeeding.
#[test]
fn node_non_utf8_output_keeps_pipe_open() {
    assert_non_utf8_output_drained("non-utf8", &[]);
}

/// The same line arriving before the startup phase lines must not hide them:
/// the watchdog still reaches chainDbReady and node_socket_ready.
#[test]
fn node_non_utf8_output_before_startup_phases() {
    assert_non_utf8_output_drained("non-utf8-early", &["before-startup"]);
}

/// Run the mock node under a watchdog started with `inherited` as the value of
/// TRACE_DISPATCHER_LOGGING_HOSTNAME (or unset) and return the value the node
/// saw, which the mock echoes as the first line of its output.
fn node_trace_hostname(label: &str, inherited: Option<&str>) -> String {
    let dir = TempDir::new(label);
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let env: Vec<(&str, &str)> = inherited
        .map(|v| ("TRACE_DISPATCHER_LOGGING_HOSTNAME", v))
        .into_iter()
        .collect();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(&cfg, &env);

    expect(&rx, "watchdog_started");
    expect(&rx, "node_started");
    expect_with(&rx, "node_startup_status", |v| v["phase"] == "chainDbReady");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();

    let log = std::fs::read(dir.path().join("logs").join("node.log")).unwrap();
    let log = String::from_utf8_lossy(&log).into_owned();
    log.lines()
        .find_map(|l| l.strip_prefix("TRACE_DISPATCHER_LOGGING_HOSTNAME="))
        .unwrap_or_else(|| panic!("mock node did not echo the variable: {log}"))
        .to_string()
}

/// The node is spawned with a fixed ASCII host name for its trace lines.
#[test]
fn node_gets_fixed_trace_hostname() {
    assert_eq!(node_trace_hostname("trace-host", None), "daedalus");
}

/// A value inherited from the environment does not reach the node: a
/// non-ASCII machine name there would reintroduce the failure.
#[test]
fn node_trace_hostname_overrides_inherited() {
    assert_eq!(
        node_trace_hostname("trace-host-inherited", Some("J\u{fc}rgen-PC")),
        "daedalus"
    );
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

/// A line exceeding the per-line bound is discarded — not treated as EOF —
/// and later commands still work. (The bound is per line: a lifetime cap
/// would eventually shut a long session down mid-flight.)
#[test]
fn oversized_stdin_line_discarded() {
    let dir = TempDir::new("oversized");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "wallet_ready");

    // 5 MB without a newline — larger than the 4 MB per-line bound.
    let junk = vec![b'x'; 5 * 1024 * 1024];
    stdin.write_all(&junk).unwrap();
    stdin.write_all(b"\n").unwrap();
    stdin.flush().unwrap();

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

/// A Mithril sync that finds the local chain close enough to the certified
/// tip changes nothing and starts the node again. Once the node has started,
/// it ends with the `completed` status, as an installed sync does, so the
/// renderer leaves the Mithril view.
#[test]
fn mithril_not_needed_ends_with_completed_once_the_node_restarts() {
    let dir = TempDir::new("not-needed-completes");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .mithril_bin(env!("CARGO_BIN_EXE_mock-mithril-client-not-needed"))
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "wallet_ready");
    send(&mut stdin, json!({"cmd": "start_mithril"}));
    expect_with(&rx, "mithril_status", |v| v["phase"] == "preparing");
    expect(&rx, "mithril_not_needed");
    expect(&rx, "node_started");
    let completed = expect_with(&rx, "mithril_status", |v| v["phase"] == "completed");
    assert_eq!(completed["rebuild"], false, "{completed}");
    expect(&rx, "wallet_ready");

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
    let preparing = expect_with(&rx, "mithril_status", |v| v["phase"] == "preparing");
    assert_eq!(preparing["rebuild"], true, "{preparing}");
    let downloading = expect_with(&rx, "mithril_status", |v| v["phase"] == "downloading");
    assert_eq!(downloading["rebuild"], true, "{downloading}");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "installing");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "finalizing");

    expect(&rx, "node_started");
    let completed = expect_with(&rx, "mithril_status", |v| v["phase"] == "completed");
    assert_eq!(completed["rebuild"], true, "{completed}");
    expect(&rx, "wallet_ready");

    assert!(
        chain_path.exists(),
        "chain dir should be recreated after install"
    );

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A rebuild asks the aggregator before it deletes anything. When the probe
/// fails, PROBE_FAILED is reported, every chain file is left as it was, and
/// the node starts again on the chain it had.
#[test]
fn wipe_chain_probe_failure_keeps_the_chain() {
    let dir = TempDir::new("wipe-probe-fail");
    let chain = dir.populate_database();
    let before = tree(&chain);
    let args_file = dir.path().join("mithril-download-args.json");

    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[
            ("MOCK_MITHRIL_PROBE_FAIL", "1"),
            ("MOCK_MITHRIL_ARGS_FILE", args_file.to_str().unwrap()),
        ],
    );

    expect(&rx, "wallet_ready");
    send(
        &mut stdin,
        json!({"cmd": "start_mithril", "wipe_chain": true}),
    );

    expect(&rx, "node_shutdown_ms");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "preparing");
    let err = expect(&rx, "mithril_error");
    assert_eq!(err["code"], "PROBE_FAILED");
    assert_eq!(err["rebuild"], true, "{err}");
    expect(&rx, "node_started");
    expect(&rx, "wallet_ready");

    assert_eq!(tree(&chain), before);
    assert!(!args_file.exists(), "a download was started");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A rebuild replaces the chain however close it is to the certified tip:
/// the local chunks neither make the sync "not needed" nor narrow the
/// download to a partial range.
#[test]
fn wipe_chain_downloads_everything_when_the_chain_is_level() {
    let dir = TempDir::new("wipe-level");
    let chain = dir.populate_database();
    let args_file = dir.path().join("mithril-download-args.json");

    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[
            ("MOCK_CERTIFIED_IMMUTABLE", "3"),
            ("MOCK_MITHRIL_ARGS_FILE", args_file.to_str().unwrap()),
        ],
    );

    expect(&rx, "wallet_ready");
    send(
        &mut stdin,
        json!({"cmd": "start_mithril", "wipe_chain": true}),
    );
    expect_with(&rx, "mithril_status", |v| v["phase"] == "finalizing");
    expect(&rx, "node_started");

    let args: Vec<String> =
        serde_json::from_str(&std::fs::read_to_string(&args_file).unwrap()).unwrap();
    assert!(
        !args.iter().any(|a| a == "--start" || a == "--end"),
        "args: {args:?}"
    );
    assert!(!chain.join("immutable").join("00000.chunk").exists());
    assert!(!chain.join("ledger").join("999").exists());
    assert_eq!(
        std::fs::read(chain.join("protocolMagicId")).unwrap(),
        b"764824073"
    );
    assert_eq!(
        download_dir(&args_file),
        chain.join("mithril-staging").join("download")
    );
    assert!(!chain.join("mithril-staging").exists());

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A rebuild checks, before it deletes anything, that the chain's volume can
/// hold the snapshot once the old chain is gone. When it cannot, it reports
/// INSUFFICIENT_SPACE, leaves every chain file as it was, downloads nothing,
/// and the node starts again on the chain it had.
#[test]
fn wipe_chain_refuses_a_snapshot_that_does_not_fit_and_deletes_nothing() {
    let dir = TempDir::new("wipe-no-space");
    let chain = dir.populate_database();
    let before = tree(&chain);
    let args_file = dir.path().join("mithril-download-args.json");

    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[
            ("MOCK_SNAPSHOT_SIZE", "1000000000000000000"),
            ("MOCK_MITHRIL_ARGS_FILE", args_file.to_str().unwrap()),
        ],
    );

    expect(&rx, "wallet_ready");
    send(
        &mut stdin,
        json!({"cmd": "start_mithril", "wipe_chain": true}),
    );

    expect(&rx, "node_shutdown_ms");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "preparing");
    let err = expect(&rx, "mithril_error");
    assert_eq!(err["code"], "INSUFFICIENT_SPACE", "{err}");
    assert_eq!(err["rebuild"], true, "{err}");
    expect(&rx, "node_started");
    expect(&rx, "wallet_ready");

    assert_eq!(tree(&chain), before);
    assert!(!args_file.exists(), "a download was started");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A rebuild that cannot record itself in the state directory refuses with
/// REBUILD_MARKER_FAILED before it deletes or downloads anything, and the
/// node starts again on the chain it had.
#[test]
fn a_rebuild_that_cannot_record_itself_changes_nothing() {
    let dir = TempDir::new("rebuild-record-fail");
    let chain = dir.populate_database();
    let before = tree(&chain);
    let args_file = dir.path().join("mithril-download-args.json");
    // The record is written through a temporary file. A link from there into
    // a directory that does not exist makes that write fail, while the record
    // itself reads as absent.
    std::os::unix::fs::symlink(
        dir.path().join("missing").join("record"),
        dir.path().join("chain-rebuild.json.tmp"),
    )
    .unwrap();

    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[("MOCK_MITHRIL_ARGS_FILE", args_file.to_str().unwrap())],
    );

    expect(&rx, "wallet_ready");
    send(
        &mut stdin,
        json!({"cmd": "start_mithril", "wipe_chain": true}),
    );

    let err = expect(&rx, "mithril_error");
    assert_eq!(err["code"], "REBUILD_MARKER_FAILED", "{err}");
    assert_eq!(err["rebuild"], true, "{err}");
    expect(&rx, "node_started");
    expect(&rx, "wallet_ready");

    assert_eq!(tree(&chain), before);
    assert!(!args_file.exists(), "a download was started");
    assert!(!dir.path().join("chain-rebuild.json").exists());

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Records, in the state directory, a rebuild of `chain` as an earlier
/// session records it before it deletes anything. The record names no kind,
/// as records written before kinds existed, and reads as a rebuild's.
fn write_rebuild_marker(dir: &TempDir, chain: &Path) {
    std::fs::write(
        dir.path().join("chain-rebuild.json"),
        json!({ "chain_path": chain.to_str().unwrap() }).to_string(),
    )
    .unwrap();
}

/// What a rebuild that began deleting `chain` in an earlier session and
/// installed nothing leaves: its record, and the database without the clean
/// marker, which the delete removes first.
fn leave_rebuild_unfinished(dir: &TempDir, chain: &Path) {
    write_rebuild_marker(dir, chain);
    std::fs::remove_file(chain.join("clean")).unwrap();
}

/// A rebuild records itself before it deletes anything. When the app stopped
/// between the two, the chain is whole and still marked clean, so at the next
/// start the record is removed and the node starts on that chain. The same
/// holds for a record that could not be deleted after an install.
#[test]
fn a_rebuild_stopped_before_it_deleted_anything_starts_the_node_on_the_chain() {
    let dir = TempDir::new("rebuild-before-wipe");
    let chain = dir.populate_database();
    let before = tree(&chain);
    write_rebuild_marker(&dir, &chain);
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    let status = expect(&rx, "chain_status");
    assert_eq!(status["has_chain"], true, "{status}");
    assert_eq!(status["rebuild_pending"], false, "{status}");
    assert_eq!(status["install_pending"], false, "{status}");
    expect(&rx, "node_started");
    assert!(!dir.path().join("chain-rebuild.json").exists());
    expect(&rx, "wallet_ready");
    assert_eq!(tree(&chain), before);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A rebuild that did not finish in an earlier session leaves database entries
/// the node would replay from genesis. At the next start the watchdog reports
/// no chain with the rebuild pending and starts no node. The Mithril sync the
/// bootstrap decision then sends is a rebuild: it deletes what is left,
/// installs a full bootstrap, and ends the pending rebuild.
#[test]
fn restart_with_a_pending_rebuild_starts_no_node_and_rebuilds() {
    let dir = TempDir::new("rebuild-pending");
    let chain = dir.populate_database();
    leave_rebuild_unfinished(&dir, &chain);
    let args_file = dir.path().join("mithril-download-args.json");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[("MOCK_MITHRIL_ARGS_FILE", args_file.to_str().unwrap())],
    );

    let status = expect(&rx, "chain_status");
    assert_eq!(status["has_chain"], false);
    assert_eq!(status["rebuild_pending"], true);
    assert_eq!(status["install_pending"], false);
    expect_none_of(&rx, &["node_started"], 1500);
    assert!(chain.join("immutable").join("00000.chunk").exists());

    send(&mut stdin, json!({"cmd": "start_mithril"}));
    let preparing = expect_with(&rx, "mithril_status", |v| v["phase"] == "preparing");
    assert_eq!(preparing["rebuild"], true, "{preparing}");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "finalizing");
    expect(&rx, "node_started");
    let completed = expect_with(&rx, "mithril_status", |v| v["phase"] == "completed");
    assert_eq!(completed["rebuild"], true, "{completed}");

    let args: Vec<String> =
        serde_json::from_str(&std::fs::read_to_string(&args_file).unwrap()).unwrap();
    assert!(
        !args.iter().any(|a| a == "--start" || a == "--end"),
        "args: {args:?}"
    );
    assert!(!chain.join("immutable").join("00000.chunk").exists());
    assert!(!dir.path().join("chain-rebuild.json").exists());

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Errors of a sync that continues a rebuild left pending in an earlier
/// session are reported as a rebuild's, so a retry repeats the rebuild.
#[test]
fn errors_of_a_pending_rebuild_are_marked_as_a_rebuild() {
    let dir = TempDir::new("rebuild-pending-error");
    let chain = dir.populate_database();
    leave_rebuild_unfinished(&dir, &chain);
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) =
        spawn_watchdog_with_env(&cfg, &[("MOCK_MITHRIL_PROBE_FAIL", "1")]);

    assert_eq!(expect(&rx, "chain_status")["rebuild_pending"], true);
    send(&mut stdin, json!({"cmd": "start_mithril"}));
    let err = expect(&rx, "mithril_error");
    assert_eq!(err["code"], "PROBE_FAILED");
    assert_eq!(err["rebuild"], true, "{err}");
    let status = expect(&rx, "chain_status");
    assert_eq!(status["has_chain"], false);
    assert_eq!(status["rebuild_pending"], true);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A pending rebuild of another chain directory does not stop the node from
/// starting on this one.
#[test]
fn a_pending_rebuild_of_another_directory_is_ignored() {
    let dir = TempDir::new("rebuild-elsewhere");
    dir.populate_database();
    write_rebuild_marker(&dir, &dir.path().join("elsewhere").join("chain"));
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    let status = expect(&rx, "chain_status");
    assert_eq!(status["has_chain"], true);
    assert_eq!(status["rebuild_pending"], false);
    expect(&rx, "node_started");
    assert!(dir.path().join("chain-rebuild.json").exists());

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Staging left in the chain directory by an interrupted sync, and in the
/// state directory by an earlier version, is deleted at startup. It is not a
/// chain: a chain directory holding nothing else reports no chain.
#[test]
fn stale_staging_is_deleted_at_startup_and_is_not_a_chain() {
    let dir = TempDir::new("stale-staging");
    let staging = dir.path().join("chain").join("mithril-staging");
    let staged_db = staging.join("download").join("db");
    std::fs::create_dir_all(staged_db.join("immutable")).unwrap();
    std::fs::write(staged_db.join("immutable").join("00000.chunk"), b"half").unwrap();
    std::fs::write(staged_db.join("protocolMagicId"), b"1").unwrap();
    let legacy = dir.path().join("mithril-partial-sync").join("download");
    std::fs::create_dir_all(&legacy).unwrap();

    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    assert!(!staging.exists(), "staging in the chain directory was kept");
    assert!(
        !dir.path().join("mithril-partial-sync").exists(),
        "staging in the state directory was kept"
    );
    assert!(dir.path().join("chain").is_dir());

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A rebuild whose download fails after the old chain was deleted does not
/// start the node on the empty directory, which would sync from genesis. It
/// reports no chain and waits for the user's choice, as on first run, and
/// starts the node only when the user chooses to sync from genesis.
#[test]
fn wipe_chain_download_failure_starts_no_node() {
    let dir = TempDir::new("wipe-dl-fail");
    let chain = dir.populate_database();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .mithril_bin(MOCK_MITHRIL_FAIL)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "wallet_ready");
    send(
        &mut stdin,
        json!({"cmd": "start_mithril", "wipe_chain": true}),
    );

    expect(&rx, "node_shutdown_ms");
    let err = expect(&rx, "mithril_error");
    assert_eq!(err["code"], "PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED");
    assert_eq!(err["rebuild"], true, "{err}");
    let status = expect(&rx, "chain_status");
    assert_eq!(status["has_chain"], false);
    assert_eq!(status["rebuild_pending"], true);
    expect_none_of(&rx, &["node_started"], 1500);
    assert!(!chain.join("immutable").exists());
    assert!(!chain.join("protocolMagicId").exists());
    assert!(dir.path().join("chain-rebuild.json").exists());

    // A plain Mithril sync chosen in the wait is still a rebuild, and its
    // failure says so.
    send(&mut stdin, json!({"cmd": "start_mithril"}));
    let err = expect(&rx, "mithril_error");
    assert_eq!(err["rebuild"], true, "{err}");
    assert_eq!(expect(&rx, "chain_status")["rebuild_pending"], true);

    // Choosing to sync from genesis ends the rebuild.
    send(&mut stdin, json!({"cmd": "start_node"}));
    expect(&rx, "node_started");
    expect(&rx, "wallet_ready");
    assert!(!dir.path().join("chain-rebuild.json").exists());

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Cancelling a rebuild's download after the old chain was deleted does the
/// same: no chain is reported and no node is started.
#[test]
fn wipe_chain_cancel_starts_no_node() {
    let dir = TempDir::new("wipe-cancel");
    let chain = dir.populate_database();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) =
        spawn_watchdog_with_env(&cfg, &[("MOCK_MITHRIL_DOWNLOAD_HANG", "1")]);

    expect(&rx, "wallet_ready");
    send(
        &mut stdin,
        json!({"cmd": "start_mithril", "wipe_chain": true}),
    );
    expect_with(&rx, "mithril_status", |v| v["phase"] == "downloading");
    send(&mut stdin, json!({"cmd": "cancel_mithril"}));

    expect_with(&rx, "mithril_status", |v| v["phase"] == "cancelled");
    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    expect_none_of(&rx, &["node_started"], 1500);
    assert!(!chain.join("immutable").exists());

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A partial sync that fails deletes nothing, so the node is started again on
/// the chain it had, as before.
#[test]
fn partial_sync_download_failure_restarts_the_node() {
    let dir = TempDir::new("partial-dl-fail");
    let chain = dir.populate_database();
    let before = tree(&chain);
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .mithril_bin(MOCK_MITHRIL_FAIL)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "wallet_ready");
    send(&mut stdin, json!({"cmd": "start_mithril", "force": true}));

    expect(&rx, "node_shutdown_ms");
    let err = expect(&rx, "mithril_error");
    assert_eq!(err["code"], "PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED");
    assert_eq!(err["rebuild"], false, "{err}");
    expect(&rx, "node_started");
    expect(&rx, "wallet_ready");
    assert_eq!(tree(&chain), before);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// When a rebuild cannot delete all of the old chain, it reports WIPE_FAILED
/// and starts no node on what is left. The Mithril sync chosen next, as the
/// bootstrap decision sends it, is a rebuild as well: it deletes the rest and
/// installs a full bootstrap instead of a partial sync of the remains.
#[test]
fn wipe_chain_delete_failure_starts_no_node_and_rebuilds_next() {
    use std::os::unix::fs::PermissionsExt;
    let dir = TempDir::new("wipe-delete-fail");
    let chain = dir.populate_database();
    let immutable = chain.join("immutable");
    let set_mode = |mode: u32| {
        std::fs::set_permissions(&immutable, std::fs::Permissions::from_mode(mode)).unwrap();
    };
    // Chunks in a directory without write permission cannot be deleted.
    set_mode(0o555);
    if std::fs::File::create(immutable.join("probe")).is_ok() {
        // Permissions are not enforced for this user, so no delete can fail.
        let _ = std::fs::remove_file(immutable.join("probe"));
        set_mode(0o755);
        eprintln!("skipped: directory permissions are not enforced for this user");
        return;
    }
    let args_file = dir.path().join("mithril-download-args.json");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[("MOCK_MITHRIL_ARGS_FILE", args_file.to_str().unwrap())],
    );

    expect(&rx, "wallet_ready");
    send(
        &mut stdin,
        json!({"cmd": "start_mithril", "wipe_chain": true}),
    );

    let err = expect(&rx, "mithril_error");
    set_mode(0o755);
    assert_eq!(err["code"], "WIPE_FAILED", "{err}");
    assert_eq!(err["rebuild"], true, "{err}");
    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    expect_none_of(&rx, &["node_started"], 1500);
    assert!(!args_file.exists(), "a download was started");
    // clean is deleted first, so a node would not trust what is left.
    assert!(!chain.join("clean").exists());
    assert!(immutable.join("00000.chunk").exists());
    // The rebuild is recorded, so a restart now would not start the node.
    assert!(dir.path().join("chain-rebuild.json").exists());

    send(&mut stdin, json!({"cmd": "start_mithril"}));
    expect_with(&rx, "mithril_status", |v| v["phase"] == "finalizing");
    expect(&rx, "node_started");
    // The plain sync was a rebuild, and completed says so.
    let completed = expect_with(&rx, "mithril_status", |v| v["phase"] == "completed");
    assert_eq!(completed["rebuild"], true, "{completed}");

    let args: Vec<String> =
        serde_json::from_str(&std::fs::read_to_string(&args_file).unwrap()).unwrap();
    assert!(
        !args.iter().any(|a| a == "--start" || a == "--end"),
        "args: {args:?}"
    );
    assert!(!immutable.join("00000.chunk").exists());
    assert!(!dir.path().join("chain-rebuild.json").exists());

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Whether this user is refused writes to a directory without write
/// permission. When it is not, no install into one can fail.
fn directory_permissions_enforced(dir: &Path) -> bool {
    use std::os::unix::fs::PermissionsExt;
    let probe = dir.join("permission-probe");
    std::fs::create_dir_all(&probe).unwrap();
    std::fs::set_permissions(&probe, std::fs::Permissions::from_mode(0o555)).unwrap();
    let enforced = std::fs::File::create(probe.join("file")).is_err();
    std::fs::set_permissions(&probe, std::fs::Permissions::from_mode(0o755)).unwrap();
    std::fs::remove_dir_all(&probe).unwrap();
    enforced
}

/// Makes the chain directory writable again after the mock converter made it
/// read-only.
fn make_writable(dir: &Path) {
    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(dir, std::fs::Permissions::from_mode(0o755)).unwrap();
}

/// Environment for a sync whose install cannot move the converted database
/// into `chain`: the mock converter makes it read-only, once, as it exits.
fn install_fails_into(dir: &TempDir, chain: &Path) -> Vec<(&'static str, String)> {
    let trigger = dir.path().join("readonly-trigger");
    std::fs::write(&trigger, b"").unwrap();
    vec![
        (
            "MOCK_CONVERTER_READONLY_TRIGGER",
            trigger.to_str().unwrap().to_string(),
        ),
        (
            "MOCK_CONVERTER_READONLY_DIR",
            chain.to_str().unwrap().to_string(),
        ),
    ]
}

/// Borrows an owned environment as `spawn_watchdog_with_env` takes it.
fn env_refs<'a>(env: &'a [(&'static str, String)]) -> Vec<(&'static str, &'a str)> {
    env.iter().map(|(k, v)| (*k, v.as_str())).collect()
}

/// A rebuild whose install fails after the old chain was deleted keeps its
/// record and waits strictly: no chain_status, and start_node starts no node
/// on what the install left. A Retry that rebuilds installs the database,
/// starts the node and ends the record.
#[test]
fn a_rebuild_whose_install_fails_waits_for_a_mithril_install() {
    let dir = TempDir::new("rebuild-install-fail");
    if !directory_permissions_enforced(dir.path()) {
        eprintln!("skipped: directory permissions are not enforced for this user");
        return;
    }
    let chain = dir.populate_database();
    let env = install_fails_into(&dir, &chain);
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(&cfg, &env_refs(&env));

    expect(&rx, "wallet_ready");
    send(
        &mut stdin,
        json!({"cmd": "start_mithril", "wipe_chain": true}),
    );
    let err = expect(&rx, "mithril_error");
    make_writable(&chain);
    assert_eq!(err["code"], "INSTALL_FAILED", "{err}");
    assert_eq!(err["rebuild"], true, "{err}");
    assert!(dir.path().join("chain-rebuild.json").exists());

    send(&mut stdin, json!({"cmd": "start_node"}));
    expect_none_of(&rx, &["node_started", "chain_status"], 1500);

    send(
        &mut stdin,
        json!({"cmd": "start_mithril", "wipe_chain": true}),
    );
    expect_with(&rx, "mithril_status", |v| v["phase"] == "finalizing");
    expect(&rx, "node_started");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "completed");
    assert_eq!(
        std::fs::read(chain.join("protocolMagicId")).unwrap(),
        b"764824073"
    );
    assert!(!dir.path().join("chain-rebuild.json").exists());

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A first Mithril bootstrap whose install fails waits the same way: the
/// wait for the user's choice does not offer a sync from genesis on what the
/// install left, and only a Mithril sync that installs a database ends it.
#[test]
fn a_first_bootstrap_whose_install_fails_waits_for_a_mithril_install() {
    let dir = TempDir::new("bootstrap-install-fail");
    if !directory_permissions_enforced(dir.path()) {
        eprintln!("skipped: directory permissions are not enforced for this user");
        return;
    }
    let chain = dir.path().join("chain");
    let env = install_fails_into(&dir, &chain);
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(&cfg, &env_refs(&env));

    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    send(&mut stdin, json!({"cmd": "start_mithril"}));
    let err = expect(&rx, "mithril_error");
    make_writable(&chain);
    assert_eq!(err["code"], "INSTALL_FAILED", "{err}");
    assert_eq!(err["rebuild"], false, "{err}");
    // Nothing reuses the rest of the download, so it does not take up space.
    assert!(
        !chain.join("mithril-staging").join("download").exists(),
        "the download was kept"
    );

    send(&mut stdin, json!({"cmd": "start_node"}));
    expect_none_of(&rx, &["node_started", "chain_status"], 1500);

    send(
        &mut stdin,
        json!({"cmd": "start_mithril", "wipe_chain": true}),
    );
    expect_with(&rx, "mithril_status", |v| v["phase"] == "finalizing");
    expect(&rx, "node_started");
    assert!(chain.join("protocolMagicId").exists());

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A partial sync whose install fails part-way leaves a chain partly
/// replaced. It recorded the install and deleted the clean marker before it
/// changed anything, so the failure outlasts a restart: the next start
/// reports the install pending and starts no node, and the Mithril sync chosen
/// then rebuilds the chain directory and removes the record.
#[test]
fn a_partial_sync_whose_install_fails_is_still_pending_at_the_next_start() {
    let dir = TempDir::new("partial-install-fail");
    if !directory_permissions_enforced(dir.path()) {
        eprintln!("skipped: directory permissions are not enforced for this user");
        return;
    }
    let chain = dir.populate_database();
    let immutable = chain.join("immutable");
    // The chunks cannot be moved in, while the clean marker beside them can
    // be deleted.
    let mut env = install_fails_into(&dir, &immutable);
    env.push(("MOCK_CERTIFIED_IMMUTABLE", "30".to_string()));
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(&cfg, &env_refs(&env));

    expect(&rx, "wallet_ready");
    send(&mut stdin, json!({"cmd": "start_mithril", "force": true}));
    let err = expect(&rx, "mithril_error");
    make_writable(&immutable);
    assert_eq!(err["code"], "INSTALL_FAILED", "{err}");
    assert_eq!(err["rebuild"], false, "{err}");
    assert!(
        !chain.join("clean").exists(),
        "a changed database is marked clean"
    );
    assert!(
        !chain.join("mithril-staging").exists(),
        "the download was kept"
    );
    let record: Value = serde_json::from_str(
        &std::fs::read_to_string(dir.path().join("chain-rebuild.json")).unwrap(),
    )
    .unwrap();
    assert_eq!(record["chain_path"], chain.to_str().unwrap());
    assert_eq!(record["kind"], "install");
    send(&mut stdin, json!({"cmd": "start_node"}));
    expect_none_of(&rx, &["node_started", "chain_status"], 1500);
    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();

    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);
    let status = expect(&rx, "chain_status");
    assert_eq!(status["has_chain"], false, "{status}");
    assert_eq!(status["install_pending"], true, "{status}");
    assert_eq!(status["rebuild_pending"], false, "{status}");
    expect_none_of(&rx, &["node_started"], 1500);

    send(&mut stdin, json!({"cmd": "start_mithril"}));
    let preparing = expect_with(&rx, "mithril_status", |v| v["phase"] == "preparing");
    assert_eq!(preparing["rebuild"], true, "{preparing}");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "finalizing");
    expect(&rx, "node_started");
    assert!(chain.join("clean").exists());
    assert!(!dir.path().join("chain-rebuild.json").exists());

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A partial sync that installs removes the record it wrote before it changed
/// the database, and leaves the chain marked clean.
#[test]
fn a_partial_sync_that_installs_leaves_no_record() {
    let dir = TempDir::new("partial-install-record");
    let chain = dir.populate_database();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) =
        spawn_watchdog_with_env(&cfg, &[("MOCK_CERTIFIED_IMMUTABLE", "30")]);

    expect(&rx, "wallet_ready");
    send(&mut stdin, json!({"cmd": "start_mithril", "force": true}));
    expect_with(&rx, "mithril_status", |v| v["phase"] == "finalizing");
    expect(&rx, "node_started");
    assert!(chain.join("clean").exists());
    assert!(!dir.path().join("chain-rebuild.json").exists());

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A new chain location sent with a rebuild right behind it, as the renderer
/// does once both are confirmed, stops the running node and rebuilds in the
/// new location. The rebuild is held while the node stops and replaces the
/// restart, so no node starts on the new location before it, and the old
/// location is left as it was.
#[test]
fn a_rebuild_sent_with_a_new_location_starts_no_node_there_first() {
    let dir = TempDir::new("rebuild-new-location");
    let old_chain = dir.populate_database();
    let before = tree(&old_chain);
    let folder = dir.path().join("picked");
    std::fs::create_dir_all(&folder).unwrap();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "wallet_ready");
    send(
        &mut stdin,
        json!({"cmd": "set_chain_path", "path": folder.to_str().unwrap()}),
    );
    send(
        &mut stdin,
        json!({"cmd": "start_mithril", "wipe_chain": true}),
    );

    loop {
        let v = rx
            .recv_timeout(Duration::from_secs(15))
            .expect("timeout waiting for the rebuild to start");
        assert_ne!(v["event"], "node_started", "a node started first: {v}");
        if v["event"] == "mithril_status" && v["phase"] == "preparing" {
            assert_eq!(v["rebuild"], true, "{v}");
            break;
        }
    }
    expect_with(&rx, "mithril_status", |v| v["phase"] == "finalizing");
    expect(&rx, "node_started");
    assert_eq!(
        std::fs::read(folder.join("chain").join("protocolMagicId")).unwrap(),
        b"764824073"
    );
    assert_eq!(tree(&old_chain), before);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A first bootstrap records its install before it moves the download into
/// the chain directory. When the install stops part-way, the record makes the
/// next start report no chain with the install pending, not a rebuild, and
/// start no node, although the entries moved so far look like a database.
#[test]
fn a_first_bootstrap_cut_short_in_its_install_is_not_a_chain_at_the_next_start() {
    let dir = TempDir::new("bootstrap-cut-short");
    if !directory_permissions_enforced(dir.path()) {
        eprintln!("skipped: directory permissions are not enforced for this user");
        return;
    }
    let chain = dir.path().join("chain");
    let env = install_fails_into(&dir, &chain);
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(&cfg, &env_refs(&env));

    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    send(&mut stdin, json!({"cmd": "start_mithril"}));
    let err = expect(&rx, "mithril_error");
    make_writable(&chain);
    assert_eq!(err["code"], "INSTALL_FAILED", "{err}");
    let record: Value = serde_json::from_str(
        &std::fs::read_to_string(dir.path().join("chain-rebuild.json")).unwrap(),
    )
    .unwrap();
    assert_eq!(record["chain_path"], chain.to_str().unwrap());
    assert_eq!(record["kind"], "install");
    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();

    // What an install that stopped after moving some entries leaves: chunks
    // and the network marker, without the clean marker that is moved last.
    std::fs::create_dir_all(chain.join("immutable")).unwrap();
    std::fs::write(chain.join("immutable").join("00000.chunk"), b"half").unwrap();
    std::fs::write(chain.join("protocolMagicId"), b"764824073").unwrap();

    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);
    let status = expect(&rx, "chain_status");
    assert_eq!(status["has_chain"], false, "{status}");
    assert_eq!(status["rebuild_pending"], false, "{status}");
    assert_eq!(status["install_pending"], true, "{status}");
    expect_none_of(&rx, &["node_started"], 1500);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// The Mithril sync chosen after an unfinished install rebuilds the chain
/// directory, deleting what the install left. When its download then fails,
/// the record still describes an install that did not finish, not a rebuild
/// that removed old data, and chain_status says so.
#[test]
fn a_rebuild_after_an_unfinished_install_keeps_it_an_install() {
    let dir = TempDir::new("install-pending-rebuild");
    let chain = dir.path().join("chain");
    std::fs::create_dir_all(chain.join("immutable")).unwrap();
    std::fs::write(chain.join("immutable").join("00000.chunk"), b"half").unwrap();
    std::fs::write(chain.join("protocolMagicId"), b"764824073").unwrap();
    std::fs::write(
        dir.path().join("chain-rebuild.json"),
        json!({ "chain_path": chain.to_str().unwrap(), "kind": "install" }).to_string(),
    )
    .unwrap();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .mithril_bin(MOCK_MITHRIL_FAIL)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    let status = expect(&rx, "chain_status");
    assert_eq!(status["install_pending"], true, "{status}");
    assert_eq!(status["rebuild_pending"], false, "{status}");
    send(&mut stdin, json!({"cmd": "start_mithril"}));
    let err = expect(&rx, "mithril_error");
    assert_eq!(err["code"], "PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED", "{err}");
    assert_eq!(err["rebuild"], true, "{err}");
    let status = expect(&rx, "chain_status");
    assert_eq!(status["has_chain"], false, "{status}");
    assert_eq!(status["install_pending"], true, "{status}");
    assert_eq!(status["rebuild_pending"], false, "{status}");
    expect_none_of(&rx, &["node_started"], 1500);
    // The rebuild deleted what the install left.
    assert!(!chain.join("immutable").exists());
    let record: Value = serde_json::from_str(
        &std::fs::read_to_string(dir.path().join("chain-rebuild.json")).unwrap(),
    )
    .unwrap();
    assert_eq!(record["kind"], "install");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// When the rebuild sent with a new location is refused before it changes
/// anything, no node starts in the new location, which holds no chain: a node
/// there would sync from genesis. The watchdog reports no chain and waits, and
/// starts the node only when the user chooses to sync from genesis.
#[test]
fn a_refused_rebuild_in_a_new_location_starts_no_node_there() {
    let dir = TempDir::new("refused-new-location");
    dir.populate_database();
    let folder = dir.path().join("picked");
    std::fs::create_dir_all(&folder).unwrap();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) =
        spawn_watchdog_with_env(&cfg, &[("MOCK_MITHRIL_PROBE_FAIL", "1")]);

    expect(&rx, "wallet_ready");
    send(
        &mut stdin,
        json!({"cmd": "set_chain_path", "path": folder.to_str().unwrap()}),
    );
    send(
        &mut stdin,
        json!({"cmd": "start_mithril", "wipe_chain": true}),
    );

    let err = expect(&rx, "mithril_error");
    assert_eq!(err["code"], "PROBE_FAILED", "{err}");
    let status = expect(&rx, "chain_status");
    assert_eq!(status["has_chain"], false, "{status}");
    assert_eq!(status["rebuild_pending"], false, "{status}");
    expect_none_of(&rx, &["node_started"], 1500);
    assert!(!folder.join("chain").join("protocolMagicId").exists());

    send(&mut stdin, json!({"cmd": "start_node"}));
    expect(&rx, "node_started");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A Mithril sync requested while the node stops for a rebuild does not turn
/// the rebuild into a partial sync over the old chain: the requests are
/// merged, and the rebuild deletes the old chain and downloads everything.
#[test]
fn held_plain_sync_does_not_cancel_a_rebuild() {
    let dir = TempDir::new("held-rebuild");
    let chain = dir.populate_database();
    let args_file = dir.path().join("mithril-download-args.json");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[
            ("MOCK_NODE_EXIT_DELAY_MS", "1500"),
            ("MOCK_CERTIFIED_IMMUTABLE", "30"),
            ("MOCK_MITHRIL_ARGS_FILE", args_file.to_str().unwrap()),
        ],
    );

    expect(&rx, "wallet_ready");
    send(
        &mut stdin,
        json!({"cmd": "start_mithril", "wipe_chain": true}),
    );
    expect_with(&rx, "backend_stop_progress", |v| {
        v["stage"] == "stopping_node"
    });
    send(&mut stdin, json!({"cmd": "start_mithril"}));

    expect_with(&rx, "mithril_status", |v| v["phase"] == "finalizing");
    expect(&rx, "node_started");
    let args: Vec<String> =
        serde_json::from_str(&std::fs::read_to_string(&args_file).unwrap()).unwrap();
    assert!(
        !args.iter().any(|a| a == "--start" || a == "--end"),
        "args: {args:?}"
    );
    assert!(!chain.join("immutable").join("00000.chunk").exists());
    assert!(!chain.join("ledger").join("999").exists());

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── T7: node crash after wallet ready ────────────────────────────────────────

/// Node is SIGKILL'd after wallet is ready → watchdog restarts node+wallet → recovers.
/// This is the core bug fix for Bug 5: previously the watchdog exited instead of restarting.
#[test]
fn node_crash_after_wallet_ready() {
    let dir = TempDir::new("node-crash-post-ready");
    dir.populate_chain();
    // Use mock-node-crash-once: first invocation crashes, second succeeds.
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE_CRASH_ONCE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect(&rx, "chain_status");

    // First node spawn crashes immediately (before socket ready).
    expect(&rx, "node_started");
    let exited = expect(&rx, "node_exited");
    assert_eq!(exited["code"], 1);

    // Watchdog restarts: second spawn succeeds.
    expect(&rx, "node_started");
    expect(&rx, "node_socket_ready");
    expect(&rx, "wallet_started");
    expect(&rx, "wallet_ready");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Mithril configured without an ancillary verification key → the pipeline
/// refuses to start (before any chain wipe) instead of downloading unverified
/// ancillary data or failing mid-download.
#[test]
fn mithril_without_ancillary_key_fails_fast() {
    let dir = TempDir::new("no-ancillary-key");
    dir.populate_chain();
    let (mut cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).mithril().build();
    cfg["mithril"]
        .as_object_mut()
        .unwrap()
        .remove("ancillary_vkey");
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "wallet_ready");
    send(
        &mut stdin,
        json!({"cmd": "start_mithril", "force": true, "wipe_chain": true}),
    );

    let err = expect(&rx, "mithril_error");
    assert_eq!(err["code"], "ANCILLARY_VKEY_MISSING");
    assert!(
        dir.path().join("chain").exists(),
        "guard must fire before the chain wipe"
    );

    // Pipeline returned Cancelled → node/wallet restart normally.
    expect(&rx, "wallet_ready");
    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// The restart-attempt counter resets once the wallet reaches ready, so
/// non-consecutive crashes never accumulate into wallet_unrecoverable.
#[test]
fn wallet_restart_counter_resets_on_ready() {
    let dir = TempDir::new("restart-counter-reset");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .max_restarts(2)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    let kill_wallet = |rx: &mpsc::Receiver<Value>| {
        let wallet_ev = expect(rx, "wallet_started");
        let wallet_pid = wallet_ev["pid"].as_u64().unwrap() as i32;
        expect(rx, "wallet_ready");
        nix::sys::signal::kill(
            nix::unistd::Pid::from_raw(wallet_pid),
            nix::sys::signal::Signal::SIGKILL,
        )
        .unwrap();
    };

    // Three ready→crash cycles with max_restart_attempts = 2: if the counter
    // accumulated across recoveries, the third crash would be unrecoverable.
    for _ in 0..3 {
        kill_wallet(&rx);
        let restarting = expect(&rx, "wallet_restarting");
        assert_eq!(
            restarting["attempt"], 1,
            "counter must reset after each successful wallet_ready"
        );
    }

    expect(&rx, "wallet_ready");
    stop(&mut stdin);
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

/// Converter writes 1 MiB to stdout and stderr per step → both steps finish and
/// the install completes; only the tail of that output reaches the log.
#[test]
fn mithril_converter_large_output() {
    let dir = TempDir::new("mithril-conv-chatty");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .converter_bin(MOCK_CONVERTER_CHATTY)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect_with(&rx, "chain_status", |v| v["has_chain"] == false);

    send(&mut stdin, json!({"cmd": "start_mithril"}));

    expect_with(&rx, "mithril_status", |v| v["phase"] == "preparing");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "downloading");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "converting");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "installing");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "finalizing");
    expect(&rx, "node_started");
    expect_with(&rx, "mithril_status", |v| v["phase"] == "completed");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();

    let log =
        std::fs::read_to_string(dir.path().join("logs").join("watchdog.log")).unwrap_or_default();
    assert!(
        log.contains("snapshot-converter convert stderr")
            && log.contains("mock-snapshot-converter-chatty: done"),
        "converter stderr tail must be logged"
    );
    assert!(
        log.len() < 256 * 1024,
        "converter output must be cut to its tail, log is {} bytes",
        log.len()
    );
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

    let mut stdin = stdin;
    stop(&mut stdin);
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
        .join("chain")
        .join("mithril-staging")
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

// ── Bug-fix regression tests ──────────────────────────────────────────────────

/// Bug 5 regression: node SIGKILL'd before socket ready → watchdog restarts, not exits.
///
/// Before the fix the watchdog emitted `stopped` immediately on unexpected node exit.
/// After the fix it emits `node_exited`, delays, and retries up to max_crash_attempts.
#[test]
fn node_sigkill_before_socket_triggers_restart() {
    let dir = TempDir::new("bug5-sigkill-pre-socket");
    dir.populate_chain();
    // crash-once: first spawn exits 1, second spawn succeeds
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE_CRASH_ONCE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect(&rx, "chain_status");

    // Attempt 1: node crashes before socket is ready.
    expect(&rx, "node_started");
    let exited = expect(&rx, "node_exited");
    assert_eq!(exited["code"], 1, "expected exit code 1 from mock crash");

    // Watchdog must NOT emit stopped here — it restarts instead.
    // Attempt 2: node succeeds.
    expect(&rx, "node_started");
    expect(&rx, "node_socket_ready");
    expect(&rx, "wallet_started");
    expect(&rx, "wallet_ready");

    // Clean stop.
    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Bug 5 regression: node SIGKILL'd while wallet running → watchdog restarts both, not exits.
#[test]
fn node_sigkill_post_wallet_ready_triggers_restart() {
    let dir = TempDir::new("bug5-sigkill-post-ready");
    dir.populate_chain();
    // crash-once: first spawn exits 1, second spawn succeeds.
    // The crash will happen pre-socket on the first spawn.
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE_CRASH_ONCE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect(&rx, "chain_status");

    // First spawn crashes.
    expect(&rx, "node_started");
    let exited = expect(&rx, "node_exited");
    assert_eq!(exited["code"], 1);

    // Restart: second spawn runs, wallet comes up.
    expect(&rx, "node_started");
    expect(&rx, "node_socket_ready");
    expect(&rx, "wallet_started");
    expect(&rx, "wallet_ready");

    // Clean stop confirms full recovery.
    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Bug 5 regression: intentional stop (Cmd::Stop) must still emit stopped, not restart.
/// This guards against a regression where the fix causes restart on clean shutdown.
#[test]
fn intentional_stop_does_not_restart() {
    let dir = TempDir::new("bug5-intentional-stop");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect(&rx, "chain_status");
    expect(&rx, "node_started");
    expect(&rx, "node_socket_ready");
    expect(&rx, "wallet_started");
    expect(&rx, "wallet_ready");

    stop(&mut stdin);

    // Must emit stopped exactly once and then the process exits.
    expect(&rx, "stopped");

    // No further node_started should arrive — verify by waiting briefly.
    let extra = rx.recv_timeout(Duration::from_millis(200));
    assert!(
        extra.is_err(),
        "expected no more events after stopped, got: {extra:?}"
    );

    drop(stdin);
    let _ = child.wait();
}

/// Bug 5 regression: node crash limit — after max_crash_attempts the watchdog
/// emits an error and node_unrecoverable, then waits; restart_node retries.
#[test]
fn node_crash_exceeds_limit_emits_error_and_waits_for_retry() {
    let dir = TempDir::new("bug5-crash-limit");
    dir.populate_chain();
    // MOCK_NODE_CRASH always exits 1; set limit to 3 to keep test fast.
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE_CRASH, MOCK_WALLET)
        .node_max_crash_attempts(3)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect(&rx, "chain_status");

    for _ in 0..3 {
        expect(&rx, "node_started");
        let exited = expect(&rx, "node_exited");
        assert_eq!(exited["code"], 1);
    }

    let err = expect(&rx, "error");
    assert!(
        err["message"]
            .as_str()
            .unwrap_or("")
            .contains("unrecoverable"),
        "error message should mention unrecoverable, got: {err}"
    );
    expect(&rx, "node_unrecoverable");
    expect_none_of(&rx, &["node_started", "stopped"], 500);

    // Retry starts the node again, with the crash count cleared.
    send(&mut stdin, json!({"cmd": "restart_node"}));
    for _ in 0..3 {
        expect(&rx, "node_started");
        expect(&rx, "node_exited");
    }
    expect(&rx, "node_unrecoverable");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A node that crashes each time after reaching chainDbReady restarts until
/// the crash window is exhausted, then the watchdog waits for a retry.
#[test]
fn node_crashing_after_ready_exhausts_the_crash_window() {
    let dir = TempDir::new("node-crash-window");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .node_crash_window(3, 600)
        .build();
    let (mut child, mut stdin, rx) =
        spawn_watchdog_with_env(&cfg, &[("MOCK_NODE_CRASH_AFTER_READY_MS", "300")]);

    for _ in 0..3 {
        expect(&rx, "node_socket_ready");
        expect(&rx, "node_exited");
    }
    let unrec = expect_within(&rx, "node_unrecoverable", Duration::from_secs(5));
    assert_eq!(unrec["crashes"], 3);
    expect_none_of(&rx, &["node_started", "stopped"], 700);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── Tests: bounded stop ───────────────────────────────────────────────────────

/// A node that ignores the stop request is killed once the stop bound passes,
/// and the watchdog still reports the backend stopped.
#[test]
fn node_ignoring_stop_is_killed_at_the_bound() {
    let dir = TempDir::new("stop-bound");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .node_stop_timeout_secs(1)
        .build();
    let (mut child, mut stdin, rx) =
        spawn_watchdog_with_env(&cfg, &[("MOCK_NODE_IGNORE_SHUTDOWN", "1")]);

    expect(&rx, "wallet_ready");
    let started = std::time::Instant::now();
    stop(&mut stdin);

    let shutdown = expect(&rx, "node_shutdown_ms");
    assert_eq!(shutdown["force_killed"], true, "{shutdown}");
    assert!(shutdown["ms"].as_u64().unwrap() >= 1000, "{shutdown}");
    expect(&rx, "stopped");
    assert!(
        started.elapsed() < Duration::from_secs(10),
        "stop took {:?}",
        started.elapsed()
    );
    drop(stdin);
    let _ = child.wait();
}

/// The same bound applies to a node restart: the old node is killed and a new
/// one starts.
#[test]
fn restart_kills_a_node_ignoring_stop_at_the_bound() {
    let dir = TempDir::new("restart-bound");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .node_stop_timeout_secs(1)
        .build();
    let (mut child, mut stdin, rx) =
        spawn_watchdog_with_env(&cfg, &[("MOCK_NODE_IGNORE_SHUTDOWN", "1")]);

    let first = expect(&rx, "node_started");
    expect(&rx, "wallet_ready");
    send(&mut stdin, json!({"cmd": "restart_node"}));

    let shutdown = expect(&rx, "node_shutdown_ms");
    assert_eq!(shutdown["force_killed"], true, "{shutdown}");
    let second = expect(&rx, "node_started");
    assert_ne!(first["pid"], second["pid"]);
    expect(&rx, "wallet_ready");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A stop reports each stage with its bound, wallet first, then node, so
/// Electron can show progress and size its own safety timer.
#[test]
fn stop_reports_progress_for_each_stage() {
    let dir = TempDir::new("stop-progress");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .node_stop_timeout_secs(7)
        .wallet_stop_timeout_secs(3)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "wallet_ready");
    stop(&mut stdin);

    let wallet = expect(&rx, "backend_stop_progress");
    assert_eq!(wallet["stage"], "stopping_wallet", "{wallet}");
    assert_eq!(wallet["timeout_ms"], 3000, "{wallet}");
    let node = expect_with(&rx, "backend_stop_progress", |v| {
        v["stage"] == "stopping_node"
    });
    assert_eq!(node["timeout_ms"], 7000, "{node}");
    let shutdown = expect(&rx, "node_shutdown_ms");
    assert_eq!(shutdown["force_killed"], false, "{shutdown}");
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// The wallet is stopped by closing its stdin (`--shutdown-handler`), and it
/// exits well inside its stop bound instead of being killed at the bound.
#[test]
fn wallet_stops_cleanly_when_its_stdin_closes() {
    let dir = TempDir::new("wallet-clean-stop");
    dir.populate_chain();
    let exit_file = dir.path().join("wallet-exit");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .wallet_stop_timeout_secs(30)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[("MOCK_WALLET_EXIT_FILE", exit_file.to_str().unwrap())],
    );

    expect(&rx, "wallet_ready");
    let started = std::time::Instant::now();
    stop(&mut stdin);
    expect_with(&rx, "backend_stop_progress", |v| {
        v["stage"] == "stopping_node"
    });
    assert!(
        started.elapsed() < Duration::from_secs(5),
        "wallet stop took {:?}",
        started.elapsed()
    );
    expect(&rx, "stopped");
    assert_eq!(std::fs::read_to_string(&exit_file).unwrap(), "stdin-eof");
    drop(stdin);
    let _ = child.wait();
}

/// A user-initiated wallet restart stops the running wallet the same way.
#[test]
fn wallet_restart_stops_the_wallet_cleanly() {
    let dir = TempDir::new("wallet-clean-restart");
    dir.populate_chain();
    let exit_file = dir.path().join("wallet-exit");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .wallet_stop_timeout_secs(30)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[("MOCK_WALLET_EXIT_FILE", exit_file.to_str().unwrap())],
    );

    let first = expect(&rx, "wallet_started");
    expect(&rx, "wallet_ready");
    send(&mut stdin, json!({"cmd": "restart_wallet"}));
    let restarting = expect(&rx, "wallet_restarting");
    assert_eq!(restarting["attempt"], 0, "a user restart is not a failure");
    let second = expect(&rx, "wallet_started");
    assert_ne!(first["pid"], second["pid"]);
    assert_eq!(std::fs::read_to_string(&exit_file).unwrap(), "stdin-eof");
    expect(&rx, "wallet_ready");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── Tests: wallet health check ───────────────────────────────────────────────

// The mock wallet binds at once, but the watchdog notices only at its next
// 500 ms poll of the port. A fault scheduled for 1,200 ms therefore always
// follows `wallet_ready`.
const FAULT_AFTER_MS: &str = "1200";

fn watchdog_log(dir: &TempDir) -> String {
    std::fs::read_to_string(dir.path().join("logs").join("watchdog.log")).unwrap_or_default()
}

/// A wallet whose server goes away while its process stays alive is stopped
/// cleanly and replaced; the replacement answers.
#[test]
fn wallet_with_a_closed_listener_is_restarted() {
    let dir = TempDir::new("health-closed");
    dir.populate_chain();
    let sentinel = dir.path().join("healthy");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .wallet_health(json!({}))
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[
            ("MOCK_WALLET_HTTP", "1"),
            ("MOCK_WALLET_CLOSE_AFTER_MS", FAULT_AFTER_MS),
            ("MOCK_WALLET_HEALTHY_SENTINEL", sentinel.to_str().unwrap()),
        ],
    );

    let first = expect(&rx, "wallet_started");
    expect(&rx, "wallet_ready");
    let verdict = expect_within(&rx, "wallet_unresponsive", Duration::from_secs(10));
    assert_eq!(verdict["answered_once"], true, "{verdict}");
    assert_eq!(verdict["will_restart"], true, "{verdict}");
    assert_eq!(verdict["restart_attempt"], 1, "{verdict}");
    assert_eq!(verdict["failed_probes"], 3, "{verdict}");
    assert!(
        verdict["last_error"]
            .as_str()
            .unwrap()
            .contains("connection refused"),
        "{verdict}"
    );
    let second = expect(&rx, "wallet_started");
    assert_ne!(first["pid"], second["pid"]);
    expect(&rx, "wallet_ready");
    expect_none_of(&rx, &["wallet_unresponsive", "wallet_exited"], 1_500);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();

    let log = watchdog_log(&dir);
    assert!(
        log.contains("stopping wallet (no response)"),
        "stop not logged:\n{log}"
    );
    assert!(
        log.contains("force_killed=false, code=Some(0)"),
        "the wallet's exit code not logged:\n{log}"
    );
}

/// A wallet that holds its listener but answers nothing, and ignores the
/// stdin close, is killed at the health stop bound and replaced.
#[test]
fn wallet_with_a_hung_server_is_killed_and_restarted() {
    let dir = TempDir::new("health-hung");
    dir.populate_chain();
    let sentinel = dir.path().join("healthy");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .wallet_health(json!({"stop_timeout_secs": 1}))
        .wallet_stop_timeout_secs(1)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[
            ("MOCK_WALLET_HTTP", "1"),
            ("MOCK_WALLET_MUTE_AFTER_MS", FAULT_AFTER_MS),
            ("MOCK_WALLET_IGNORE_STDIN", "1"),
            ("MOCK_WALLET_HEALTHY_SENTINEL", sentinel.to_str().unwrap()),
        ],
    );

    expect(&rx, "wallet_ready");
    let verdict = expect_within(&rx, "wallet_unresponsive", Duration::from_secs(10));
    assert_eq!(verdict["answered_once"], true, "{verdict}");
    assert!(
        verdict["last_error"].as_str().unwrap().contains("timeout"),
        "{verdict}"
    );
    expect(&rx, "wallet_started");
    expect(&rx, "wallet_ready");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();

    let log = watchdog_log(&dir);
    assert!(
        stop_line_after(&log, "stopping wallet (no response)").contains("force_killed=true"),
        "the hung wallet was not killed at the stop that follows the verdict:\n{log}"
    );
}

/// The first `wallet stopped in` line after the first line containing `marker`.
fn stop_line_after(log: &str, marker: &str) -> String {
    log.lines()
        .skip_while(|l| !l.contains(marker))
        .find(|l| l.contains("wallet stopped in"))
        .unwrap_or_else(|| panic!("no stop line after '{marker}':\n{log}"))
        .to_string()
}

/// The stop that follows a node exit precedes an automatic restart, so it uses
/// the short bound and not the 60 s bound of a requested stop.
#[test]
fn wallet_stop_after_a_node_exit_uses_the_restart_bound() {
    let dir = TempDir::new("health-node-exit");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .wallet_health(json!({"stop_timeout_secs": 1}))
        .wallet_stop_timeout_secs(6)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[
            ("MOCK_WALLET_HTTP", "1"),
            ("MOCK_WALLET_IGNORE_STDIN", "1"),
            ("MOCK_NODE_CRASH_AFTER_READY_MS", "1500"),
        ],
    );

    expect(&rx, "wallet_ready");
    expect(&rx, "node_exited");
    expect(&rx, "node_started");

    stop(&mut stdin);
    drop(stdin);
    let _ = child.wait();

    let log = watchdog_log(&dir);
    let line = stop_line_after(&log, "stopping wallet (node exited)");
    assert!(line.contains("force_killed=true"), "{line}");
    // The restart bound is 1 s; the requested-stop bound here is 6 s.
    let ms: u64 = line
        .split("stopped in ")
        .nth(1)
        .and_then(|r| r.split("ms").next())
        .and_then(|n| n.parse().ok())
        .unwrap_or_else(|| panic!("no duration in {line}"));
    assert!(ms < 4_000, "the stop took {ms} ms: {line}");
}

/// After `max_restarts` verdicts the wallet is left stopped with the existing
/// unrecoverable state, and a user restart starts the count again.
#[test]
fn wallet_health_restarts_are_capped_and_a_retry_clears_the_cap() {
    let dir = TempDir::new("health-cap");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .wallet_health(json!({"max_restarts": 2, "failure_threshold": 2}))
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[
            ("MOCK_WALLET_HTTP", "1"),
            ("MOCK_WALLET_CLOSE_AFTER_MS", FAULT_AFTER_MS),
        ],
    );

    for (attempt, will_restart) in [(1, true), (2, true), (3, false)] {
        expect(&rx, "wallet_ready");
        let verdict = expect_within(&rx, "wallet_unresponsive", Duration::from_secs(10));
        assert_eq!(verdict["restart_attempt"], attempt, "{verdict}");
        assert_eq!(verdict["will_restart"], will_restart, "{verdict}");
    }
    let unrec = expect_within(&rx, "wallet_unrecoverable", Duration::from_secs(10));
    assert_eq!(unrec["attempt"], 3, "{unrec}");
    expect_none_of(&rx, &["wallet_started", "stopped"], 700);

    // The retry clears the budget: the next verdict is attempt 1 again.
    send(&mut stdin, json!({"cmd": "restart_wallet"}));
    expect(&rx, "wallet_ready");
    let verdict = expect_within(&rx, "wallet_unresponsive", Duration::from_secs(10));
    assert_eq!(verdict["restart_attempt"], 1, "{verdict}");
    assert_eq!(verdict["will_restart"], true, "{verdict}");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A wallet that answers is left alone.
#[test]
fn healthy_wallet_is_not_restarted() {
    let dir = TempDir::new("health-ok");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .wallet_health(json!({}))
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(&cfg, &[("MOCK_WALLET_HTTP", "1")]);

    expect(&rx, "wallet_ready");
    // Twenty probe intervals.
    expect_none_of(&rx, &["wallet_unresponsive", "wallet_exited"], 1_000);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Timeouts before the first answer are not failures; the startup grace ends
/// the wait, with a verdict that says the wallet never answered.
#[test]
fn wallet_that_never_answers_gets_a_verdict_when_the_grace_ends() {
    let dir = TempDir::new("health-grace");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .wallet_health(json!({"startup_grace_ms": 1_000}))
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[
            ("MOCK_WALLET_HTTP", "1"),
            ("MOCK_WALLET_MUTE_AFTER_MS", "0"),
        ],
    );

    expect(&rx, "wallet_ready");
    let verdict = expect_within(&rx, "wallet_unresponsive", Duration::from_secs(10));
    assert_eq!(verdict["answered_once"], false, "{verdict}");
    assert_eq!(verdict["failed_probes"], 0, "{verdict}");
    assert!(
        verdict["unresponsive_ms"].as_u64().unwrap() >= 1_000,
        "{verdict}"
    );

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Refusals count from the first probe: a listener that closes before any
/// answer gives a verdict after the threshold, long before the grace.
#[test]
fn refusals_before_the_first_answer_give_a_verdict_before_the_grace() {
    let dir = TempDir::new("health-refused");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .wallet_health(json!({}))
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[
            ("MOCK_WALLET_HTTP", "1"),
            ("MOCK_WALLET_MUTE_AFTER_MS", "0"),
            ("MOCK_WALLET_CLOSE_AFTER_MS", FAULT_AFTER_MS),
        ],
    );

    expect(&rx, "wallet_ready");
    let verdict = expect_within(&rx, "wallet_unresponsive", Duration::from_secs(10));
    assert_eq!(verdict["answered_once"], false, "{verdict}");
    assert_eq!(verdict["failed_probes"], 3, "{verdict}");
    assert!(
        verdict["last_error"]
            .as_str()
            .unwrap()
            .contains("connection refused"),
        "{verdict}"
    );
    assert!(
        verdict["unresponsive_ms"].as_u64().unwrap() < 30_000,
        "{verdict}"
    );

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A restart the user asks for between two verdicts starts the count again.
#[test]
fn user_wallet_restart_clears_the_health_budget() {
    let dir = TempDir::new("health-user-restart");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET)
        .wallet_health(json!({"max_restarts": 1}))
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog_with_env(
        &cfg,
        &[
            ("MOCK_WALLET_HTTP", "1"),
            ("MOCK_WALLET_CLOSE_AFTER_MS", FAULT_AFTER_MS),
        ],
    );

    expect(&rx, "wallet_ready");
    let verdict = expect_within(&rx, "wallet_unresponsive", Duration::from_secs(10));
    assert_eq!(verdict["restart_attempt"], 1, "{verdict}");
    assert_eq!(verdict["will_restart"], true, "{verdict}");

    // The replacement closes its listener 1,200 ms after it binds, and
    // becomes ready after about 500 ms, so the command lands before that.
    expect(&rx, "wallet_ready");
    send(&mut stdin, json!({"cmd": "restart_wallet"}));
    expect(&rx, "wallet_restarting");
    expect(&rx, "wallet_ready");
    let verdict = expect_within(&rx, "wallet_unresponsive", Duration::from_secs(10));
    assert_eq!(
        verdict["restart_attempt"], 1,
        "the budget was not cleared: {verdict}"
    );
    assert_eq!(verdict["will_restart"], true, "{verdict}");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// Without a `wallet.health` section the check is off: a wallet that drops
/// every connection is left alone.
#[test]
fn wallet_health_is_off_without_its_section() {
    let dir = TempDir::new("health-off");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "wallet_ready");
    expect_none_of(&rx, &["wallet_unresponsive", "wallet_exited"], 1_000);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

// ── Tests: restart and stop ordering ─────────────────────────────────────────

/// Collect events up to and including `stopped`; panic after 15 s of silence.
fn events_until_stopped(rx: &mpsc::Receiver<Value>) -> Vec<String> {
    let mut seen = Vec::new();
    loop {
        let v = rx
            .recv_timeout(Duration::from_secs(15))
            .unwrap_or_else(|_| panic!("timeout waiting for 'stopped'; saw {seen:?}"));
        let name = v["event"].as_str().unwrap_or_default().to_string();
        let done = name == "stopped";
        seen.push(name);
        if done {
            return seen;
        }
    }
}

/// A stop requested while a restart waits for the old node to exit ends the
/// restart there: no new node is started only to be stopped.
#[test]
fn stop_during_a_node_restart_does_not_start_a_new_node() {
    let dir = TempDir::new("stop-during-restart");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) =
        spawn_watchdog_with_env(&cfg, &[("MOCK_NODE_EXIT_DELAY_MS", "1500")]);

    expect(&rx, "wallet_ready");
    send(
        &mut stdin,
        json!({"cmd": "set_node_extra_args", "args": ["+RTS", "-c", "-RTS"]}),
    );
    expect_with(&rx, "backend_stop_progress", |v| {
        v["stage"] == "stopping_node"
    });
    stop(&mut stdin);

    let events = events_until_stopped(&rx);
    assert!(
        !events.iter().any(|e| e == "node_started"),
        "a node was started after the stop request: {events:?}"
    );
    assert_eq!(
        events.iter().filter(|e| *e == "node_shutdown_ms").count(),
        1,
        "{events:?}"
    );
    drop(stdin);
    let _ = child.wait();
}

/// A restart starts the new node only once the old one has exited.
#[test]
fn restart_starts_the_new_node_after_the_old_one_has_exited() {
    let dir = TempDir::new("restart-order");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) =
        spawn_watchdog_with_env(&cfg, &[("MOCK_NODE_EXIT_DELAY_MS", "1000")]);

    let first = expect(&rx, "node_started");
    let old_pid = first["pid"].as_i64().unwrap() as i32;
    expect(&rx, "wallet_ready");
    send(&mut stdin, json!({"cmd": "restart_node"}));

    expect(&rx, "node_started");
    // Signal 0 only probes: ESRCH means the old node has exited and been reaped.
    assert!(
        nix::sys::signal::kill(nix::unistd::Pid::from_raw(old_pid), None).is_err(),
        "old node {old_pid} was still running when the new one started"
    );
    expect(&rx, "wallet_ready");

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A setting changed while a restart waits for the old node is saved and
/// applied to that restart, rather than restarting the new node again.
#[test]
fn settings_sent_during_a_node_restart_apply_to_that_restart() {
    let dir = TempDir::new("settings-during-restart");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) =
        spawn_watchdog_with_env(&cfg, &[("MOCK_NODE_EXIT_DELAY_MS", "1500")]);

    expect(&rx, "wallet_ready");
    send(&mut stdin, json!({"cmd": "restart_node"}));
    expect_with(&rx, "backend_stop_progress", |v| {
        v["stage"] == "stopping_node"
    });
    let args = json!(["+RTS", "-N2", "-RTS"]);
    send(
        &mut stdin,
        json!({"cmd": "set_node_extra_args", "args": args}),
    );

    let mut node_starts = 0;
    loop {
        let v = rx
            .recv_timeout(Duration::from_secs(15))
            .expect("timeout waiting for wallet_ready");
        match v["event"].as_str() {
            Some("node_started") => node_starts += 1,
            Some("wallet_ready") => break,
            _ => {}
        }
    }
    assert_eq!(node_starts, 1, "the restarted node was restarted again");
    let state: Value = serde_json::from_str(
        &std::fs::read_to_string(dir.path().join("watchdog-state.json")).unwrap(),
    )
    .unwrap();
    assert_eq!(state["node_extra_args"], args);

    stop(&mut stdin);
    expect(&rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// A stop requested during the delay before a crashed node is restarted ends
/// the watchdog at once, without starting another node.
#[test]
fn stop_during_the_crash_restart_delay_does_not_start_a_node() {
    let dir = TempDir::new("stop-during-crash-delay");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE_CRASH, MOCK_WALLET)
        .node_crash_restart_delay_ms(3000)
        .build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "node_started");
    expect(&rx, "node_exited");
    let started = std::time::Instant::now();
    stop(&mut stdin);

    let events = events_until_stopped(&rx);
    assert!(
        !events.iter().any(|e| e == "node_started"),
        "a node was started after the stop request: {events:?}"
    );
    assert!(
        started.elapsed() < Duration::from_millis(2500),
        "stop waited out the restart delay: {:?}",
        started.elapsed()
    );
    drop(stdin);
    let _ = child.wait();
}

// ── Tests: update installer ──────────────────────────────────────────────────

/// install_update stops the wallet and the node, and starts the installer
/// only once both have exited, then the watchdog exits.
///
/// On macOS the watchdog hands the installer to `open`, which passes `--args`
/// to the application that opens the file rather than to the file itself, so
/// the mock installer cannot run there. `installer_that_cannot_start_is_reported`
/// checks on every platform that the installer step follows the stop.
#[test]
#[cfg_attr(
    target_os = "macos",
    ignore = "macOS starts the installer through `open`, which cannot run the mock installer"
)]
fn installer_starts_only_after_node_and_wallet_have_exited() {
    let dir = TempDir::new("installer");
    dir.populate_chain();
    let report = dir.path().join("installer-report");
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    // The node takes 1.5 s to exit after the stop request.
    let (mut child, mut stdin, rx) =
        spawn_watchdog_with_env(&cfg, &[("MOCK_NODE_EXIT_DELAY_MS", "1500")]);

    let node_pid = expect(&rx, "node_started")["pid"].as_u64().unwrap();
    let wallet_pid = expect(&rx, "wallet_started")["pid"].as_u64().unwrap();
    expect(&rx, "wallet_ready");
    send(
        &mut stdin,
        json!({
            "cmd": "install_update",
            "path": MOCK_INSTALLER,
            "args": [
                report.to_str().unwrap(),
                node_pid.to_string(),
                wallet_pid.to_string()
            ]
        }),
    );

    let shutdown = expect(&rx, "node_shutdown_ms");
    assert!(shutdown["ms"].as_u64().unwrap() >= 1500, "{shutdown}");
    expect(&rx, "stopped");
    let launched = expect(&rx, "update_installer_launched");
    assert!(launched["pid"].as_u64().unwrap() > 0, "{launched}");
    drop(stdin);
    wait_for_exit(&mut child, Duration::from_secs(15));

    let start = std::time::Instant::now();
    while !report.exists() {
        assert!(
            start.elapsed() < Duration::from_secs(5),
            "installer did not run"
        );
        std::thread::sleep(Duration::from_millis(50));
    }
    std::thread::sleep(Duration::from_millis(100));
    assert_eq!(
        std::fs::read_to_string(&report).unwrap(),
        format!("{node_pid} exited\n{wallet_pid} exited\n")
    );
}

/// An installer that cannot be started is reported, after the backend has
/// stopped, rather than failing silently.
#[test]
fn installer_that_cannot_start_is_reported() {
    let dir = TempDir::new("installer-missing");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "wallet_ready");
    let missing = dir.path().join("no-such-installer");
    send(
        &mut stdin,
        json!({"cmd": "install_update", "path": missing.to_str().unwrap()}),
    );
    expect(&rx, "node_shutdown_ms");
    expect(&rx, "stopped");
    let failed = expect(&rx, "update_installer_failed");
    assert!(
        !failed["message"].as_str().unwrap_or_default().is_empty(),
        "{failed}"
    );
    drop(stdin);
    wait_for_exit(&mut child, Duration::from_secs(15));
}

/// Bug 5 regression: stopped event is emitted when clean stop is sent during
/// socket wait (not a restart).
#[test]
fn stop_during_socket_wait_does_not_restart() {
    let dir = TempDir::new("bug5-stop-socket-wait");
    dir.populate_chain();
    let (cfg, _port) = Cfg::new(&dir, MOCK_NODE_NO_SOCKET, MOCK_WALLET).build();
    let (mut child, mut stdin, rx) = spawn_watchdog(&cfg);

    expect(&rx, "watchdog_started");
    expect(&rx, "chain_status");
    expect(&rx, "node_started");
    std::thread::sleep(Duration::from_millis(50));

    stop(&mut stdin);
    expect(&rx, "node_shutdown_ms");
    expect(&rx, "stopped");

    let extra = rx.recv_timeout(Duration::from_millis(200));
    assert!(extra.is_err(), "unexpected events after stopped: {extra:?}");

    drop(stdin);
    let _ = child.wait();
}
