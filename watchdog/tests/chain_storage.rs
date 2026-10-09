// Chain storage in a folder the user picked: the database lives in
// `<folder>/chain` for the node and for Mithril, whether the folder came from
// the storage picker or from an earlier version's settings, and a Mithril
// bootstrap never deletes files that are not a node database.
//
// Unix-only, like the other process tests: the node is a shell script that
// reads the shutdown pipe on fd 3.
#![cfg(unix)]

use serde_json::{Value, json};
use std::collections::BTreeMap;
use std::io::{BufRead, BufReader, Write};
use std::net::TcpListener;
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc;
use std::time::Duration;

const WATCHDOG: &str = env!("CARGO_BIN_EXE_cardano-watchdog");
const MOCK_WALLET: &str = env!("CARGO_BIN_EXE_mock-wallet");
const MOCK_MITHRIL: &str = env!("CARGO_BIN_EXE_mock-mithril-client");
const MOCK_CONVERTER: &str = env!("CARGO_BIN_EXE_mock-snapshot-converter");

/// A node like mock-node that also records its arguments in $NODE_ARGS_FILE.
/// `$1` is the socket path; the watchdog appends the database path and
/// `--shutdown-ipc 3`.
const RECORDING_NODE: &str = r#"
printf '%s\n' "$@" > "$NODE_ARGS_FILE"
for phase in StartedOpeningDB StartedOpeningImmutableDB OpenedImmutableDB \
             StartedOpeningVolatileDB OpenedVolatileDB StartedOpeningLgrDB \
             OpenedLgrDB OpenedDB; do
  echo "$phase"
done
: > "$1"
cat <&3 >/dev/null
"#;

struct TempDir(PathBuf);

impl TempDir {
    fn new(label: &str) -> Self {
        let path = std::env::temp_dir().join(format!(
            "wdg-storage-{}-{}-{}",
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

    /// A folder outside the state directory, as picked in the storage picker,
    /// holding a file of the user's own.
    fn picked_folder(&self) -> PathBuf {
        let folder = self.0.join("picked");
        std::fs::create_dir_all(&folder).unwrap();
        std::fs::write(folder.join("notes.txt"), b"user data").unwrap();
        folder
    }

    fn node_args_file(&self) -> PathBuf {
        self.0.join("node-args.txt")
    }

    fn node_database_path(&self) -> Option<String> {
        let args = std::fs::read_to_string(self.node_args_file()).ok()?;
        let args: Vec<&str> = args.lines().collect();
        let i = args.iter().position(|a| *a == "--database-path")?;
        args.get(i + 1).map(|s| s.to_string())
    }

    fn saved_state(&self) -> Value {
        let text = std::fs::read_to_string(self.state().join("watchdog-state.json")).unwrap();
        serde_json::from_str(&text).unwrap()
    }

    fn log(&self) -> String {
        std::fs::read_to_string(self.state().join("logs").join("watchdog.log")).unwrap_or_default()
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

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
            "exe": "/bin/sh",
            "args": ["-c", RECORDING_NODE, "recording-node", socket.to_str().unwrap()],
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
        "mithril": {
            "mithril_bin": MOCK_MITHRIL,
            "snapshot_converter_bin": MOCK_CONVERTER,
            "converter_config": "/dev/null",
            "aggregator_url": "http://localhost:0",
            "genesis_vkey": "test",
            "ancillary_vkey": "test",
            "state_dir": state.to_str().unwrap(),
            "chain_path": state.join("chain").to_str().unwrap(),
        },
    })
}

/// The network these tests run on: the one whose marker mock-mithril-client
/// writes into the databases it stages.
const NETWORK_MAGIC: &str = "764824073";

/// Another network's marker.
const OTHER_NETWORK_MAGIC: &str = "1";

/// `config` with a node configuration whose Byron genesis has `magic`, passed
/// to the node with `--config` as in the shipped configuration.
fn config_on_network(dir: &TempDir, magic: &str) -> Value {
    let genesis = dir.0.join("genesis-byron.json");
    std::fs::write(
        &genesis,
        format!(r#"{{"protocolConsts":{{"k":2160,"protocolMagic":{magic}}}}}"#),
    )
    .unwrap();
    let node_config = dir.0.join("config.yaml");
    std::fs::write(&node_config, r#"{"ByronGenesisFile":"genesis-byron.json"}"#).unwrap();
    let mut cfg = config(dir);
    let args = cfg["node"]["args"].as_array_mut().unwrap();
    args.push(json!("--config"));
    args.push(json!(node_config.to_str().unwrap()));
    cfg
}

/// A database of another network in `dir`, with chunks 0 to 9, a ledger
/// snapshot, an LSM directory and a clean marker: everything a Mithril
/// install would rewrite.
fn make_other_network_db(dir: &Path) {
    let immutable = dir.join("immutable");
    std::fs::create_dir_all(&immutable).unwrap();
    for n in 0..10 {
        for ext in ["chunk", "primary", "secondary"] {
            std::fs::write(
                immutable.join(format!("{n:05}.{ext}")),
                format!("local {n}"),
            )
            .unwrap();
        }
    }
    std::fs::create_dir_all(dir.join("ledger").join("999")).unwrap();
    std::fs::write(dir.join("ledger").join("999").join("state"), b"ledger").unwrap();
    std::fs::create_dir_all(dir.join("lsm")).unwrap();
    std::fs::write(dir.join("lsm").join("table"), b"lsm").unwrap();
    std::fs::write(dir.join("clean"), b"").unwrap();
    std::fs::write(dir.join("protocolMagicId"), OTHER_NETWORK_MAGIC).unwrap();
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

fn spawn(dir: &TempDir, envs: &[(&str, &Path)]) -> (Child, ChildStdin, mpsc::Receiver<Value>) {
    spawn_with(dir, &config(dir), envs)
}

fn spawn_with(
    dir: &TempDir,
    cfg: &Value,
    envs: &[(&str, &Path)],
) -> (Child, ChildStdin, mpsc::Receiver<Value>) {
    let path = dir.state().join("daedalus-config.json");
    std::fs::write(&path, serde_json::to_string_pretty(cfg).unwrap()).unwrap();
    let mut child = Command::new(WATCHDOG)
        .arg("--config")
        .arg(&path)
        .env("NODE_ARGS_FILE", dir.node_args_file())
        .envs(envs.iter().map(|(k, v)| (*k, *v)))
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

fn stop(mut child: Child, mut stdin: ChildStdin, rx: &mpsc::Receiver<Value>) {
    send(&mut stdin, json!({"cmd": "stop"}));
    expect(rx, "stopped");
    drop(stdin);
    let _ = child.wait();
}

/// No state file yet, and an empty default location: the first-run path
/// where the storage picker is shown.
fn first_run(dir: &TempDir) {
    std::fs::write(dir.state().join("watchdog-state.json"), b"{}").unwrap();
}

/// A folder picked in the storage picker keeps its own files through a
/// Mithril bootstrap, and the node and Mithril both use `<folder>/chain`.
#[test]
fn picked_folder_keeps_its_files_and_holds_the_database_in_chain() {
    let dir = TempDir::new("picked");
    first_run(&dir);
    let folder = dir.picked_folder();
    let (child, mut stdin, rx) = spawn(&dir, &[]);

    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    send(
        &mut stdin,
        json!({"cmd": "set_chain_path", "path": folder.to_str().unwrap()}),
    );
    send(&mut stdin, json!({"cmd": "start_mithril"}));
    expect(&rx, "node_started");
    expect(&rx, "wallet_ready");

    let db = folder.join("chain");
    assert_eq!(
        std::fs::read(folder.join("notes.txt")).unwrap(),
        b"user data"
    );
    assert!(
        db.join("protocolMagicId").exists(),
        "Mithril did not install into {db:?}"
    );
    assert!(
        !folder.join("protocolMagicId").exists(),
        "Mithril installed into the folder itself"
    );
    assert_eq!(dir.node_database_path().as_deref(), db.to_str());
    assert_eq!(dir.saved_state()["chain_path"], folder.to_str().unwrap());

    stop(child, stdin, &rx);
}

/// A bootstrap into a database directory that holds anything other than a
/// node database is refused before anything is downloaded or deleted.
#[test]
fn bootstrap_refuses_a_chain_directory_with_unrelated_files() {
    let dir = TempDir::new("refuse");
    first_run(&dir);
    let folder = dir.picked_folder();
    std::fs::create_dir_all(folder.join("chain")).unwrap();
    std::fs::write(folder.join("chain").join("photo.jpg"), b"user data").unwrap();
    let mithril_args = dir.0.join("mithril-args.json");
    let (child, mut stdin, rx) = spawn(&dir, &[("MOCK_MITHRIL_ARGS_FILE", &mithril_args)]);

    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    send(
        &mut stdin,
        json!({"cmd": "set_chain_path", "path": folder.to_str().unwrap()}),
    );
    send(&mut stdin, json!({"cmd": "start_mithril"}));

    let err = expect(&rx, "mithril_error");
    assert_eq!(err["code"], "CHAIN_DIR_NOT_REPLACEABLE");
    assert!(
        err["message"].as_str().unwrap().contains("photo.jpg"),
        "{err}"
    );
    // The user is asked again rather than left on a half-done install.
    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    assert!(!mithril_args.exists(), "a download was started");
    assert_eq!(
        std::fs::read(folder.join("chain").join("photo.jpg")).unwrap(),
        b"user data"
    );
    assert_eq!(
        std::fs::read(folder.join("notes.txt")).unwrap(),
        b"user data"
    );

    stop(child, stdin, &rx);
}

/// The value 11.3 and 11.4 kept in electron-store is the picked folder, with
/// their Mithril download in `<folder>/chain`. When the default location holds
/// no database, migration adopts the folder and resolves it to the same
/// database path as picking the folder does.
#[test]
fn migrated_folder_resolves_to_the_same_database_path() {
    let dir = TempDir::new("migrate");
    let folder = dir.picked_folder();
    make_db(&folder.join("chain"));
    let (child, mut stdin, rx) = spawn(&dir, &[]);

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
    assert_eq!(expect(&rx, "chain_status")["has_chain"], true);
    // The node records its arguments before it reports any startup phase.
    expect(&rx, "node_socket_ready");

    assert_eq!(
        dir.node_database_path().as_deref(),
        folder.join("chain").to_str()
    );
    assert_eq!(dir.saved_state()["chain_path"], folder.to_str().unwrap());

    stop(child, stdin, &rx);
}

/// A migrated folder whose `chain` subdirectory holds no database is not
/// adopted: the node stays on the database it already has.
#[test]
fn migrated_folder_without_a_database_keeps_the_default() {
    let dir = TempDir::new("keep");
    make_db(&dir.state().join("chain"));
    let folder = dir.picked_folder();
    let (child, mut stdin, rx) = spawn(&dir, &[]);

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
    assert_eq!(expect(&rx, "chain_status")["has_chain"], true);
    // The node records its arguments before it reports any startup phase.
    expect(&rx, "node_socket_ready");

    assert!(dir.saved_state()["chain_path"].is_null());
    // The configured default is passed through unchanged; the test config has
    // no --database-path of its own.
    assert_eq!(dir.node_database_path(), None);
    assert!(dir.log().contains("keeping"), "{}", dir.log());

    stop(child, stdin, &rx);
}

/// 11.3 and 11.4 ran the node on the default location whatever the folder, so
/// a user who picked one can have a database in both. Migration keeps the
/// node on the database it ran on, leaves the other in place, and logs both.
#[test]
fn migrated_folder_does_not_replace_the_node_database() {
    let dir = TempDir::new("both");
    let default = dir.state().join("chain");
    make_db(&default);
    let folder = dir.picked_folder();
    make_db(&folder.join("chain"));
    let (child, mut stdin, rx) = spawn(&dir, &[]);

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
    assert_eq!(expect(&rx, "chain_status")["has_chain"], true);
    expect(&rx, "node_socket_ready");

    assert!(dir.saved_state()["chain_path"].is_null());
    assert_eq!(dir.node_database_path(), None);
    let log = dir.log();
    assert!(log.contains(default.to_str().unwrap()), "{log}");
    assert!(
        log.contains(folder.join("chain").to_str().unwrap()),
        "{log}"
    );
    assert!(default.join("protocolMagicId").exists());
    assert!(folder.join("chain").join("protocolMagicId").exists());

    stop(child, stdin, &rx);
}

/// Resetting the storage picker to the default after picking a folder moves
/// the node and Mithril back to the configured location together.
#[test]
fn reset_to_default_after_picking_a_folder_moves_node_and_mithril_back() {
    let dir = TempDir::new("reset");
    first_run(&dir);
    let folder = dir.picked_folder();
    let default = dir.state().join("chain");
    let mut cfg = config(&dir);
    // As in the shipped configuration, the node's --database-path and
    // Mithril's chain_path are the same configured location.
    let args = cfg["node"]["args"].as_array_mut().unwrap();
    args.push(json!("--database-path"));
    args.push(json!(default.to_str().unwrap()));
    let (child, mut stdin, rx) = spawn_with(&dir, &cfg, &[]);

    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    send(
        &mut stdin,
        json!({"cmd": "set_chain_path", "path": folder.to_str().unwrap()}),
    );
    send(&mut stdin, json!({"cmd": "set_chain_path", "path": null}));
    send(&mut stdin, json!({"cmd": "start_mithril"}));
    expect(&rx, "node_started");
    expect(&rx, "wallet_ready");

    assert!(
        default.join("protocolMagicId").exists(),
        "Mithril did not install into {default:?}"
    );
    assert!(
        !folder.join("chain").exists(),
        "Mithril installed into the folder picked before the reset"
    );
    assert_eq!(dir.node_database_path().as_deref(), default.to_str());
    assert!(dir.saved_state()["chain_path"].is_null());

    stop(child, stdin, &rx);
}

/// A picked folder whose `chain` subdirectory holds only files of the user's
/// own is not taken for a chain: the user is asked again on every start rather
/// than left with a node that refuses to open it, and the files stay.
#[test]
fn chain_subdirectory_holding_other_files_is_not_a_chain() {
    let dir = TempDir::new("not-a-chain");
    let folder = dir.picked_folder();
    std::fs::create_dir_all(folder.join("chain")).unwrap();
    std::fs::write(folder.join("chain").join("photo.jpg"), b"user data").unwrap();
    std::fs::write(
        dir.state().join("watchdog-state.json"),
        json!({ "chain_path": folder.to_str().unwrap() }).to_string(),
    )
    .unwrap();
    let (child, stdin, rx) = spawn(&dir, &[]);

    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    assert!(!dir.node_args_file().exists(), "a node was started");
    assert_eq!(
        std::fs::read(folder.join("chain").join("photo.jpg")).unwrap(),
        b"user data"
    );

    stop(child, stdin, &rx);
}

/// The case where two clusters share one storage folder: on this cluster's
/// first run the folder's chain subdirectory holds the other network's live
/// database. Choosing Mithril would make a partial sync of it; it is refused
/// before anything is downloaded, and not one file changes.
#[test]
fn partial_sync_refuses_another_networks_database_and_changes_nothing() {
    let dir = TempDir::new("other-partial");
    first_run(&dir);
    let folder = dir.picked_folder();
    let db = folder.join("chain");
    make_other_network_db(&db);
    let before = tree(&folder);
    let mithril_args = dir.0.join("mithril-args.json");
    let cfg = config_on_network(&dir, NETWORK_MAGIC);
    let (child, mut stdin, rx) = spawn_with(
        &dir,
        &cfg,
        &[
            ("MOCK_MITHRIL_ARGS_FILE", &mithril_args),
            ("MOCK_CERTIFIED_IMMUTABLE", Path::new("4")),
        ],
    );

    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    send(
        &mut stdin,
        json!({"cmd": "set_chain_path", "path": folder.to_str().unwrap()}),
    );
    send(&mut stdin, json!({"cmd": "start_mithril"}));

    let err = expect(&rx, "mithril_error");
    assert_eq!(err["code"], "CHAIN_DIR_OTHER_NETWORK");
    let message = err["message"].as_str().unwrap();
    assert!(message.contains(db.to_str().unwrap()), "{err}");
    assert!(message.contains("another Cardano network"), "{err}");
    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    assert!(!mithril_args.exists(), "a download was started");
    assert!(!dir.node_args_file().exists(), "a node was started");
    assert_eq!(tree(&folder), before);

    stop(child, stdin, &rx);
}

/// A database of another network that holds only database entries and no
/// chunks would be replaced whole by a full install, which takes any such
/// directory for its own. It is refused, and not one file changes.
#[test]
fn full_install_refuses_another_networks_database_and_changes_nothing() {
    let dir = TempDir::new("other-full");
    first_run(&dir);
    let folder = dir.picked_folder();
    let db = folder.join("chain");
    std::fs::create_dir_all(db.join("volatile")).unwrap();
    std::fs::write(db.join("volatile").join("blocks-0.dat"), b"blocks").unwrap();
    std::fs::write(db.join("lock"), b"").unwrap();
    std::fs::write(db.join("protocolMagicId"), OTHER_NETWORK_MAGIC).unwrap();
    let before = tree(&folder);
    let mithril_args = dir.0.join("mithril-args.json");
    let cfg = config_on_network(&dir, NETWORK_MAGIC);
    let (child, mut stdin, rx) =
        spawn_with(&dir, &cfg, &[("MOCK_MITHRIL_ARGS_FILE", &mithril_args)]);

    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    send(
        &mut stdin,
        json!({"cmd": "set_chain_path", "path": folder.to_str().unwrap()}),
    );
    send(&mut stdin, json!({"cmd": "start_mithril"}));

    assert_eq!(
        expect(&rx, "mithril_error")["code"],
        "CHAIN_DIR_OTHER_NETWORK"
    );
    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    assert!(!mithril_args.exists(), "a download was started");
    assert_eq!(tree(&folder), before);

    stop(child, stdin, &rx);
}

/// A storage folder that already holds another network's database, picked
/// before such folders were refused or written to later by the other cluster,
/// is not taken for a chain: the user is asked again, and choosing to sync
/// from genesis does not start a node on it.
#[test]
fn configured_folder_with_another_networks_database_starts_no_node() {
    let dir = TempDir::new("other-configured");
    let folder = dir.picked_folder();
    let db = folder.join("chain");
    make_other_network_db(&db);
    std::fs::write(
        dir.state().join("watchdog-state.json"),
        json!({ "chain_path": folder.to_str().unwrap() }).to_string(),
    )
    .unwrap();
    let before = tree(&folder);
    let cfg = config_on_network(&dir, NETWORK_MAGIC);
    let (child, mut stdin, rx) = spawn_with(&dir, &cfg, &[]);

    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    assert!(dir.log().contains("network magic 1"), "{}", dir.log());

    send(&mut stdin, json!({"cmd": "start_node"}));
    let err = expect(&rx, "error");
    assert!(
        err["message"]
            .as_str()
            .unwrap()
            .contains("another Cardano network"),
        "{err}"
    );
    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    assert!(!dir.node_args_file().exists(), "a node was started");
    assert_eq!(tree(&folder), before);

    stop(child, stdin, &rx);
}

/// Every start of the node is checked, not only the first: moving a running
/// node to a folder that holds another network's database stops it and does
/// not start it again there.
#[test]
fn node_is_not_restarted_on_another_networks_database() {
    let dir = TempDir::new("other-restart");
    let default = dir.state().join("chain");
    std::fs::create_dir_all(default.join("immutable")).unwrap();
    std::fs::write(default.join("protocolMagicId"), NETWORK_MAGIC).unwrap();
    std::fs::write(dir.state().join("watchdog-state.json"), b"{}").unwrap();
    let folder = dir.picked_folder();
    make_other_network_db(&folder.join("chain"));
    let before = tree(&folder);
    let cfg = config_on_network(&dir, NETWORK_MAGIC);
    let (child, mut stdin, rx) = spawn_with(&dir, &cfg, &[]);

    assert_eq!(expect(&rx, "chain_status")["has_chain"], true);
    expect(&rx, "node_socket_ready");
    std::fs::remove_file(dir.node_args_file()).unwrap();

    send(
        &mut stdin,
        json!({"cmd": "set_chain_path", "path": folder.to_str().unwrap()}),
    );
    expect(&rx, "node_shutdown_ms");
    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    assert!(!dir.node_args_file().exists(), "the node was started again");
    assert_eq!(tree(&folder), before);

    stop(child, stdin, &rx);
}

/// This network's database in a picked folder is used as before: it counts
/// as a chain, and a Mithril partial sync brings it up to the certified tip.
#[test]
fn this_networks_database_in_a_picked_folder_is_synced_as_before() {
    let dir = TempDir::new("same-network");
    first_run(&dir);
    let folder = dir.picked_folder();
    let db = folder.join("chain");
    make_other_network_db(&db);
    std::fs::write(db.join("protocolMagicId"), NETWORK_MAGIC).unwrap();
    let cfg = config_on_network(&dir, NETWORK_MAGIC);
    let (child, mut stdin, rx) =
        spawn_with(&dir, &cfg, &[("MOCK_CERTIFIED_IMMUTABLE", Path::new("4"))]);

    assert_eq!(expect(&rx, "chain_status")["has_chain"], false);
    send(
        &mut stdin,
        json!({"cmd": "set_chain_path", "path": folder.to_str().unwrap()}),
    );
    send(&mut stdin, json!({"cmd": "start_mithril"}));
    expect(&rx, "node_started");
    expect(&rx, "node_socket_ready");

    assert_eq!(
        std::fs::read_to_string(db.join("immutable").join("00004.chunk")).unwrap(),
        "certified 4"
    );
    assert!(!db.join("immutable").join("00009.chunk").exists());
    assert_eq!(dir.node_database_path().as_deref(), db.to_str());

    stop(child, stdin, &rx);
}
