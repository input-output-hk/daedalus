use serde::Deserialize;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::Duration;

use crate::tls::TlsClientMaterial;

#[derive(Debug, Deserialize)]
pub struct WatchdogConfig {
    pub node: NodeConfig,
    pub wallet: WalletConfig,
    #[serde(default)]
    pub pub_logs_dir: Option<String>,
    pub mithril: Option<MithrilConfig>,
    #[serde(default)]
    pub tls_dir: Option<String>,
    pub electron: Option<ElectronConfig>,
    /// The generated client certificate and CA, set at startup when
    /// `tls_dir` is set, for the wallet health probe.
    #[serde(skip)]
    pub wallet_client_tls: Option<TlsClientMaterial>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct ElectronConfig {
    pub exe: String,
    #[serde(default)]
    pub args: Vec<String>,
    #[serde(default)]
    pub env: HashMap<String, String>,
}

#[derive(Debug, Deserialize)]
pub struct NodeConfig {
    pub exe: String,
    /// Args for cardano-node, NOT including --shutdown-ipc (watchdog adds that).
    pub args: Vec<String>,
    pub state_dir: String,
    /// Absolute path to the node socket file; watchdog waits for this before starting wallet.
    pub socket_path: String,
    /// Milliseconds to wait before restarting after an unexpected node exit.
    #[serde(default = "default_node_crash_restart_delay_ms")]
    pub crash_restart_delay_ms: u64,
    /// Maximum number of unexpected node exits before giving up.
    #[serde(default = "default_node_max_crash_attempts")]
    pub max_crash_attempts: u32,
    /// Seconds to wait for cardano-node to exit after it is asked to stop.
    /// A node still running after this is killed, which leaves the chain
    /// database unclean and makes the next start revalidate it.
    #[serde(default = "default_node_stop_timeout_secs")]
    pub stop_timeout_secs: u64,
    /// Unexpected node exits within `crash_window_secs` that make the node
    /// unrecoverable, whether or not it became ready in between. 0 disables
    /// this limit.
    #[serde(default = "default_max_crashes_in_window")]
    pub max_crashes_in_window: u32,
    /// Length of the window `max_crashes_in_window` is counted over.
    #[serde(default = "default_crash_window_secs")]
    pub crash_window_secs: u64,
    /// Seconds the node may write nothing on either output stream while it
    /// starts, before `chainDbReady`, until the watchdog reports the silence
    /// with `node_silent`. 0 turns the report off.
    #[serde(default = "default_startup_silence_secs")]
    pub startup_silence_secs: u64,
    /// Network magic of the cluster, which cardano-node writes to a new
    /// database's `protocolMagicId` file and requires of an existing one.
    /// Set by `WatchdogConfig::set_network_magic`; None when it is unknown.
    #[serde(skip)]
    pub network_magic: Option<u32>,
    /// How the node lays out its database, from its configuration. Set at
    /// startup by `read_node_storage`; None when it could not be read, and
    /// then the ledger point check before a node start is skipped.
    #[serde(skip)]
    pub storage: Option<NodeStorage>,
}

fn default_node_crash_restart_delay_ms() -> u64 {
    5_000
}

fn default_node_max_crash_attempts() -> u32 {
    10
}

/// Upper bound on a cardano-node stop, in seconds. Every path that stops the
/// node (quit, Electron exit, restart, Mithril) waits at most this long. A
/// clean stop takes under a second, so this leaves wide headroom for a slow
/// disk while keeping a stop that will not finish from holding the window.
pub const DEFAULT_NODE_STOP_TIMEOUT_SECS: u64 = 60;

/// Time a stopping watchdog may need beyond its two stop bounds: killing a
/// process that reached its bound, and exiting.
pub const BACKEND_STOP_MARGIN: Duration = Duration::from_secs(30);

/// Name of the variable that passes the network magic to Electron, whose
/// storage picker checks a chosen folder's database against it.
pub const NETWORK_MAGIC_ENV: &str = "DAEDALUS_NETWORK_MAGIC";

impl WatchdogConfig {
    /// Records the cluster's network magic for every check of a database's
    /// network: the has-chain check and the node start in the supervisor, and
    /// the Mithril pipeline. Electron receives it in `NETWORK_MAGIC_ENV`.
    pub fn set_network_magic(&mut self, magic: u32) {
        self.node.network_magic = Some(magic);
        if let Some(ref mut mithril) = self.mithril {
            mithril.network_magic = Some(magic);
        }
        if let Some(ref mut electron) = self.electron {
            electron
                .env
                .insert(NETWORK_MAGIC_ENV.to_string(), magic.to_string());
        }
    }

    /// The longest a backend stop takes: the wallet's bound, then the node's,
    /// plus a margin. A later launch waits this long for a stopping instance
    /// to exit, so it cannot give up while that stop is still within bounds.
    pub fn backend_stop_limit(&self) -> Duration {
        Duration::from_secs(self.wallet.stop_timeout_secs)
            + Duration::from_secs(self.node.stop_timeout_secs)
            + BACKEND_STOP_MARGIN
    }
}

/// How long a starting node may write nothing before the user is told and
/// offered a restart.
pub const DEFAULT_STARTUP_SILENCE_SECS: u64 = 300;

fn default_startup_silence_secs() -> u64 {
    DEFAULT_STARTUP_SILENCE_SECS
}

fn default_node_stop_timeout_secs() -> u64 {
    DEFAULT_NODE_STOP_TIMEOUT_SECS
}

fn default_max_crashes_in_window() -> u32 {
    5
}

fn default_crash_window_secs() -> u64 {
    600
}

#[derive(Debug, Deserialize, Clone)]
pub struct WalletConfig {
    pub exe: String,
    pub args: Vec<String>,
    pub state_dir: String,
    pub api_port: Option<u16>,
    #[serde(default = "default_restart_delay_ms")]
    pub restart_delay_ms: u64,
    #[serde(default = "default_max_restart_attempts")]
    pub max_restart_attempts: u32,
    /// Seconds to wait for cardano-wallet to exit after it is asked to stop,
    /// before killing it.
    #[serde(default = "default_wallet_stop_timeout_secs")]
    pub stop_timeout_secs: u64,
    /// The wallet health check. Absent, the check is off.
    #[serde(default)]
    pub health: Option<WalletHealthConfig>,
    /// Unexpected wallet exits within `crash_window_secs` that make the
    /// wallet unrecoverable, whether or not it became ready in between. 0
    /// disables this limit.
    #[serde(default = "default_max_crashes_in_window")]
    pub max_crashes_in_window: u32,
    /// Length of the window `max_crashes_in_window` is counted over.
    #[serde(default = "default_crash_window_secs")]
    pub crash_window_secs: u64,
}

fn default_restart_delay_ms() -> u64 {
    1000
}

/// Upper bound on a requested cardano-wallet stop (quit, restarts the user
/// asks for, Mithril, update install), in seconds. A restart for no response
/// and the stop that follows a node exit use `wallet.health.stop_timeout_secs`
/// (15 s) instead. A stop with nothing running
/// against the wallet takes milliseconds, but a wallet that closes its
/// databases while queries are in flight can need much longer, and a kill
/// mid-close costs time on the next start. The bound matches the node's.
pub const DEFAULT_WALLET_STOP_TIMEOUT_SECS: u64 = 60;

fn default_wallet_stop_timeout_secs() -> u64 {
    DEFAULT_WALLET_STOP_TIMEOUT_SECS
}

/// Settings of the wallet health check: the watchdog asks the wallet API for
/// the network information on a new connection every `probe_interval_ms`,
/// and restarts a wallet whose API stops answering while its process stays
/// alive. Every field has a default, so `"health": {}` turns the check on.
#[derive(Debug, Deserialize, Clone)]
pub struct WalletHealthConfig {
    /// Time between probes. A probe is a request and a reply in the wallet's
    /// log, so this sets how many lines the check adds.
    #[serde(default = "default_probe_interval_ms")]
    pub probe_interval_ms: u64,
    /// How long one probe may take, from connect to the status line.
    #[serde(default = "default_probe_timeout_ms")]
    pub probe_timeout_ms: u64,
    /// Consecutive failed probes that make a verdict. Doubled for each health
    /// restart already in `restart_window_ms`.
    #[serde(default = "default_failure_threshold")]
    pub failure_threshold: u32,
    /// How long a wallet may go without answering for the first time. The
    /// first request waits for the wallet's first connection to the node, so
    /// timeouts are ignored until it answers; refused connections are not.
    #[serde(default = "default_startup_grace_ms")]
    pub startup_grace_ms: u64,
    /// Most health restarts in any `restart_window_ms`; the next verdict
    /// leaves the wallet stopped and offers the user a retry.
    #[serde(default = "default_health_max_restarts")]
    pub max_restarts: u32,
    /// The sliding window `max_restarts` is counted over.
    #[serde(default = "default_health_restart_window_ms")]
    pub restart_window_ms: u64,
    /// Seconds a stop that precedes an automatic restart waits for the wallet
    /// to exit after its stdin is closed, before killing it: the restart for
    /// no response, and the stop that follows a node exit. Shorter than
    /// `wallet.stop_timeout_secs`, because every second of grace is a second
    /// of outage.
    #[serde(default = "default_health_stop_timeout_secs")]
    pub stop_timeout_secs: u64,
}

impl Default for WalletHealthConfig {
    fn default() -> Self {
        Self {
            probe_interval_ms: default_probe_interval_ms(),
            probe_timeout_ms: default_probe_timeout_ms(),
            failure_threshold: default_failure_threshold(),
            startup_grace_ms: default_startup_grace_ms(),
            max_restarts: default_health_max_restarts(),
            restart_window_ms: default_health_restart_window_ms(),
            stop_timeout_secs: default_health_stop_timeout_secs(),
        }
    }
}

fn default_probe_interval_ms() -> u64 {
    15_000
}

fn default_probe_timeout_ms() -> u64 {
    10_000
}

fn default_failure_threshold() -> u32 {
    4
}

fn default_startup_grace_ms() -> u64 {
    300_000
}

fn default_health_max_restarts() -> u32 {
    5
}

fn default_health_restart_window_ms() -> u64 {
    3_600_000
}

/// Default bound, in seconds, on a wallet stop that precedes an automatic
/// restart.
pub const DEFAULT_RESTART_STOP_TIMEOUT_SECS: u64 = 15;

fn default_health_stop_timeout_secs() -> u64 {
    DEFAULT_RESTART_STOP_TIMEOUT_SECS
}

fn default_max_restart_attempts() -> u32 {
    10
}

#[derive(Debug, Deserialize, Clone)]
pub struct MithrilConfig {
    pub mithril_bin: String,
    pub snapshot_converter_bin: String,
    pub converter_config: String,
    pub aggregator_url: String,
    pub genesis_vkey: String,
    pub ancillary_vkey: Option<String>,
    pub state_dir: String,
    pub chain_path: String,
    #[serde(default = "default_behind_threshold")]
    pub behind_threshold: u64,
    /// How long, in seconds, an install keeps trying a rename that fails
    /// without crossing volumes before the install fails. On Windows a file
    /// that another program, such as a virus scanner, holds open cannot be
    /// renamed until it lets go.
    #[serde(default = "default_rename_retry_secs")]
    pub rename_retry_secs: u64,
    /// How long, in seconds, a download may go without its progress growing
    /// before the watchdog asks mithril-client whether it can reach the
    /// aggregator. 0 turns the check off.
    #[serde(default = "default_download_stall_secs")]
    pub download_stall_secs: u64,
    /// How often, in seconds, that check is repeated while the download
    /// stays stalled.
    #[serde(default = "default_network_recheck_secs")]
    pub network_recheck_secs: u64,
    /// `chain_path` as configured. A storage folder picked by the user
    /// replaces `chain_path`, and clearing the folder restores this value.
    /// Recorded by `state::apply_to_config` the first time it runs.
    #[serde(skip)]
    pub configured_chain_path: Option<String>,
    /// Network magic of the cluster, as in `NodeConfig::network_magic`.
    #[serde(skip)]
    pub network_magic: Option<u32>,
}

fn default_behind_threshold() -> u64 {
    20
}

fn default_rename_retry_secs() -> u64 {
    60
}

fn default_download_stall_secs() -> u64 {
    60
}

fn default_network_recheck_secs() -> u64 {
    20
}

/// The node configuration given by `--config`, and its path.
async fn read_node_config(node_args: &[String]) -> anyhow::Result<(PathBuf, serde_json::Value)> {
    let config_path = node_args
        .windows(2)
        .find(|w| w[0] == "--config")
        .map(|w| PathBuf::from(&w[1]))
        .ok_or_else(|| anyhow::anyhow!("the node arguments have no --config"))?;
    let node_config = read_json(&config_path).await?;
    Ok((config_path, node_config))
}

/// The Byron genesis that `node_config`, read from `config_path`, names. A
/// relative genesis path is relative to the configuration file, as
/// cardano-node resolves it.
async fn read_byron_genesis(
    config_path: &Path,
    node_config: &serde_json::Value,
) -> anyhow::Result<(PathBuf, serde_json::Value)> {
    let genesis = node_config
        .get("ByronGenesisFile")
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow::anyhow!("{} names no ByronGenesisFile", config_path.display()))?;
    let genesis_path = config_path.parent().unwrap_or(Path::new(".")).join(genesis);
    let genesis = read_json(&genesis_path).await?;
    Ok((genesis_path, genesis))
}

/// The network magic cardano-node checks a database against: the
/// `protocolConsts.protocolMagic` of the Byron genesis that the node
/// configuration given by `--config` names.
pub async fn read_network_magic(node_args: &[String]) -> anyhow::Result<u32> {
    let (config_path, node_config) = read_node_config(node_args).await?;
    let (genesis_path, genesis) = read_byron_genesis(&config_path, &node_config).await?;
    genesis
        .pointer("/protocolConsts/protocolMagic")
        .and_then(|v| v.as_u64())
        .and_then(|v| u32::try_from(v).ok())
        .ok_or_else(|| {
            anyhow::anyhow!(
                "{} has no protocolConsts.protocolMagic",
                genesis_path.display()
            )
        })
}

/// How cardano-node lays out the parts of its database that the check before
/// a node start reads (`ledger_point`): the ImmutableDB chunk size and the
/// LedgerDB backend.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeStorage {
    /// Slots in each ImmutableDB chunk. For Cardano this is the Byron epoch
    /// size, ten times the security parameter `protocolConsts.k` of the
    /// Byron genesis: ouroboros-consensus gives the hard fork combinator the
    /// chunk info of its first era (`nodeImmutableDbChunkInfo` in
    /// `HardFork.Combinator.Node.InitStorage`), and Byron's is
    /// `simpleChunkInfo (kEpochSlots k)`. That is 21600 slots on mainnet and
    /// preprod, where k is 2160, and 4320 on preview.
    pub chunk_slots: u64,
    pub backend: LedgerBackend,
}

/// The LedgerDB backend cardano-node opens snapshots with, from the
/// `LedgerDB` section of its configuration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LedgerBackend {
    /// `V2LSM`, with the LSM database directory relative to the database
    /// directory: `LSMDatabasePath`, or `lsm` when the configuration names
    /// none.
    Lsm(PathBuf),
    /// Any other backend, by name. cardano-node uses `V2InMemory` when the
    /// configuration has no `LedgerDB` section or names no `Backend`.
    Other(String),
}

/// The node storage layout that the configuration given by `--config` and
/// its Byron genesis describe.
pub async fn read_node_storage(node_args: &[String]) -> anyhow::Result<NodeStorage> {
    let (config_path, node_config) = read_node_config(node_args).await?;
    let (genesis_path, genesis) = read_byron_genesis(&config_path, &node_config).await?;
    let chunk_slots = genesis
        .pointer("/protocolConsts/k")
        .and_then(|v| v.as_u64())
        .filter(|k| *k > 0)
        .and_then(|k| k.checked_mul(10))
        .ok_or_else(|| {
            anyhow::anyhow!("{} has no usable protocolConsts.k", genesis_path.display())
        })?;
    let backend = ledger_backend(&node_config)
        .map_err(|e| anyhow::anyhow!("{}: {e}", config_path.display()))?;
    Ok(NodeStorage {
        chunk_slots,
        backend,
    })
}

/// The LedgerDB backend of a node configuration, read as cardano-node's
/// `Cardano.Node.Configuration.POM` reads it: a missing section or `Backend`
/// means `V2InMemory`, and a null `LSMDatabasePath` counts as absent.
fn ledger_backend(node_config: &serde_json::Value) -> anyhow::Result<LedgerBackend> {
    let Some(section) = node_config.get("LedgerDB") else {
        return Ok(LedgerBackend::Other("V2InMemory".to_string()));
    };
    let backend = match section.get("Backend") {
        None | Some(serde_json::Value::Null) => "V2InMemory",
        Some(v) => v
            .as_str()
            .ok_or_else(|| anyhow::anyhow!("LedgerDB.Backend is not a string"))?,
    };
    if backend != "V2LSM" {
        return Ok(LedgerBackend::Other(backend.to_string()));
    }
    let dir = match section.get("LSMDatabasePath") {
        None | Some(serde_json::Value::Null) => "lsm",
        Some(v) => v
            .as_str()
            .ok_or_else(|| anyhow::anyhow!("LedgerDB.LSMDatabasePath is not a string"))?,
    };
    let dir = PathBuf::from(dir);
    if dir.is_absolute() || dir.has_root() {
        anyhow::bail!(
            "LedgerDB.LSMDatabasePath {} is not relative to the database directory",
            dir.display()
        );
    }
    Ok(LedgerBackend::Lsm(dir))
}

async fn read_json(path: &Path) -> anyhow::Result<serde_json::Value> {
    let text = tokio::fs::read_to_string(path)
        .await
        .map_err(|e| anyhow::anyhow!("cannot read {}: {e}", path.display()))?;
    serde_json::from_str(&text).map_err(|e| anyhow::anyhow!("cannot parse {}: {e}", path.display()))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn minimal_json(extra_wallet: &str) -> String {
        format!(
            r#"{{
                "node": {{"exe":"n","args":[],"state_dir":"/","socket_path":"/s"}},
                "wallet": {{"exe":"w","args":[],"state_dir":"/"{}}}
            }}"#,
            extra_wallet
        )
    }

    #[test]
    fn parse_full_config() {
        let json = r#"{
            "node": {"exe":"/bin/cardano-node","args":["--config","cfg.json"],
                     "state_dir":"/state/node","socket_path":"/state/node/node.socket"},
            "wallet": {"exe":"/bin/cardano-wallet","args":["serve"],
                       "state_dir":"/state/wallet","api_port":8090,"restart_delay_ms":2000},
            "pub_logs_dir":"/logs"
        }"#;
        let c: WatchdogConfig = serde_json::from_str(json).unwrap();
        assert_eq!(c.node.exe, "/bin/cardano-node");
        assert_eq!(c.node.args, vec!["--config", "cfg.json"]);
        assert_eq!(c.node.socket_path, "/state/node/node.socket");
        assert_eq!(c.wallet.api_port, Some(8090));
        assert_eq!(c.wallet.restart_delay_ms, 2000);
        assert_eq!(c.pub_logs_dir.as_deref(), Some("/logs"));
    }

    #[test]
    fn parse_config_without_optional_fields() {
        let json = r#"{
            "node": {"exe":"n","args":[],"state_dir":"/","socket_path":"/s"},
            "wallet": {"exe":"w","args":[],"state_dir":"/"}
        }"#;
        let c: WatchdogConfig = serde_json::from_str(json).unwrap();
        assert!(c.wallet.api_port.is_none());
        assert!(c.pub_logs_dir.is_none());
        assert!(c.tls_dir.is_none());
        assert!(c.electron.is_none());
        assert!(c.mithril.is_none());
    }

    #[test]
    fn parse_electron_config() {
        let json = r#"{
            "node": {"exe":"n","args":[],"state_dir":"/","socket_path":"/s"},
            "wallet": {"exe":"w","args":[],"state_dir":"/"},
            "electron": {
                "exe": "/usr/bin/electron",
                "args": ["/app/js"],
                "env": {"SAMPLE_ENV": "/config/value"}
            }
        }"#;
        let c: WatchdogConfig = serde_json::from_str(json).unwrap();
        let el = c.electron.unwrap();
        assert_eq!(el.exe, "/usr/bin/electron");
        assert_eq!(el.args, vec!["/app/js"]);
        assert_eq!(
            el.env.get("SAMPLE_ENV").map(|s| s.as_str()),
            Some("/config/value")
        );
    }

    #[test]
    fn parse_electron_config_minimal() {
        let json = r#"{
            "node": {"exe":"n","args":[],"state_dir":"/","socket_path":"/s"},
            "wallet": {"exe":"w","args":[],"state_dir":"/"},
            "electron": {"exe": "/bin/electron"}
        }"#;
        let c: WatchdogConfig = serde_json::from_str(json).unwrap();
        let el = c.electron.unwrap();
        assert_eq!(el.exe, "/bin/electron");
        assert!(el.args.is_empty());
        assert!(el.env.is_empty());
    }

    #[test]
    fn node_crash_restart_delay_defaults_to_5000ms() {
        let c: WatchdogConfig = serde_json::from_str(&minimal_json("")).unwrap();
        assert_eq!(c.node.crash_restart_delay_ms, 5_000);
    }

    #[test]
    fn node_max_crash_attempts_defaults_to_10() {
        let c: WatchdogConfig = serde_json::from_str(&minimal_json("")).unwrap();
        assert_eq!(c.node.max_crash_attempts, 10);
    }

    #[test]
    fn explicit_node_crash_config_overrides_defaults() {
        let json = r#"{
            "node": {"exe":"n","args":[],"state_dir":"/","socket_path":"/s",
                     "crash_restart_delay_ms":100,"max_crash_attempts":3},
            "wallet": {"exe":"w","args":[],"state_dir":"/"}
        }"#;
        let c: WatchdogConfig = serde_json::from_str(json).unwrap();
        assert_eq!(c.node.crash_restart_delay_ms, 100);
        assert_eq!(c.node.max_crash_attempts, 3);
    }

    #[test]
    fn node_stop_timeout_defaults_to_named_constant() {
        let c: WatchdogConfig = serde_json::from_str(&minimal_json("")).unwrap();
        assert_eq!(c.node.stop_timeout_secs, DEFAULT_NODE_STOP_TIMEOUT_SECS);
    }

    #[test]
    fn backend_stop_limit_covers_both_stop_bounds() {
        let json = r#"{
            "node": {"exe":"n","args":[],"state_dir":"/","socket_path":"/s",
                     "stop_timeout_secs":120},
            "wallet": {"exe":"w","args":[],"state_dir":"/","stop_timeout_secs":10}
        }"#;
        let c: WatchdogConfig = serde_json::from_str(json).unwrap();
        assert_eq!(
            c.backend_stop_limit(),
            Duration::from_secs(130) + BACKEND_STOP_MARGIN
        );
    }

    #[test]
    fn startup_silence_defaults_to_five_minutes() {
        let c: WatchdogConfig = serde_json::from_str(&minimal_json("")).unwrap();
        assert_eq!(c.node.startup_silence_secs, DEFAULT_STARTUP_SILENCE_SECS);
        assert_eq!(DEFAULT_STARTUP_SILENCE_SECS, 300);
    }

    #[test]
    fn explicit_startup_silence_overrides_default() {
        let json = r#"{
            "node": {"exe":"n","args":[],"state_dir":"/","socket_path":"/s",
                     "startup_silence_secs":2},
            "wallet": {"exe":"w","args":[],"state_dir":"/"}
        }"#;
        let c: WatchdogConfig = serde_json::from_str(json).unwrap();
        assert_eq!(c.node.startup_silence_secs, 2);
    }

    #[test]
    fn explicit_node_stop_timeout_overrides_default() {
        let json = r#"{
            "node": {"exe":"n","args":[],"state_dir":"/","socket_path":"/s",
                     "stop_timeout_secs":42},
            "wallet": {"exe":"w","args":[],"state_dir":"/"}
        }"#;
        let c: WatchdogConfig = serde_json::from_str(json).unwrap();
        assert_eq!(c.node.stop_timeout_secs, 42);
    }

    #[test]
    fn wallet_stop_timeout_defaults_to_named_constant() {
        let c: WatchdogConfig = serde_json::from_str(&minimal_json("")).unwrap();
        assert_eq!(c.wallet.stop_timeout_secs, DEFAULT_WALLET_STOP_TIMEOUT_SECS);
        assert_eq!(c.wallet.stop_timeout_secs, 60);
    }

    #[test]
    fn backend_stop_limit_defaults_cover_both_default_bounds() {
        let c: WatchdogConfig = serde_json::from_str(&minimal_json("")).unwrap();
        assert_eq!(c.backend_stop_limit(), Duration::from_secs(150));
    }

    #[test]
    fn wallet_health_is_off_when_the_section_is_absent() {
        let c: WatchdogConfig = serde_json::from_str(&minimal_json("")).unwrap();
        assert!(c.wallet.health.is_none());
    }

    #[test]
    fn wallet_health_empty_section_turns_the_check_on_with_defaults() {
        let c: WatchdogConfig = serde_json::from_str(&minimal_json(r#","health":{}"#)).unwrap();
        let h = c.wallet.health.expect("check is on");
        assert_eq!(h.probe_interval_ms, 15_000);
        assert_eq!(h.probe_timeout_ms, 10_000);
        assert_eq!(h.failure_threshold, 4);
        assert_eq!(h.startup_grace_ms, 300_000);
        assert_eq!(h.max_restarts, 5);
        assert_eq!(h.restart_window_ms, 3_600_000);
        assert_eq!(h.stop_timeout_secs, 15);
    }

    #[test]
    fn wallet_health_fields_override_defaults() {
        let c: WatchdogConfig = serde_json::from_str(&minimal_json(
            r#","health":{"probe_interval_ms":50,"failure_threshold":2,"stop_timeout_secs":1}"#,
        ))
        .unwrap();
        let h = c.wallet.health.unwrap();
        assert_eq!(h.probe_interval_ms, 50);
        assert_eq!(h.failure_threshold, 2);
        assert_eq!(h.stop_timeout_secs, 1);
        assert_eq!(h.max_restarts, 5);
    }

    #[test]
    fn explicit_wallet_stop_timeout_overrides_default() {
        let c: WatchdogConfig =
            serde_json::from_str(&minimal_json(r#","stop_timeout_secs":3"#)).unwrap();
        assert_eq!(c.wallet.stop_timeout_secs, 3);
    }

    #[test]
    fn crash_windows_default_to_5_crashes_in_10_minutes() {
        let c: WatchdogConfig = serde_json::from_str(&minimal_json("")).unwrap();
        assert_eq!(c.node.max_crashes_in_window, 5);
        assert_eq!(c.node.crash_window_secs, 600);
        assert_eq!(c.wallet.max_crashes_in_window, 5);
        assert_eq!(c.wallet.crash_window_secs, 600);
    }

    #[test]
    fn explicit_crash_windows_override_defaults() {
        let json = r#"{
            "node": {"exe":"n","args":[],"state_dir":"/","socket_path":"/s",
                     "max_crashes_in_window":2,"crash_window_secs":30},
            "wallet": {"exe":"w","args":[],"state_dir":"/",
                       "max_crashes_in_window":0,"crash_window_secs":60}
        }"#;
        let c: WatchdogConfig = serde_json::from_str(json).unwrap();
        assert_eq!(c.node.max_crashes_in_window, 2);
        assert_eq!(c.node.crash_window_secs, 30);
        assert_eq!(c.wallet.max_crashes_in_window, 0);
        assert_eq!(c.wallet.crash_window_secs, 60);
    }

    #[test]
    fn restart_delay_defaults_to_1000ms() {
        let c: WatchdogConfig = serde_json::from_str(&minimal_json("")).unwrap();
        assert_eq!(c.wallet.restart_delay_ms, 1000);
    }

    #[test]
    fn explicit_restart_delay_overrides_default() {
        let c: WatchdogConfig =
            serde_json::from_str(&minimal_json(r#","restart_delay_ms":500"#)).unwrap();
        assert_eq!(c.wallet.restart_delay_ms, 500);
    }

    #[test]
    fn max_restart_attempts_defaults_to_10() {
        let c: WatchdogConfig = serde_json::from_str(&minimal_json("")).unwrap();
        assert_eq!(c.wallet.max_restart_attempts, 10);
    }

    #[test]
    fn explicit_max_restart_attempts_overrides_default() {
        let c: WatchdogConfig =
            serde_json::from_str(&minimal_json(r#","max_restart_attempts":5"#)).unwrap();
        assert_eq!(c.wallet.max_restart_attempts, 5);
    }

    #[test]
    fn missing_node_field_fails() {
        let json = r#"{"wallet":{"exe":"w","args":[],"state_dir":"/"}}"#;
        assert!(serde_json::from_str::<WatchdogConfig>(json).is_err());
    }

    #[test]
    fn missing_socket_path_fails() {
        let json = r#"{
            "node":{"exe":"n","args":[],"state_dir":"/"},
            "wallet":{"exe":"w","args":[],"state_dir":"/"}
        }"#;
        assert!(serde_json::from_str::<WatchdogConfig>(json).is_err());
    }

    // --- Property-like tests: assert invariants hold across a range of values ---

    #[test]
    fn api_port_preserved_across_valid_range() {
        for port in [0u16, 1, 80, 443, 1024, 8090, 49152, 65535] {
            let json = format!(
                r#"{{"node":{{"exe":"n","args":[],"state_dir":"/","socket_path":"/s"}},
                    "wallet":{{"exe":"w","args":[],"state_dir":"/","api_port":{port}}}}}"#
            );
            let c: WatchdogConfig = serde_json::from_str(&json)
                .unwrap_or_else(|e| panic!("failed for port {port}: {e}"));
            assert_eq!(c.wallet.api_port, Some(port), "port {port} not preserved");
        }
    }

    #[test]
    fn max_restart_attempts_preserved_across_range() {
        for n in [0u32, 1, 2, 3, 5, 10, 100, u32::MAX] {
            let c: WatchdogConfig =
                serde_json::from_str(&minimal_json(&format!(r#","max_restart_attempts":{n}"#)))
                    .unwrap_or_else(|e| panic!("failed for max_restart_attempts={n}: {e}"));
            assert_eq!(c.wallet.max_restart_attempts, n);
        }
    }

    #[test]
    fn restart_delay_ms_preserved_across_range() {
        for ms in [0u64, 1, 100, 500, 1000, 5000, 60_000, u64::MAX] {
            let c: WatchdogConfig =
                serde_json::from_str(&minimal_json(&format!(r#","restart_delay_ms":{ms}"#)))
                    .unwrap_or_else(|e| panic!("failed for restart_delay_ms={ms}: {e}"));
            assert_eq!(c.wallet.restart_delay_ms, ms);
        }
    }

    #[test]
    fn numeric_field_rejects_string_value() {
        let bad = r#"{"node":{"exe":"n","args":[],"state_dir":"/","socket_path":"/s"},
                      "wallet":{"exe":"w","args":[],"state_dir":"/","api_port":"not-a-number"}}"#;
        assert!(
            serde_json::from_str::<WatchdogConfig>(bad).is_err(),
            "expected parse error for string api_port"
        );
    }

    #[test]
    fn extra_unknown_fields_are_ignored() {
        let json = r#"{"node":{"exe":"n","args":[],"state_dir":"/","socket_path":"/s","unknown_node_field":42},
                       "wallet":{"exe":"w","args":[],"state_dir":"/","api_port":8090,"future_field":"ignored"},
                       "pub_logs_dir":"/logs","top_level_extra":true}"#;
        // serde uses deny_unknown_fields only if explicitly annotated; default is to ignore.
        let result = serde_json::from_str::<WatchdogConfig>(json);
        // Document the actual behaviour: unknown fields are currently accepted.
        assert!(
            result.is_ok(),
            "unexpected parse failure: {:?}",
            result.err()
        );
    }

    // ── read_network_magic and set_network_magic ────────────────────────────

    struct Dir(PathBuf);

    impl Dir {
        fn new(label: &str) -> Self {
            let p = std::env::temp_dir().join(format!(
                "wdg-magic-{label}-{}-{}",
                std::process::id(),
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .subsec_nanos()
            ));
            std::fs::create_dir_all(&p).unwrap();
            Dir(p)
        }

        /// A node configuration naming `genesis` as its Byron genesis, and
        /// node arguments that pass it with `--config`.
        fn node_args(&self, genesis: &str) -> Vec<String> {
            self.node_args_with(serde_json::json!({ "ByronGenesisFile": genesis }))
        }

        /// Node arguments that pass `json` as the node configuration.
        fn node_args_with(&self, json: serde_json::Value) -> Vec<String> {
            let config = self.0.join("config.yaml");
            std::fs::write(&config, json.to_string()).unwrap();
            vec![
                "run".to_string(),
                "--config".to_string(),
                config.to_string_lossy().into_owned(),
                "--database-path".to_string(),
                "/state/chain".to_string(),
            ]
        }

        fn write_genesis(&self, path: &Path, magic: u64) {
            let json = serde_json::json!({
                "protocolConsts": { "k": 2160, "protocolMagic": magic }
            });
            std::fs::write(path, json.to_string()).unwrap();
        }
    }

    impl Drop for Dir {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    #[tokio::test]
    async fn network_magic_comes_from_the_byron_genesis_next_to_the_node_config() {
        let d = Dir::new("relative");
        d.write_genesis(&d.0.join("genesis-byron.json"), 764824073);
        let args = d.node_args("genesis-byron.json");
        assert_eq!(read_network_magic(&args).await.unwrap(), 764824073);
    }

    #[tokio::test]
    async fn network_magic_follows_an_absolute_genesis_path() {
        let d = Dir::new("absolute");
        let genesis = d.0.join("elsewhere.json");
        d.write_genesis(&genesis, 1);
        let args = d.node_args(genesis.to_str().unwrap());
        assert_eq!(read_network_magic(&args).await.unwrap(), 1);
    }

    #[tokio::test]
    async fn network_magic_is_unknown_without_a_node_config() {
        let args = vec!["run".to_string(), "--database-path".to_string()];
        assert!(read_network_magic(&args).await.is_err());
    }

    #[tokio::test]
    async fn network_magic_is_unknown_when_the_genesis_has_none() {
        let d = Dir::new("no-magic");
        std::fs::write(d.0.join("genesis-byron.json"), b"{}").unwrap();
        let args = d.node_args("genesis-byron.json");
        assert!(read_network_magic(&args).await.is_err());
    }

    #[tokio::test]
    async fn network_magic_out_of_range_is_unknown() {
        let d = Dir::new("range");
        d.write_genesis(&d.0.join("genesis-byron.json"), u64::from(u32::MAX) + 1);
        let args = d.node_args("genesis-byron.json");
        assert!(read_network_magic(&args).await.is_err());
    }

    // ── read_node_storage ───────────────────────────────────────────────────

    /// Node arguments for a configuration with `ledger_db` as its LedgerDB
    /// section, or none, and a Byron genesis with k = 2160.
    fn storage_args(d: &Dir, ledger_db: Option<serde_json::Value>) -> Vec<String> {
        d.write_genesis(&d.0.join("genesis-byron.json"), 1);
        let mut json = serde_json::json!({ "ByronGenesisFile": "genesis-byron.json" });
        if let Some(section) = ledger_db {
            json["LedgerDB"] = section;
        }
        d.node_args_with(json)
    }

    #[tokio::test]
    async fn node_storage_reads_the_chunk_size_and_the_lsm_directory() {
        let d = Dir::new("storage-lsm");
        let args = storage_args(
            &d,
            Some(serde_json::json!({ "Backend": "V2LSM", "LSMDatabasePath": "lsm/" })),
        );
        assert_eq!(
            read_node_storage(&args).await.unwrap(),
            NodeStorage {
                chunk_slots: 21600,
                backend: LedgerBackend::Lsm(PathBuf::from("lsm/")),
            }
        );
    }

    #[tokio::test]
    async fn node_storage_puts_the_lsm_database_in_lsm_by_default() {
        let d = Dir::new("storage-lsm-default");
        let args = storage_args(&d, Some(serde_json::json!({ "Backend": "V2LSM" })));
        assert_eq!(
            read_node_storage(&args).await.unwrap().backend,
            LedgerBackend::Lsm(PathBuf::from("lsm"))
        );
    }

    #[tokio::test]
    async fn node_storage_is_in_memory_without_a_backend() {
        let d = Dir::new("storage-default");
        let none = storage_args(&d, None);
        assert_eq!(
            read_node_storage(&none).await.unwrap().backend,
            LedgerBackend::Other("V2InMemory".to_string())
        );
        let empty = storage_args(&d, Some(serde_json::json!({})));
        assert_eq!(
            read_node_storage(&empty).await.unwrap().backend,
            LedgerBackend::Other("V2InMemory".to_string())
        );
    }

    #[tokio::test]
    async fn node_storage_names_another_backend() {
        let d = Dir::new("storage-other");
        let args = storage_args(&d, Some(serde_json::json!({ "Backend": "V2InMemory" })));
        assert_eq!(
            read_node_storage(&args).await.unwrap().backend,
            LedgerBackend::Other("V2InMemory".to_string())
        );
    }

    #[tokio::test]
    async fn node_storage_refuses_an_absolute_lsm_directory() {
        let d = Dir::new("storage-absolute");
        let absolute = std::env::temp_dir().join("lsm");
        let args = storage_args(
            &d,
            Some(serde_json::json!({
                "Backend": "V2LSM",
                "LSMDatabasePath": absolute.to_str().unwrap()
            })),
        );
        assert!(read_node_storage(&args).await.is_err());
    }

    #[tokio::test]
    async fn node_storage_is_unknown_without_k() {
        let d = Dir::new("storage-no-k");
        std::fs::write(
            d.0.join("genesis-byron.json"),
            br#"{"protocolConsts":{"protocolMagic":1}}"#,
        )
        .unwrap();
        let args = d.node_args("genesis-byron.json");
        assert!(read_node_storage(&args).await.is_err());
    }

    #[test]
    fn set_network_magic_reaches_node_mithril_and_electron() {
        let mut c: WatchdogConfig = serde_json::from_str(
            r#"{
                "node": {"exe":"n","args":[],"state_dir":"/","socket_path":"/s"},
                "wallet": {"exe":"w","args":[],"state_dir":"/"},
                "mithril": {"mithril_bin":"m","snapshot_converter_bin":"c",
                            "converter_config":"cfg","aggregator_url":"u",
                            "genesis_vkey":"g","state_dir":"/","chain_path":"/chain"},
                "electron": {"exe": "/bin/electron"}
            }"#,
        )
        .unwrap();
        assert_eq!(c.node.network_magic, None);
        let mithril = c.mithril.as_ref().unwrap();
        assert_eq!(mithril.download_stall_secs, 60);
        assert_eq!(mithril.network_recheck_secs, 20);
        c.set_network_magic(2);
        assert_eq!(c.node.network_magic, Some(2));
        assert_eq!(c.mithril.unwrap().network_magic, Some(2));
        assert_eq!(
            c.electron
                .unwrap()
                .env
                .get(NETWORK_MAGIC_ENV)
                .map(String::as_str),
            Some("2")
        );
    }

    #[test]
    fn args_list_preserved_for_various_lengths() {
        for args in [
            vec![],
            vec!["a"],
            vec!["--port", "8090"],
            vec!["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"],
        ] {
            let args_json = serde_json::to_string(&args).unwrap();
            let json = format!(
                r#"{{"node":{{"exe":"n","args":{args_json},"state_dir":"/","socket_path":"/s"}},
                    "wallet":{{"exe":"w","args":{args_json},"state_dir":"/"}}}}"#
            );
            let c: WatchdogConfig = serde_json::from_str(&json).unwrap();
            assert_eq!(c.node.args, args);
            assert_eq!(c.wallet.args, args);
        }
    }
}
