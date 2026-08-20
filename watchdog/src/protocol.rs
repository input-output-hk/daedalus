use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize)]
#[serde(tag = "cmd", rename_all = "snake_case")]
pub enum Command {
    Stop,
    /// Start cardano-node from genesis (user chose normal sync after chain_status empty prompt).
    StartNode,
    StartMithril {
        /// Skip the "not needed" immutable-count probe. Set by TypeScript when
        /// the user explicitly requests sync during ledger replay, where the probe
        /// is misleading (files exist but ledger state is at genesis).
        #[serde(default)]
        force: bool,
        /// When true, wipe the chain directory before downloading so the result
        /// is a full bootstrap rather than an incremental partial sync.
        #[serde(default)]
        wipe_chain: bool,
    },
    CancelMithril,
    /// Run the behind-ness probe without starting a download. Emits
    /// `MithrilSignificantlyBehind` or `MithrilNotNeeded` with the result.
    ProbeMithril,
    /// Validate a candidate chain-storage directory. Emits `ChainDirValidation`.
    ValidateChainDir {
        path: String,
        default_chain_path: String,
        required_space_bytes: u64,
    },
    /// Gracefully stop the node (and wallet) then restart both. Does not
    /// increment the crash counter.
    RestartNode,
    /// Kill and restart cardano-wallet without touching cardano-node. Resets
    /// the wallet restart-attempt counter so the limit is not consumed.
    RestartWallet,
}

#[derive(Debug, Serialize)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum Event {
    WatchdogStarted {
        pid: u32,
    },
    NodeStarted {
        pid: u32,
        started_at_unix_ms: u64,
    },
    NodeSocketReady {
        waited_ms: u64,
    },
    NodeShutdownMs {
        ms: u64,
        force_killed: bool,
    },
    WalletStarted {
        pid: u32,
        started_at_unix_ms: u64,
    },
    WalletReady {
        port: u16,
        waited_ms: u64,
    },
    WalletExited {
        code: Option<i32>,
        signal: Option<String>,
        phase: String,
    },
    WalletRestarting {
        attempt: u32,
        last_exit_code: Option<i32>,
        last_exit_signal: Option<String>,
    },
    WalletUnrecoverable {
        attempt: u32,
    },
    NodeExited {
        code: Option<i32>,
        signal: Option<String>,
    },
    /// Block-replay / ledger-validation progress parsed from cardano-node stdout.
    /// `kind` is one of: "replayedBlock", "validatingChunk", "pushingLedger"
    NodeBlockSyncProgress {
        kind: String,
        progress: f64,
    },
    /// ChainDB startup phase parsed from cardano-node stdout.
    /// `phase` is one of: "openingChainDb", "openingImmutableDb", "openedImmutableDb",
    /// "openingVolatileDb", "openedVolatileDb", "openingLedgerDb",
    /// "replayingLedger", "openedLedgerDb", "chainDbReady"
    NodeStartupStatus {
        phase: String,
    },
    Stopped,
    #[allow(dead_code)]
    Error {
        message: String,
    },
    MithrilStatus {
        phase: String,
    },
    MithrilProgress {
        files_downloaded: u64,
        files_total: u64,
        bytes_downloaded: u64,
        bytes_total: u64,
        seconds_elapsed: f64,
        step_num: u32,
        total_steps: u32,
        /// "snapshot" for step 1 (file download), "ledger" for step 2+ (verification).
        phase: String,
    },
    MithrilNotNeeded {
        local_immutable_count: u64,
        latest_certified_immutable: u64,
    },
    /// Emitted in response to `probe_mithril` when the node IS significantly
    /// behind the certified tip (gap ≥ behind_threshold).
    MithrilSignificantlyBehind {
        local_immutable_count: u64,
        latest_certified_immutable: u64,
    },
    MithrilError {
        code: String,
        message: String,
    },
    /// Emitted once on startup. When `has_chain` is false the supervisor waits
    /// for a `start_node` or `start_mithril` command before doing anything.
    ChainStatus {
        has_chain: bool,
    },
    /// Result of a `validate_chain_dir` command.
    ChainDirValidation {
        is_valid: bool,
        /// Path to store in settings. Absent means reset to the managed default.
        #[serde(skip_serializing_if = "Option::is_none")]
        path: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        resolved_path: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        reason: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        available_space_bytes: Option<u64>,
        required_space_bytes: u64,
    },
}

pub fn emit(event: &Event) {
    use std::io::{ErrorKind, Write};
    use std::sync::atomic::{AtomicBool, Ordering};

    // Sticky flag set once the parent has closed our stdout. From then on
    // events are silently dropped. We must NOT process::exit() here: that
    // would skip Drop handlers (kill_on_drop on node/wallet/mithril children)
    // and orphan them. A dead parent also closes our stdin, and the stdin
    // reader in main.rs turns EOF into Command::Stop — the orderly path that
    // stops the wallet and node before the runtime is torn down.
    static STDOUT_GONE: AtomicBool = AtomicBool::new(false);
    if STDOUT_GONE.load(Ordering::Relaxed) {
        return;
    }
    if let Ok(line) = serde_json::to_string(event) {
        let stdout = std::io::stdout();
        let mut lock = stdout.lock();
        let write_err = writeln!(lock, "{line}").and_then(|_| lock.flush()).err();
        if let Some(e) = write_err {
            if e.kind() == ErrorKind::BrokenPipe {
                STDOUT_GONE.store(true, Ordering::Relaxed);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn to_json(ev: &Event) -> serde_json::Value {
        serde_json::to_value(ev).unwrap()
    }

    #[test]
    fn watchdog_started() {
        let j = to_json(&Event::WatchdogStarted { pid: 1234 });
        assert_eq!(j["event"], "watchdog_started");
        assert_eq!(j["pid"], 1234);
    }

    #[test]
    fn node_started() {
        let j = to_json(&Event::NodeStarted {
            pid: 42,
            started_at_unix_ms: 1000,
        });
        assert_eq!(j["event"], "node_started");
        assert_eq!(j["pid"], 42);
        assert_eq!(j["started_at_unix_ms"], 1000);
    }

    #[test]
    fn node_socket_ready() {
        let j = to_json(&Event::NodeSocketReady { waited_ms: 3500 });
        assert_eq!(j["event"], "node_socket_ready");
        assert_eq!(j["waited_ms"], 3500);
    }

    #[test]
    fn node_shutdown_ms() {
        let j = to_json(&Event::NodeShutdownMs {
            ms: 2500,
            force_killed: false,
        });
        assert_eq!(j["event"], "node_shutdown_ms");
        assert_eq!(j["ms"], 2500);
        assert_eq!(j["force_killed"], false);
    }

    #[test]
    fn node_shutdown_ms_force_killed() {
        let j = to_json(&Event::NodeShutdownMs {
            ms: 31000,
            force_killed: true,
        });
        assert_eq!(j["force_killed"], true);
    }

    #[test]
    fn wallet_started() {
        let j = to_json(&Event::WalletStarted {
            pid: 99,
            started_at_unix_ms: 2000,
        });
        assert_eq!(j["event"], "wallet_started");
        assert_eq!(j["pid"], 99);
        assert_eq!(j["started_at_unix_ms"], 2000);
    }

    #[test]
    fn wallet_ready() {
        let j = to_json(&Event::WalletReady {
            port: 8090,
            waited_ms: 1200,
        });
        assert_eq!(j["event"], "wallet_ready");
        assert_eq!(j["port"], 8090);
        assert_eq!(j["waited_ms"], 1200);
    }

    #[test]
    fn wallet_exited_with_code() {
        let j = to_json(&Event::WalletExited {
            code: Some(1),
            signal: None,
            phase: "pre_ready".to_string(),
        });
        assert_eq!(j["event"], "wallet_exited");
        assert_eq!(j["code"], 1);
        assert!(j["signal"].is_null());
        assert_eq!(j["phase"], "pre_ready");
    }

    #[test]
    fn wallet_exited_with_signal() {
        let j = to_json(&Event::WalletExited {
            code: None,
            signal: Some("SIGTERM".into()),
            phase: "post_ready".to_string(),
        });
        assert_eq!(j["event"], "wallet_exited");
        assert!(j["code"].is_null());
        assert_eq!(j["signal"], "SIGTERM");
        assert_eq!(j["phase"], "post_ready");
    }

    #[test]
    fn wallet_restarting() {
        let j = to_json(&Event::WalletRestarting {
            attempt: 3,
            last_exit_code: Some(1),
            last_exit_signal: None,
        });
        assert_eq!(j["event"], "wallet_restarting");
        assert_eq!(j["attempt"], 3);
        assert_eq!(j["last_exit_code"], 1);
        assert!(j["last_exit_signal"].is_null());
    }

    #[test]
    fn wallet_restarting_with_signal() {
        let j = to_json(&Event::WalletRestarting {
            attempt: 2,
            last_exit_code: None,
            last_exit_signal: Some("SIGSEGV".into()),
        });
        assert_eq!(j["last_exit_code"], serde_json::Value::Null);
        assert_eq!(j["last_exit_signal"], "SIGSEGV");
    }

    #[test]
    fn wallet_unrecoverable() {
        let j = to_json(&Event::WalletUnrecoverable { attempt: 10 });
        assert_eq!(j["event"], "wallet_unrecoverable");
        assert_eq!(j["attempt"], 10);
    }

    #[test]
    fn node_exited() {
        let j = to_json(&Event::NodeExited {
            code: Some(0),
            signal: None,
        });
        assert_eq!(j["event"], "node_exited");
        assert_eq!(j["code"], 0);
    }

    #[test]
    fn stopped() {
        let j = to_json(&Event::Stopped);
        assert_eq!(j["event"], "stopped");
    }

    // ── Missing event serialization tests ────────────────────────────────────

    #[test]
    fn node_block_sync_progress() {
        let j = to_json(&Event::NodeBlockSyncProgress {
            kind: "replayedBlock".to_string(),
            progress: 42.5,
        });
        assert_eq!(j["event"], "node_block_sync_progress");
        assert_eq!(j["kind"], "replayedBlock");
        assert!((j["progress"].as_f64().unwrap() - 42.5).abs() < f64::EPSILON);
    }

    #[test]
    fn node_startup_status() {
        let j = to_json(&Event::NodeStartupStatus {
            phase: "openingImmutableDb".to_string(),
        });
        assert_eq!(j["event"], "node_startup_status");
        assert_eq!(j["phase"], "openingImmutableDb");
    }

    #[test]
    fn error_event() {
        let j = to_json(&Event::Error {
            message: "something went wrong".to_string(),
        });
        assert_eq!(j["event"], "error");
        assert_eq!(j["message"], "something went wrong");
    }

    #[test]
    fn mithril_status() {
        for phase in [
            "preparing",
            "downloading",
            "converting",
            "installing",
            "finalizing",
            "completed",
            "cancelled",
        ] {
            let j = to_json(&Event::MithrilStatus {
                phase: phase.to_string(),
            });
            assert_eq!(j["event"], "mithril_status", "phase={phase}");
            assert_eq!(j["phase"], phase, "phase={phase}");
        }
    }

    #[test]
    fn mithril_progress() {
        let j = to_json(&Event::MithrilProgress {
            files_downloaded: 10,
            files_total: 100,
            bytes_downloaded: 1024,
            bytes_total: 10240,
            seconds_elapsed: 3.5,
            step_num: 1,
            total_steps: 4,
            phase: "snapshot".to_string(),
        });
        assert_eq!(j["event"], "mithril_progress");
        assert_eq!(j["files_downloaded"], 10);
        assert_eq!(j["files_total"], 100);
        assert_eq!(j["bytes_downloaded"], 1024);
        assert_eq!(j["bytes_total"], 10240);
        assert!((j["seconds_elapsed"].as_f64().unwrap() - 3.5).abs() < f64::EPSILON);
        assert_eq!(j["step_num"], 1);
        assert_eq!(j["total_steps"], 4);
        assert_eq!(j["phase"], "snapshot");
    }

    #[test]
    fn mithril_progress_ledger_phase() {
        let j = to_json(&Event::MithrilProgress {
            files_downloaded: 0,
            files_total: 0,
            bytes_downloaded: 512,
            bytes_total: 2048,
            seconds_elapsed: 1.0,
            step_num: 2,
            total_steps: 4,
            phase: "ledger".to_string(),
        });
        assert_eq!(j["phase"], "ledger");
        assert_eq!(j["step_num"], 2);
    }

    #[test]
    fn mithril_not_needed() {
        let j = to_json(&Event::MithrilNotNeeded {
            local_immutable_count: 5000,
            latest_certified_immutable: 5010,
        });
        assert_eq!(j["event"], "mithril_not_needed");
        assert_eq!(j["local_immutable_count"], 5000);
        assert_eq!(j["latest_certified_immutable"], 5010);
    }

    #[test]
    fn mithril_significantly_behind() {
        let j = to_json(&Event::MithrilSignificantlyBehind {
            local_immutable_count: 100,
            latest_certified_immutable: 500,
        });
        assert_eq!(j["event"], "mithril_significantly_behind");
        assert_eq!(j["local_immutable_count"], 100);
        assert_eq!(j["latest_certified_immutable"], 500);
    }

    #[test]
    fn mithril_error() {
        let j = to_json(&Event::MithrilError {
            code: "PROBE_FAILED".to_string(),
            message: "connection refused".to_string(),
        });
        assert_eq!(j["event"], "mithril_error");
        assert_eq!(j["code"], "PROBE_FAILED");
        assert_eq!(j["message"], "connection refused");
    }

    #[test]
    fn chain_status_true() {
        let j = to_json(&Event::ChainStatus { has_chain: true });
        assert_eq!(j["event"], "chain_status");
        assert_eq!(j["has_chain"], true);
    }

    #[test]
    fn chain_status_false() {
        let j = to_json(&Event::ChainStatus { has_chain: false });
        assert_eq!(j["has_chain"], false);
    }

    // ── Command deserialization tests ─────────────────────────────────────────

    #[test]
    fn stop_command_deserializes() {
        let cmd: Command = serde_json::from_str(r#"{"cmd":"stop"}"#).unwrap();
        assert!(matches!(cmd, Command::Stop));
    }

    #[test]
    fn start_node_command() {
        let cmd: Command = serde_json::from_str(r#"{"cmd":"start_node"}"#).unwrap();
        assert!(matches!(cmd, Command::StartNode));
    }

    #[test]
    fn start_mithril_defaults() {
        let cmd: Command = serde_json::from_str(r#"{"cmd":"start_mithril"}"#).unwrap();
        match cmd {
            Command::StartMithril { force, wipe_chain } => {
                assert!(!force);
                assert!(!wipe_chain);
            }
            _ => panic!("wrong variant"),
        }
    }

    #[test]
    fn start_mithril_force_and_wipe() {
        let cmd: Command =
            serde_json::from_str(r#"{"cmd":"start_mithril","force":true,"wipe_chain":true}"#)
                .unwrap();
        match cmd {
            Command::StartMithril { force, wipe_chain } => {
                assert!(force);
                assert!(wipe_chain);
            }
            _ => panic!("wrong variant"),
        }
    }

    #[test]
    fn cancel_mithril_command() {
        let cmd: Command = serde_json::from_str(r#"{"cmd":"cancel_mithril"}"#).unwrap();
        assert!(matches!(cmd, Command::CancelMithril));
    }

    #[test]
    fn probe_mithril_command() {
        let cmd: Command = serde_json::from_str(r#"{"cmd":"probe_mithril"}"#).unwrap();
        assert!(matches!(cmd, Command::ProbeMithril));
    }

    #[test]
    fn restart_node_command() {
        let cmd: Command = serde_json::from_str(r#"{"cmd":"restart_node"}"#).unwrap();
        assert!(matches!(cmd, Command::RestartNode));
    }

    #[test]
    fn restart_wallet_command() {
        let cmd: Command = serde_json::from_str(r#"{"cmd":"restart_wallet"}"#).unwrap();
        assert!(matches!(cmd, Command::RestartWallet));
    }

    #[test]
    fn unknown_command_fails() {
        assert!(serde_json::from_str::<Command>(r#"{"cmd":"restart"}"#).is_err());
    }
}
