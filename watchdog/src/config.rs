use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct WatchdogConfig {
    pub node: NodeConfig,
    pub wallet: WalletConfig,
    pub pub_logs_dir: String,
    pub mithril: Option<MithrilConfig>,
}

#[derive(Debug, Deserialize)]
pub struct NodeConfig {
    pub exe: String,
    /// Args for cardano-node, NOT including --shutdown-ipc (watchdog adds that).
    pub args: Vec<String>,
    pub state_dir: String,
    /// Absolute path to the node socket file; watchdog waits for this before starting wallet.
    pub socket_path: String,
}

#[derive(Debug, Deserialize, Clone)]
pub struct WalletConfig {
    pub exe: String,
    pub args: Vec<String>,
    pub state_dir: String,
    pub api_port: u16,
    #[serde(default = "default_restart_delay_ms")]
    pub restart_delay_ms: u64,
    #[serde(default = "default_max_restart_attempts")]
    pub max_restart_attempts: u32,
}

fn default_restart_delay_ms() -> u64 {
    1000
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
}

fn default_behind_threshold() -> u64 {
    20
}

#[cfg(test)]
mod tests {
    use super::*;

    fn minimal_json(extra_wallet: &str) -> String {
        format!(
            r#"{{
                "node": {{"exe":"n","args":[],"state_dir":"/","socket_path":"/s"}},
                "wallet": {{"exe":"w","args":[],"state_dir":"/","api_port":8090{}}},
                "pub_logs_dir":"/logs"
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
        assert_eq!(c.wallet.api_port, 8090);
        assert_eq!(c.wallet.restart_delay_ms, 2000);
        assert_eq!(c.pub_logs_dir, "/logs");
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
        let json = r#"{"wallet":{"exe":"w","args":[],"state_dir":"/","api_port":8090},
                       "pub_logs_dir":"/logs"}"#;
        assert!(serde_json::from_str::<WatchdogConfig>(json).is_err());
    }

    #[test]
    fn missing_socket_path_fails() {
        let json = r#"{
            "node":{"exe":"n","args":[],"state_dir":"/"},
            "wallet":{"exe":"w","args":[],"state_dir":"/","api_port":8090},
            "pub_logs_dir":"/logs"
        }"#;
        assert!(serde_json::from_str::<WatchdogConfig>(json).is_err());
    }

    // --- Property-like tests: assert invariants hold across a range of values ---

    #[test]
    fn api_port_preserved_across_valid_range() {
        for port in [0u16, 1, 80, 443, 1024, 8090, 49152, 65535] {
            let json = format!(
                r#"{{"node":{{"exe":"n","args":[],"state_dir":"/","socket_path":"/s"}},
                    "wallet":{{"exe":"w","args":[],"state_dir":"/","api_port":{port}}},
                    "pub_logs_dir":"/logs"}}"#
            );
            let c: WatchdogConfig = serde_json::from_str(&json)
                .unwrap_or_else(|e| panic!("failed for port {port}: {e}"));
            assert_eq!(c.wallet.api_port, port, "port {port} not preserved");
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
                      "wallet":{"exe":"w","args":[],"state_dir":"/","api_port":"not-a-number"},
                      "pub_logs_dir":"/logs"}"#;
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
                    "wallet":{{"exe":"w","args":{args_json},"state_dir":"/","api_port":8090}},
                    "pub_logs_dir":"/logs"}}"#
            );
            let c: WatchdogConfig = serde_json::from_str(&json).unwrap();
            assert_eq!(c.node.args, args);
            assert_eq!(c.wallet.args, args);
        }
    }
}
