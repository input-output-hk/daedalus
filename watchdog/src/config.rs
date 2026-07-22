use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct WatchdogConfig {
    pub node: NodeConfig,
    pub wallet: WalletConfig,
    pub node_log_file: String,
    pub wallet_log_file: String,
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
}

fn default_restart_delay_ms() -> u64 {
    1000
}

#[cfg(test)]
mod tests {
    use super::*;

    fn minimal_json(extra_wallet: &str) -> String {
        format!(
            r#"{{
                "node": {{"exe":"n","args":[],"state_dir":"/","socket_path":"/s"}},
                "wallet": {{"exe":"w","args":[],"state_dir":"/","api_port":8090{}}},
                "node_log_file":"/n.log","wallet_log_file":"/w.log"
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
            "node_log_file":"/logs/node.log","wallet_log_file":"/logs/wallet.log"
        }"#;
        let c: WatchdogConfig = serde_json::from_str(json).unwrap();
        assert_eq!(c.node.exe, "/bin/cardano-node");
        assert_eq!(c.node.args, vec!["--config", "cfg.json"]);
        assert_eq!(c.node.socket_path, "/state/node/node.socket");
        assert_eq!(c.wallet.api_port, 8090);
        assert_eq!(c.wallet.restart_delay_ms, 2000);
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
    fn missing_node_field_fails() {
        let json = r#"{"wallet":{"exe":"w","args":[],"state_dir":"/","api_port":8090},
                       "node_log_file":"/n.log","wallet_log_file":"/w.log"}"#;
        assert!(serde_json::from_str::<WatchdogConfig>(json).is_err());
    }

    #[test]
    fn missing_socket_path_fails() {
        let json = r#"{
            "node":{"exe":"n","args":[],"state_dir":"/"},
            "wallet":{"exe":"w","args":[],"state_dir":"/","api_port":8090},
            "node_log_file":"/n.log","wallet_log_file":"/w.log"
        }"#;
        assert!(serde_json::from_str::<WatchdogConfig>(json).is_err());
    }
}
