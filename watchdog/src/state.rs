use serde::{Deserialize, Serialize};
use std::path::Path;
use tracing::warn;

use crate::config::WatchdogConfig;

#[derive(Debug, Serialize, Deserialize, Default, Clone)]
pub struct WatchdogState {
    /// Override for --database-path passed to cardano-node (and mithril chain_path).
    /// None means use the value from daedalus-config.json.
    #[serde(default)]
    pub chain_path: Option<String>,
    /// Extra args appended to cardano-node args (e.g. ["+RTS","-c","-RTS"]).
    #[serde(default)]
    pub node_extra_args: Vec<String>,
    /// Extra args appended to Electron's args on spawn (e.g. ["--safe-mode"]).
    #[serde(default)]
    pub electron_flags: Vec<String>,
}

pub async fn exists(state_dir: &str) -> bool {
    let path = Path::new(state_dir).join("watchdog-state.json");
    tokio::fs::metadata(&path).await.is_ok()
}

pub async fn load(state_dir: &str) -> WatchdogState {
    let path = Path::new(state_dir).join("watchdog-state.json");
    match tokio::fs::read_to_string(&path).await {
        Ok(text) => serde_json::from_str(&text).unwrap_or_else(|e| {
            warn!("Failed to parse watchdog-state.json, using defaults: {e}");
            WatchdogState::default()
        }),
        Err(_) => WatchdogState::default(),
    }
}

pub async fn save(state_dir: &str, state: &WatchdogState) -> anyhow::Result<()> {
    let path = Path::new(state_dir).join("watchdog-state.json");
    let text = serde_json::to_string_pretty(state)?;
    tokio::fs::write(path, text).await?;
    Ok(())
}

/// Rebuild `config.node.args` from `base_args`, applying chain_path override
/// and node_extra_args from the watchdog state.
pub fn apply_to_config(config: &mut WatchdogConfig, base_args: &[String], state: &WatchdogState) {
    let mut args = base_args.to_vec();
    if let Some(ref chain_path) = state.chain_path {
        patch_database_path(&mut args, chain_path);
        if let Some(ref mut mithril) = config.mithril {
            mithril.chain_path = chain_path.clone();
        }
    }
    args.extend_from_slice(&state.node_extra_args);
    config.node.args = args;
}

/// Replace the value that follows `--database-path` in `args` with `new_path`.
/// If `--database-path` is absent, appends both flag and value.
fn patch_database_path(args: &mut Vec<String>, new_path: &str) {
    for i in 0..args.len().saturating_sub(1) {
        if args[i] == "--database-path" {
            args[i + 1] = new_path.to_string();
            return;
        }
    }
    args.push("--database-path".to_string());
    args.push(new_path.to_string());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn patch_replaces_existing_database_path() {
        let mut args = vec![
            "--config".to_string(),
            "cfg.json".to_string(),
            "--database-path".to_string(),
            "/old/chain".to_string(),
        ];
        patch_database_path(&mut args, "/new/chain");
        assert_eq!(args[3], "/new/chain");
        assert_eq!(args.len(), 4);
    }

    #[test]
    fn patch_appends_when_absent() {
        let mut args = vec!["--config".to_string(), "cfg.json".to_string()];
        patch_database_path(&mut args, "/my/chain");
        assert_eq!(args.last().unwrap(), "/my/chain");
        assert_eq!(args[args.len() - 2], "--database-path");
    }

    #[test]
    fn patch_ignores_database_path_as_last_arg() {
        // --database-path with no following value — should append (doesn't panic)
        let mut args = vec!["--database-path".to_string()];
        patch_database_path(&mut args, "/x");
        // saturating_sub(1) = 0, so loop doesn't run → appends
        assert!(args.contains(&"--database-path".to_string()));
        assert!(args.contains(&"/x".to_string()));
    }

    #[test]
    fn state_default_is_empty() {
        let s = WatchdogState::default();
        assert!(s.chain_path.is_none());
        assert!(s.node_extra_args.is_empty());
        assert!(s.electron_flags.is_empty());
    }

    #[test]
    fn state_round_trips_through_json() {
        let s = WatchdogState {
            chain_path: Some("/custom/chain".to_string()),
            node_extra_args: vec!["+RTS".to_string(), "-c".to_string(), "-RTS".to_string()],
            electron_flags: vec!["--safe-mode".to_string()],
        };
        let json = serde_json::to_string(&s).unwrap();
        let s2: WatchdogState = serde_json::from_str(&json).unwrap();
        assert_eq!(s2.chain_path, s.chain_path);
        assert_eq!(s2.node_extra_args, s.node_extra_args);
        assert_eq!(s2.electron_flags, s.electron_flags);
    }

    #[test]
    fn state_missing_fields_default_to_empty() {
        let s: WatchdogState = serde_json::from_str("{}").unwrap();
        assert!(s.chain_path.is_none());
        assert!(s.node_extra_args.is_empty());
        assert!(s.electron_flags.is_empty());
    }
}
