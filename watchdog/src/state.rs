use serde::{Deserialize, Serialize};
use std::path::Path;
use tracing::{info, warn};

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

/// True when `dir` holds a cardano-node database: the node writes
/// `protocolMagicId` into the database root on first open, and every database,
/// including one installed by Mithril, has an `immutable` directory.
async fn holds_node_db(dir: &Path) -> bool {
    tokio::fs::metadata(dir.join("protocolMagicId"))
        .await
        .is_ok_and(|m| m.is_file())
        || tokio::fs::metadata(dir.join("immutable"))
            .await
            .is_ok_and(|m| m.is_dir())
}

/// True when `dir` does not exist or has no entries.
async fn is_empty_or_missing(dir: &Path) -> bool {
    match tokio::fs::read_dir(dir).await {
        Ok(mut entries) => matches!(entries.next_entry().await, Ok(None)),
        Err(e) => e.kind() == std::io::ErrorKind::NotFound,
    }
}

/// Decide which chain path to keep from a `migrate_state` reply.
///
/// The value is the folder chosen in the storage picker of 11.3 or 11.4. Those
/// versions stored it in electron-store but ran cardano-node on
/// `<state_dir>/chain` all the same; only Mithril downloaded into
/// `<folder>/chain`. 11.0 to 11.2 stored no such value: they kept a custom
/// location as a link at `<state_dir>/chain`, which the default path still
/// follows. Either way the previous version's database is at the default path.
///
/// The node keeps the database it ran on: the default is kept whenever it
/// holds a cardano-node database. Only when it holds none is the folder
/// adopted, and then only when the folder holds a node database or is empty
/// (or missing). cardano-node refuses a non-empty directory without a database
/// marker, and a Mithril bootstrap replaces the whole directory it installs
/// into. The decision is logged with both paths, and neither is changed.
pub async fn migrated_chain_path(state_dir: &str, migrated: Option<String>) -> Option<String> {
    let folder = migrated?;
    let default = Path::new(state_dir).join("chain");
    let candidate = Path::new(&folder);
    if holds_node_db(&default).await {
        info!(
            "settings migration: keeping {}, which holds the cardano-node database the node ran on before the upgrade; {folder} is left as it is and is not used",
            default.display()
        );
        return None;
    }
    if holds_node_db(candidate).await || is_empty_or_missing(candidate).await {
        info!(
            "settings migration: {} holds no cardano-node database; using {folder}",
            default.display()
        );
        return Some(folder);
    }
    warn!(
        "settings migration: neither {} nor {folder} holds a cardano-node database, and {folder} is not empty; keeping {}",
        default.display(),
        default.display()
    );
    None
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

    // ── migrated_chain_path ─────────────────────────────────────────────────

    struct Dirs {
        root: std::path::PathBuf,
    }

    impl Dirs {
        fn new(label: &str) -> Self {
            let root = std::env::temp_dir().join(format!(
                "wdg-migrate-{label}-{}-{}",
                std::process::id(),
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .subsec_nanos()
            ));
            std::fs::create_dir_all(root.join("state")).unwrap();
            Dirs { root }
        }
        fn state(&self) -> String {
            self.root.join("state").to_string_lossy().into_owned()
        }
        fn folder(&self) -> std::path::PathBuf {
            self.root.join("chosen")
        }
        fn folder_str(&self) -> Option<String> {
            Some(self.folder().to_string_lossy().into_owned())
        }
        fn default_chain(&self) -> std::path::PathBuf {
            self.root.join("state").join("chain")
        }
    }

    impl Drop for Dirs {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.root);
        }
    }

    fn make_db(dir: &Path) {
        std::fs::create_dir_all(dir.join("immutable")).unwrap();
        std::fs::write(dir.join("protocolMagicId"), b"1").unwrap();
    }

    #[tokio::test]
    async fn no_migrated_path_keeps_the_default() {
        let d = Dirs::new("none");
        assert_eq!(migrated_chain_path(&d.state(), None).await, None);
    }

    #[tokio::test]
    async fn folder_with_a_database_is_adopted() {
        let d = Dirs::new("db");
        make_db(&d.folder());
        assert_eq!(
            migrated_chain_path(&d.state(), d.folder_str()).await,
            d.folder_str()
        );
    }

    #[tokio::test]
    async fn default_database_is_kept_over_a_folder_database() {
        let d = Dirs::new("both");
        make_db(&d.folder());
        make_db(&d.default_chain());
        assert_eq!(migrated_chain_path(&d.state(), d.folder_str()).await, None);
        assert!(d.folder().join("protocolMagicId").exists());
        assert!(d.default_chain().join("protocolMagicId").exists());
    }

    #[tokio::test]
    async fn folder_with_only_a_marker_counts_as_a_database() {
        let d = Dirs::new("marker");
        std::fs::create_dir_all(d.folder()).unwrap();
        std::fs::write(d.folder().join("protocolMagicId"), b"1").unwrap();
        assert_eq!(
            migrated_chain_path(&d.state(), d.folder_str()).await,
            d.folder_str()
        );
    }

    #[tokio::test]
    async fn empty_folder_is_adopted_when_there_is_no_database_yet() {
        let d = Dirs::new("fresh");
        std::fs::create_dir_all(d.folder()).unwrap();
        assert_eq!(
            migrated_chain_path(&d.state(), d.folder_str()).await,
            d.folder_str()
        );
    }

    #[tokio::test]
    async fn missing_folder_is_adopted_when_there_is_no_database_yet() {
        let d = Dirs::new("missing");
        assert_eq!(
            migrated_chain_path(&d.state(), d.folder_str()).await,
            d.folder_str()
        );
    }

    #[tokio::test]
    async fn empty_folder_does_not_strand_the_existing_database() {
        let d = Dirs::new("strand");
        std::fs::create_dir_all(d.folder()).unwrap();
        make_db(&d.default_chain());
        assert_eq!(migrated_chain_path(&d.state(), d.folder_str()).await, None);
    }

    #[tokio::test]
    async fn folder_holding_a_mithril_download_in_a_subdirectory_is_not_adopted() {
        // 11.3 and 11.4 installed Mithril snapshots into <folder>/chain.
        let d = Dirs::new("subdir");
        make_db(&d.folder().join("chain"));
        assert_eq!(migrated_chain_path(&d.state(), d.folder_str()).await, None);
    }

    #[tokio::test]
    async fn folder_with_unrelated_files_is_not_adopted() {
        let d = Dirs::new("unrelated");
        std::fs::create_dir_all(d.folder()).unwrap();
        std::fs::write(d.folder().join("notes.txt"), b"user data").unwrap();
        assert_eq!(migrated_chain_path(&d.state(), d.folder_str()).await, None);
    }
}
