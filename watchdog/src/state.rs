use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use tracing::{info, warn};

use crate::config::WatchdogConfig;

#[derive(Debug, Serialize, Deserialize, Default, Clone)]
pub struct WatchdogState {
    /// Storage folder chosen in the chain-storage picker. The database lives
    /// in `<chain_path>/chain` (see `database_dir`), for cardano-node and for
    /// Mithril alike. None means the location from daedalus-config.json.
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
pub async fn holds_node_db(dir: &Path) -> bool {
    tokio::fs::metadata(dir.join("protocolMagicId"))
        .await
        .is_ok_and(|m| m.is_file())
        || tokio::fs::metadata(dir.join("immutable"))
            .await
            .is_ok_and(|m| m.is_dir())
}

/// Name of the cardano-node database directory, inside the state directory by
/// default and inside the storage folder when the user picked one. The picker
/// shows the user `<folder>/chain`, and 11.3 and 11.4 downloaded Mithril
/// snapshots there.
const DATABASE_DIR: &str = "chain";

/// The cardano-node database directory for a storage folder, or for the
/// default location when there is none.
///
/// This is the only place a picked folder becomes a database path. The node's
/// `--database-path`, Mithril's install target, the has-chain check and the
/// settings migration all go through it, so they cannot disagree. Using the
/// folder itself would put the database among the user's own files, and a
/// Mithril bootstrap replaces the directory it installs into.
pub fn database_dir(state_dir: &str, storage_folder: Option<&str>) -> PathBuf {
    Path::new(storage_folder.unwrap_or(state_dir)).join(DATABASE_DIR)
}

/// Decide which storage folder to keep from a `migrate_state` reply.
///
/// The value is the folder chosen in the storage picker of 11.3 or 11.4. Those
/// versions ran cardano-node on `<state_dir>/chain` whatever the folder, and
/// installed Mithril snapshots into `<folder>/chain`, so a user who picked a
/// folder can have a database in both places. 11.0 to 11.2 stored no such
/// value: they kept a custom location as a link at `<state_dir>/chain`, which
/// the default path follows.
///
/// The node keeps the database it ran on: the default is kept whenever it
/// holds a cardano-node database. The folder is adopted only when the default
/// holds none and `<folder>/chain` holds one. The decision is logged with both
/// paths, and neither directory is changed.
pub async fn migrated_chain_path(state_dir: &str, migrated: Option<String>) -> Option<String> {
    let folder = migrated?;
    let default = database_dir(state_dir, None);
    let candidate = database_dir(state_dir, Some(&folder));
    match (
        holds_node_db(&default).await,
        holds_node_db(&candidate).await,
    ) {
        (true, true) => {
            warn!(
                "settings migration: {} and {} both hold a cardano-node database; keeping {}, \
                 the database the node ran on before the upgrade. {} is left as it is and is not used",
                default.display(),
                candidate.display(),
                default.display(),
                candidate.display()
            );
            None
        }
        (true, false) => {
            info!(
                "settings migration: keeping {}, which holds a cardano-node database; \
                 {} holds none, so storage folder {folder} is not used",
                default.display(),
                candidate.display()
            );
            None
        }
        (false, true) => {
            info!(
                "settings migration: {} holds no cardano-node database; \
                 adopting storage folder {folder}, whose {} holds one",
                default.display(),
                candidate.display()
            );
            Some(folder)
        }
        (false, false) => {
            warn!(
                "settings migration: neither {} nor {} holds a cardano-node database; keeping {}",
                default.display(),
                candidate.display(),
                default.display()
            );
            None
        }
    }
}

/// Rebuild `config.node.args` from `base_args`, applying the storage folder
/// and node_extra_args from the watchdog state, and point Mithril at the same
/// database. Without a folder both use their configured location, so clearing
/// the folder moves them back together.
pub fn apply_to_config(config: &mut WatchdogConfig, base_args: &[String], state: &WatchdogState) {
    let mut args = base_args.to_vec();
    let db = state.chain_path.as_deref().map(|folder| {
        database_dir(&config.node.state_dir, Some(folder))
            .to_string_lossy()
            .into_owned()
    });
    if let Some(ref db) = db {
        patch_database_path(&mut args, db);
    }
    if let Some(ref mut mithril) = config.mithril {
        let configured = mithril
            .configured_chain_path
            .get_or_insert_with(|| mithril.chain_path.clone());
        mithril.chain_path = db.unwrap_or_else(|| configured.clone());
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

    // ── database_dir and apply_to_config ────────────────────────────────────

    #[test]
    fn database_dir_is_the_chain_subdirectory_of_a_picked_folder() {
        assert_eq!(
            database_dir("/state", Some("/mnt/cardano")),
            Path::new("/mnt/cardano").join("chain")
        );
    }

    #[test]
    fn database_dir_defaults_to_the_state_directory() {
        assert_eq!(
            database_dir("/state", None),
            Path::new("/state").join("chain")
        );
    }

    fn config_with_mithril() -> WatchdogConfig {
        serde_json::from_str(
            r#"{
                "node": {"exe":"n","args":["run","--database-path","/state/chain"],
                         "state_dir":"/state","socket_path":"/state/s"},
                "wallet": {"exe":"w","args":[],"state_dir":"/state"},
                "mithril": {"mithril_bin":"m","snapshot_converter_bin":"c",
                            "converter_config":"cfg","aggregator_url":"u",
                            "genesis_vkey":"g","state_dir":"/state",
                            "chain_path":"/state/chain"}
            }"#,
        )
        .unwrap()
    }

    #[test]
    fn picked_folder_gives_node_and_mithril_the_same_database() {
        let mut config = config_with_mithril();
        let base = config.node.args.clone();
        let state = WatchdogState {
            chain_path: Some("/mnt/cardano".to_string()),
            ..WatchdogState::default()
        };
        apply_to_config(&mut config, &base, &state);
        let db = Path::new("/mnt/cardano")
            .join("chain")
            .to_string_lossy()
            .into_owned();
        assert_eq!(
            config.node.args,
            vec!["run", "--database-path", db.as_str()]
        );
        assert_eq!(config.mithril.unwrap().chain_path, db);
    }

    #[test]
    fn no_picked_folder_keeps_the_configured_database() {
        let mut config = config_with_mithril();
        let base = config.node.args.clone();
        apply_to_config(&mut config, &base, &WatchdogState::default());
        assert_eq!(config.node.args, base);
        assert_eq!(config.mithril.unwrap().chain_path, "/state/chain");
    }

    #[test]
    fn reset_after_a_picked_folder_restores_the_configured_database() {
        let mut config = config_with_mithril();
        let base = config.node.args.clone();
        let picked = WatchdogState {
            chain_path: Some("/mnt/cardano".to_string()),
            ..WatchdogState::default()
        };
        apply_to_config(&mut config, &base, &picked);
        apply_to_config(&mut config, &base, &WatchdogState::default());
        assert_eq!(config.node.args, base);
        assert_eq!(config.mithril.unwrap().chain_path, "/state/chain");
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
    async fn no_migrated_folder_keeps_the_default() {
        let d = Dirs::new("none");
        assert_eq!(migrated_chain_path(&d.state(), None).await, None);
    }

    #[tokio::test]
    async fn folder_with_a_database_in_its_chain_subdirectory_is_adopted() {
        // Where 11.3 and 11.4 installed Mithril snapshots.
        let d = Dirs::new("chain-db");
        make_db(&d.folder().join("chain"));
        assert_eq!(
            migrated_chain_path(&d.state(), d.folder_str()).await,
            d.folder_str()
        );
    }

    #[tokio::test]
    async fn default_database_is_kept_over_a_database_in_the_folder() {
        // 11.3 and 11.4 ran the node on the default whatever the folder.
        let d = Dirs::new("both");
        make_db(&d.folder().join("chain"));
        make_db(&d.default_chain());
        assert_eq!(migrated_chain_path(&d.state(), d.folder_str()).await, None);
        assert!(d.folder().join("chain").join("protocolMagicId").exists());
        assert!(d.default_chain().join("protocolMagicId").exists());
    }

    #[tokio::test]
    async fn default_database_is_kept_when_the_folder_holds_none() {
        let d = Dirs::new("default-only");
        std::fs::create_dir_all(d.folder()).unwrap();
        make_db(&d.default_chain());
        assert_eq!(migrated_chain_path(&d.state(), d.folder_str()).await, None);
    }

    #[tokio::test]
    async fn chain_subdirectory_with_only_a_marker_counts_as_a_database() {
        let d = Dirs::new("marker");
        std::fs::create_dir_all(d.folder().join("chain")).unwrap();
        std::fs::write(d.folder().join("chain").join("protocolMagicId"), b"1").unwrap();
        assert_eq!(
            migrated_chain_path(&d.state(), d.folder_str()).await,
            d.folder_str()
        );
    }

    #[tokio::test]
    async fn database_in_the_folder_itself_is_not_adopted() {
        let d = Dirs::new("flat");
        make_db(&d.folder());
        assert_eq!(migrated_chain_path(&d.state(), d.folder_str()).await, None);
    }

    #[tokio::test]
    async fn empty_folder_is_not_adopted() {
        let d = Dirs::new("empty");
        std::fs::create_dir_all(d.folder()).unwrap();
        assert_eq!(migrated_chain_path(&d.state(), d.folder_str()).await, None);
    }

    #[tokio::test]
    async fn missing_folder_is_not_adopted() {
        let d = Dirs::new("missing");
        assert_eq!(migrated_chain_path(&d.state(), d.folder_str()).await, None);
    }

    #[tokio::test]
    async fn folder_with_unrelated_files_is_not_adopted() {
        let d = Dirs::new("unrelated");
        std::fs::create_dir_all(d.folder().join("chain")).unwrap();
        std::fs::write(d.folder().join("chain").join("notes.txt"), b"user data").unwrap();
        assert_eq!(migrated_chain_path(&d.state(), d.folder_str()).await, None);
    }
}
