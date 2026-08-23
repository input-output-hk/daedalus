//! Local update-mirror server for tester workflows.
//!
//! Serves:
//!   GET /daedalus-latest-version.json   — version JSON with localhost URLs
//!   GET /by-hash/<blake2b-hex>          — installer file keyed by hash
//!   GET /<installer-filename>           — installer files (via ServeDir fallback)
//!   GET /<installer>.asc                — GPG signatures (via ServeDir fallback)
//!   GET /newsfeed/newsfeed_<env>.json   — generated newsfeed with localhost URLs
//!   GET /newsfeed-verification/<env>/<ts>.txt — SHA-256 of the newsfeed JSON
//!
//! The newsfeed and verification files are generated on the fly from the
//! installer set — no external repos required.
//!
//! All URLs printed at startup can be pasted directly into
//! launcher-config.yaml for local testing.

use crate::hash::{hash_file, Hashes};
use crate::installers::{InstallerDir, Platform};
use crate::version_json::VersionJson;
use anyhow::{Context, Result};
use axum::{
    body::Body,
    extract::{Path as AxumPath, State},
    http::{header, StatusCode},
    response::IntoResponse,
    routing::get,
    Router,
};
use sha2::{Digest, Sha256};
use std::{collections::HashMap, net::SocketAddr, path::PathBuf, sync::Arc};
use tokio_util::io::ReaderStream;
use tower_http::services::ServeDir;

struct AppState {
    version_json_bytes: Vec<u8>,
    /// blake2b-cbor hex → absolute path of installer file.
    by_hash: HashMap<String, PathBuf>,
}

pub async fn serve(
    host: &str,
    port: u16,
    installer_dir: InstallerDir,
    release_notes: Option<String>,
) -> Result<()> {
    let base = format!("http://{host}:{port}");

    // ── Hash all installers ───────────────────────────────────────────────────
    println!("Hashing installers…");
    let mut hashes: HashMap<Platform, Hashes> = HashMap::new();
    let mut by_hash: HashMap<String, PathBuf> = HashMap::new();

    for inst in &installer_dir.installers {
        print!("  {} … ", inst.filename);
        let h = hash_file(&inst.path)?;
        println!("blake2b-cbor={:.16}…", h.blake2b_cbor);
        by_hash.insert(h.blake2b_cbor.clone(), inst.path.clone());
        hashes.insert(inst.platform, h);
    }

    // ── Build version JSON with localhost URLs ────────────────────────────────
    let urls: HashMap<Platform, String> = installer_dir
        .installers
        .iter()
        .map(|inst| (inst.platform, format!("{base}/{}", inst.filename)))
        .collect();

    let mut signatures: HashMap<Platform, Option<String>> = HashMap::new();
    for inst in &installer_dir.installers {
        signatures.insert(inst.platform, inst.read_signature()?);
    }

    let vj = VersionJson::build(
        &installer_dir.version,
        &hashes,
        &urls,
        &signatures,
        release_notes,
    );
    let version_json_bytes = serde_json::to_vec_pretty(&vj)?;

    // ── Generate newsfeed and verification in a temp dir ─────────────────────
    let env = installer_dir
        .meta
        .env
        .as_deref()
        .unwrap_or("mainnet")
        .to_string();
    let newsfeed_tmp = generate_local_newsfeed(&installer_dir, &hashes, &urls, &env)?;
    println!("  newsfeed : {base}/newsfeed/newsfeed_{env}.json");
    println!("  nf-verify: {base}/newsfeed-verification/{env}/<timestamp>.txt");

    let state = Arc::new(AppState {
        version_json_bytes,
        by_hash,
    });

    // ── Build router ──────────────────────────────────────────────────────────
    let app = Router::new()
        .route("/daedalus-latest-version.json", get(version_json_handler))
        .route("/by-hash/:hash", get(by_hash_handler))
        .nest_service("/newsfeed", ServeDir::new(newsfeed_tmp.join("newsfeed")))
        .nest_service(
            "/newsfeed-verification",
            ServeDir::new(newsfeed_tmp.join("newsfeed-verification")),
        )
        .fallback_service(ServeDir::new(&installer_dir.dir))
        .with_state(state);

    let addr: SocketAddr = format!("{host}:{port}")
        .parse()
        .map_err(|e| anyhow::anyhow!("invalid address {host}:{port}: {e}"))?;

    let listener = tokio::net::TcpListener::bind(addr).await?;

    // Also try to bind the IPv6 loopback so that `localhost` resolving to ::1
    // works without the user needing to change NEWS_URL to 127.0.0.1.
    let ipv6_listener =
        tokio::net::TcpListener::bind(format!("[::1]:{port}").parse::<SocketAddr>().unwrap())
            .await
            .ok();

    println!();
    println!("=== Local update mirror ===");
    println!("  Version JSON : {base}/daedalus-latest-version.json");
    println!("  Newsfeed     : {base}/newsfeed/newsfeed_{env}.json");
    println!("  NF verify    : {base}/newsfeed-verification/");
    if ipv6_listener.is_some() {
        println!("  Also listening on [::1]:{port} (localhost → ::1 fallback)");
    }
    println!();
    println!("Paste into launcher-config.yaml:");
    println!("  update: \"{base}/daedalus-latest-version.json\"");
    println!();
    println!("Press Ctrl-C to stop.");

    let app2 = app.clone();
    tokio::select! {
        r = axum::serve(listener, app) => r?,
        r = async move {
            match ipv6_listener {
                Some(l) => axum::serve(l, app2).await,
                None => std::future::pending().await,
            }
        } => r?,
    }
    Ok(())
}

/// Newsfeed JSON and verification data, ready to be written to disk or uploaded.
pub struct NewsfeedData {
    pub bytes: Vec<u8>,
    /// Hex-encoded SHA-256 of `bytes` (no trailing newline).
    pub sha256_hex: String,
    /// Millisecond timestamp used as the `updatedAt` field and verification filename.
    pub timestamp_ms: u64,
}

/// Build a newsfeed stub from the current installer set.
///
/// Returns the pretty-printed JSON bytes, its SHA-256 hex, and the rounded
/// timestamp, without touching the filesystem.
pub fn build_newsfeed(
    installer_dir: &InstallerDir,
    hashes: &HashMap<Platform, Hashes>,
    urls: &HashMap<Platform, String>,
) -> Result<NewsfeedData> {
    let version = &installer_dir.version;
    let has_deb = installer_dir
        .installers
        .iter()
        .any(|installer| installer.platform == Platform::LinuxDeb);
    let has_rpm = installer_dir
        .installers
        .iter()
        .any(|installer| installer.platform == Platform::LinuxRpm);
    anyhow::ensure!(
        has_deb == has_rpm,
        "local newsfeed requires both linux-deb and linux-rpm when Linux is present"
    );

    // Timestamp: current wall-clock time in milliseconds.
    // Using the exact time (not rounded) ensures each run produces a unique
    // verification file key, avoiding CDN cache collisions between re-runs.
    let timestamp_ms = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as u64;

    // Linux system packages are intentionally excluded from softwareUpdate.
    // Their URLs remain independently available in the version manifest.
    let mut software_update = serde_json::Map::new();
    let mut update_platforms = Vec::new();
    let mut linux_target = None;
    for inst in &installer_dir.installers {
        if inst.platform.is_linux_package() {
            linux_target = Some(inst.platform.newsfeed_key());
            continue;
        }
        let Some(key) = inst.platform.software_update_key() else {
            continue;
        };
        if let (Some(h), Some(url)) = (hashes.get(&inst.platform), urls.get(&inst.platform)) {
            software_update.insert(
                key.to_string(),
                serde_json::json!({
                    "version": version,
                    "hash": h.sha256,
                    "url": url,
                }),
            );
            update_platforms.push(key);
        }
    }
    update_platforms.sort();
    update_platforms.dedup();

    let mut items = Vec::new();
    if !software_update.is_empty() {
        items.push(serde_json::json!({
            "title": {
                "en-US": format!("Daedalus {version} now available"),
                "ja-JP": format!("Daedalus {version} 現在配信中"),
            },
            "content": {
                "en-US": format!(
                    "Daedalus {version} is now available. \
                     It is recommended that all Daedalus users upgrade to this version."
                ),
                "ja-JP": format!(
                    "Daedalus {version}が利用可能になりました。\
                     すべてのDaedalusユーザーはこのバージョンにアップグレードすることが推奨されます。"
                ),
            },
            "target": {
                "daedalusVersion": format!("<{version}"),
                "platforms": update_platforms,
            },
            "action": {
                "label": { "en-US": "", "ja-JP": "" },
                "url":   { "en-US": "", "ja-JP": "" },
            },
            "date": timestamp_ms,
            "type": "software-update",
            "softwareUpdate": software_update,
        }));
    }

    if let Some(linux_target) = linux_target {
        let release_notes_url =
            format!("https://github.com/input-output-hk/daedalus/releases/tag/{version}");
        let release_notes_url_ja = format!("{release_notes_url}#japanese");
        items.push(serde_json::json!({
            "title": {
                "en-US": format!("Daedalus {version} Linux upgrade available"),
                "ja-JP": format!("Daedalus {version} Linux アップグレード"),
            },
            "content": {
                "en-US": format!(
                    "Daedalus {version} is available for Linux as .deb and .rpm system packages. \
                     Close Daedalus and follow the release instructions to upgrade with your \
                     package manager. Your wallet data remains in place."
                ),
                "ja-JP": format!(
                    "Daedalus {version} は Linux 用 .deb / .rpm システムパッケージとして利用できます。\
                     Daedalus を終了し、リリース手順に従ってパッケージマネージャーで\
                     アップグレードしてください。ウォレットデータはそのまま保持されます。"
                ),
            },
            "target": {
                "daedalusVersion": format!("<{version}"),
                "platforms": [linux_target],
            },
            "action": {
                "label": {
                    "en-US": "Linux upgrade instructions",
                    "ja-JP": "Linux アップグレード手順",
                },
                "url": {
                    "en-US": release_notes_url,
                    "ja-JP": release_notes_url_ja,
                },
            },
            "date": timestamp_ms,
            "type": "announcement",
        }));
    }

    let newsfeed = serde_json::json!({
        "updatedAt": timestamp_ms,
        "items": items,
    });

    let bytes = serde_json::to_vec_pretty(&newsfeed)?;
    let sha256_hex = hex::encode(Sha256::digest(&bytes));
    Ok(NewsfeedData {
        bytes,
        sha256_hex,
        timestamp_ms,
    })
}

/// Write newsfeed data to a temp directory for `drt serve`.
///
/// Directory layout:
/// ```
/// <tmpdir>/
///   newsfeed/
///     newsfeed_<env>.json
///   newsfeed-verification/
///     <env>/
///       <updatedAt>.txt     ← SHA-256 of newsfeed JSON, no trailing newline
/// ```
fn generate_local_newsfeed(
    installer_dir: &InstallerDir,
    hashes: &HashMap<Platform, Hashes>,
    urls: &HashMap<Platform, String>,
    env: &str,
) -> Result<PathBuf> {
    let nf = build_newsfeed(installer_dir, hashes, urls)?;

    let tmp = std::env::temp_dir().join(format!("drt-serve-{}", nf.timestamp_ms));
    let newsfeed_dir = tmp.join("newsfeed");
    let verify_dir = tmp.join("newsfeed-verification").join(env);
    std::fs::create_dir_all(&newsfeed_dir).context("creating tmp newsfeed dir")?;
    std::fs::create_dir_all(&verify_dir).context("creating tmp verification dir")?;

    let newsfeed_path = newsfeed_dir.join(format!("newsfeed_{env}.json"));
    std::fs::write(&newsfeed_path, &nf.bytes)
        .with_context(|| format!("writing {}", newsfeed_path.display()))?;

    let verify_path = verify_dir.join(format!("{}.txt", nf.timestamp_ms));
    // No trailing newline — this is how the real verification files are formatted.
    std::fs::write(&verify_path, nf.sha256_hex.as_bytes())
        .with_context(|| format!("writing {}", verify_path.display()))?;

    Ok(tmp)
}

async fn version_json_handler(State(state): State<Arc<AppState>>) -> impl IntoResponse {
    (
        StatusCode::OK,
        [(header::CONTENT_TYPE, "application/json")],
        state.version_json_bytes.clone(),
    )
}

async fn by_hash_handler(
    State(state): State<Arc<AppState>>,
    AxumPath(hash): AxumPath<String>,
) -> impl IntoResponse {
    match state.by_hash.get(&hash) {
        Some(path) => match tokio::fs::File::open(path).await {
            Ok(file) => {
                let stream = ReaderStream::new(file);
                (StatusCode::OK, Body::from_stream(stream)).into_response()
            }
            Err(e) => {
                tracing::error!("opening {:?}: {e}", path);
                StatusCode::INTERNAL_SERVER_ERROR.into_response()
            }
        },
        None => StatusCode::NOT_FOUND.into_response(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::installers::{Installer, Meta};

    fn filename(platform: Platform) -> &'static str {
        match platform {
            Platform::LinuxDeb => "daedalus-6.0.1-mainnet-x86_64-linux.deb",
            Platform::LinuxRpm => "daedalus-6.0.1-mainnet-x86_64-linux.rpm",
            Platform::DarwinArm => "daedalus-6.0.1-mainnet-aarch64-darwin.pkg",
            Platform::DarwinX86 => "daedalus-6.0.1-mainnet-x86_64-darwin.pkg",
            Platform::Windows => "daedalus-6.0.1-mainnet-x86_64-windows.exe",
        }
    }

    fn fixture(
        platforms: &[Platform],
    ) -> (
        InstallerDir,
        HashMap<Platform, Hashes>,
        HashMap<Platform, String>,
    ) {
        let installers = platforms
            .iter()
            .copied()
            .map(|platform| Installer {
                path: PathBuf::from(filename(platform)),
                filename: filename(platform).to_string(),
                platform,
            })
            .collect();
        let hashes = platforms
            .iter()
            .copied()
            .map(|platform| {
                (
                    platform,
                    Hashes {
                        blake2b_cbor: format!("{}-blake", platform.json_key()),
                        sha256: format!("{}-sha", platform.json_key()),
                    },
                )
            })
            .collect();
        let urls = platforms
            .iter()
            .copied()
            .map(|platform| {
                (
                    platform,
                    format!("https://updates.example/{}", filename(platform)),
                )
            })
            .collect();
        let meta = Meta {
            version: "6.0.1".to_string(),
            gitrev: None,
            nar_hash: None,
            env: Some("mainnet".to_string()),
            eval_url: None,
        };
        (
            InstallerDir {
                dir: PathBuf::from("installers"),
                version: meta.version.clone(),
                meta,
                installers,
            },
            hashes,
            urls,
        )
    }

    fn newsfeed(platforms: &[Platform]) -> (serde_json::Value, HashMap<Platform, String>) {
        let (installer_dir, hashes, urls) = fixture(platforms);
        let data = build_newsfeed(&installer_dir, &hashes, &urls).expect("build local newsfeed");
        let value = serde_json::from_slice(&data.bytes).expect("parse local newsfeed");
        assert_eq!(data.sha256_hex, hex::encode(Sha256::digest(&data.bytes)));
        (value, urls)
    }

    #[test]
    fn linux_pair_has_one_announcement_and_no_software_update() {
        let (newsfeed, urls) = newsfeed(&[Platform::LinuxDeb, Platform::LinuxRpm]);
        let items = newsfeed["items"].as_array().expect("newsfeed items");

        assert_eq!(items.len(), 1);
        assert_eq!(items[0]["type"], "announcement");
        assert_eq!(
            items[0]["target"]["platforms"],
            serde_json::json!(["linux"])
        );
        assert!(items[0].get("softwareUpdate").is_none());

        let bytes = serde_json::to_string(&newsfeed).unwrap();
        assert!(!bytes.contains(&urls[&Platform::LinuxDeb]));
        assert!(!bytes.contains(&urls[&Platform::LinuxRpm]));
    }

    #[test]
    fn mixed_feed_keeps_linux_out_of_software_update() {
        let (newsfeed, urls) = newsfeed(&[
            Platform::LinuxDeb,
            Platform::LinuxRpm,
            Platform::Windows,
            Platform::DarwinX86,
        ]);
        let items = newsfeed["items"].as_array().expect("newsfeed items");
        let update = items
            .iter()
            .find(|item| item["type"] == "software-update")
            .expect("software update");

        assert_eq!(
            update["target"]["platforms"],
            serde_json::json!(["darwin", "win32"])
        );
        assert!(update["softwareUpdate"].get("linux").is_none());
        assert_eq!(
            update["softwareUpdate"]["win32"]["url"],
            urls[&Platform::Windows]
        );
        let update_json = serde_json::to_string(update).unwrap();
        assert!(!update_json.contains(&urls[&Platform::LinuxDeb]));
        assert!(!update_json.contains(&urls[&Platform::LinuxRpm]));

        let linux_announcements = items
            .iter()
            .filter(|item| {
                item["type"] == "announcement"
                    && item["target"]["platforms"] == serde_json::json!(["linux"])
            })
            .count();
        assert_eq!(linux_announcements, 1);
    }

    #[test]
    fn non_linux_local_update_shape_is_unchanged() {
        let (newsfeed, urls) = newsfeed(&[Platform::Windows]);
        let items = newsfeed["items"].as_array().expect("newsfeed items");

        assert_eq!(items.len(), 1);
        assert_eq!(items[0]["type"], "software-update");
        assert_eq!(
            items[0]["target"]["platforms"],
            serde_json::json!(["win32"])
        );
        assert_eq!(
            items[0]["softwareUpdate"]["win32"]["url"],
            urls[&Platform::Windows]
        );
    }
}
