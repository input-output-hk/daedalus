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

use crate::hash::{Hashes, hash_file};
use crate::installers::{InstallerDir, Platform};
use crate::version_json::VersionJson;
use anyhow::{Context, Result};
use axum::{
    Router,
    extract::{Path as AxumPath, State},
    http::{StatusCode, header},
    response::IntoResponse,
    routing::get,
};
use sha2::{Digest, Sha256};
use std::{collections::HashMap, net::SocketAddr, path::PathBuf, sync::Arc};
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

    // Timestamp: current wall-clock time in milliseconds.
    // Using the exact time (not rounded) ensures each run produces a unique
    // verification file key, avoiding CDN cache collisions between re-runs.
    let timestamp_ms = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as u64;

    // Build softwareUpdate entries for each platform present.
    let mut software_update = serde_json::Map::new();
    let mut target_platforms = Vec::new();
    for inst in &installer_dir.installers {
        let key = inst.platform.newsfeed_key();
        if let (Some(h), Some(url)) = (hashes.get(&inst.platform), urls.get(&inst.platform)) {
            software_update.insert(
                key.to_string(),
                serde_json::json!({
                    "version": version,
                    "hash": h.sha256,
                    "url": url,
                }),
            );
            target_platforms.push(key);
        }
    }
    target_platforms.sort();

    let newsfeed = serde_json::json!({
        "updatedAt": timestamp_ms,
        "items": [{
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
                "platforms": target_platforms,
            },
            "action": {
                "label": { "en-US": "", "ja-JP": "" },
                "url":   { "en-US": "", "ja-JP": "" },
            },
            "date": timestamp_ms,
            "type": "software-update",
            "softwareUpdate": software_update,
        }]
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
        Some(path) => match tokio::fs::read(path).await {
            Ok(bytes) => (StatusCode::OK, bytes).into_response(),
            Err(e) => {
                tracing::error!("reading {:?}: {e}", path);
                StatusCode::INTERNAL_SERVER_ERROR.into_response()
            }
        },
        None => StatusCode::NOT_FOUND.into_response(),
    }
}
