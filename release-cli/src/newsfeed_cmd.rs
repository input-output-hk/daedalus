//! `drt newsfeed` subcommands: release, publish, message.
//!
//! release — add a software-update + announcement pair for a new Daedalus version.
//! publish — upload the current newsfeed + verification file to an S3 bucket for testing.
//! message — add a standalone announcement item.

use anyhow::{Context, Result};
use reqwest::Client;
use serde::Deserialize;
use serde_json::{Value, json};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::io::{BufRead, Write};
use std::path::Path;

/// Earliest Daedalus version that supports the automatic-update newsfeed.
const MIN_AUTO_UPDATE_VERSION: &str = "2.3.0";

// ── daedalus-latest-version.json types ──────────────────────────────────────

#[derive(Deserialize)]
struct VersionPlatform {
    version: String,
    #[serde(rename = "URL")]
    url: String,
    #[serde(rename = "SHA256")]
    sha256: String,
}

#[derive(Deserialize)]
struct VersionJson {
    platforms: HashMap<String, VersionPlatform>,
}

// ── newsfeed release ─────────────────────────────────────────────────────────

pub async fn cmd_newsfeed_release(
    env: &str,
    newsfeed_repo: &Path,
    verification_repo: &Path,
    installer_json_url: &str,
    release_notes_url: Option<&str>,
) -> Result<()> {
    let newsfeed_path = newsfeed_path(newsfeed_repo, env);
    let mut newsfeed = read_newsfeed(&newsfeed_path)?;

    // Fetch installer JSON
    println!("Fetching {installer_json_url} …");
    let client = Client::builder().user_agent("drt/0.1").build()?;
    let vj: VersionJson = client
        .get(installer_json_url)
        .send()
        .await
        .context("GET installer JSON")?
        .error_for_status()
        .context("installer JSON returned an error status")?
        .json()
        .await
        .context("parsing installer JSON")?;

    let version = vj
        .platforms
        .values()
        .next()
        .map(|p| p.version.clone())
        .ok_or_else(|| anyhow::anyhow!("installer JSON has no platforms"))?;

    {
        let mut keys: Vec<&str> = vj.platforms.keys().map(String::as_str).collect();
        keys.sort();
        println!("Version   : {version}");
        println!("Platforms : {}", keys.join(", "));
    }

    let release_notes_url = release_notes_url.map(str::to_owned).unwrap_or_else(|| {
        format!("https://github.com/input-output-hk/daedalus/releases/tag/{version}")
    });
    let release_notes_url_ja = format!("{release_notes_url}#japanese");
    println!("Notes     : {release_notes_url}");

    let (now_ms, updated_at) = timestamps();

    // Build softwareUpdate map and platform list (newsfeed keys).
    // The version JSON uses "windows"; the newsfeed uses "win32".
    let mut sw_map = serde_json::Map::new();
    let mut nf_platforms: Vec<String> = Vec::new();

    let mut entries: Vec<(&String, &VersionPlatform)> = vj.platforms.iter().collect();
    entries.sort_by_key(|(k, _)| k.as_str());

    for (vj_key, plat) in &entries {
        let nf_key = if vj_key.as_str() == "windows" {
            "win32"
        } else {
            vj_key.as_str()
        };
        nf_platforms.push(nf_key.to_string());
        sw_map.insert(
            nf_key.to_string(),
            json!({ "version": &plat.version, "hash": &plat.sha256, "url": &plat.url }),
        );
    }
    nf_platforms.sort();

    let update_target = format!(">={MIN_AUTO_UPDATE_VERSION} <{version}");

    // Give the two items distinct dates: software-update 30 min before
    // updatedAt, announcement at updatedAt.
    const THIRTY_MIN_MS: u64 = 30 * 60 * 1000;
    let date_update = updated_at - THIRTY_MIN_MS;
    let date_announce = updated_at;

    let update_item = json!({
        "title": {
            "en-US": format!("NEW Daedalus {version} update"),
            "ja-JP": format!("Daedalus {version} の新バージョンがリリースされました"),
        },
        "content": {
            "en-US": format!(
                "Daedalus {version} is now available.\n\n\
                 It is recommended that all Daedalus users upgrade to this version.\n\n\
                 Please read the release notes for more information."
            ),
            "ja-JP": format!(
                "Daedalus {version} が利用可能になりました。\n\n\
                 すべてのユーザーに、このバージョンへのアップグレードを推奨します。\n\n\
                 詳細についてはリリースノートをご確認ください。"
            ),
        },
        "target": { "daedalusVersion": &update_target, "platforms": &nf_platforms },
        "action": {
            "label": { "en-US": "", "ja-JP": "" },
            "url":   { "en-US": "", "ja-JP": "" },
        },
        "date": date_update,
        "type": "software-update",
        "softwareUpdate": Value::Object(sw_map),
    });

    let announce_item = json!({
        "title": {
            "en-US": format!("Daedalus {version} - Release notes"),
            "ja-JP": format!("Daedalus {version} リリースノート"),
        },
        "content": {
            "en-US": format!(
                "Daedalus {version} is now available.\n\n\
                 It is recommended that all Daedalus users upgrade to this version."
            ),
            "ja-JP": format!(
                "Daedalus {version} が利用可能になりました。\n\n\
                 すべてのユーザーに、このバージョンへのアップグレードを推奨します。"
            ),
        },
        "target": { "daedalusVersion": &version, "platforms": &nf_platforms },
        "action": {
            "label": { "en-US": "Release notes", "ja-JP": "リリースノート" },
            "url":   { "en-US": release_notes_url, "ja-JP": release_notes_url_ja },
        },
        "date": date_announce,
        "type": "announcement",
    });

    println!();
    println!("  • Fill in the ja-JP content fields (marked TODO)");
    println!("  • The softwareUpdate hashes and URLs are pre-filled from the installer JSON");

    let new_items = open_editor_draft(&json!([update_item, announce_item]), now_ms)?;

    apply_and_write(
        &mut newsfeed,
        new_items,
        updated_at,
        &newsfeed_path,
        &verify_dir(verification_repo, env),
    )
}

// ── newsfeed publish ─────────────────────────────────────────────────────────

pub async fn cmd_newsfeed_publish(
    env: &str,
    newsfeed_repo: &Path,
    verification_repo: &Path,
    bucket: &str,
    bucket_url: &str,
    dry_run: bool,
) -> Result<()> {
    let newsfeed_path = newsfeed_path(newsfeed_repo, env);
    let newsfeed_bytes = std::fs::read(&newsfeed_path)
        .with_context(|| format!("reading {}", newsfeed_path.display()))?;

    // Parse updatedAt to locate the verification file.
    let newsfeed: Value = serde_json::from_slice(&newsfeed_bytes)
        .with_context(|| format!("parsing {}", newsfeed_path.display()))?;
    let updated_at = newsfeed
        .get("updatedAt")
        .and_then(Value::as_u64)
        .ok_or_else(|| anyhow::anyhow!("newsfeed JSON has no 'updatedAt' field"))?;

    let verify_path = verify_dir(verification_repo, env).join(format!("{updated_at}.txt"));
    let verify_bytes = std::fs::read(&verify_path)
        .with_context(|| format!("reading {}", verify_path.display()))?;

    let newsfeed_key = format!("newsfeed/newsfeed_{env}.json");
    let verify_key = format!("newsfeed-verification/{env}/{updated_at}.txt");

    println!("Newsfeed  : {}", newsfeed_path.display());
    println!("Verify    : {}", verify_path.display());
    println!("updatedAt : {updated_at}");
    println!();

    if dry_run {
        println!("Dry run — would upload:");
        println!("  {newsfeed_key}  →  s3://{bucket}");
        println!("  {verify_key}  →  s3://{bucket}");
        return Ok(());
    }

    let s3 = crate::s3::S3Client::new(bucket.to_string(), bucket_url.to_string()).await?;

    // no-store: newsfeed changes on every update; avoid stale hashes being served.
    s3.upload_bytes(
        &newsfeed_key,
        &newsfeed_bytes,
        "application/json",
        Some("no-store"),
    )
    .await?;
    s3.upload_bytes(&verify_key, &verify_bytes, "text/plain", Some("no-store"))
        .await?;

    println!();
    println!("Done.");
    println!("  Newsfeed : {}", s3.cdn_url(&newsfeed_key));
    println!("  Verify   : {}", s3.cdn_url(&verify_key));

    Ok(())
}

// ── newsfeed message ─────────────────────────────────────────────────────────

pub async fn cmd_newsfeed_message(
    env: &str,
    newsfeed_repo: &Path,
    verification_repo: &Path,
    min_version: &str,
) -> Result<()> {
    let newsfeed_path = newsfeed_path(newsfeed_repo, env);
    let mut newsfeed = read_newsfeed(&newsfeed_path)?;

    let (now_ms, updated_at) = timestamps();
    let target = format!(">={min_version}");
    let all_platforms = ["darwin", "darwin-arm", "linux", "win32"];

    let draft_item = json!({
        "title": {
            "en-US": "TODO: title",
            "ja-JP": "TODO: タイトル",
        },
        "content": {
            "en-US": "TODO: content",
            "ja-JP": "TODO: 内容",
        },
        "target": { "daedalusVersion": &target, "platforms": &all_platforms },
        "action": {
            "label": { "en-US": "", "ja-JP": "" },
            "url":   { "en-US": "", "ja-JP": "" },
        },
        "date": updated_at,
        "type": "announcement",
    });

    println!("  • Fill in title, content, and action fields");
    println!("  • Target is pre-set to \"{target}\" — adjust if needed");

    let new_items = open_editor_draft(&json!([draft_item]), now_ms)?;

    apply_and_write(
        &mut newsfeed,
        new_items,
        updated_at,
        &newsfeed_path,
        &verify_dir(verification_repo, env),
    )
}

// ── Shared helpers ────────────────────────────────────────────────────────────

fn newsfeed_path(repo: &Path, env: &str) -> std::path::PathBuf {
    repo.join("newsfeed").join(format!("newsfeed_{env}.json"))
}

fn verify_dir(repo: &Path, env: &str) -> std::path::PathBuf {
    repo.join("newsfeed-verification").join(env)
}

fn read_newsfeed(path: &Path) -> Result<Value> {
    let text =
        std::fs::read_to_string(path).with_context(|| format!("reading {}", path.display()))?;
    serde_json::from_str(&text).with_context(|| format!("parsing {}", path.display()))
}

/// Returns (now_ms, updated_at) where updated_at is rounded down to 30 minutes.
fn timestamps() -> (u64, u64) {
    let now_ms = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as u64;
    const THIRTY_MIN_MS: u64 = 30 * 60 * 1000;
    let updated_at = (now_ms / THIRTY_MIN_MS) * THIRTY_MIN_MS;
    (now_ms, updated_at)
}

/// Write `draft` to a temp file, open `$EDITOR`, read back the edited JSON array.
fn open_editor_draft(draft: &Value, now_ms: u64) -> Result<Vec<Value>> {
    let tmp = std::env::temp_dir().join(format!("drt-newsfeed-{now_ms}.json"));
    std::fs::write(&tmp, serde_json::to_string_pretty(draft)?)
        .with_context(|| format!("writing draft to {}", tmp.display()))?;

    let editor_str = std::env::var("EDITOR")
        .or_else(|_| std::env::var("VISUAL"))
        .unwrap_or_else(|_| "vi".to_string());
    let mut parts = editor_str.split_whitespace();
    let bin = parts.next().unwrap_or("vi");
    let args: Vec<&str> = parts.collect();

    println!("Draft: {}", tmp.display());
    println!("Opening in {editor_str} …");
    println!("Save and close to continue, or quit without saving to abort.");

    let status = std::process::Command::new(bin)
        .args(&args)
        .arg(&tmp)
        .status()
        .with_context(|| format!("launching '{editor_str}'"))?;

    if !status.success() {
        anyhow::bail!("editor exited with {status}");
    }

    let edited =
        std::fs::read_to_string(&tmp).with_context(|| format!("reading {}", tmp.display()))?;
    let _ = std::fs::remove_file(&tmp);

    let items: Vec<Value> = serde_json::from_str(&edited)
        .context("edited draft is not valid JSON — must be a JSON array of item objects")?;

    anyhow::ensure!(!items.is_empty(), "draft array is empty — nothing to add");
    Ok(items)
}

/// Insert `new_items` into the newsfeed, update `updatedAt`, write both output
/// files, and confirm with the user before writing.
fn apply_and_write(
    newsfeed: &mut Value,
    new_items: Vec<Value>,
    updated_at: u64,
    newsfeed_path: &Path,
    verify_dir: &Path,
) -> Result<()> {
    println!();
    println!("{} new item(s) will be added.", new_items.len());

    // Insert after any leading "incident" items (those stay at position 0 by convention).
    let items = newsfeed
        .get_mut("items")
        .and_then(Value::as_array_mut)
        .ok_or_else(|| anyhow::anyhow!("newsfeed JSON has no 'items' array"))?;

    let insert_at = items
        .iter()
        .position(|item| {
            item.get("type")
                .and_then(Value::as_str)
                .map(|t| t != "incident")
                .unwrap_or(true)
        })
        .unwrap_or(0);

    for (i, item) in new_items.into_iter().enumerate() {
        items.insert(insert_at + i, item);
    }

    newsfeed["updatedAt"] = json!(updated_at);

    let mut bytes = serde_json::to_vec_pretty(newsfeed)?;
    if bytes.last() != Some(&b'\n') {
        bytes.push(b'\n');
    }

    let sha256_hex = hex::encode(Sha256::digest(&bytes));
    let verify_path = verify_dir.join(format!("{updated_at}.txt"));

    println!();
    println!("Ready to write:");
    println!("  newsfeed  : {}", newsfeed_path.display());
    println!("  updatedAt : {updated_at}");
    println!("  verify    : {}", verify_path.display());
    println!("  sha256    : {sha256_hex}");
    println!();

    if !confirm("Proceed?")? {
        println!("Aborted.");
        return Ok(());
    }

    std::fs::write(newsfeed_path, &bytes)
        .with_context(|| format!("writing {}", newsfeed_path.display()))?;

    std::fs::create_dir_all(verify_dir)
        .with_context(|| format!("creating {}", verify_dir.display()))?;

    // No trailing newline — matches the existing verification file format.
    std::fs::write(&verify_path, sha256_hex.as_bytes())
        .with_context(|| format!("writing {}", verify_path.display()))?;

    println!();
    println!("Done.");
    println!("  Newsfeed : {}", newsfeed_path.display());
    println!("  Verify   : {}", verify_path.display());

    Ok(())
}

fn confirm(prompt: &str) -> Result<bool> {
    print!("{prompt} [y/N] ");
    std::io::stdout().flush().context("flushing stdout")?;
    let mut answer = String::new();
    std::io::stdin()
        .lock()
        .read_line(&mut answer)
        .context("reading stdin")?;
    Ok(matches!(answer.trim().to_lowercase().as_str(), "y" | "yes"))
}
