//! `drt newsfeed` subcommands: release, publish, message.
//!
//! release — add app-managed software updates and ordinary release announcements.
//! publish — upload the current newsfeed + verification file to an S3 bucket for testing.
//! message — add a standalone announcement item.

use anyhow::{Context, Result};
use reqwest::Client;
use serde::Deserialize;
use serde_json::{json, Value};
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

    let version = validate_version_json(&vj)?;

    {
        let mut keys: Vec<&str> = vj.platforms.keys().map(String::as_str).collect();
        keys.sort();
        println!("Version   : {version}");
        println!("Platforms : {}", keys.join(", "));
    }

    let release_notes_url = release_notes_url.map(str::to_owned).unwrap_or_else(|| {
        format!("https://github.com/input-output-hk/daedalus/releases/tag/{version}")
    });
    println!("Notes     : {release_notes_url}");

    let (now_ms, updated_at) = timestamps();
    let draft = build_release_draft(&vj, &version, &release_notes_url, updated_at)?;

    println!();
    println!("  • Fill in the ja-JP content fields as needed");
    println!("  • macOS/Windows softwareUpdate hashes and URLs are pre-filled");
    if vj.platforms.contains_key("linux-deb") {
        println!("  • Linux receives one ordinary package-manager upgrade announcement");
    }

    let new_items = open_editor_draft(&Value::Array(draft.clone()), now_ms)?;
    validate_release_draft(&new_items, &draft)?;

    apply_and_write(
        &mut newsfeed,
        new_items,
        updated_at,
        &newsfeed_path,
        &verify_dir(verification_repo, env),
    )
}

fn validate_version_json(vj: &VersionJson) -> Result<String> {
    let version = vj
        .platforms
        .values()
        .next()
        .map(|platform| platform.version.clone())
        .ok_or_else(|| anyhow::anyhow!("installer JSON has no platforms"))?;

    for (key, platform) in &vj.platforms {
        anyhow::ensure!(
            platform.version == version,
            "installer JSON mixes versions {} and {}",
            version,
            platform.version
        );
        if key == "linux" {
            anyhow::bail!(
                "installer JSON platform key 'linux' is the retired portable .bin channel; use linux-deb and linux-rpm"
            );
        }
        anyhow::ensure!(
            matches!(
                key.as_str(),
                "linux-deb" | "linux-rpm" | "darwin-arm" | "darwin" | "windows"
            ),
            "installer JSON contains unsupported platform key '{key}'"
        );
    }

    let has_deb = vj.platforms.contains_key("linux-deb");
    let has_rpm = vj.platforms.contains_key("linux-rpm");
    anyhow::ensure!(
        has_deb == has_rpm,
        "installer JSON requires both linux-deb and linux-rpm when Linux is present"
    );

    Ok(version)
}

fn build_release_draft(
    vj: &VersionJson,
    version: &str,
    release_notes_url: &str,
    updated_at: u64,
) -> Result<Vec<Value>> {
    let release_notes_url_ja = format!("{release_notes_url}#japanese");
    let update_target = format!(">={MIN_AUTO_UPDATE_VERSION} <{version}");
    let mut software_update = serde_json::Map::new();
    let mut app_platforms = Vec::new();

    let mut entries: Vec<(&String, &VersionPlatform)> = vj.platforms.iter().collect();
    entries.sort_by_key(|(key, _)| key.as_str());
    for (key, platform) in entries {
        let software_update_key = match key.as_str() {
            "linux-deb" | "linux-rpm" => continue,
            "windows" => "win32",
            "darwin" => "darwin",
            "darwin-arm" => "darwin-arm",
            _ => anyhow::bail!("installer JSON contains unsupported platform key '{key}'"),
        };
        app_platforms.push(software_update_key.to_string());
        software_update.insert(
            software_update_key.to_string(),
            json!({
                "version": &platform.version,
                "hash": &platform.sha256,
                "url": &platform.url
            }),
        );
    }
    app_platforms.sort();
    app_platforms.dedup();

    const THIRTY_MIN_MS: u64 = 30 * 60 * 1000;
    let date_update = updated_at.saturating_sub(THIRTY_MIN_MS);
    let mut items = Vec::new();

    if !software_update.is_empty() {
        items.push(json!({
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
            "target": {
                "daedalusVersion": &update_target,
                "platforms": &app_platforms
            },
            "action": {
                "label": { "en-US": "", "ja-JP": "" },
                "url":   { "en-US": "", "ja-JP": "" },
            },
            "date": date_update,
            "type": "software-update",
            "softwareUpdate": Value::Object(software_update),
        }));
    }

    if vj.platforms.contains_key("linux-deb") {
        items.push(json!({
            "title": {
                "en-US": format!("Daedalus {version} Linux upgrade available"),
                "ja-JP": format!("Daedalus {version} Linux アップグレード"),
            },
            "content": {
                "en-US": format!(
                    "Daedalus {version} is available for Linux as .deb and .rpm system packages.\n\n\
                     Close Daedalus and follow the release instructions to upgrade with your \
                     package manager. Your wallet data remains in place."
                ),
                "ja-JP": format!(
                    "Daedalus {version} は Linux 用 .deb / .rpm システムパッケージとして利用できます。\n\n\
                     Daedalus を終了し、リリース手順に従ってパッケージマネージャーで\
                     アップグレードしてください。ウォレットデータはそのまま保持されます。"
                ),
            },
            "target": {
                "daedalusVersion": &update_target,
                "platforms": ["linux"]
            },
            "action": {
                "label": {
                    "en-US": "Linux upgrade instructions",
                    "ja-JP": "Linux アップグレード手順"
                },
                "url": {
                    "en-US": release_notes_url,
                    "ja-JP": &release_notes_url_ja
                },
            },
            "date": updated_at,
            "type": "announcement",
        }));
    }

    if !app_platforms.is_empty() {
        items.push(json!({
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
            "target": {
                "daedalusVersion": version,
                "platforms": &app_platforms
            },
            "action": {
                "label": { "en-US": "Release notes", "ja-JP": "リリースノート" },
                "url": {
                    "en-US": release_notes_url,
                    "ja-JP": &release_notes_url_ja
                },
            },
            "date": updated_at,
            "type": "announcement",
        }));
    }

    Ok(items)
}

fn immutable_release_structure(item: &Value, index: usize) -> Result<Value> {
    let mut structure = item.clone();
    for pointer in ["/title", "/content", "/action/label"] {
        let copy = structure
            .pointer_mut(pointer)
            .and_then(Value::as_object_mut)
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "newsfeed draft item {index} localized copy field '{pointer}' is missing or not an object"
                )
            })?;
        for (locale, value) in copy {
            anyhow::ensure!(
                value.is_string(),
                "newsfeed draft item {index} localized copy '{pointer}/{locale}' is not a string"
            );
            *value = Value::Null;
        }
    }
    Ok(structure)
}

fn validate_release_draft(edited: &[Value], generated: &[Value]) -> Result<()> {
    anyhow::ensure!(
        edited.len() == generated.len(),
        "newsfeed draft item count changed from {} to {}; only localized copy edits are allowed",
        generated.len(),
        edited.len()
    );

    for (index, (edited_item, generated_item)) in edited.iter().zip(generated.iter()).enumerate() {
        let edited_structure = immutable_release_structure(edited_item, index)?;
        let generated_structure = immutable_release_structure(generated_item, index)?;
        anyhow::ensure!(
            edited_structure == generated_structure,
            "newsfeed draft item {index} changed immutable release structure; only localized title, content, and action label text may be edited"
        );
    }

    Ok(())
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

#[cfg(test)]
mod tests {
    use super::*;

    fn platform(version: &str, url: &str, sha256: &str) -> VersionPlatform {
        VersionPlatform {
            version: version.to_string(),
            url: url.to_string(),
            sha256: sha256.to_string(),
        }
    }

    fn linux_manifest() -> VersionJson {
        VersionJson {
            platforms: HashMap::from([
                (
                    "linux-deb".to_string(),
                    platform("6.0.1", "https://updates.example/daedalus.deb", "deb-sha"),
                ),
                (
                    "linux-rpm".to_string(),
                    platform("6.0.1", "https://updates.example/daedalus.rpm", "rpm-sha"),
                ),
            ]),
        }
    }

    fn assert_immutable_rejected(edited: &[Value], generated: &[Value]) {
        assert!(validate_release_draft(edited, generated)
            .expect_err("immutable release mutation must fail")
            .to_string()
            .contains("immutable release structure"));
    }

    #[test]
    fn linux_pair_creates_one_ordinary_announcement_only() {
        let manifest = linux_manifest();
        let version = validate_version_json(&manifest).expect("validate Linux manifest");
        let items = build_release_draft(
            &manifest,
            &version,
            "https://releases.example/6.0.1",
            3_600_000,
        )
        .expect("build Linux release draft");
        validate_release_draft(&items, &items).expect("validate generated Linux draft");

        assert_eq!(items.len(), 1);
        assert_eq!(items[0]["type"], "announcement");
        assert_eq!(items[0]["target"]["platforms"], json!(["linux"]));
        assert_eq!(
            items[0]["target"]["daedalusVersion"],
            format!(">={MIN_AUTO_UPDATE_VERSION} <6.0.1")
        );
        assert!(items[0].get("softwareUpdate").is_none());
        assert_eq!(
            items[0]["action"]["url"]["en-US"],
            "https://releases.example/6.0.1"
        );
    }

    #[test]
    fn mixed_release_excludes_linux_from_software_update() {
        let mut manifest = linux_manifest();
        manifest.platforms.insert(
            "windows".to_string(),
            platform("6.0.1", "https://updates.example/daedalus.exe", "win-sha"),
        );
        manifest.platforms.insert(
            "darwin".to_string(),
            platform("6.0.1", "https://updates.example/daedalus.pkg", "mac-sha"),
        );
        let version = validate_version_json(&manifest).expect("validate mixed manifest");
        let items = build_release_draft(
            &manifest,
            &version,
            "https://releases.example/6.0.1",
            3_600_000,
        )
        .expect("build mixed release draft");

        validate_release_draft(&items, &items).expect("validate generated mixed draft");
        let update = items
            .iter()
            .find(|item| item["type"] == "software-update")
            .expect("software update item");
        assert_eq!(update["target"]["platforms"], json!(["darwin", "win32"]));
        assert!(update["softwareUpdate"].get("linux").is_none());
        assert!(update["softwareUpdate"].get("linux-deb").is_none());
        assert!(update["softwareUpdate"].get("linux-rpm").is_none());
        assert_eq!(
            update["softwareUpdate"]["win32"]["url"],
            "https://updates.example/daedalus.exe"
        );

        let linux_targets = items
            .iter()
            .filter(|item| {
                item["target"]["platforms"]
                    .as_array()
                    .is_some_and(|platforms| platforms.iter().any(|value| value == "linux"))
            })
            .count();
        assert_eq!(linux_targets, 1);
    }

    #[test]
    fn preserves_non_linux_update_and_release_announcement() {
        let manifest = VersionJson {
            platforms: HashMap::from([(
                "windows".to_string(),
                platform("6.0.1", "https://updates.example/daedalus.exe", "win-sha"),
            )]),
        };
        let version = validate_version_json(&manifest).expect("validate Windows manifest");
        let items = build_release_draft(
            &manifest,
            &version,
            "https://releases.example/6.0.1",
            3_600_000,
        )
        .expect("build Windows release draft");

        assert_eq!(items.len(), 2);
        assert_eq!(items[0]["type"], "software-update");
        assert_eq!(items[0]["target"]["platforms"], json!(["win32"]));
        assert_eq!(items[1]["type"], "announcement");
        assert_eq!(items[1]["target"]["platforms"], json!(["win32"]));
        assert_eq!(items[1]["target"]["daedalusVersion"], "6.0.1");
    }

    #[test]
    fn rejects_editor_mutations_of_linux_announcement_contract() {
        let manifest = linux_manifest();
        let version = validate_version_json(&manifest).expect("validate Linux manifest");
        let original = build_release_draft(
            &manifest,
            &version,
            "https://releases.example/6.0.1",
            3_600_000,
        )
        .expect("build Linux release draft");

        let mut localized_copy = original.clone();
        localized_copy[0]["title"]["en-US"] = json!("Edited Linux upgrade title");
        localized_copy[0]["content"]["ja-JP"] = json!("編集済み本文");
        validate_release_draft(&localized_copy, &original)
            .expect("localized copy edits remain allowed");

        let mut target = original.clone();
        target[0]["target"]["daedalusVersion"] = json!(">=0.0.0");
        assert_immutable_rejected(&target, &original);

        let mut action = original.clone();
        action[0]["action"]["url"]["en-US"] = json!("https://malicious.example/");
        assert_immutable_rejected(&action, &original);

        let mut item_type = original.clone();
        item_type[0]["type"] = json!("software-update");
        assert_immutable_rejected(&item_type, &original);

        assert!(validate_release_draft(&[], &original)
            .expect_err("removed Linux announcement must fail")
            .to_string()
            .contains("item count changed"));
    }

    #[test]
    fn rejects_editor_mutations_of_app_update_artifacts() {
        let mut manifest = linux_manifest();
        manifest.platforms.insert(
            "windows".to_string(),
            platform("6.0.1", "https://updates.example/daedalus.exe", "win-sha"),
        );
        manifest.platforms.insert(
            "darwin".to_string(),
            platform("6.0.1", "https://updates.example/daedalus.pkg", "mac-sha"),
        );
        let version = validate_version_json(&manifest).expect("validate mixed manifest");
        let original = build_release_draft(
            &manifest,
            &version,
            "https://releases.example/6.0.1",
            3_600_000,
        )
        .expect("build mixed release draft");
        let update_index = original
            .iter()
            .position(|item| item["type"] == "software-update")
            .expect("software-update item");

        let mut linux_target = original.clone();
        linux_target[update_index]["target"]["platforms"]
            .as_array_mut()
            .unwrap()
            .push(json!("linux"));
        assert_immutable_rejected(&linux_target, &original);

        let mut win32_url = original.clone();
        win32_url[update_index]["softwareUpdate"]["win32"]["url"] =
            json!("https://malicious.example/daedalus.exe");
        assert_immutable_rejected(&win32_url, &original);

        let mut darwin_hash = original.clone();
        darwin_hash[update_index]["softwareUpdate"]["darwin"]["hash"] = json!("changed-mac-sha");
        assert_immutable_rejected(&darwin_hash, &original);

        let mut removed_win32 = original.clone();
        removed_win32[update_index]["softwareUpdate"]
            .as_object_mut()
            .unwrap()
            .remove("win32");
        assert_immutable_rejected(&removed_win32, &original);
    }

    #[test]
    fn validates_pair_and_one_release_version() {
        let mut missing_rpm = linux_manifest();
        missing_rpm.platforms.remove("linux-rpm");
        assert!(validate_version_json(&missing_rpm)
            .expect_err("partial Linux release must fail")
            .to_string()
            .contains("requires both linux-deb and linux-rpm"));

        let mut mixed_versions = linux_manifest();
        mixed_versions
            .platforms
            .get_mut("linux-rpm")
            .unwrap()
            .version = "6.0.2".to_string();
        assert!(validate_version_json(&mixed_versions)
            .expect_err("mixed release versions must fail")
            .to_string()
            .contains("mixes versions"));

        let retired = VersionJson {
            platforms: HashMap::from([(
                "linux".to_string(),
                platform("6.0.1", "https://updates.example/daedalus.bin", "bin-sha"),
            )]),
        };
        assert!(validate_version_json(&retired)
            .expect_err("retired portable manifest must fail")
            .to_string()
            .contains("retired portable .bin channel"));
    }
}
