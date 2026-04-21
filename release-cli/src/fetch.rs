//! `drt fetch-installers` — download unsigned installer artifacts from a Hydra eval.
//!
//! Job naming in the Daedalus flake: `installer.{system}.{cluster}`
//! e.g. `installer.x86_64-linux.mainnet`, `installer.x86_64-windows.mainnet`
//!
//! Downloads one file per platform (.bin / .pkg / .exe) into OUT_DIR and
//! writes a `meta.json` file.  The SHA-256 from the Hydra API is verified
//! after each download.

use anyhow::{Context, Result};
use reqwest::Client;
use serde::Deserialize;
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;
use tokio::io::AsyncWriteExt;
use tokio::sync::Semaphore;

// ── Hydra JSON types ──────────────────────────────────────────────────────────

#[derive(Deserialize)]
struct HydraEval {
    builds: Vec<u64>,
    flake: Option<String>,
}

#[derive(Deserialize, Debug)]
struct HydraProduct {
    name: String,
    #[serde(rename = "type")]
    kind: String,
    sha256hash: Option<String>,
}

#[derive(Deserialize, Debug)]
struct HydraBuild {
    id: u64,
    job: String,
    buildstatus: Option<i32>,
    finished: Option<i32>,
    #[serde(default)]
    buildproducts: HashMap<String, HydraProduct>,
}

// ── Public entry point ────────────────────────────────────────────────────────

pub async fn fetch_installers(eval_url: &str, env: &str, out_dir: &Path) -> Result<()> {
    let (base_url, eval_id) = parse_eval_url(eval_url)?;

    let client = Client::builder().user_agent("drt/0.1").build()?;

    // ── 1. Fetch eval ─────────────────────────────────────────────────────────
    println!("Fetching eval {eval_id} from {base_url} …");
    let eval: HydraEval = client
        .get(format!("{base_url}/eval/{eval_id}"))
        .header("Accept", "application/json")
        .send()
        .await
        .context("fetching eval")?
        .error_for_status()
        .context("eval request failed")?
        .json()
        .await
        .context("parsing eval JSON")?;

    println!(
        "Eval has {} builds total; scanning for installer.*.{env} …",
        eval.builds.len()
    );

    // ── 2. Fetch build info concurrently (max 16 in-flight) ──────────────────
    let sem = Arc::new(Semaphore::new(16));
    let mut tasks = tokio::task::JoinSet::new();

    for build_id in eval.builds {
        let client = client.clone();
        let base_url = base_url.clone();
        let sem = sem.clone();
        tasks.spawn(async move {
            let _permit = sem.acquire().await.unwrap();
            fetch_build(&client, &base_url, build_id).await
        });
    }

    let mut installer_builds: Vec<HydraBuild> = Vec::new();
    while let Some(res) = tasks.join_next().await {
        let build = res.context("build fetch task panicked")??;
        if is_installer_for(env, &build) {
            installer_builds.push(build);
        }
    }

    installer_builds.sort_by(|a, b| a.job.cmp(&b.job));

    if installer_builds.is_empty() {
        anyhow::bail!(
            "no finished installer builds found for cluster '{env}' in eval {eval_id}\n\
             (expected jobs named installer.<system>.{env})"
        );
    }

    println!("Found {} installer build(s):", installer_builds.len());
    for b in &installer_builds {
        println!("  [{}] {}", b.id, b.job);
    }

    // ── 3. Download artifacts ─────────────────────────────────────────────────
    std::fs::create_dir_all(out_dir).with_context(|| format!("creating {}", out_dir.display()))?;

    let mut version: Option<String> = None;

    for build in &installer_builds {
        for (product_nr, product) in &build.buildproducts {
            if product.kind != "file" {
                continue;
            }
            let ext = Path::new(&product.name)
                .extension()
                .and_then(|e| e.to_str())
                .unwrap_or("");
            if !matches!(ext, "bin" | "pkg" | "exe") {
                continue;
            }

            let dest = out_dir.join(&product.name);

            // Skip if the file already exists and its hash matches.
            if dest.exists() {
                if let Some(expected) = &product.sha256hash {
                    print!("  {} exists, verifying … ", product.name);
                    let existing = sha256_file(&dest)?;
                    if existing == *expected {
                        println!("✓ already downloaded");
                        if version.is_none() {
                            version = extract_version(&product.name);
                        }
                        continue;
                    }
                    println!("hash mismatch, re-downloading");
                }
            }

            println!("  Downloading {} …", product.name);
            let sha256 = download_with_retry(
                &client,
                &base_url,
                build.id,
                product_nr,
                &product.name,
                &dest,
            )
            .await?;

            if let Some(expected) = &product.sha256hash {
                anyhow::ensure!(
                    sha256 == *expected,
                    "SHA-256 mismatch for {}:\n  got      {}\n  expected {}",
                    product.name,
                    sha256,
                    expected
                );
                println!("    ✓ sha256 verified");
            }

            if version.is_none() {
                version = extract_version(&product.name);
            }
        }
    }

    // ── 4. Write meta.json ────────────────────────────────────────────────────
    let ver = version.ok_or_else(|| {
        anyhow::anyhow!(
            "could not extract version from any installer filename; \
             please create 'meta.json' manually"
        )
    })?;

    let (gitrev, nar_hash) = eval
        .flake
        .as_deref()
        .map(parse_flake)
        .unwrap_or((None, None));

    let meta = crate::installers::Meta {
        version: ver.clone(),
        gitrev,
        nar_hash,
        env: Some(env.to_string()),
        eval_url: Some(eval_url.to_string()),
    };

    let meta_path = out_dir.join("meta.json");
    std::fs::write(
        &meta_path,
        serde_json::to_string_pretty(&meta).context("serialising meta.json")? + "\n",
    )
    .with_context(|| format!("writing {}", meta_path.display()))?;

    println!("\nVersion : {ver}");
    println!("Output  : {}", out_dir.display());
    println!("\nNext steps:");
    println!("  drt sign {}", out_dir.display());

    Ok(())
}

// ── Helpers ───────────────────────────────────────────────────────────────────

fn parse_eval_url(url: &str) -> Result<(String, u64)> {
    let url = url.trim_end_matches('/');
    let (base, id_str) = url.rsplit_once("/eval/").ok_or_else(|| {
        anyhow::anyhow!("expected a URL like https://ci.iog.io/eval/107478, got: {url}")
    })?;
    let eval_id = id_str
        .parse::<u64>()
        .with_context(|| format!("eval ID is not a number: {id_str}"))?;
    Ok((base.to_string(), eval_id))
}

async fn fetch_build(client: &Client, base_url: &str, build_id: u64) -> Result<HydraBuild> {
    client
        .get(format!("{base_url}/build/{build_id}"))
        .header("Accept", "application/json")
        .send()
        .await
        .with_context(|| format!("GET /build/{build_id}"))?
        .error_for_status()
        .with_context(|| format!("build {build_id} request failed"))?
        .json()
        .await
        .with_context(|| format!("parsing build {build_id} JSON"))
}

fn is_installer_for(env: &str, build: &HydraBuild) -> bool {
    build.finished == Some(1)
        && build.buildstatus == Some(0)
        && build.job.starts_with("installer.")
        && build.job.ends_with(&format!(".{env}"))
}

/// Compute the SHA-256 of an existing file on disk.
fn sha256_file(path: &Path) -> Result<String> {
    use std::io::Read;
    let mut file =
        std::fs::File::open(path).with_context(|| format!("opening {}", path.display()))?;
    let mut hasher = Sha256::new();
    let mut buf = [0u8; 64 * 1024];
    loop {
        let n = file.read(&mut buf)?;
        if n == 0 {
            break;
        }
        hasher.update(&buf[..n]);
    }
    Ok(hex::encode(hasher.finalize()))
}

/// Retry wrapper around `download_file` with exponential backoff.
/// Retries up to 5 times on any transient error (timeout, connection reset, etc.).
async fn download_with_retry(
    client: &Client,
    base_url: &str,
    build_id: u64,
    product_nr: &str,
    filename: &str,
    dest: &Path,
) -> Result<String> {
    const MAX_ATTEMPTS: u32 = 5;
    let mut delay_secs = 3u64;

    for attempt in 1..=MAX_ATTEMPTS {
        match download_file(client, base_url, build_id, product_nr, filename, dest).await {
            Ok(sha) => return Ok(sha),
            Err(e) if attempt < MAX_ATTEMPTS => {
                println!(
                    "    attempt {attempt}/{MAX_ATTEMPTS} failed: {e:#}\n    retrying in {delay_secs}s …"
                );
                tokio::time::sleep(std::time::Duration::from_secs(delay_secs)).await;
                delay_secs *= 2;
            }
            Err(e) => return Err(e),
        }
    }
    unreachable!()
}

/// Download a build product, returning its hex SHA-256.
async fn download_file(
    client: &Client,
    base_url: &str,
    build_id: u64,
    product_nr: &str,
    filename: &str,
    dest: &Path,
) -> Result<String> {
    let url = format!("{base_url}/build/{build_id}/download/{product_nr}/{filename}");
    let mut resp = client
        .get(&url)
        .send()
        .await
        .with_context(|| format!("GET {url}"))?
        .error_for_status()
        .with_context(|| format!("download failed: {url}"))?;

    let mut file = tokio::fs::File::create(dest)
        .await
        .with_context(|| format!("creating {}", dest.display()))?;

    let mut hasher = Sha256::new();
    while let Some(chunk) = resp.chunk().await? {
        hasher.update(&chunk);
        file.write_all(&chunk).await?;
    }
    file.flush().await?;

    Ok(hex::encode(hasher.finalize()))
}

/// Extract the version from a Daedalus installer filename.
/// `daedalus-7.3.0-83575-mainnet-fbb43f32c-x86_64-linux.bin` → `"7.3.0"`
fn extract_version(filename: &str) -> Option<String> {
    filename
        .split('-')
        .skip(1) // skip "daedalus"
        .find(|part| {
            part.chars()
                .next()
                .map(|c| c.is_ascii_digit())
                .unwrap_or(false)
                && part.contains('.')
        })
        .map(|s| s.to_string())
}

/// Extract (gitrev, nar_hash) from a locked flake URL.
/// e.g. `github:input-output-hk/daedalus/<rev>?narHash=sha256-...`
fn parse_flake(flake: &str) -> (Option<String>, Option<String>) {
    let (path, query) = flake.split_once('?').unwrap_or((flake, ""));
    let gitrev = path.rsplit('/').next().map(|s| s.to_string());
    let nar_hash = query.split('&').find_map(|kv| {
        let (k, v) = kv.split_once('=')?;
        if k == "narHash" {
            Some(percent_decode(v))
        } else {
            None
        }
    });
    (gitrev, nar_hash)
}

/// Decode the subset of percent-encoding used in Hydra flake URLs.
fn percent_decode(s: &str) -> String {
    s.replace("%2B", "+")
        .replace("%3D", "=")
        .replace("%2F", "/")
}
