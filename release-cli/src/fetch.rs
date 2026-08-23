//! `drt fetch-installers` — download unsigned installer artifacts from a Hydra eval.
//!
//! Linux packages use the exact jobs
//! `deb-installer.x86_64-linux.{cluster}` and
//! `rpm-installer.x86_64-linux.{cluster}`. macOS and Windows keep using
//! `installer.{system}.{cluster}`.
//!
//! Downloads one file per release artifact (.deb / .rpm / .pkg / .exe) into
//! OUT_DIR and writes a `meta.json` file. The SHA-256 from the Hydra API is
//! verified after each download.

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
        "Eval has {} builds total; scanning exact deb/rpm and non-Linux installer jobs for {env} …",
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
             (expected deb-installer.x86_64-linux.{env}, \
             rpm-installer.x86_64-linux.{env}, or installer.<non-linux-system>.{env})"
        );
    }

    println!("Found {} installer build(s):", installer_builds.len());
    for b in &installer_builds {
        println!("  [{}] {}", b.id, b.job);
    }

    // ── 3. Download artifacts ─────────────────────────────────────────────────
    std::fs::create_dir_all(out_dir).with_context(|| format!("creating {}", out_dir.display()))?;

    let mut version: Option<String> = None;
    let mut linux_deb_products = 0usize;
    let mut linux_rpm_products = 0usize;

    for build in &installer_builds {
        for (product_nr, product) in &build.buildproducts {
            if product.kind != "file" {
                continue;
            }
            let ext = Path::new(&product.name)
                .extension()
                .and_then(|e| e.to_str())
                .unwrap_or("");
            if ext == "bin" {
                anyhow::bail!(
                    "Hydra job '{}' exposed retired portable Linux .bin artifact '{}'",
                    build.job,
                    product.name
                );
            }
            if !is_supported_product_for_job(&build.job, &product.name) {
                continue;
            }
            validate_hydra_product_policy(&product.name, product.sha256hash.as_deref())?;

            record_version(&mut version, &product.name)?;
            match linux_job_platform(&build.job) {
                Some(crate::installers::Platform::LinuxDeb) => linux_deb_products += 1,
                Some(crate::installers::Platform::LinuxRpm) => linux_rpm_products += 1,
                _ => {}
            }

            let dest = out_dir.join(&product.name);

            // Skip if the file already exists and its hash matches.
            if dest.exists() {
                if let Some(expected) = &product.sha256hash {
                    print!("  {} exists, verifying … ", product.name);
                    let existing = sha256_file(&dest)?;
                    if existing == *expected {
                        println!("✓ already downloaded");
                        continue;
                    }
                    if allows_unsigned_companion(&product.name) {
                        // Remote macOS/Windows code signing preserves the
                        // Hydra bytes in an unsigned companion.
                        let unsigned_name = match product.name.rsplit_once('.') {
                            Some((stem, ext)) => format!("{stem}-unsigned.{ext}"),
                            None => format!("{}-unsigned", product.name),
                        };
                        let unsigned_path = out_dir.join(&unsigned_name);
                        if unsigned_path.exists() {
                            let unsigned_hash = sha256_file(&unsigned_path)?;
                            if unsigned_hash == *expected {
                                println!("already signed ({}), skipping", unsigned_name);
                                continue;
                            }
                            println!("unsigned companion hash mismatch, re-downloading");
                        } else {
                            println!("hash mismatch, re-downloading");
                        }
                    } else {
                        println!("Linux package hash mismatch, re-downloading exact Hydra bytes");
                    }
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

    let installer_dir = crate::installers::InstallerDir::from_meta(out_dir, meta)
        .context("validating downloaded installer set")?;
    let has_linux = installer_dir
        .installers
        .iter()
        .any(|installer| installer.platform.is_linux_package());
    validate_linux_job_products(has_linux, linux_deb_products, linux_rpm_products)?;

    let meta_json =
        serde_json::to_string_pretty(&installer_dir.meta).context("serialising meta.json")? + "\n";
    let meta_path = out_dir.join("meta.json");
    std::fs::write(&meta_path, meta_json)
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
    if build.finished != Some(1) || build.buildstatus != Some(0) {
        return false;
    }

    let mut parts = build.job.split('.');
    let job = (parts.next(), parts.next(), parts.next(), parts.next());
    match job {
        (Some("deb-installer" | "rpm-installer"), Some("x86_64-linux"), Some(cluster), None) => {
            cluster == env
        }
        (Some("installer"), Some(system), Some(cluster), None) => {
            cluster == env && system != "x86_64-linux"
        }
        _ => false,
    }
}

fn is_supported_product_for_job(job: &str, filename: &str) -> bool {
    let extension = Path::new(filename).extension().and_then(|ext| ext.to_str());
    let mut parts = job.split('.');
    let job = (parts.next(), parts.next(), parts.next(), parts.next());
    match job {
        (Some("deb-installer"), Some("x86_64-linux"), Some(_), None) => extension == Some("deb"),
        (Some("rpm-installer"), Some("x86_64-linux"), Some(_), None) => extension == Some("rpm"),
        (Some("installer"), Some(system), Some(_), None) if system != "x86_64-linux" => {
            matches!(extension, Some("pkg" | "exe"))
        }
        _ => false,
    }
}

fn allows_unsigned_companion(filename: &str) -> bool {
    matches!(
        Path::new(filename).extension().and_then(|ext| ext.to_str()),
        Some("pkg" | "exe")
    )
}

fn validate_hydra_product_policy(filename: &str, expected_sha256: Option<&str>) -> Result<()> {
    let extension = Path::new(filename).extension().and_then(|ext| ext.to_str());
    if matches!(extension, Some("deb" | "rpm")) {
        anyhow::ensure!(
            !filename.contains("-unsigned."),
            "Hydra Linux package product '{}' is an unsigned companion; only the main package is allowed",
            filename
        );
        anyhow::ensure!(
            expected_sha256.is_some(),
            "Hydra Linux package product '{}' has no SHA-256; exact main-file verification is required",
            filename
        );
    }
    Ok(())
}

fn linux_job_platform(job: &str) -> Option<crate::installers::Platform> {
    let mut parts = job.split('.');
    match (parts.next(), parts.next(), parts.next(), parts.next()) {
        (Some("deb-installer"), Some("x86_64-linux"), Some(_), None) => {
            Some(crate::installers::Platform::LinuxDeb)
        }
        (Some("rpm-installer"), Some("x86_64-linux"), Some(_), None) => {
            Some(crate::installers::Platform::LinuxRpm)
        }
        _ => None,
    }
}

fn validate_linux_job_products(has_linux: bool, deb_count: usize, rpm_count: usize) -> Result<()> {
    if has_linux {
        anyhow::ensure!(
            deb_count == 1 && rpm_count == 1,
            "downloaded Linux artifacts require exactly one product from each exact Hydra job; \
             got deb-installer.x86_64-linux={deb_count}, \
             rpm-installer.x86_64-linux={rpm_count}"
        );
    } else {
        anyhow::ensure!(
            deb_count == 0 && rpm_count == 0,
            "Hydra exposed Linux package products but no validated Linux package pair was downloaded"
        );
    }
    Ok(())
}

fn record_version(version: &mut Option<String>, filename: &str) -> Result<()> {
    let artifact_version = crate::installers::release_version_from_filename(filename)
        .ok_or_else(|| anyhow::anyhow!("could not extract release version from '{filename}'"))?;
    if let Some(expected) = version {
        anyhow::ensure!(
            artifact_version == expected.as_str(),
            "installer '{}' reports version {}, but earlier artifacts report {}",
            filename,
            artifact_version,
            expected
        );
    } else {
        *version = Some(artifact_version.to_string());
    }
    Ok(())
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

/// Extract (gitrev, nar_hash) from a locked flake URL.
///
/// Handles two formats Hydra has used:
///   Old: `github:input-output-hk/daedalus/<rev>?narHash=sha256-...`
///   New: `git+https://github.com/input-output-hk/daedalus?ref=refs/heads/master&rev=<rev>&submodules=1`
fn parse_flake(flake: &str) -> (Option<String>, Option<String>) {
    let (path, query) = flake.split_once('?').unwrap_or((flake, ""));

    let mut gitrev = None;
    let mut nar_hash = None;
    for kv in query.split('&') {
        if let Some((k, v)) = kv.split_once('=') {
            match k {
                "rev" => gitrev = Some(percent_decode(v)),
                "narHash" => nar_hash = Some(percent_decode(v)),
                _ => {}
            }
        }
    }

    // Fall back to last path component for the old github: flake format
    if gitrev.is_none() {
        gitrev = path.rsplit('/').next().map(|s| s.to_string());
    }

    (gitrev, nar_hash)
}

/// Decode the subset of percent-encoding used in Hydra flake URLs.
fn percent_decode(s: &str) -> String {
    s.replace("%2B", "+")
        .replace("%3D", "=")
        .replace("%2F", "/")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn hydra_build(job: &str) -> HydraBuild {
        HydraBuild {
            id: 1,
            job: job.to_string(),
            buildstatus: Some(0),
            finished: Some(1),
            buildproducts: HashMap::new(),
        }
    }

    #[test]
    fn selects_exact_linux_package_and_non_linux_job_families() {
        for job in [
            "deb-installer.x86_64-linux.mainnet",
            "rpm-installer.x86_64-linux.mainnet",
            "installer.aarch64-darwin.mainnet",
            "installer.x86_64-darwin.mainnet",
            "installer.x86_64-windows.mainnet",
        ] {
            assert!(is_installer_for("mainnet", &hydra_build(job)), "{job}");
        }
    }

    #[test]
    fn rejects_portable_or_inexact_hydra_jobs() {
        for job in [
            "installer.x86_64-linux.mainnet",
            "deb-installer.aarch64-linux.mainnet",
            "rpm-installer.x86_64-linux.preview",
            "prefix.deb-installer.x86_64-linux.mainnet",
            "installer.x86_64-windows.extra.mainnet",
        ] {
            assert!(!is_installer_for("mainnet", &hydra_build(job)), "{job}");
        }

        let mut failed = hydra_build("deb-installer.x86_64-linux.mainnet");
        failed.buildstatus = Some(1);
        assert!(!is_installer_for("mainnet", &failed));
    }

    #[test]
    fn filters_products_by_exact_job_family() {
        assert!(is_supported_product_for_job(
            "deb-installer.x86_64-linux.mainnet",
            "daedalus-6.0.1-mainnet-x86_64-linux.deb"
        ));
        assert!(!is_supported_product_for_job(
            "deb-installer.x86_64-linux.mainnet",
            "daedalus-6.0.1-mainnet-x86_64-linux.rpm"
        ));
        assert!(is_supported_product_for_job(
            "rpm-installer.x86_64-linux.mainnet",
            "daedalus-6.0.1-mainnet-x86_64-linux.rpm"
        ));
        assert!(is_supported_product_for_job(
            "installer.x86_64-windows.mainnet",
            "daedalus-6.0.1-mainnet-x86_64-windows.exe"
        ));
        assert!(is_supported_product_for_job(
            "installer.aarch64-darwin.mainnet",
            "daedalus-6.0.1-mainnet-aarch64-darwin.pkg"
        ));
        assert!(!is_supported_product_for_job(
            "installer.x86_64-linux.mainnet",
            "daedalus-6.0.1-mainnet-x86_64-linux.bin"
        ));
    }

    #[test]
    fn restricts_unsigned_companions_to_remote_code_signed_formats() {
        assert!(allows_unsigned_companion("daedalus.pkg"));
        assert!(allows_unsigned_companion("daedalus.exe"));
        assert!(!allows_unsigned_companion("daedalus.deb"));
        assert!(!allows_unsigned_companion("daedalus.rpm"));

        assert!(validate_hydra_product_policy("daedalus.deb", Some("sha")).is_ok());
        assert!(validate_hydra_product_policy("daedalus.rpm", Some("sha")).is_ok());
        assert!(validate_hydra_product_policy("daedalus.pkg", None).is_ok());
        assert!(validate_hydra_product_policy("daedalus.exe", None).is_ok());
        assert!(validate_hydra_product_policy("daedalus.deb", None)
            .expect_err("Linux package without Hydra hash must fail")
            .to_string()
            .contains("exact main-file verification"));
        assert!(
            validate_hydra_product_policy("daedalus-unsigned.rpm", Some("sha"))
                .expect_err("Linux unsigned companion must fail")
                .to_string()
                .contains("only the main package is allowed")
        );
    }

    #[test]
    fn requires_one_product_from_each_linux_hydra_job() {
        assert!(validate_linux_job_products(true, 1, 1).is_ok());
        assert!(validate_linux_job_products(false, 0, 0).is_ok());

        for (has_linux, deb_count, rpm_count) in
            [(true, 1, 0), (true, 0, 1), (true, 0, 0), (true, 2, 1)]
        {
            let error = validate_linux_job_products(has_linux, deb_count, rpm_count)
                .expect_err("incomplete or stale Linux pair must fail");
            assert!(error.to_string().contains("exactly one product"));
        }
    }

    #[test]
    fn rejects_mixed_artifact_versions() {
        let mut version = None;
        record_version(&mut version, "daedalus-6.0.1-mainnet-x86_64-linux.deb")
            .expect("record first version");
        let error = record_version(&mut version, "daedalus-6.0.2-mainnet-x86_64-linux.rpm")
            .expect_err("mixed versions must fail");
        assert!(error.to_string().contains("earlier artifacts report 6.0.1"));
    }

    #[test]
    fn parse_flake_old_github_format() {
        let (gitrev, nar_hash) = parse_flake(
            "github:input-output-hk/daedalus/50706edb8f5a772bc961d0ff6bcc54b9e4f8403f?narHash=sha256-pLZ7mM65BevAulHq%2B43ecshkgHSarNvDUTeldp8KxjY%3D",
        );
        assert_eq!(
            gitrev.as_deref(),
            Some("50706edb8f5a772bc961d0ff6bcc54b9e4f8403f")
        );
        assert_eq!(
            nar_hash.as_deref(),
            Some("sha256-pLZ7mM65BevAulHq+43ecshkgHSarNvDUTeldp8KxjY=")
        );
    }

    #[test]
    fn parse_flake_new_git_https_format() {
        let (gitrev, nar_hash) = parse_flake(
            "git+https://github.com/input-output-hk/daedalus?ref=refs/heads/master&rev=32af7f154b8556dbf794eed2365661ef4ed2670d&submodules=1",
        );
        assert_eq!(
            gitrev.as_deref(),
            Some("32af7f154b8556dbf794eed2365661ef4ed2670d")
        );
        assert_eq!(nar_hash, None);
    }
}
