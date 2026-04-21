mod cli;
mod fetch;
mod hash;
mod installers;
mod s3;
mod serve;
mod sign;
mod version_json;

use anyhow::Result;
use clap::Parser;
use cli::{Cli, Commands};
use std::collections::HashMap;

struct SignFlags {
    skip_gpg: bool,
    skip_code_sign: bool,
    skip_upload: bool,
    skip_darwin: bool,
    skip_darwin_legacy: bool,
    skip_windows: bool,
    verbose: bool,
}

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::WARN.into()),
        )
        .init();

    let cli = Cli::parse();

    match cli.command {
        Commands::Sign {
            installers_dir,
            gpg_user,
            skip_gpg,
            skip_code_sign,
            skip_upload,
            skip_darwin,
            skip_darwin_legacy,
            skip_windows,
            verbose,
        } => {
            cmd_sign(
                &installers_dir,
                gpg_user.as_deref(),
                SignFlags {
                    skip_gpg,
                    skip_code_sign,
                    skip_upload,
                    skip_darwin,
                    skip_darwin_legacy,
                    skip_windows,
                    verbose,
                },
            )
            .await
        }

        Commands::Release {
            installers_dir,
            bucket,
            bucket_url,
            release_notes,
            dry_run,
            test,
        } => {
            cmd_release(
                &installers_dir,
                &bucket,
                &bucket_url,
                release_notes,
                dry_run,
                test,
            )
            .await
        }

        Commands::FetchInstallers { url, env, out_dir } => {
            fetch::fetch_installers(&url, &env, &out_dir).await
        }

        Commands::Serve {
            port,
            host,
            installers,
            release_notes,
        } => {
            let dir = installers::InstallerDir::load(&installers)?;
            serve::serve(&host, port, dir, release_notes).await
        }
    }
}

async fn cmd_sign(
    installers_dir: &std::path::Path,
    gpg_user_arg: Option<&str>,
    flags: SignFlags,
) -> Result<()> {
    let SignFlags {
        skip_gpg,
        skip_code_sign,
        skip_upload,
        skip_darwin,
        skip_darwin_legacy,
        skip_windows,
        verbose,
    } = flags;

    let installer_dir = installers::InstallerDir::load(installers_dir)?;

    println!("Version  : {}", installer_dir.version);
    println!();
    for inst in &installer_dir.installers {
        println!("  {} [{}]", inst.filename, inst.platform.display_name());
    }
    println!();

    // ── Code signing (macOS / Windows via SSH) ────────────────────────────────
    if !skip_code_sign {
        println!("=== Code Signing ===");
        let osx_host = std::env::var("OSX_SIGN_HOST").ok();
        let win_host = std::env::var("WIN_SIGN_HOST").ok();

        for inst in &installer_dir.installers {
            if inst.is_already_code_signed() {
                if inst.path.exists() {
                    // Both the signed file and its -unsigned companion exist → done.
                    println!(
                        "  {} [already signed — {} exists, skipping]",
                        inst.filename,
                        inst.unsigned_path().display()
                    );
                    continue;
                }
                // The -unsigned companion exists but the signed file is missing
                // (e.g. network error during SCP/pipe).  Fall through and retry.
                println!(
                    "  {} [resuming — signed file missing, retrying from {}]",
                    inst.filename,
                    inst.unsigned_path().display()
                );
            }
            match inst.platform {
                installers::Platform::DarwinArm => {
                    if skip_darwin {
                        println!("  {} [skip — --skip-darwin]", inst.filename);
                    } else {
                        match &osx_host {
                            Some(host) => {
                                println!("  {} → {host}", inst.filename);
                                sign::code_sign_remote(
                                    host,
                                    &inst.path,
                                    skip_upload,
                                    &installer_dir.meta,
                                    verbose,
                                )?;
                            }
                            None => println!("  {} [skip — OSX_SIGN_HOST not set]", inst.filename),
                        }
                    }
                }
                installers::Platform::DarwinX86 => {
                    if skip_darwin_legacy {
                        println!("  {} [skip — --skip-darwin-legacy]", inst.filename);
                    } else {
                        match &osx_host {
                            Some(host) => {
                                println!("  {} → {host}", inst.filename);
                                sign::code_sign_remote(
                                    host,
                                    &inst.path,
                                    skip_upload,
                                    &installer_dir.meta,
                                    verbose,
                                )?;
                            }
                            None => println!("  {} [skip — OSX_SIGN_HOST not set]", inst.filename),
                        }
                    }
                }
                installers::Platform::Windows => {
                    if skip_windows {
                        println!("  {} [skip — --skip-windows]", inst.filename);
                    } else {
                        match &win_host {
                            Some(host) => {
                                println!("  {} → {host}", inst.filename);
                                sign::code_sign_remote(
                                    host,
                                    &inst.path,
                                    skip_upload,
                                    &installer_dir.meta,
                                    verbose,
                                )?;
                            }
                            None => println!("  {} [skip — WIN_SIGN_HOST not set]", inst.filename),
                        }
                    }
                }
                installers::Platform::Linux => {
                    println!("  {} [no code signing for Linux]", inst.filename);
                }
            }
        }
    }

    // ── GPG signing ───────────────────────────────────────────────────────────
    if !skip_gpg {
        println!("\n=== GPG Signing ===");
        let gpg_user_env = std::env::var("GPG_USER").ok();
        let effective_gpg_user: Option<&str> = gpg_user_arg.or(gpg_user_env.as_deref());

        // Reload: filenames may have changed after code signing
        let installer_dir = installers::InstallerDir::load(installers_dir)?;
        for inst in &installer_dir.installers {
            println!("  {}", inst.filename);
            sign::sign_file(&inst.path, effective_gpg_user)?;
        }
    }

    println!("\nNext steps:");
    println!(
        "  drt release --bucket <bucket> --bucket-url <url> {}",
        installers_dir.display()
    );

    Ok(())
}

async fn cmd_release(
    installers_dir: &std::path::Path,
    bucket: &str,
    bucket_url: &str,
    release_notes: Option<String>,
    dry_run: bool,
    test: bool,
) -> Result<()> {
    let installer_dir = installers::InstallerDir::load(installers_dir)?;

    println!("Version  : {}", installer_dir.version);
    println!("Cluster  : {}", bucket_url);
    println!("Bucket   : {}", bucket);
    println!("Dry run  : {dry_run}");
    println!("Test mode: {test}");
    println!();

    for inst in &installer_dir.installers {
        println!("  {} [{}]", inst.filename, inst.platform.display_name());
    }
    println!();

    // ── 1. Hash ──────────────────────────────────────────────────────────────
    println!("=== Hashing ===");
    let mut hashes: HashMap<installers::Platform, hash::Hashes> = HashMap::new();
    for inst in &installer_dir.installers {
        print!("  {} … ", inst.filename);
        let h = hash::hash_file(&inst.path)?;
        println!(
            "blake2b-cbor={:.16}…  sha256={:.16}…",
            h.blake2b_cbor, h.sha256
        );
        hashes.insert(inst.platform, h);
    }

    // ── 2. Collect signatures ─────────────────────────────────────────────────
    let mut signatures: HashMap<installers::Platform, Option<String>> = HashMap::new();
    for inst in &installer_dir.installers {
        let sig = inst.read_signature()?;
        if sig.is_some() {
            println!("  sig found: {}", inst.sig_path().display());
        }
        signatures.insert(inst.platform, sig);
    }

    // ── 4a. Dry run ───────────────────────────────────────────────────────────
    if dry_run {
        let urls: HashMap<_, _> = installer_dir
            .installers
            .iter()
            .map(|inst| {
                (
                    inst.platform,
                    format!("https://{bucket_url}/{}", inst.filename),
                )
            })
            .collect();

        let vj = version_json::VersionJson::build(
            &installer_dir.version,
            &hashes,
            &urls,
            &signatures,
            release_notes,
        );
        println!("\n=== Version JSON (dry run) ===");
        println!("{}", serde_json::to_string_pretty(&vj)?);
        return Ok(());
    }

    // ── 4b. Upload ────────────────────────────────────────────────────────────
    let s3 = s3::S3Client::new(bucket.to_string(), bucket_url.to_string()).await?;
    if s3.key_prefix.is_empty() {
        println!("\n=== Uploading to s3://{} ===", s3.bucket);
    } else {
        println!(
            "\n=== Uploading to s3://{}/{} ===",
            s3.bucket, s3.key_prefix
        );
    }

    let mut urls: HashMap<installers::Platform, String> = HashMap::new();
    for inst in &installer_dir.installers {
        let h = &hashes[&inst.platform];
        let url = s3
            .upload_installer(&inst.path, &h.blake2b_cbor, &inst.filename)
            .await?;
        urls.insert(inst.platform, url);
    }

    for inst in &installer_dir.installers {
        let sig_path = inst.sig_path();
        if sig_path.exists() {
            let sig_filename = sig_path.file_name().unwrap().to_str().unwrap().to_string();
            s3.upload_signature(&sig_path, &sig_filename).await?;
        }
    }

    // ── 5. Generate + upload version JSON ─────────────────────────────────────
    let vj = version_json::VersionJson::build(
        &installer_dir.version,
        &hashes,
        &urls,
        &signatures,
        release_notes,
    );
    let json = serde_json::to_vec_pretty(&vj)?;
    let version_url = s3.upload_version_json(&json).await?;

    // ── 6. (Test mode) Upload newsfeed stub + verification ────────────────────
    let mut newsfeed_urls: Option<(String, String)> = None;
    if test {
        let env = installer_dir
            .meta
            .env
            .as_deref()
            .unwrap_or("mainnet")
            .to_string();
        println!("\n=== Uploading newsfeed stub (--test) ===");
        let nf = serve::build_newsfeed(&installer_dir, &hashes, &urls)?;
        let newsfeed_key = format!("newsfeed/newsfeed_{env}.json");
        let verification_key = format!("newsfeed-verification/{env}/{}.txt", nf.timestamp_ms);
        // no-store: newsfeed JSON changes on every run; verification files are
        // unique-by-timestamp so caching is fine, but no-store avoids any
        // chance of a stale hash being served after a re-run.
        s3.upload_bytes(&newsfeed_key, &nf.bytes, "application/json", Some("no-store"))
            .await?;
        s3.upload_bytes(
            &verification_key,
            nf.sha256_hex.as_bytes(),
            "text/plain",
            Some("no-store"),
        )
        .await?;
        newsfeed_urls = Some((
            s3.cdn_url(&newsfeed_key),
            s3.cdn_url(&verification_key),
        ));
    }

    println!("\n=== Done ===");
    println!("Version : {}", installer_dir.version);
    println!("JSON    : {version_url}");
    println!();
    for inst in &installer_dir.installers {
        if let Some(url) = urls.get(&inst.platform) {
            println!("  {} [{}]", url, inst.platform.display_name());
        }
    }
    if let Some((newsfeed_url, verification_url)) = newsfeed_urls {
        println!();
        println!("Newsfeed     : {newsfeed_url}");
        println!("Verification : {verification_url}");
    }

    Ok(())
}
