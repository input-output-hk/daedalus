//! GPG and SSH-based code signing.
//!
//! * `sign_file`          — GPG detached-signature via `gpg2`
//! * `code_sign_remote`   — macOS / Windows code signing via an SSH host

use anyhow::{Context, Result, bail};
use std::io::Read;
use std::path::Path;
use std::process::{Command, Stdio};

/// Credentials for Apple's notarization service.
/// Read from `APPLE_NOTARY_USER`, `APPLE_NOTARY_PASS`, `APPLE_TEAM_ID`.
pub struct NotaryCreds {
    pub apple_id: String,
    pub password: String,
    pub team_id: String,
}

/// GPG-sign `path`, producing `<path>.asc`.
///
/// `gpg_user` is passed as `--local-user` when supplied; otherwise
/// GPG uses the default secret key.  stdin/stdout/stderr are inherited
/// so that pinentry prompts work.
pub fn sign_file(path: &Path, gpg_user: Option<&str>) -> Result<()> {
    let path_str = path
        .to_str()
        .ok_or_else(|| anyhow::anyhow!("non-UTF-8 path: {}", path.display()))?;

    let mut cmd = Command::new("gpg2");
    cmd.arg("--detach-sig").arg("--armor").arg("--sign");
    if let Some(user) = gpg_user {
        cmd.arg("--local-user").arg(user);
    }
    cmd.arg(path_str);

    let status = cmd
        .status()
        .map_err(|e| anyhow::anyhow!("failed to spawn gpg2: {e}"))?;

    if !status.success() {
        bail!("gpg2 exited with {} for {}", status, path.display());
    }
    Ok(())
}

/// Code-sign an installer via an SSH signing host.
///
/// The original unsigned file is renamed to include "unsigned" in the name:
///   `daedalus-7.3.0-mainnet.pkg` → `daedalus-7.3.0-mainnet-unsigned.pkg`
///
/// Supported file types:
/// * `.pkg` (macOS) — SSH to `OSX_SIGN_HOST` and run
///   `nix run github:input-output-hk/daedalus/<gitrev>#packages.<arch>-darwin.makeSignedInstaller-<env>`,
///   which handles app-bundle codesigning + productsign.  SCP the result back.
///   Requires `gitrev` to be present in `meta` (written by `drt fetch-installers`).
/// * `.exe` (Windows) — `WIN_SIGN_HOST` is the HSM; pipe the file directly
///   through `ssh host < unsigned.exe > signed.exe` (ProxyJump handled by
///   the caller's SSH config).
pub fn code_sign_remote(
    host: &str,
    installer_path: &Path,
    skip_upload: bool,
    meta: &crate::installers::Meta,
    notary_creds: Option<&NotaryCreds>,
    verbose: bool,
) -> Result<()> {
    let filename = installer_path
        .file_name()
        .and_then(|f| f.to_str())
        .ok_or_else(|| anyhow::anyhow!("invalid path: {}", installer_path.display()))?;

    let ext = Path::new(filename)
        .extension()
        .and_then(|e| e.to_str())
        .unwrap_or("");

    match ext {
        "pkg" => code_sign_macos(
            host,
            installer_path,
            filename,
            skip_upload,
            meta,
            notary_creds,
            verbose,
        ),
        "exe" => code_sign_windows(host, installer_path, filename, verbose),
        other => bail!("no code signing support for .{other} installers"),
    }
}

/// macOS: SSH to the signing host and run `nix run` to do full app-bundle
/// codesigning + productsign via `makeSignedInstaller`, then SCP the result back.
///
/// stdout is captured to extract the signed .pkg path (the last non-empty line
/// printed by makeSignedInstaller).  stderr is suppressed unless `verbose` is
/// set; on failure the captured stderr is always printed.
fn code_sign_macos(
    host: &str,
    installer_path: &Path,
    filename: &str,
    skip_upload: bool,
    meta: &crate::installers::Meta,
    notary_creds: Option<&NotaryCreds>,
    verbose: bool,
) -> Result<()> {
    let gitrev = meta.gitrev.as_deref().ok_or_else(|| {
        anyhow::anyhow!("gitrev not available in meta.json — re-fetch with: drt fetch-installers")
    })?;

    let (arch, env) = parse_darwin_installer_filename(filename)?;

    let flake_ref = format!(
        "github:input-output-hk/daedalus/{gitrev}#packages.{arch}-darwin.makeSignedInstaller-{env}"
    );

    if skip_upload {
        println!("    --skip-upload has no effect for macOS (nix run builds from store)");
    }

    println!("    nix run  → {flake_ref}");
    println!("    signing on {host}…");

    // stdout is piped so we can extract the signed .pkg path from the last line.
    // stderr is inherited when --verbose; otherwise piped and printed only on failure.
    // Use the full path to nix — SSH non-interactive sessions don't source the
    // shell profile, so /run/current-system/sw/bin is not in PATH.
    let mut child = Command::new("ssh")
        .args([host, "/run/current-system/sw/bin/nix", "run", &flake_ref])
        .stdout(Stdio::piped())
        .stderr(if verbose {
            Stdio::inherit()
        } else {
            Stdio::piped()
        })
        .spawn()
        .context("ssh nix run failed to start")?;

    let mut stdout_bytes = Vec::new();
    child
        .stdout
        .as_mut()
        .unwrap()
        .read_to_end(&mut stdout_bytes)
        .context("reading nix run stdout")?;

    let mut stderr_bytes = Vec::new();
    if !verbose {
        child
            .stderr
            .as_mut()
            .unwrap()
            .read_to_end(&mut stderr_bytes)
            .context("reading nix run stderr")?;
    }

    let st = child.wait().context("ssh nix run wait failed")?;
    if !st.success() {
        if !stderr_bytes.is_empty() {
            eprintln!("{}", String::from_utf8_lossy(&stderr_bytes));
        }
        bail!("nix run on {host} exited with {st}");
    }

    let stdout = String::from_utf8_lossy(&stdout_bytes);

    // makeSignedInstaller prints the signed .pkg path as its last output line.
    let remote_pkg = stdout
        .lines()
        .rev()
        .find(|l| !l.trim().is_empty())
        .ok_or_else(|| anyhow::anyhow!("nix run produced no output on stdout"))?
        .trim()
        .to_string();

    anyhow::ensure!(
        remote_pkg.ends_with(".pkg"),
        "unexpected last line from nix run (expected a .pkg path): {remote_pkg:?}"
    );

    // Notarize + staple on the signing host before downloading
    if let Some(creds) = notary_creds {
        println!("    notarizing on {host}…");
        let st = Command::new("ssh")
            .args([
                host,
                "xcrun",
                "notarytool",
                "submit",
                &remote_pkg,
                "--apple-id",
                &creds.apple_id,
                "--password",
                &creds.password,
                "--team-id",
                &creds.team_id,
                "--wait",
            ])
            .status()
            .context("xcrun notarytool failed to start")?;
        anyhow::ensure!(st.success(), "notarytool submit exited with {st}");

        println!("    stapling…");
        let st = Command::new("ssh")
            .args([host, "xcrun", "stapler", "staple", &remote_pkg])
            .status()
            .context("xcrun stapler failed to start")?;
        anyhow::ensure!(st.success(), "stapler staple exited with {st}");
    }

    // Rename local unsigned original
    let unsigned_path = installer_path.with_file_name(unsigned_filename(filename));
    if installer_path.exists() {
        println!("    rename  → {}", unsigned_path.display());
        std::fs::rename(installer_path, &unsigned_path)
            .with_context(|| format!("renaming {}", installer_path.display()))?;
    }

    // SCP signed (and notarized+stapled if creds were provided) pkg back
    println!("    download← {host}:{remote_pkg}");
    let st = Command::new("scp")
        .args([
            "-q",
            &format!("{host}:{remote_pkg}"),
            installer_path.to_str().unwrap(),
        ])
        .status()
        .context("scp download failed to start")?;
    anyhow::ensure!(st.success(), "scp download exited with {st}");

    // Cleanup remote temp dir
    if let Some(parent) = Path::new(&remote_pkg).parent() {
        let _ = Command::new("ssh")
            .args([
                host,
                "rm",
                "-rf",
                parent.to_str().unwrap_or("/tmp/drt-nix-tmp"),
            ])
            .status();
    }

    println!("    ✓ {}", installer_path.display());
    Ok(())
}

/// Windows: WIN_SIGN_HOST is the HSM itself (ProxyJump already in SSH config).
/// Pipe the .exe directly: `ssh host < unsigned.exe > signed.exe`.
fn code_sign_windows(
    host: &str,
    installer_path: &Path,
    filename: &str,
    verbose: bool,
) -> Result<()> {
    use std::fs::File;

    let unsigned_path = installer_path.with_file_name(unsigned_filename(filename));

    // Rename original to -unsigned before writing signed version.
    // Skip if already renamed (re-entrant: partial run where SCP/pipe failed).
    if installer_path.exists() {
        println!("    rename  → {}", unsigned_path.display());
        std::fs::rename(installer_path, &unsigned_path)
            .with_context(|| format!("renaming {}", installer_path.display()))?;
    }

    println!("    signing via {host} (stdin→stdout)…");
    let input = File::open(&unsigned_path)
        .with_context(|| format!("opening {}", unsigned_path.display()))?;
    let output = File::create(installer_path)
        .with_context(|| format!("creating {}", installer_path.display()))?;

    let mut child = Command::new("ssh")
        .arg(host)
        .stdin(Stdio::from(input))
        .stdout(Stdio::from(output))
        .stderr(if verbose {
            Stdio::inherit()
        } else {
            Stdio::piped()
        })
        .spawn()
        .context("ssh to WIN_SIGN_HOST failed to start")?;

    let mut stderr_bytes = Vec::new();
    if !verbose {
        child
            .stderr
            .as_mut()
            .unwrap()
            .read_to_end(&mut stderr_bytes)
            .context("reading ssh stderr")?;
    }

    let st = child.wait().context("ssh WIN_SIGN_HOST wait failed")?;
    if !st.success() {
        if !stderr_bytes.is_empty() {
            eprintln!("{}", String::from_utf8_lossy(&stderr_bytes));
        }
        bail!("WIN_SIGN_HOST ssh exited with {st}");
    }

    println!("    ✓ {}", installer_path.display());
    Ok(())
}

/// Parse `arch` and `env` from a Darwin installer filename.
/// `daedalus-7.3.0-83575-mainnet-fbb43f32c-aarch64-darwin.pkg` → `("aarch64", "mainnet")`
fn parse_darwin_installer_filename(filename: &str) -> Result<(String, String)> {
    for arch in &["aarch64", "x86_64"] {
        let suffix = format!("-{arch}-darwin.pkg");
        if let Some(stem) = filename.strip_suffix(&suffix) {
            // stem: daedalus-7.3.0-83575-mainnet-fbb43f32c
            // idx:  0        1     2     3       4
            let parts: Vec<&str> = stem.split('-').collect();
            if parts.len() >= 4 {
                return Ok((arch.to_string(), parts[3].to_string()));
            }
        }
    }
    bail!("cannot determine arch/env from Darwin installer filename: {filename}")
}

/// Rename `foo.pkg` → `foo-unsigned.pkg` (preserves extension).
fn unsigned_filename(filename: &str) -> String {
    match filename.rsplit_once('.') {
        Some((stem, ext)) => format!("{stem}-unsigned.{ext}"),
        None => format!("{filename}-unsigned"),
    }
}
