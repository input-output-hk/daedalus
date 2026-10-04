//! `drt publish-linux-repos` — build and publish APT, YUM, and Arch Linux
//! package repositories to a Cloudflare R2 (or any S3-compatible) bucket.
//!
//! ## Bucket layout
//!
//! ```text
//! apt/
//!   pool/main/d/<pkgname>/<filename>.deb
//!   pool/main/d/<pkgname>/<filename>.deb.asc     (if present)
//!   dists/stable/
//!     Release
//!     Release.gpg
//!     InRelease
//!     main/binary-amd64/Packages
//!     main/binary-amd64/Packages.gz
//!   daedalus-release.gpg                         (public key)
//!
//! yum/
//!   packages/<filename>.rpm
//!   packages/<filename>.rpm.asc                  (if present)
//!   repodata/
//!     primary.xml.gz
//!     repomd.xml
//!     repomd.xml.asc
//!   daedalus-release.gpg                         (public key)
//!
//! arch/
//!   <filename>.pkg.tar.zst
//!   <filename>.pkg.tar.zst.asc                   (if present)
//!   daedalus.db.tar.gz
//!   daedalus.db.tar.gz.sig
//!   daedalus.db              (duplicate of daedalus.db.tar.gz for pacman compat)
//!   daedalus-release.gpg                         (public key)
//! ```
//!
//! ## External tool requirements (available in the `ops` / `release-cli` devShell)
//!
//! - `gpg2`        — GPG signing (same key as used by `drt sign`)
//! - `dpkg-deb`    — read control metadata from `.deb` packages
//! - `rpm`         — read header metadata from `.rpm` packages
//! - `bsdtar`      — extract `.PKGINFO` from `.pkg.tar.zst` archives

use anyhow::{Context, Result};
use flate2::Compression;
use flate2::write::GzEncoder;
use sha2::{Digest as _, Sha256};
use std::io::Write as _;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::s3::S3Client;
use crate::sign;

// ── Serve entry point (disk) ──────────────────────────────────────────────────

/// Generate APT/YUM/Arch repo trees under `dest_dir` from packages in
/// `installers_dir`.  Package files are symlinked rather than copied.
/// Signing is attempted if `gpg_user` is Some or `GPG_USER` env is set;
/// failures are silently skipped so the serve command still works without a
/// configured GPG key (callers print unsigned-mode instructions in that case).
///
/// Returns `true` if any Linux packages were found, `false` otherwise.
pub fn write_linux_repos_to_dir(
    installers_dir: &Path,
    gpg_user: Option<&str>,
    dest: &Path,
) -> Result<bool> {
    let pkgs = scan_linux_packages(installers_dir)?;
    if pkgs.is_empty() {
        return Ok(false);
    }

    let gpg_env = std::env::var("GPG_USER").ok();
    let effective_gpg = gpg_user.or(gpg_env.as_deref());

    let now_ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();

    let debs: Vec<_> = pkgs.iter().filter(|p| p.format == Format::Deb).collect();
    let rpms: Vec<_> = pkgs.iter().filter(|p| p.format == Format::Rpm).collect();
    let archs: Vec<_> = pkgs
        .iter()
        .filter(|p| p.format == Format::ArchPkg)
        .collect();

    if !debs.is_empty() {
        write_apt_to_dir(&debs, effective_gpg, now_ts, &dest.join("apt"))?;
    }
    if !rpms.is_empty() {
        write_yum_to_dir(&rpms, effective_gpg, now_ts, &dest.join("yum"))?;
    }
    if !archs.is_empty() {
        write_arch_to_dir(&archs, effective_gpg, &dest.join("arch"))?;
    }

    Ok(true)
}

fn link_into(src: &Path, dest: &Path) -> Result<()> {
    if dest.exists() {
        return Ok(());
    }
    if let Some(parent) = dest.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::os::unix::fs::symlink(src, dest)
        .with_context(|| format!("symlinking {} → {}", src.display(), dest.display()))
}

fn write_apt_to_dir(
    debs: &[&LinuxPackage],
    gpg_user: Option<&str>,
    now_ts: u64,
    apt_dir: &Path,
) -> Result<()> {
    let mut entries: Vec<String> = Vec::new();

    for pkg in debs {
        let info = read_deb_info(&pkg.path)?;
        let hashes = apt_hash_file(&pkg.path)?;
        let pool_rel = format!("pool/main/d/{}/{}", info.package_name, pkg.filename);
        entries.push(build_packages_entry(&info, &pool_rel, &hashes));

        link_into(&pkg.path, &apt_dir.join(&pool_rel))?;

        let sig_path = pkg.path.with_file_name(format!("{}.asc", pkg.filename));
        if sig_path.exists() {
            link_into(&sig_path, &apt_dir.join(format!("{pool_rel}.asc")))?;
        }
    }

    let packages_content = entries.join("\n");
    let packages_bytes = packages_content.as_bytes();
    let packages_gz = gzip_compress(packages_bytes)?;
    let release = build_release_file(packages_bytes, &packages_gz, now_ts);
    let release_bytes = release.as_bytes();

    let dists = apt_dir.join("dists/stable");
    let binary = dists.join("main/binary-amd64");
    std::fs::create_dir_all(&binary)?;
    std::fs::write(binary.join("Packages"), packages_bytes)?;
    std::fs::write(binary.join("Packages.gz"), &packages_gz)?;
    std::fs::write(dists.join("Release"), release_bytes)?;

    if let Ok(sig) = sign::sign_data(release_bytes, gpg_user) {
        std::fs::write(dists.join("Release.gpg"), sig)?;
        if let Ok(inrelease) = sign::clearsign_data(release_bytes, gpg_user) {
            std::fs::write(dists.join("InRelease"), inrelease)?;
        }
    }
    if let Ok(pubkey) = sign::export_public_key(gpg_user) {
        std::fs::write(apt_dir.join("daedalus-release.gpg"), pubkey)?;
    }

    Ok(())
}

fn write_yum_to_dir(
    rpms: &[&LinuxPackage],
    gpg_user: Option<&str>,
    now_ts: u64,
    yum_dir: &Path,
) -> Result<()> {
    let mut rpm_data: Vec<(RpmInfo, String, u64, String)> = Vec::new();

    for pkg in rpms {
        let info = read_rpm_info(&pkg.path)?;
        let (sha256, pkg_size) = sha256_file(&pkg.path)?;
        let pkg_dest = yum_dir.join("packages").join(&pkg.filename);
        link_into(&pkg.path, &pkg_dest)?;

        let sig_path = pkg.path.with_file_name(format!("{}.asc", pkg.filename));
        if sig_path.exists() {
            link_into(&sig_path, &pkg_dest.with_extension("rpm.asc"))?;
        }

        rpm_data.push((info, sha256, pkg_size, pkg.filename.clone()));
    }

    let primary_xml = build_primary_xml(&rpm_data);
    let primary_bytes = primary_xml.as_bytes();
    let primary_gz = gzip_compress(primary_bytes)?;
    let primary_sha256 = sha256_bytes(primary_bytes);
    let primary_gz_sha256 = sha256_bytes(&primary_gz);

    let repomd = build_repomd_xml(
        &primary_gz_sha256,
        primary_gz.len() as u64,
        &primary_sha256,
        primary_bytes.len() as u64,
        now_ts,
    );
    let repomd_bytes = repomd.as_bytes();

    let repodata = yum_dir.join("repodata");
    std::fs::create_dir_all(&repodata)?;
    std::fs::write(repodata.join("primary.xml.gz"), &primary_gz)?;
    std::fs::write(repodata.join("repomd.xml"), repomd_bytes)?;

    if let Ok(sig) = sign::sign_data(repomd_bytes, gpg_user) {
        std::fs::write(repodata.join("repomd.xml.asc"), sig)?;
    }
    if let Ok(pubkey) = sign::export_public_key(gpg_user) {
        std::fs::write(yum_dir.join("daedalus-release.gpg"), pubkey)?;
    }

    Ok(())
}

fn write_arch_to_dir(
    pkgs: &[&LinuxPackage],
    gpg_user: Option<&str>,
    arch_dir: &Path,
) -> Result<()> {
    std::fs::create_dir_all(arch_dir)?;
    let mut db_entries: Vec<(String, String, String)> = Vec::new();

    for pkg in pkgs {
        let info = read_arch_pkginfo(&pkg.path)?;
        let (sha256, csize) = sha256_file(&pkg.path)?;

        link_into(&pkg.path, &arch_dir.join(&pkg.filename))?;

        let sig_path = pkg.path.with_file_name(format!("{}.asc", pkg.filename));
        let pgpsig_b64 = if sig_path.exists() {
            let asc = std::fs::read(&sig_path)?;
            link_into(&sig_path, &arch_dir.join(format!("{}.asc", pkg.filename)))?;
            asc_to_base64(&asc)
        } else {
            None
        };

        db_entries.push(build_arch_db_entry_dir(
            &info,
            &pkg.filename,
            csize,
            &sha256,
            pgpsig_b64.as_deref(),
        ));
    }

    let db_gz = build_arch_db(&db_entries)?;
    std::fs::write(arch_dir.join("daedalus.db.tar.gz"), &db_gz)?;
    std::fs::write(arch_dir.join("daedalus.db"), &db_gz)?;

    if let Ok(sig) = sign::sign_data(&db_gz, gpg_user) {
        std::fs::write(arch_dir.join("daedalus.db.tar.gz.sig"), sig)?;
    }
    if let Ok(pubkey) = sign::export_public_key(gpg_user) {
        std::fs::write(arch_dir.join("daedalus-release.gpg"), pubkey)?;
    }

    Ok(())
}

// ── Public entry point ────────────────────────────────────────────────────────

pub struct PublishLinuxReposOpts<'a> {
    pub installers_dir: &'a Path,
    pub bucket: &'a str,
    pub bucket_url: &'a str,
    pub endpoint_url: Option<String>,
    pub gpg_user: Option<&'a str>,
    pub no_acl: bool,
    pub dry_run: bool,
    pub skip_apt: bool,
    pub skip_yum: bool,
    pub skip_arch: bool,
}

pub async fn cmd_publish_linux_repos(opts: PublishLinuxReposOpts<'_>) -> Result<()> {
    let PublishLinuxReposOpts {
        installers_dir,
        bucket,
        bucket_url,
        endpoint_url,
        gpg_user,
        no_acl,
        dry_run,
        skip_apt,
        skip_yum,
        skip_arch,
    } = opts;

    // ── Resolve GPG user ──────────────────────────────────────────────────────
    let gpg_user_env = std::env::var("GPG_USER").ok();
    let effective_gpg_user: Option<&str> = gpg_user.or(gpg_user_env.as_deref());

    // ── Scan directory ────────────────────────────────────────────────────────
    println!("Scanning {} for Linux packages…", installers_dir.display());
    let pkgs = scan_linux_packages(installers_dir)?;

    if pkgs.is_empty() {
        anyhow::bail!(
            "no Linux packages (.deb, .rpm, .pkg.tar.zst) found in {}",
            installers_dir.display()
        );
    }

    let debs: Vec<_> = pkgs.iter().filter(|p| p.format == Format::Deb).collect();
    let rpms: Vec<_> = pkgs.iter().filter(|p| p.format == Format::Rpm).collect();
    let archs: Vec<_> = pkgs
        .iter()
        .filter(|p| p.format == Format::ArchPkg)
        .collect();

    for p in &pkgs {
        println!("  {} [{}]", p.filename, p.format.label());
    }
    println!();

    if dry_run {
        println!("=== Dry run — no uploads will occur ===");
        if !skip_apt && !debs.is_empty() {
            println!("\n[APT] would publish {} package(s)", debs.len());
        }
        if !skip_yum && !rpms.is_empty() {
            println!("[YUM] would publish {} package(s)", rpms.len());
        }
        if !skip_arch && !archs.is_empty() {
            println!("[Arch] would publish {} package(s)", archs.len());
        }
        return Ok(());
    }

    // ── S3 client ─────────────────────────────────────────────────────────────
    // REPO_ACCESS_KEY_ID / REPO_SECRET_ACCESS_KEY are read explicitly so they don't
    // collide with the AWS_* vars used by `drt release` for the S3 update bucket.
    let r2_key = std::env::var("REPO_ACCESS_KEY_ID").ok();
    let r2_secret = std::env::var("REPO_SECRET_ACCESS_KEY").ok();
    let r2_credentials = match (r2_key, r2_secret) {
        (Some(k), Some(s)) => Some((k, s)),
        _ => None,
    };
    // REPO_ENDPOINT_URL takes priority over --endpoint-url.
    let resolved_endpoint = std::env::var("REPO_ENDPOINT_URL")
        .ok()
        .or(endpoint_url.map(|s| s.to_string()));
    let s3 = S3Client::new(
        bucket.to_string(),
        bucket_url.to_string(),
        resolved_endpoint,
        !no_acl,
        r2_credentials,
    )
    .await?;

    let now_ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();

    // ── APT ───────────────────────────────────────────────────────────────────
    if !skip_apt {
        if debs.is_empty() {
            println!("=== APT: no .deb packages found, skipping ===\n");
        } else {
            publish_apt(&s3, &debs, effective_gpg_user, now_ts).await?;
        }
    }

    // ── YUM ───────────────────────────────────────────────────────────────────
    if !skip_yum {
        if rpms.is_empty() {
            println!("=== YUM: no .rpm packages found, skipping ===\n");
        } else {
            publish_yum(&s3, &rpms, effective_gpg_user, now_ts).await?;
        }
    }

    // ── Arch ──────────────────────────────────────────────────────────────────
    if !skip_arch {
        if archs.is_empty() {
            println!("=== Arch: no .pkg.tar.zst packages found, skipping ===\n");
        } else {
            publish_arch(&s3, &archs, effective_gpg_user, now_ts).await?;
        }
    }

    println!("=== Done ===");
    if !skip_apt && !debs.is_empty() {
        println!();
        println!("APT setup:");
        println!(
            "  curl https://{bucket_url}/apt/daedalus-release.gpg | gpg --dearmor \\\n    | sudo tee /etc/apt/keyrings/daedalus.gpg"
        );
        println!(
            "  echo 'deb [signed-by=/etc/apt/keyrings/daedalus.gpg] https://{bucket_url}/apt stable main' \\\n    | sudo tee /etc/apt/sources.list.d/daedalus.list"
        );
    }
    if !skip_yum && !rpms.is_empty() {
        println!();
        println!("YUM/DNF setup:");
        println!("  sudo rpm --import https://{bucket_url}/yum/daedalus-release.gpg");
        println!(
            "  sudo tee /etc/yum.repos.d/daedalus.repo <<'EOF'\n\
             [daedalus]\n\
             name=Daedalus\n\
             baseurl=https://{bucket_url}/yum\n\
             enabled=1\n\
             gpgcheck=1\n\
             gpgkey=https://{bucket_url}/yum/daedalus-release.gpg\n\
             EOF"
        );
    }
    if !skip_arch && !archs.is_empty() {
        println!();
        println!("Arch setup (add to /etc/pacman.conf):");
        println!("  [daedalus]");
        println!("  Server = https://{bucket_url}/arch");
        println!("  SigLevel = Required DatabaseOptional");
    }

    Ok(())
}

// ── Package scanning ──────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Format {
    Deb,
    Rpm,
    ArchPkg,
}

impl Format {
    fn from_filename(name: &str) -> Option<Self> {
        let lc = name.to_ascii_lowercase();
        if lc.ends_with(".deb") {
            Some(Self::Deb)
        } else if lc.ends_with(".rpm") {
            Some(Self::Rpm)
        } else if lc.ends_with(".pkg.tar.zst") || lc.ends_with(".pkg.tar.gz") {
            Some(Self::ArchPkg)
        } else {
            None
        }
    }

    fn label(self) -> &'static str {
        match self {
            Self::Deb => "DEB",
            Self::Rpm => "RPM",
            Self::ArchPkg => "Arch",
        }
    }
}

pub struct LinuxPackage {
    pub path: PathBuf,
    pub filename: String,
    pub format: Format,
}

pub fn scan_linux_packages(dir: &Path) -> Result<Vec<LinuxPackage>> {
    let mut pkgs: Vec<LinuxPackage> = std::fs::read_dir(dir)
        .with_context(|| format!("reading directory {}", dir.display()))?
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if !path.is_file() {
                return None;
            }
            let filename = path.file_name()?.to_str()?.to_string();
            let format = Format::from_filename(&filename)?;
            Some(LinuxPackage {
                path,
                filename,
                format,
            })
        })
        .collect();
    pkgs.sort_by(|a, b| a.filename.cmp(&b.filename));
    Ok(pkgs)
}

// ── Hashing helpers ───────────────────────────────────────────────────────────

struct AptHashes {
    sha256: String,
    md5: String,
    sha1: String,
    size: u64,
}

fn apt_hash_file(path: &Path) -> Result<AptHashes> {
    use md5::Digest as _;

    let mut file =
        std::fs::File::open(path).with_context(|| format!("opening {}", path.display()))?;
    let mut sha256 = Sha256::new();
    let mut md5_h = md5::Md5::new();
    let mut sha1_h = sha1::Sha1::new();
    let mut size = 0u64;
    let mut buf = [0u8; 65536];
    loop {
        use std::io::Read as _;
        let n = file.read(&mut buf)?;
        if n == 0 {
            break;
        }
        sha256.update(&buf[..n]);
        md5_h.update(&buf[..n]);
        sha1_h.update(&buf[..n]);
        size += n as u64;
    }
    Ok(AptHashes {
        sha256: hex::encode(sha256.finalize()),
        md5: hex::encode(md5_h.finalize()),
        sha1: hex::encode(sha1_h.finalize()),
        size,
    })
}

fn sha256_file(path: &Path) -> Result<(String, u64)> {
    let mut file =
        std::fs::File::open(path).with_context(|| format!("opening {}", path.display()))?;
    let mut h = Sha256::new();
    let mut size = 0u64;
    let mut buf = [0u8; 65536];
    loop {
        use std::io::Read as _;
        let n = file.read(&mut buf)?;
        if n == 0 {
            break;
        }
        h.update(&buf[..n]);
        size += n as u64;
    }
    Ok((hex::encode(h.finalize()), size))
}

fn sha256_bytes(data: &[u8]) -> String {
    hex::encode(Sha256::digest(data))
}

fn gzip_compress(data: &[u8]) -> Result<Vec<u8>> {
    let mut enc = GzEncoder::new(Vec::new(), Compression::default());
    enc.write_all(data)?;
    enc.finish().context("gzip finish")
}

fn xml_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

fn format_rfc2822(ts: u64) -> String {
    // Convert unix timestamp to RFC 2822 format without an external crate.
    const DAYS_IN_MONTH: [u32; 12] = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    // Array starts at Thursday because 1970-01-01 (days=0) was a Thursday.
    const WEEKDAYS: [&str; 7] = ["Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed"];
    const MONTHS: [&str; 12] = [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
    ];

    let h = (ts / 3600) % 24;
    let m = (ts % 3600) / 60;
    let s = ts % 60;

    // Separate total days from seconds-within-day.
    let total_days = (ts / 86400) as u32;
    let dow = WEEKDAYS[total_days as usize % 7];

    // Walk years from 1970 to find current year and remaining days.
    let mut days = total_days;
    let mut year = 1970u32;
    loop {
        let is_leap =
            (year.is_multiple_of(4) && !year.is_multiple_of(100)) || year.is_multiple_of(400);
        let days_in_year = if is_leap { 366 } else { 365 };
        if days < days_in_year {
            break;
        }
        days -= days_in_year;
        year += 1;
    }

    let is_leap = (year.is_multiple_of(4) && !year.is_multiple_of(100)) || year.is_multiple_of(400);
    let mut month = 0usize;
    loop {
        let dim = if month == 1 && is_leap {
            29
        } else {
            DAYS_IN_MONTH[month]
        };
        if days < dim {
            break;
        }
        days -= dim;
        month += 1;
    }
    let day = days + 1;

    format!(
        "{dow}, {day:02} {mon} {year} {h:02}:{m:02}:{s:02} +0000",
        mon = MONTHS[month],
    )
}

// ── APT repository ────────────────────────────────────────────────────────────

struct DebInfo {
    /// All fields from the control file as-is (key: value, multi-line preserved).
    control_blob: String,
    /// Value of the "Package:" field (package name, e.g. "daedalus-mainnet").
    package_name: String,
}

fn read_deb_info(path: &Path) -> Result<DebInfo> {
    let path_str = path
        .to_str()
        .ok_or_else(|| anyhow::anyhow!("non-UTF-8 path: {}", path.display()))?;

    let out = Command::new("dpkg-deb")
        .args(["--field", path_str])
        .output()
        .context("running dpkg-deb --field (is dpkg-deb in PATH?)")?;

    if !out.status.success() {
        anyhow::bail!(
            "dpkg-deb --field exited {}: {}",
            out.status,
            String::from_utf8_lossy(&out.stderr)
        );
    }

    let blob = String::from_utf8(out.stdout).context("dpkg-deb output is not UTF-8")?;

    // Extract Package: field value.
    let package_name = blob
        .lines()
        .find_map(|l| l.strip_prefix("Package: "))
        .map(|v| v.trim().to_string())
        .ok_or_else(|| anyhow::anyhow!("Package: field not found in {}", path.display()))?;

    Ok(DebInfo {
        control_blob: blob.trim_end().to_string(),
        package_name,
    })
}

fn build_packages_entry(info: &DebInfo, pool_path: &str, hashes: &AptHashes) -> String {
    format!(
        "{control}\nFilename: {pool_path}\nSize: {size}\nMD5sum: {md5}\nSHA1: {sha1}\nSHA256: {sha256}\n",
        control = info.control_blob,
        size = hashes.size,
        md5 = hashes.md5,
        sha1 = hashes.sha1,
        sha256 = hashes.sha256,
    )
}

fn build_release_file(packages: &[u8], packages_gz: &[u8], now_ts: u64) -> String {
    #[allow(unused_imports)]
    use md5::Digest as _;
    #[allow(unused_imports)]
    use sha1::Digest as _;

    let pkg_sha256 = sha256_bytes(packages);
    let pkg_gz_sha256 = sha256_bytes(packages_gz);
    let pkg_md5 = hex::encode(md5::Md5::digest(packages));
    let pkg_gz_md5 = hex::encode(md5::Md5::digest(packages_gz));
    let pkg_sha1 = hex::encode(sha1::Sha1::digest(packages));
    let pkg_gz_sha1 = hex::encode(sha1::Sha1::digest(packages_gz));
    let pkg_sz = packages.len();
    let pkg_gz_sz = packages_gz.len();
    let date = format_rfc2822(now_ts);

    // Build with explicit push_str so leading spaces on checksum lines are preserved.
    // Using format! with line-continuations strips leading whitespace, which would
    // break the APT RFC 822 multi-value field format.
    let mut r = String::new();
    r.push_str("Origin: Daedalus\n");
    r.push_str("Label: Daedalus\n");
    r.push_str("Suite: stable\n");
    r.push_str("Codename: stable\n");
    r.push_str(&format!("Date: {date}\n"));
    r.push_str("Architectures: amd64\n");
    r.push_str("Components: main\n");
    r.push_str("Description: Daedalus wallet packages\n");
    r.push_str("MD5Sum:\n");
    r.push_str(&format!(" {pkg_md5} {pkg_sz} main/binary-amd64/Packages\n"));
    r.push_str(&format!(
        " {pkg_gz_md5} {pkg_gz_sz} main/binary-amd64/Packages.gz\n"
    ));
    r.push_str("SHA1:\n");
    r.push_str(&format!(
        " {pkg_sha1} {pkg_sz} main/binary-amd64/Packages\n"
    ));
    r.push_str(&format!(
        " {pkg_gz_sha1} {pkg_gz_sz} main/binary-amd64/Packages.gz\n"
    ));
    r.push_str("SHA256:\n");
    r.push_str(&format!(
        " {pkg_sha256} {pkg_sz} main/binary-amd64/Packages\n"
    ));
    r.push_str(&format!(
        " {pkg_gz_sha256} {pkg_gz_sz} main/binary-amd64/Packages.gz\n"
    ));
    r
}

async fn publish_apt(
    s3: &S3Client,
    debs: &[&LinuxPackage],
    gpg_user: Option<&str>,
    now_ts: u64,
) -> Result<()> {
    println!("=== APT repository ===");

    // ── Read metadata and compute hashes ─────────────────────────────────────
    let mut entries: Vec<String> = Vec::new();
    for pkg in debs {
        print!("  [DEB] {} … ", pkg.filename);
        let info = read_deb_info(&pkg.path)?;
        let hashes = apt_hash_file(&pkg.path)?;
        let pool_path = format!("pool/main/d/{}/{}", info.package_name, pkg.filename);
        entries.push(build_packages_entry(&info, &pool_path, &hashes));
        println!("{}", info.package_name);

        // Upload package
        let s3_key = format!("apt/{pool_path}");
        println!("    → {s3_key}");
        s3.upload_bytes(
            &s3_key,
            &std::fs::read(&pkg.path)?,
            "application/octet-stream",
            None,
        )
        .await?;

        // Upload .asc if present
        let sig_path = pkg.path.with_file_name(format!("{}.asc", pkg.filename));
        if sig_path.exists() {
            let sig_key = format!("{s3_key}.asc");
            s3.upload_bytes(&sig_key, &std::fs::read(&sig_path)?, "text/plain", None)
                .await?;
        }
    }

    // ── Build Packages and Packages.gz ────────────────────────────────────────
    println!("  Generating Packages index…");
    let packages_content: String = entries.join("\n");
    let packages_bytes = packages_content.as_bytes();
    let packages_gz = gzip_compress(packages_bytes)?;

    // ── Build Release ─────────────────────────────────────────────────────────
    println!("  Generating Release…");
    let release = build_release_file(packages_bytes, &packages_gz, now_ts);
    let release_bytes = release.as_bytes();

    // ── GPG sign ──────────────────────────────────────────────────────────────
    println!("  Signing Release.gpg…");
    let release_gpg =
        sign::sign_data(release_bytes, gpg_user).context("signing apt/dists/stable/Release")?;
    println!("  Signing InRelease (clearsign)…");
    let in_release = sign::clearsign_data(release_bytes, gpg_user)
        .context("clearsigning apt/dists/stable/InRelease")?;

    // ── Upload ────────────────────────────────────────────────────────────────
    s3.upload_bytes(
        "apt/dists/stable/main/binary-amd64/Packages",
        packages_bytes,
        "text/plain",
        Some("no-store"),
    )
    .await?;
    s3.upload_bytes(
        "apt/dists/stable/main/binary-amd64/Packages.gz",
        &packages_gz,
        "application/gzip",
        Some("no-store"),
    )
    .await?;
    s3.upload_bytes(
        "apt/dists/stable/Release",
        release_bytes,
        "text/plain",
        Some("no-store"),
    )
    .await?;
    s3.upload_bytes(
        "apt/dists/stable/Release.gpg",
        &release_gpg,
        "text/plain",
        Some("no-store"),
    )
    .await?;
    s3.upload_bytes(
        "apt/dists/stable/InRelease",
        &in_release,
        "text/plain",
        Some("no-store"),
    )
    .await?;

    // ── Public key ────────────────────────────────────────────────────────────
    println!("  Uploading public key…");
    let pubkey = sign::export_public_key(gpg_user)?;
    s3.upload_bytes("apt/daedalus-release.gpg", &pubkey, "text/plain", None)
        .await?;

    println!();
    Ok(())
}

// ── YUM repository ────────────────────────────────────────────────────────────

struct RpmInfo {
    name: String,
    version: String,
    release: String,
    epoch: String,
    arch: String,
    installed_size: u64,
    build_time: u64,
    summary: String,
    description: String,
    license: String,
    url: String,
    packager: String,
}

fn rpm_query(path: &Path, fmt: &str) -> Result<String> {
    let path_str = path
        .to_str()
        .ok_or_else(|| anyhow::anyhow!("non-UTF-8 path"))?;
    let out = Command::new("rpm")
        .args(["--qf", fmt, "-qp", path_str])
        .output()
        .context("running rpm --qf (is rpm in PATH?)")?;
    if !out.status.success() {
        anyhow::bail!(
            "rpm --qf exited {}: {}",
            out.status,
            String::from_utf8_lossy(&out.stderr)
        );
    }
    String::from_utf8(out.stdout).context("rpm output is not UTF-8")
}

fn read_rpm_info(path: &Path) -> Result<RpmInfo> {
    // Single call for all scalar fields.
    let scalar = rpm_query(
        path,
        "%{NAME}\n%{VERSION}\n%{RELEASE}\n%{EPOCH}\n%{ARCH}\n%{SIZE}\n%{BUILDTIME}\n%{SUMMARY}\n%{LICENSE}\n%{URL}\n%{PACKAGER}",
    )?;
    let mut lines = scalar.lines();
    macro_rules! next {
        ($field:literal) => {
            lines
                .next()
                .ok_or_else(|| anyhow::anyhow!("rpm output missing {} field", $field))?
                .to_string()
        };
    }
    let name = next!("NAME");
    let version = next!("VERSION");
    let release = next!("RELEASE");
    let epoch_raw = next!("EPOCH");
    let epoch = if epoch_raw == "(none)" {
        "0".to_string()
    } else {
        epoch_raw
    };
    let arch = next!("ARCH");
    let size_raw = next!("SIZE");
    let installed_size: u64 = size_raw
        .parse()
        .with_context(|| format!("parsing RPM SIZE: {size_raw:?}"))?;
    let bt_raw = next!("BUILDTIME");
    let build_time: u64 = bt_raw
        .parse()
        .with_context(|| format!("parsing RPM BUILDTIME: {bt_raw:?}"))?;
    let summary = next!("SUMMARY");
    let license = next!("LICENSE");
    let url = next!("URL");
    let packager = next!("PACKAGER");

    // Separate call for description (may contain newlines).
    let description = rpm_query(path, "%{DESCRIPTION}")?;

    Ok(RpmInfo {
        name,
        version,
        release,
        epoch,
        arch,
        installed_size,
        build_time,
        summary,
        description,
        license,
        url,
        packager,
    })
}

fn build_primary_xml(rpms: &[(RpmInfo, String, u64, String)]) -> String {
    let mut xml = format!(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
         <metadata xmlns=\"http://linux.duke.edu/metadata/common\" \
         xmlns:rpm=\"http://linux.duke.edu/metadata/rpm\" \
         packages=\"{}\">\n",
        rpms.len()
    );
    for (info, sha256, pkg_size, filename) in rpms {
        let loc = format!("packages/{filename}");
        xml.push_str(&format!(
            "<package type=\"rpm\">\n\
               <name>{name}</name>\n\
               <arch>{arch}</arch>\n\
               <version epoch=\"{epoch}\" ver=\"{ver}\" rel=\"{rel}\"/>\n\
               <checksum type=\"sha256\" pkgid=\"YES\">{sha256}</checksum>\n\
               <summary>{summary}</summary>\n\
               <description>{desc}</description>\n\
               <packager>{packager}</packager>\n\
               <url>{url}</url>\n\
               <time file=\"{ts}\" build=\"{bt}\"/>\n\
               <size package=\"{pkg_size}\" installed=\"{inst}\" archive=\"{inst}\"/>\n\
               <location href=\"{loc}\"/>\n\
               <format>\n\
                 <rpm:license>{license}</rpm:license>\n\
                 <rpm:vendor/>\n\
                 <rpm:group>Unspecified</rpm:group>\n\
                 <rpm:buildhost>nix-build</rpm:buildhost>\n\
                 <rpm:sourcerpm/>\n\
                 <rpm:provides>\n\
                   <rpm:entry name=\"{name}\" flags=\"EQ\" epoch=\"{epoch}\" \
                    ver=\"{ver}\" rel=\"{rel}\"/>\n\
                 </rpm:provides>\n\
               </format>\n\
             </package>\n",
            name = xml_escape(&info.name),
            arch = xml_escape(&info.arch),
            epoch = xml_escape(&info.epoch),
            ver = xml_escape(&info.version),
            rel = xml_escape(&info.release),
            summary = xml_escape(&info.summary),
            desc = xml_escape(&info.description),
            packager = xml_escape(&info.packager),
            url = xml_escape(&info.url),
            ts = info.build_time,
            bt = info.build_time,
            inst = info.installed_size,
            license = xml_escape(&info.license),
            loc = xml_escape(&loc),
        ));
    }
    xml.push_str("</metadata>\n");
    xml
}

fn build_repomd_xml(
    primary_gz_sha256: &str,
    primary_gz_size: u64,
    primary_sha256: &str,
    primary_size: u64,
    ts: u64,
) -> String {
    format!(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
         <repomd xmlns=\"http://linux.duke.edu/metadata/repo\" \
         xmlns:rpm=\"http://linux.duke.edu/metadata/rpm\">\n\
           <revision>{ts}</revision>\n\
           <data type=\"primary\">\n\
             <checksum type=\"sha256\">{gz_sha}</checksum>\n\
             <open-checksum type=\"sha256\">{open_sha}</open-checksum>\n\
             <location href=\"repodata/primary.xml.gz\"/>\n\
             <timestamp>{ts}</timestamp>\n\
             <size>{gz_sz}</size>\n\
             <open-size>{open_sz}</open-size>\n\
           </data>\n\
         </repomd>\n",
        gz_sha = primary_gz_sha256,
        open_sha = primary_sha256,
        gz_sz = primary_gz_size,
        open_sz = primary_size,
    )
}

async fn publish_yum(
    s3: &S3Client,
    rpms: &[&LinuxPackage],
    gpg_user: Option<&str>,
    now_ts: u64,
) -> Result<()> {
    println!("=== YUM repository ===");

    let mut rpm_data: Vec<(RpmInfo, String, u64, String)> = Vec::new();
    for pkg in rpms {
        print!("  [RPM] {} … ", pkg.filename);
        let info = read_rpm_info(&pkg.path)?;
        let (sha256, pkg_size) = sha256_file(&pkg.path)?;
        println!("{}-{}-{}", info.name, info.version, info.release);

        // Upload package
        let s3_key = format!("yum/packages/{}", pkg.filename);
        println!("    → {s3_key}");
        s3.upload_bytes(
            &s3_key,
            &std::fs::read(&pkg.path)?,
            "application/octet-stream",
            None,
        )
        .await?;

        // Upload .asc if present
        let sig_path = pkg.path.with_file_name(format!("{}.asc", pkg.filename));
        if sig_path.exists() {
            s3.upload_bytes(
                &format!("{s3_key}.asc"),
                &std::fs::read(&sig_path)?,
                "text/plain",
                None,
            )
            .await?;
        }

        rpm_data.push((info, sha256, pkg_size, pkg.filename.clone()));
    }

    // ── Build primary.xml ─────────────────────────────────────────────────────
    println!("  Generating primary.xml…");
    let primary_xml = build_primary_xml(&rpm_data);
    let primary_bytes = primary_xml.as_bytes();
    let primary_gz = gzip_compress(primary_bytes)?;

    let primary_sha256 = sha256_bytes(primary_bytes);
    let primary_gz_sha256 = sha256_bytes(&primary_gz);

    // ── Build repomd.xml ──────────────────────────────────────────────────────
    println!("  Generating repomd.xml…");
    let repomd = build_repomd_xml(
        &primary_gz_sha256,
        primary_gz.len() as u64,
        &primary_sha256,
        primary_bytes.len() as u64,
        now_ts,
    );
    let repomd_bytes = repomd.as_bytes();

    // ── GPG sign repomd.xml ───────────────────────────────────────────────────
    println!("  Signing repomd.xml.asc…");
    let repomd_asc =
        sign::sign_data(repomd_bytes, gpg_user).context("signing yum/repodata/repomd.xml")?;

    // ── Upload ────────────────────────────────────────────────────────────────
    s3.upload_bytes(
        "yum/repodata/primary.xml.gz",
        &primary_gz,
        "application/gzip",
        Some("no-store"),
    )
    .await?;
    s3.upload_bytes(
        "yum/repodata/repomd.xml",
        repomd_bytes,
        "application/xml",
        Some("no-store"),
    )
    .await?;
    s3.upload_bytes(
        "yum/repodata/repomd.xml.asc",
        &repomd_asc,
        "text/plain",
        Some("no-store"),
    )
    .await?;

    // ── Public key ────────────────────────────────────────────────────────────
    println!("  Uploading public key…");
    let pubkey = sign::export_public_key(gpg_user)?;
    s3.upload_bytes("yum/daedalus-release.gpg", &pubkey, "text/plain", None)
        .await?;

    println!();
    Ok(())
}

// ── Arch repository ───────────────────────────────────────────────────────────

struct PkgInfo {
    pkgname: String,
    pkgver: String,
    pkgdesc: String,
    url: String,
    builddate: u64,
    packager: String,
    size: u64,
    arch: String,
    license: String,
    depends: Vec<String>,
}

fn read_arch_pkginfo(path: &Path) -> Result<PkgInfo> {
    let path_str = path
        .to_str()
        .ok_or_else(|| anyhow::anyhow!("non-UTF-8 path"))?;

    // bsdtar from libarchive handles .pkg.tar.zst natively.
    let out = Command::new("bsdtar")
        .args(["-xOf", path_str, ".PKGINFO"])
        .output()
        .context("running bsdtar (is libarchive/bsdtar in PATH?)")?;

    if !out.status.success() {
        anyhow::bail!(
            "bsdtar exited {}: {}",
            out.status,
            String::from_utf8_lossy(&out.stderr)
        );
    }

    let text = String::from_utf8(out.stdout).context("bsdtar output is not UTF-8")?;

    let mut info = PkgInfo {
        pkgname: String::new(),
        pkgver: String::new(),
        pkgdesc: String::new(),
        url: String::new(),
        builddate: 0,
        packager: String::new(),
        size: 0,
        arch: String::new(),
        license: String::new(),
        depends: Vec::new(),
    };

    for line in text.lines() {
        if let Some(rest) = line.strip_prefix("# ") {
            // comment lines like "# Generated by makepkg..."
            let _ = rest;
            continue;
        }
        if let Some((key, val)) = line.split_once(" = ") {
            match key.trim() {
                "pkgname" => info.pkgname = val.trim().to_string(),
                "pkgver" => info.pkgver = val.trim().to_string(),
                "pkgdesc" => info.pkgdesc = val.trim().to_string(),
                "url" => info.url = val.trim().to_string(),
                "builddate" => {
                    info.builddate = val.trim().parse().unwrap_or(0);
                }
                "packager" => info.packager = val.trim().to_string(),
                "size" => {
                    info.size = val.trim().parse().unwrap_or(0);
                }
                "arch" => info.arch = val.trim().to_string(),
                "license" => info.license = val.trim().to_string(),
                "depend" => info.depends.push(val.trim().to_string()),
                _ => {}
            }
        }
    }

    if info.pkgname.is_empty() {
        anyhow::bail!("pkgname not found in .PKGINFO of {}", path.display());
    }

    Ok(info)
}

fn build_arch_db_entry_dir(
    info: &PkgInfo,
    filename: &str,
    csize: u64,
    sha256: &str,
    pgpsig_b64: Option<&str>,
) -> (String, String, String) {
    // Directory name in the tar: "<pkgname>-<pkgver>/"
    let dir = format!("{}-{}", info.pkgname, info.pkgver);

    let mut desc = format!(
        "%FILENAME%\n{filename}\n\n\
         %NAME%\n{name}\n\n\
         %BASE%\n{name}\n\n\
         %VERSION%\n{ver}\n\n\
         %DESC%\n{desc}\n\n\
         %CSIZE%\n{csize}\n\n\
         %ISIZE%\n{isize}\n\n\
         %SHA256SUM%\n{sha256}\n\n\
         %URL%\n{url}\n\n\
         %LICENSE%\n{license}\n\n\
         %ARCH%\n{arch}\n\n\
         %BUILDDATE%\n{builddate}\n\n\
         %PACKAGER%\n{packager}\n\n",
        name = info.pkgname,
        ver = info.pkgver,
        desc = info.pkgdesc,
        isize = info.size,
        url = info.url,
        license = info.license,
        arch = info.arch,
        builddate = info.builddate,
        packager = info.packager,
    );
    if let Some(sig) = pgpsig_b64 {
        desc.push_str(&format!("%PGPSIG%\n{sig}\n\n"));
    }

    let mut depends = String::new();
    if !info.depends.is_empty() {
        depends.push_str("%DEPENDS%\n");
        for dep in &info.depends {
            depends.push_str(dep);
            depends.push('\n');
        }
        depends.push('\n');
    }

    (dir, desc, depends)
}

fn build_arch_db(entries: &[(String, String, String)]) -> Result<Vec<u8>> {
    // Build a gzip-compressed tar with <pkgname-pkgver>/desc and
    // <pkgname-pkgver>/depends entries.
    let mut archive = tar::Builder::new(GzEncoder::new(Vec::new(), Compression::default()));

    let mtime = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();

    for (dir, desc, depends) in entries {
        // Directory entry
        let dir_path = format!("{dir}/");
        let mut hdr = tar::Header::new_gnu();
        hdr.set_entry_type(tar::EntryType::Directory);
        hdr.set_path(&dir_path)?;
        hdr.set_size(0);
        hdr.set_uid(0);
        hdr.set_gid(0);
        hdr.set_mode(0o755);
        hdr.set_mtime(mtime);
        hdr.set_cksum();
        archive.append(&hdr, std::io::empty())?;

        // desc file
        let desc_bytes = desc.as_bytes();
        let mut hdr = tar::Header::new_gnu();
        hdr.set_path(format!("{dir}/desc"))?;
        hdr.set_size(desc_bytes.len() as u64);
        hdr.set_uid(0);
        hdr.set_gid(0);
        hdr.set_mode(0o644);
        hdr.set_mtime(mtime);
        hdr.set_cksum();
        archive.append(&hdr, desc_bytes)?;

        // depends file (may be empty)
        if !depends.is_empty() {
            let dep_bytes = depends.as_bytes();
            let mut hdr = tar::Header::new_gnu();
            hdr.set_path(format!("{dir}/depends"))?;
            hdr.set_size(dep_bytes.len() as u64);
            hdr.set_uid(0);
            hdr.set_gid(0);
            hdr.set_mode(0o644);
            hdr.set_mtime(mtime);
            hdr.set_cksum();
            archive.append(&hdr, dep_bytes)?;
        }
    }

    let gz = archive.into_inner()?.finish()?;
    Ok(gz)
}

fn asc_to_base64(asc: &[u8]) -> Option<String> {
    // Strip PGP armor headers/footers, returning just the base64 body.
    let text = std::str::from_utf8(asc).ok()?;
    let mut in_body = false;
    let mut base64 = String::new();
    for line in text.lines() {
        if line.starts_with("-----BEGIN PGP SIGNATURE") {
            in_body = false; // skip header line
            continue;
        }
        if line.starts_with("-----END PGP SIGNATURE") {
            break;
        }
        if in_body && !line.is_empty() {
            // The CRC24 armor checksum appears as "=xxxx" — skip it; it's not
            // part of the binary signature data.
            if !line.starts_with('=') {
                base64.push_str(line);
            }
        }
        if !in_body && line.is_empty() {
            in_body = true; // blank line separates armor headers from base64 body
        }
    }
    if base64.is_empty() {
        None
    } else {
        Some(base64)
    }
}

async fn publish_arch(
    s3: &S3Client,
    pkgs: &[&LinuxPackage],
    gpg_user: Option<&str>,
    now_ts: u64,
) -> Result<()> {
    let _ = now_ts;
    println!("=== Arch repository ===");

    let mut db_entries: Vec<(String, String, String)> = Vec::new();

    for pkg in pkgs {
        print!("  [Arch] {} … ", pkg.filename);
        let info = read_arch_pkginfo(&pkg.path)?;
        let (sha256, csize) = sha256_file(&pkg.path)?;
        println!("{} {}", info.pkgname, info.pkgver);

        // Upload package
        let s3_key = format!("arch/{}", pkg.filename);
        println!("    → {s3_key}");
        s3.upload_bytes(
            &s3_key,
            &std::fs::read(&pkg.path)?,
            "application/octet-stream",
            None,
        )
        .await?;

        // Upload .asc if present; extract base64 body for the DB entry.
        let sig_path = pkg.path.with_file_name(format!("{}.asc", pkg.filename));
        let pgpsig_b64 = if sig_path.exists() {
            let asc = std::fs::read(&sig_path)?;
            s3.upload_bytes(&format!("{s3_key}.asc"), &asc, "text/plain", None)
                .await?;
            asc_to_base64(&asc)
        } else {
            None
        };

        let entry =
            build_arch_db_entry_dir(&info, &pkg.filename, csize, &sha256, pgpsig_b64.as_deref());
        db_entries.push(entry);
    }

    // ── Build daedalus.db.tar.gz ──────────────────────────────────────────────
    println!("  Generating daedalus.db.tar.gz…");
    let db_gz = build_arch_db(&db_entries)?;

    // ── Sign ──────────────────────────────────────────────────────────────────
    println!("  Signing daedalus.db.tar.gz.sig…");
    let db_sig = sign::sign_data(&db_gz, gpg_user).context("signing arch/daedalus.db.tar.gz")?;

    // ── Upload ────────────────────────────────────────────────────────────────
    s3.upload_bytes(
        "arch/daedalus.db.tar.gz",
        &db_gz,
        "application/gzip",
        Some("no-store"),
    )
    .await?;
    s3.upload_bytes(
        "arch/daedalus.db.tar.gz.sig",
        &db_sig,
        "text/plain",
        Some("no-store"),
    )
    .await?;
    // pacman looks for <repo>.db as a symlink or copy; upload at both keys.
    s3.upload_bytes(
        "arch/daedalus.db",
        &db_gz,
        "application/gzip",
        Some("no-store"),
    )
    .await?;

    // ── Public key ────────────────────────────────────────────────────────────
    println!("  Uploading public key…");
    let pubkey = sign::export_public_key(gpg_user)?;
    s3.upload_bytes("arch/daedalus-release.gpg", &pubkey, "text/plain", None)
        .await?;

    println!();
    Ok(())
}
