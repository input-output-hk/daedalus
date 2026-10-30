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
//!   <filename>.pkg.tar.zst.sig                   (verified binary signature)
//!   daedalus.db.tar.gz
//!   daedalus.db.tar.gz.sig
//!   daedalus.db              (duplicate of daedalus.db.tar.gz for pacman compat)
//!   daedalus.db.sig
//!   daedalus.files.tar.gz
//!   daedalus.files.tar.gz.sig
//!   daedalus.files
//!   daedalus.files.sig
//!   daedalus-release.gpg                         (public key)
//! ```
//!
//! ## External tool requirements (available in the `ops` / `release-cli` devShell)
//!
//! - `gpg2`        — GPG signing (same key as used by `drt sign`)
//! - `dpkg-deb`    — read control metadata from `.deb` packages
//! - `rpm`         — read header metadata from `.rpm` packages
//! - `bsdtar`      — extract `.PKGINFO` from `.pkg.tar.zst` archives
//! - `repo-add`    — generate Arch package and file databases

use anyhow::{Context, Result};
use flate2::Compression;
use flate2::write::GzEncoder;
use sha2::{Digest as _, Sha256};
use std::io::Write as _;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::arch_repo::{self, Signing};
use crate::sign;

// ── Serve entry point (disk) ──────────────────────────────────────────────────

/// Generate APT/YUM/Arch repo trees under `dest_dir` from packages in
/// `installers_dir`.  Package files are symlinked rather than copied.
/// Arch signing is required when `gpg_user` or `GPG_USER` is set; otherwise
/// Arch repositories are explicitly unsigned for local development. APT/YUM
/// retain their existing best-effort local signing behavior.
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
        write_apt_to_dir(&debs, effective_gpg, now_ts, &dest.join("apt"), false)?;
    }
    if !rpms.is_empty() {
        write_yum_to_dir(&rpms, effective_gpg, now_ts, &dest.join("yum"), false)?;
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
    std::os::unix::fs::symlink(std::fs::canonicalize(src)?, dest)
        .with_context(|| format!("symlinking {} → {}", src.display(), dest.display()))
}

fn write_apt_to_dir(
    debs: &[&LinuxPackage],
    gpg_user: Option<&str>,
    now_ts: u64,
    apt_dir: &Path,
    require_signing: bool,
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

    write_signature(
        &dists.join("Release.gpg"),
        sign::sign_data(release_bytes, gpg_user),
        require_signing,
    )?;
    write_signature(
        &dists.join("InRelease"),
        sign::clearsign_data(release_bytes, gpg_user),
        require_signing,
    )?;
    write_signature(
        &apt_dir.join("daedalus-release.gpg"),
        sign::export_public_key(gpg_user),
        require_signing,
    )?;

    Ok(())
}

fn write_yum_to_dir(
    rpms: &[&LinuxPackage],
    gpg_user: Option<&str>,
    now_ts: u64,
    yum_dir: &Path,
    require_signing: bool,
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

    write_signature(
        &repodata.join("repomd.xml.asc"),
        sign::sign_data(repomd_bytes, gpg_user),
        require_signing,
    )?;
    write_signature(
        &yum_dir.join("daedalus-release.gpg"),
        sign::export_public_key(gpg_user),
        require_signing,
    )?;

    Ok(())
}

fn write_arch_to_dir(
    pkgs: &[&LinuxPackage],
    gpg_user: Option<&str>,
    arch_dir: &Path,
) -> Result<()> {
    let packages: Vec<_> = pkgs
        .iter()
        .map(|pkg| (pkg.path.as_path(), pkg.filename.as_str()))
        .collect();
    let signing = gpg_user.map_or(Signing::Unsigned, Signing::Local);
    arch_repo::prepare(&packages, signing)?.write_to(arch_dir)
}

// ── Public entry point ────────────────────────────────────────────────────────

pub struct PublishLinuxReposOpts<'a> {
    pub installers_dir: &'a Path,
    pub state_dir: &'a Path,
    pub initialize: bool,
    pub gc_grace_days: u64,
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

fn write_signature(path: &Path, result: Result<Vec<u8>>, required: bool) -> Result<()> {
    match result {
        Ok(bytes) => {
            std::fs::write(path, bytes).with_context(|| format!("writing {}", path.display()))
        }
        Err(error) if required => Err(error).with_context(|| format!("signing {}", path.display())),
        Err(_) => Ok(()),
    }
}

/// Production preparation is fail-closed and completes all signing before S3.
pub(crate) fn prepare_publication(
    packages: &[LinuxPackage],
    gpg_user: Option<&str>,
    dest: &Path,
    now: u64,
) -> Result<()> {
    let debs: Vec<_> = packages
        .iter()
        .filter(|p| p.format == Format::Deb)
        .collect();
    let rpms: Vec<_> = packages
        .iter()
        .filter(|p| p.format == Format::Rpm)
        .collect();
    let archs: Vec<_> = packages
        .iter()
        .filter(|p| p.format == Format::ArchPkg)
        .map(|p| (p.path.as_path(), p.filename.as_str()))
        .collect();
    if !debs.is_empty() {
        write_apt_to_dir(&debs, gpg_user, now, &dest.join("apt"), true)?;
    }
    if !rpms.is_empty() {
        write_yum_to_dir(&rpms, gpg_user, now, &dest.join("yum"), true)?;
    }
    if !archs.is_empty() {
        arch_repo::prepare(&archs, Signing::Iohk(gpg_user))?.write_to(&dest.join("arch"))?;
    }
    Ok(())
}

pub async fn cmd_publish_linux_repos(opts: PublishLinuxReposOpts<'_>) -> Result<()> {
    crate::linux_repo_publish::publish(opts).await
}

// ── Package scanning ──────────────────────────────────────────────────────────

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
pub enum Format {
    Deb,
    Rpm,
    ArchPkg,
}

impl Format {
    pub(crate) fn from_filename(name: &str) -> Option<Self> {
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
}

pub struct LinuxPackage {
    pub path: PathBuf,
    pub filename: String,
    pub format: Format,
}

pub fn scan_linux_packages(dir: &Path) -> Result<Vec<LinuxPackage>> {
    let mut pkgs = Vec::new();
    for entry in
        std::fs::read_dir(dir).with_context(|| format!("reading directory {}", dir.display()))?
    {
        let path = entry?.path();
        if !path.is_file() {
            continue;
        }
        let filename = path
            .file_name()
            .and_then(|name| name.to_str())
            .context("non-UTF-8 incoming filename")?
            .to_owned();
        if let Some(format) = Format::from_filename(&filename) {
            pkgs.push(LinuxPackage {
                path,
                filename,
                format,
            });
        }
    }
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

pub(crate) fn sha256_file(path: &Path) -> Result<(String, u64)> {
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
