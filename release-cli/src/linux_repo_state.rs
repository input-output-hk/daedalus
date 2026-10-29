//! Deployment-host inventory and retention. Runtime environments inside a pilot
//! package do not participate in selection: native package identity is the unit.

use anyhow::{Context, Result, bail, ensure};
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::fs::{File, OpenOptions};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::linux_repo_cmd::{Format, LinuxPackage, sha256_file};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Target {
    pub bucket: String,
    pub endpoint: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Identity {
    pub format: Format,
    pub name: String,
    pub version: String,
    pub arch: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Artifact {
    pub filename: String,
    pub sha256: String,
    pub size: u64,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Package {
    pub identity: Identity,
    /// Primary package first, then any adjacent signature files.
    pub artifacts: Vec<Artifact>,
    pub retired_at: Option<u64>,
}

impl Package {
    pub fn filename(&self) -> &str {
        &self.artifacts[0].filename
    }

    pub fn remote_key(&self) -> String {
        let filename = self.filename();
        match self.identity.format {
            Format::Deb => format!("apt/pool/main/d/{}/{filename}", self.identity.name),
            Format::Rpm => format!("yum/packages/{filename}"),
            Format::ArchPkg => format!("arch/{filename}"),
        }
    }

    pub fn remote_keys(&self) -> Vec<String> {
        let key = self.remote_key();
        let mut keys = vec![key.clone(), format!("{key}.asc")];
        if self.identity.format == Format::ArchPkg {
            keys.push(format!("{key}.sig"));
        }
        keys
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct Manifest {
    schema: u32,
    target: Target,
    packages: Vec<Package>,
}

pub struct Inventory {
    root: PathBuf,
    manifest: Manifest,
    // Held for the entire import/publication/GC transaction; OS releases on exit.
    lock: Option<File>,
    dry_run: bool,
}

#[derive(Debug)]
pub struct Selection {
    /// Oldest to newest within each identity group, for repo-add replacement.
    pub retained: Vec<String>,
    pub retired: Vec<String>,
    pub delete: Vec<String>,
}

impl Drop for Inventory {
    fn drop(&mut self) {
        // Explicit unlock also releases an open-file-description lock while a
        // concurrently spawned child still holds a pre-exec duplicate descriptor.
        if let Some(lock) = &self.lock {
            if let Err(error) = lock.unlock() {
                tracing::warn!(%error, "releasing repository publication lock");
            }
        }
    }
}

impl Inventory {
    pub fn open(root: &Path, target: Target, initialize: bool, dry_run: bool) -> Result<Self> {
        let root = std::path::absolute(root)?;
        let lock = if dry_run && !root.exists() {
            None
        } else {
            if !dry_run {
                std::fs::create_dir_all(&root)?;
            }
            let file = OpenOptions::new()
                .read(true)
                .write(!dry_run)
                .create(!dry_run)
                .truncate(false)
                .open(root.join("publish.lock"))
                .context("opening repository publication lock")?;
            file.try_lock()
                .context("repository publication is already locked")?;
            Some(file)
        };
        let path = root.join("inventory.json");
        let manifest = match std::fs::read(&path) {
            Ok(bytes) => {
                ensure!(
                    !initialize,
                    "inventory already initialized; omit --initialize"
                );
                let manifest: Manifest = serde_json::from_slice(&bytes)
                    .context("parsing repository inventory; restore a backup if corrupt")?;
                ensure!(
                    manifest.schema == 1,
                    "unsupported repository inventory schema"
                );
                ensure!(
                    manifest.target == target,
                    "inventory belongs to another bucket/prefix or endpoint"
                );
                manifest
            }
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                ensure!(
                    initialize,
                    "new inventory requires --initialize and a complete existing package set"
                );
                Manifest {
                    schema: 1,
                    target,
                    packages: Vec::new(),
                }
            }
            Err(error) => return Err(error).context("reading repository inventory"),
        };
        let inventory = Self {
            root,
            manifest,
            lock,
            dry_run,
        };
        inventory.validate()?;
        Ok(inventory)
    }

    pub fn package_path(&self, filename: &str) -> PathBuf {
        self.root.join("packages").join(filename)
    }

    pub fn packages(&self) -> &[Package] {
        &self.manifest.packages
    }

    fn validate(&self) -> Result<()> {
        let mut filenames = BTreeSet::new();
        for package in &self.manifest.packages {
            validate_identity(&package.identity)?;
            ensure!(
                !package.artifacts.is_empty(),
                "inventory package has no artifacts"
            );
            let filename = package.filename();
            ensure!(
                Format::from_filename(filename) == Some(package.identity.format),
                "package format mismatch"
            );
            for (index, artifact) in package.artifacts.iter().enumerate() {
                validate_filename(&artifact.filename)?;
                ensure!(
                    index == 0
                        || artifact.filename == format!("{filename}.asc")
                        || artifact.filename == format!("{filename}.sig"),
                    "invalid adjacent signature name"
                );
                ensure!(
                    filenames.insert(&artifact.filename),
                    "duplicate inventory artifact"
                );
                ensure!(
                    artifact.sha256.len() == 64
                        && artifact.sha256.bytes().all(|b| b.is_ascii_hexdigit()),
                    "invalid inventory checksum"
                );
                let path = self.package_path(&artifact.filename);
                // An interrupted collection can leave retired local files absent.
                if package.retired_at.is_some() && !path.try_exists()? {
                    continue;
                }
                let (hash, size) = sha256_file(&path)
                    .with_context(|| format!("checking inventory artifact {}", path.display()))?;
                ensure!(
                    hash == artifact.sha256 && size == artifact.size,
                    "inventory artifact changed: {}",
                    artifact.filename
                );
            }
        }
        Ok(())
    }

    /// Snapshot incoming bytes before metadata extraction. Dry runs stage only in
    /// a temporary directory and leave the persistent inventory untouched.
    pub fn import(&mut self, packages: &[LinuxPackage]) -> Result<()> {
        let stage = tempfile::tempdir()?;
        let mut next = self.manifest.clone();
        for package in packages {
            validate_filename(&package.filename)?;
            let mut artifacts = Vec::new();
            for suffix in ["", ".asc", ".sig"] {
                let filename = format!("{}{suffix}", package.filename);
                let source = package.path.with_file_name(&filename);
                if !suffix.is_empty() && !source.try_exists()? {
                    continue;
                }
                let destination = stage.path().join(&filename);
                std::fs::copy(&source, &destination)?;
                let (sha256, size) = sha256_file(&destination)?;
                artifacts.push(Artifact {
                    filename,
                    sha256,
                    size,
                });
            }
            let identity = read_identity(&stage.path().join(&package.filename), package.format)?;
            validate_identity(&identity)?;
            for old in &next.packages {
                let same_filename = old.filename() == package.filename;
                let same_group = old.identity.format == identity.format
                    && old.identity.name == identity.name
                    && old.identity.arch == identity.arch;
                let same_version = same_group
                    && compare_versions(identity.format, &old.identity.version, &identity.version)?
                        == Ordering::Equal;
                if same_filename || same_version {
                    ensure!(
                        same_filename
                            && old.identity == identity
                            && old.artifacts[0].sha256 == artifacts[0].sha256,
                        "conflicting immutable package identity or filename: {}",
                        package.filename
                    );
                }
            }
            if let Some(old) = next
                .packages
                .iter_mut()
                .find(|p| p.filename() == package.filename)
            {
                for artifact in &artifacts {
                    if let Some(existing) = old
                        .artifacts
                        .iter()
                        .find(|a| a.filename == artifact.filename)
                    {
                        ensure!(
                            existing.sha256 == artifact.sha256,
                            "conflicting immutable signature: {}",
                            artifact.filename
                        );
                    } else {
                        old.artifacts.push(artifact.clone());
                    }
                }
            } else {
                next.packages.push(Package {
                    identity,
                    artifacts,
                    retired_at: None,
                });
            }
        }
        // Commit bytes only after the entire incoming set has passed validation.
        if !self.dry_run {
            std::fs::create_dir_all(self.root.join("packages"))?;
            for package in &next.packages {
                for artifact in &package.artifacts {
                    let staged = stage.path().join(&artifact.filename);
                    if !staged.exists() {
                        continue;
                    }
                    let destination = self.package_path(&artifact.filename);
                    if destination.try_exists()? {
                        ensure!(
                            sha256_file(&destination)?.0 == artifact.sha256,
                            "conflicting package snapshot: {}",
                            artifact.filename
                        );
                        continue;
                    }
                    let mut temporary =
                        tempfile::NamedTempFile::new_in(self.root.join("packages"))?;
                    std::io::copy(&mut File::open(staged)?, &mut temporary)?;
                    temporary.as_file().sync_all()?;
                    temporary
                        .persist(&destination)
                        .with_context(|| format!("saving {}", destination.display()))?;
                }
            }
            File::open(self.root.join("packages"))?.sync_all()?;
        }
        self.manifest = next;
        self.save()
    }

    pub fn select(&self, enabled: &[Format], now: u64, grace: u64) -> Result<Selection> {
        let mut groups: BTreeMap<(Format, &str, &str), Vec<&Package>> = BTreeMap::new();
        for package in &self.manifest.packages {
            if enabled.contains(&package.identity.format) {
                groups
                    .entry((
                        package.identity.format,
                        &package.identity.name,
                        &package.identity.arch,
                    ))
                    .or_default()
                    .push(package);
            }
        }
        let mut selection = Selection {
            retained: Vec::new(),
            retired: Vec::new(),
            delete: Vec::new(),
        };
        for ((format, _, _), packages) in groups {
            // Fallible insertion sort: do not hide comparator errors in sort_by.
            let mut ordered: Vec<&Package> = Vec::new();
            for package in packages {
                let mut position = ordered.len();
                for (index, old) in ordered.iter().enumerate() {
                    let order =
                        compare_versions(format, &package.identity.version, &old.identity.version)?;
                    ensure!(
                        order != Ordering::Equal,
                        "duplicate native version in inventory"
                    );
                    if order == Ordering::Less {
                        position = index;
                        break;
                    }
                }
                ordered.insert(position, package);
            }
            let split = ordered.len().saturating_sub(2);
            for (index, package) in ordered.into_iter().enumerate() {
                let filename = package.filename().to_owned();
                if index >= split {
                    // Retired packages are not expected to return to the newest
                    // two, but never promote a previously collected local file.
                    ensure!(
                        self.dry_run || self.package_path(&filename).is_file(),
                        "retained package is missing: {filename}"
                    );
                    selection.retained.push(filename);
                } else {
                    if now
                        .checked_sub(package.retired_at.unwrap_or(now))
                        .is_some_and(|age| age >= grace)
                    {
                        selection.delete.push(filename.clone());
                    }
                    selection.retired.push(filename);
                }
            }
        }
        Ok(selection)
    }

    /// Call only after every enabled repository has been uploaded and verified.
    pub fn commit_retirement(&mut self, selection: &Selection, now: u64) -> Result<()> {
        for package in &mut self.manifest.packages {
            if selection.retired.contains(&package.filename().to_owned()) {
                package.retired_at.get_or_insert(now);
            } else if selection.retained.contains(&package.filename().to_owned()) {
                package.retired_at = None;
            }
        }
        self.save()
    }

    /// Remote deletion must finish first. Keep the manifest record if any local
    /// removal fails, so both remote and local cleanup can be retried.
    pub fn finish_collection(&mut self, filename: &str) -> Result<()> {
        ensure!(!self.dry_run, "cannot collect during dry run");
        let package = self
            .manifest
            .packages
            .iter()
            .find(|p| p.filename() == filename)
            .context("unknown collection package")?;
        ensure!(
            package.retired_at.is_some(),
            "cannot collect an active package"
        );
        for artifact in &package.artifacts {
            match std::fs::remove_file(self.package_path(&artifact.filename)) {
                Ok(()) => {}
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                Err(error) => return Err(error).context("removing retired local artifact"),
            }
        }
        self.manifest.packages.retain(|p| p.filename() != filename);
        self.save()
    }

    fn save(&self) -> Result<()> {
        if self.dry_run {
            return Ok(());
        }
        let mut file = tempfile::NamedTempFile::new_in(&self.root)?;
        serde_json::to_writer_pretty(&mut file, &self.manifest)?;
        file.write_all(b"\n")?;
        file.as_file().sync_all()?;
        file.persist(self.root.join("inventory.json"))?;
        File::open(&self.root)?.sync_all()?;
        Ok(())
    }
}

fn validate_filename(name: &str) -> Result<()> {
    ensure!(
        !name.is_empty()
            && !name.starts_with(['.', '-'])
            && name
                .bytes()
                .all(|b| b.is_ascii_alphanumeric() || b"-_.+".contains(&b)),
        "unsafe package filename: {name}"
    );
    Ok(())
}

fn validate_identity(identity: &Identity) -> Result<()> {
    validate_filename(&identity.name)?;
    ensure!(
        !identity.version.is_empty()
            && identity
                .version
                .bytes()
                .all(|b| b.is_ascii_alphanumeric() || b"-_.+:~^".contains(&b)),
        "unsafe native package version"
    );
    let expected = if identity.format == Format::Deb {
        "amd64"
    } else {
        "x86_64"
    };
    ensure!(
        identity.arch == expected,
        "unsupported architecture: {}",
        identity.arch
    );
    Ok(())
}

fn output(command: &mut Command) -> Result<String> {
    let result = command
        .output()
        .with_context(|| format!("running {command:?}"))?;
    ensure!(
        result.status.success(),
        "{command:?} failed: {}",
        String::from_utf8_lossy(&result.stderr)
    );
    String::from_utf8(result.stdout).context("native package tool returned invalid UTF-8")
}

fn read_identity(path: &Path, format: Format) -> Result<Identity> {
    let (name, version, arch) = match format {
        Format::Deb => {
            let fields = output(Command::new("dpkg-deb").arg("--field").arg(path))?;
            let field = |key: &str| -> Result<String> {
                fields
                    .lines()
                    .find_map(|line| line.strip_prefix(&format!("{key}: ")))
                    .map(str::to_owned)
                    .with_context(|| format!("missing DEB {key}"))
            };
            (field("Package")?, field("Version")?, field("Architecture")?)
        }
        Format::Rpm => {
            let fields = output(
                Command::new("rpm")
                    .args([
                        "-qp",
                        "--qf",
                        "%{NAME}\n%{EPOCHNUM}:%{VERSION}-%{RELEASE}\n%{ARCH}\n",
                    ])
                    .arg(path),
            )?;
            let mut lines = fields.lines();
            (
                lines.next().context("missing RPM name")?.to_owned(),
                lines.next().context("missing RPM version")?.to_owned(),
                lines.next().context("missing RPM architecture")?.to_owned(),
            )
        }
        Format::ArchPkg => {
            let fields = output(
                Command::new("bsdtar")
                    .args(["-xOf"])
                    .arg(path)
                    .arg(".PKGINFO"),
            )?;
            let field = |key: &str| -> Result<String> {
                fields
                    .lines()
                    .find_map(|line| line.strip_prefix(&format!("{key} = ")))
                    .map(str::to_owned)
                    .with_context(|| format!("missing Arch {key}"))
            };
            (field("pkgname")?, field("pkgver")?, field("arch")?)
        }
    };
    Ok(Identity {
        format,
        name,
        version,
        arch,
    })
}

fn compare_versions(format: Format, left: &str, right: &str) -> Result<Ordering> {
    if left == right {
        return Ok(Ordering::Equal);
    }
    match format {
        Format::Deb => {
            for (operator, ordering) in [("lt", Ordering::Less), ("gt", Ordering::Greater)] {
                let status = Command::new("dpkg")
                    .args(["--compare-versions", left, operator, right])
                    .status()?;
                match status.code() {
                    Some(0) => return Ok(ordering),
                    Some(1) => {}
                    _ => bail!("dpkg version comparison failed"),
                }
            }
            Ok(Ordering::Equal)
        }
        Format::Rpm | Format::ArchPkg => {
            let text = if format == Format::Rpm {
                // Versions were restricted to native version characters before
                // interpolation; neither macro nor Lua injection is possible.
                ensure!(
                    [left, right].iter().all(|v| v
                        .bytes()
                        .all(|b| b.is_ascii_alphanumeric() || b"-_.+:~^".contains(&b))),
                    "unsafe RPM version comparison"
                );
                output(Command::new("rpm").args([
                    "--eval",
                    &format!("%{{lua:print(rpm.vercmp(\"{left}\", \"{right}\"))}}"),
                ]))?
            } else {
                output(Command::new("vercmp").args([left, right]))?
            };
            let comparison: i32 = text
                .trim()
                .parse()
                .context("invalid native version comparison result")?;
            Ok(comparison.cmp(&0))
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    pub fn target() -> Target {
        Target {
            bucket: "fixture/repositories".to_owned(),
            endpoint: Some("http://unused.invalid".to_owned()),
        }
    }

    pub fn fixture(
        dir: &Path,
        format: Format,
        name: &str,
        version: &str,
        filename: &str,
    ) -> LinuxPackage {
        let root = tempfile::tempdir().unwrap();
        let path = dir.join(filename);
        std::fs::create_dir_all(root.path().join("usr/share/fixture")).unwrap();
        std::fs::write(
            root.path().join("usr/share/fixture/payload"),
            format!("{name} {version}"),
        )
        .unwrap();
        match format {
            Format::Deb => {
                std::fs::create_dir(root.path().join("DEBIAN")).unwrap();
                std::fs::write(root.path().join("DEBIAN/control"), format!("Package: {name}\nVersion: {version}\nArchitecture: amd64\nMaintainer: Fixture <fixture@example.invalid>\nDescription: retention fixture\n")).unwrap();
                output(
                    Command::new("dpkg-deb")
                        .args(["--build", "--root-owner-group"])
                        .arg(root.path())
                        .arg(&path),
                )
                .unwrap();
            }
            Format::ArchPkg => {
                std::fs::write(root.path().join(".PKGINFO"), format!("pkgname = {name}\npkgbase = {name}\npkgver = {version}\npkgdesc = retention fixture\nurl = https://example.invalid\nbuilddate = 1791847794\npackager = fixture\nsize = 32\narch = x86_64\nlicense = custom\n")).unwrap();
                output(
                    Command::new("bsdtar")
                        .args(["--zstd", "-cf"])
                        .arg(&path)
                        .arg("-C")
                        .arg(root.path())
                        .args([".PKGINFO", "usr"]),
                )
                .unwrap();
            }
            Format::Rpm => {
                let (epoch, rest) = version.split_once(':').unwrap_or(("0", version));
                let (ver, release) = rest.rsplit_once('-').unwrap_or((rest, "1"));
                let spec = root.path().join("fixture.spec");
                std::fs::write(&spec, format!("Name: {name}\nEpoch: {epoch}\nVersion: {ver}\nRelease: {release}\nSummary: retention fixture\nLicense: MIT\nBuildArch: x86_64\n%description\nretention fixture\n%install\nmkdir -p %{{buildroot}}/usr/share/fixture\necho fixture > %{{buildroot}}/usr/share/fixture/payload\n%files\n/usr/share/fixture/payload\n")).unwrap();
                output(
                    Command::new("rpmbuild")
                        .args(["-bb", "--quiet", "--define"])
                        .arg(format!("_topdir {}", root.path().display()))
                        .arg(&spec),
                )
                .unwrap();
                std::fs::copy(
                    root.path()
                        .join(format!("RPMS/x86_64/{name}-{ver}-{release}.x86_64.rpm")),
                    &path,
                )
                .unwrap();
            }
        }
        LinuxPackage {
            path,
            filename: filename.to_owned(),
            format,
        }
    }

    #[test]
    fn sequential_production_and_unified_pilot_imports_retain_independent_histories() {
        for (format, extension) in [
            (Format::Deb, "deb"),
            (Format::Rpm, "rpm"),
            (Format::ArchPkg, "pkg.tar.zst"),
        ] {
            let state = tempfile::tempdir().unwrap();
            let incoming = tempfile::tempdir().unwrap();
            let mut inventory = Inventory::open(state.path(), target(), true, false).unwrap();
            for (name, version, filename) in [
                (
                    "daedalus-mainnet",
                    "1.9-1",
                    format!("z-mainnet-1.9.{extension}"),
                ),
                ("daedalus-pilot", "2.0-1", format!("pilot-2.0.{extension}")),
                (
                    "daedalus-mainnet",
                    "1.10-1",
                    format!("b-mainnet-1.10.{extension}"),
                ),
                (
                    "daedalus-mainnet",
                    "1.11-1",
                    format!("a-mainnet-1.11.{extension}"),
                ),
                ("daedalus-pilot", "2.1-1", format!("pilot-2.1.{extension}")),
                (
                    "daedalus-mainnet",
                    "1.0-1",
                    format!("late-mainnet-1.0.{extension}"),
                ),
            ] {
                let package = fixture(incoming.path(), format, name, version, &filename);
                inventory.import(&[package]).unwrap();
                // Release directories may be ephemeral; later imports use snapshots.
                std::fs::remove_file(incoming.path().join(filename)).unwrap();
            }
            drop(inventory);
            let inventory = Inventory::open(state.path(), target(), false, false).unwrap();
            let selection = inventory.select(&[format], 100, 7 * 86400).unwrap();
            assert_eq!(
                selection.retained,
                [
                    format!("b-mainnet-1.10.{extension}"),
                    format!("a-mainnet-1.11.{extension}"),
                    format!("pilot-2.0.{extension}"),
                    format!("pilot-2.1.{extension}")
                ]
            );
            assert_eq!(selection.retired.len(), 2);
            assert!(selection.delete.is_empty());
            if format == Format::ArchPkg {
                let paths: Vec<_> = selection
                    .retained
                    .iter()
                    .map(|name| inventory.package_path(name))
                    .collect();
                let packages: Vec<_> = paths
                    .iter()
                    .zip(&selection.retained)
                    .map(|(path, name)| (path.as_path(), name.as_str()))
                    .collect();
                let repo =
                    crate::arch_repo::prepare(&packages, crate::arch_repo::Signing::Unsigned)
                        .unwrap();
                for kind in ["db", "files"] {
                    let listing = output(
                        Command::new("bsdtar")
                            .arg("-xOf")
                            .arg(repo.path(&format!("daedalus.{kind}"))),
                    )
                    .unwrap();
                    assert!(listing.contains("a-mainnet-1.11.pkg.tar.zst"));
                    assert!(listing.contains("pilot-2.1.pkg.tar.zst"));
                    assert!(!listing.contains("b-mainnet-1.10.pkg.tar.zst"));
                    assert!(!listing.contains("pilot-2.0.pkg.tar.zst"));
                }
                assert_eq!(
                    repo.packages.len(),
                    4,
                    "both rollback packages must remain downloadable"
                );
            }
        }
    }

    #[test]
    fn native_comparators_handle_numeric_versions_epochs_revisions_and_prereleases() {
        for format in [Format::Deb, Format::Rpm, Format::ArchPkg] {
            for (old, new) in [
                ("1.9-1", "1.10-1"),
                ("1:99.0-1", "2:1.0-1"),
                ("1.0-2", "1.0-10"),
            ] {
                assert_eq!(
                    compare_versions(format, old, new).unwrap(),
                    Ordering::Less,
                    "{format:?}: {old} < {new}"
                );
                assert_eq!(
                    compare_versions(format, new, old).unwrap(),
                    Ordering::Greater
                );
            }
            let prerelease = if format == Format::ArchPkg {
                "1.0rc1-1"
            } else {
                "1.0~rc1-1"
            };
            assert_eq!(
                compare_versions(format, prerelease, "1.0-1").unwrap(),
                Ordering::Less
            );
        }
    }

    #[test]
    fn immutable_import_is_idempotent_and_rejects_conflicting_filename_or_identity() {
        let state = tempfile::tempdir().unwrap();
        let incoming = tempfile::tempdir().unwrap();
        let mut inventory = Inventory::open(state.path(), target(), true, false).unwrap();
        let package = fixture(
            incoming.path(),
            Format::Deb,
            "daedalus-pilot",
            "1.0",
            "pilot.deb",
        );
        std::fs::write(
            package.path.with_file_name("pilot.deb.asc"),
            b"sidecar fixture",
        )
        .unwrap();
        inventory.import(std::slice::from_ref(&package)).unwrap();
        inventory.import(std::slice::from_ref(&package)).unwrap();
        assert_eq!(inventory.packages().len(), 1);
        assert_eq!(inventory.packages()[0].artifacts.len(), 2);
        let alias = fixture(
            incoming.path(),
            Format::Deb,
            "daedalus-pilot",
            "1.0",
            "other.deb",
        );
        assert!(
            inventory
                .import(&[alias])
                .unwrap_err()
                .to_string()
                .contains("conflicting immutable")
        );
        fixture(
            incoming.path(),
            Format::Deb,
            "daedalus-pilot",
            "2.0",
            "pilot.deb",
        );
        assert!(inventory.import(&[package]).is_err());
        assert_eq!(inventory.packages()[0].identity.version, "1.0");
    }

    #[test]
    fn inventory_lock_target_binding_and_snapshot_corruption_fail_closed() {
        let state = tempfile::tempdir().unwrap();
        let incoming = tempfile::tempdir().unwrap();
        let mut inventory = Inventory::open(state.path(), target(), true, false).unwrap();
        assert!(Inventory::open(state.path(), target(), true, false).is_err());
        let package = fixture(
            incoming.path(),
            Format::Deb,
            "daedalus-mainnet",
            "1.0",
            "mainnet.deb",
        );
        inventory.import(&[package]).unwrap();
        let path = inventory.package_path("mainnet.deb");
        drop(inventory);
        let wrong = Target {
            bucket: "other/prefix".to_owned(),
            ..target()
        };
        assert!(Inventory::open(state.path(), wrong, false, false).is_err());
        std::fs::write(path, b"tampered").unwrap();
        assert!(Inventory::open(state.path(), target(), false, false).is_err());
    }

    #[test]
    fn inventory_drop_explicitly_unlocks_even_with_an_inherited_descriptor() {
        let state = tempfile::tempdir().unwrap();
        let inventory = Inventory::open(state.path(), target(), true, false).unwrap();
        let inherited = inventory.lock.as_ref().unwrap().try_clone().unwrap();
        assert!(Inventory::open(state.path(), target(), true, false).is_err());
        drop(inventory);
        let _next = Inventory::open(state.path(), target(), true, false).unwrap();
        drop(inherited);
    }

    #[test]
    fn dry_run_stages_hypothetical_import_without_creating_state() {
        let parent = tempfile::tempdir().unwrap();
        let incoming = tempfile::tempdir().unwrap();
        let state = parent.path().join("absent");
        let mut inventory = Inventory::open(&state, target(), true, true).unwrap();
        let package = fixture(
            incoming.path(),
            Format::Deb,
            "daedalus-pilot",
            "1.0",
            "pilot.deb",
        );
        inventory.import(&[package]).unwrap();
        assert_eq!(
            inventory.select(&[Format::Deb], 0, 0).unwrap().retained,
            ["pilot.deb"]
        );
        assert!(!state.exists());
    }

    #[test]
    fn dry_run_preserves_existing_manifest_and_package_snapshots() {
        let state = tempfile::tempdir().unwrap();
        let incoming = tempfile::tempdir().unwrap();
        let mut inventory = Inventory::open(state.path(), target(), true, false).unwrap();
        inventory
            .import(&[fixture(
                incoming.path(),
                Format::Deb,
                "daedalus-mainnet",
                "1",
                "mainnet.deb",
            )])
            .unwrap();
        drop(inventory);
        let before = std::fs::read(state.path().join("inventory.json")).unwrap();
        let mut preview = Inventory::open(state.path(), target(), false, true).unwrap();
        preview
            .import(&[fixture(
                incoming.path(),
                Format::Deb,
                "daedalus-pilot",
                "1",
                "pilot.deb",
            )])
            .unwrap();
        assert_eq!(
            preview
                .select(&[Format::Deb], 100, 0)
                .unwrap()
                .retained
                .len(),
            2
        );
        drop(preview);
        assert_eq!(
            before,
            std::fs::read(state.path().join("inventory.json")).unwrap()
        );
        assert!(!state.path().join("packages/pilot.deb").exists());
        assert_eq!(
            Inventory::open(state.path(), target(), false, false)
                .unwrap()
                .packages()
                .len(),
            1
        );
    }

    #[test]
    fn interrupted_local_collection_can_reopen_and_finish_missing_artifacts() {
        let state = tempfile::tempdir().unwrap();
        let incoming = tempfile::tempdir().unwrap();
        let mut inventory = Inventory::open(state.path(), target(), true, false).unwrap();
        for version in ["1", "2", "3"] {
            inventory
                .import(&[fixture(
                    incoming.path(),
                    Format::Deb,
                    "daedalus-mainnet",
                    version,
                    &format!("mainnet-{version}.deb"),
                )])
                .unwrap();
        }
        let selection = inventory.select(&[Format::Deb], 100, 0).unwrap();
        inventory.commit_retirement(&selection, 100).unwrap();
        std::fs::remove_file(inventory.package_path("mainnet-1.deb")).unwrap();
        drop(inventory);
        let mut inventory = Inventory::open(state.path(), target(), false, false).unwrap();
        inventory.finish_collection("mainnet-1.deb").unwrap();
        assert_eq!(inventory.packages().len(), 2);
    }

    #[test]
    fn retirement_grace_clock_rollback_and_skipped_formats() {
        let state = tempfile::tempdir().unwrap();
        let incoming = tempfile::tempdir().unwrap();
        let mut inventory = Inventory::open(state.path(), target(), true, false).unwrap();
        for version in ["1.0", "2.0", "3.0"] {
            inventory
                .import(&[fixture(
                    incoming.path(),
                    Format::Deb,
                    "daedalus-mainnet",
                    version,
                    &format!("mainnet-{version}.deb"),
                )])
                .unwrap();
        }
        let empty = inventory.select(&[Format::Rpm], 10, 0).unwrap();
        assert!(empty.retained.is_empty() && empty.retired.is_empty() && empty.delete.is_empty());
        let selection = inventory.select(&[Format::Deb], 1000, 100).unwrap();
        assert!(selection.delete.is_empty());
        inventory.commit_retirement(&selection, 1000).unwrap();
        assert!(
            inventory
                .select(&[Format::Deb], 999, 0)
                .unwrap()
                .delete
                .is_empty()
        );
        assert!(
            inventory
                .select(&[Format::Deb], 1099, 100)
                .unwrap()
                .delete
                .is_empty()
        );
        assert_eq!(
            inventory.select(&[Format::Deb], 1100, 100).unwrap().delete,
            ["mainnet-1.0.deb"]
        );
        assert!(inventory.finish_collection("mainnet-3.0.deb").is_err());
        inventory.finish_collection("mainnet-1.0.deb").unwrap();
        assert_eq!(inventory.packages().len(), 2);
        assert!(!inventory.package_path("mainnet-1.0.deb").exists());
    }

    #[test]
    fn zero_grace_collects_new_retirements_and_unsafe_paths_are_rejected() {
        let state = tempfile::tempdir().unwrap();
        let incoming = tempfile::tempdir().unwrap();
        let mut inventory = Inventory::open(state.path(), target(), true, false).unwrap();
        for version in ["1", "2", "3"] {
            inventory
                .import(&[fixture(
                    incoming.path(),
                    Format::Deb,
                    "daedalus-mainnet",
                    version,
                    &format!("mainnet-{version}.deb"),
                )])
                .unwrap();
        }
        assert_eq!(
            inventory.select(&[Format::Deb], 100, 0).unwrap().delete,
            ["mainnet-1.deb"]
        );
        for name in [
            "../outside.deb",
            "-option.deb",
            "a/b.deb",
            "",
            ".",
            "a%bad.deb",
        ] {
            assert!(validate_filename(name).is_err());
        }
        assert!(compare_versions(Format::Rpm, "1%{lua:evil}", "2").is_err());
    }
}
