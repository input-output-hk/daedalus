use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

/// Which release artifact an installer targets, inferred from the filename.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Platform {
    LinuxDeb,
    LinuxRpm,
    DarwinArm, // aarch64-darwin (Apple Silicon)
    DarwinX86, // x86_64-darwin  (Intel)
    Windows,
}

impl Platform {
    /// Infer the release artifact from the installer filename.
    /// `.pkg` files must contain `aarch64-darwin` or `x86_64-darwin`.
    pub fn from_filename(filename: &str) -> Option<Self> {
        let ext = Path::new(filename).extension()?.to_str()?;
        match ext {
            "deb" => Some(Self::LinuxDeb),
            "rpm" => Some(Self::LinuxRpm),
            "exe" => Some(Self::Windows),
            "pkg" if filename.contains("aarch64-darwin") => Some(Self::DarwinArm),
            "pkg" if filename.contains("x86_64-darwin") => Some(Self::DarwinX86),
            _ => None,
        }
    }

    /// Key used in the platforms map of daedalus-latest-version.json.
    pub fn json_key(self) -> &'static str {
        match self {
            Platform::LinuxDeb => "linux-deb",
            Platform::LinuxRpm => "linux-rpm",
            Platform::DarwinArm => "darwin-arm",
            Platform::DarwinX86 => "darwin",
            Platform::Windows => "windows",
        }
    }

    pub fn display_name(self) -> &'static str {
        match self {
            Platform::LinuxDeb => "Linux (.deb)",
            Platform::LinuxRpm => "Linux (.rpm)",
            Platform::DarwinArm => "macOS (Apple Silicon)",
            Platform::DarwinX86 => "macOS (Intel)",
            Platform::Windows => "Windows",
        }
    }

    /// Key used to target ordinary newsfeed items. Both Linux package formats
    /// target the same operating system and must be deduplicated by callers.
    pub fn newsfeed_key(self) -> &'static str {
        match self {
            Platform::LinuxDeb | Platform::LinuxRpm => "linux",
            Platform::DarwinArm => "darwin-arm",
            Platform::DarwinX86 => "darwin",
            Platform::Windows => "win32",
        }
    }

    /// Key used for executable software updates. Linux system packages are
    /// deliberately excluded because upgrades are mediated by apt or dnf.
    pub fn software_update_key(self) -> Option<&'static str> {
        match self {
            Platform::LinuxDeb | Platform::LinuxRpm => None,
            Platform::DarwinArm => Some("darwin-arm"),
            Platform::DarwinX86 => Some("darwin"),
            Platform::Windows => Some("win32"),
        }
    }

    pub fn is_linux_package(self) -> bool {
        matches!(self, Platform::LinuxDeb | Platform::LinuxRpm)
    }
}

pub struct Installer {
    pub path: PathBuf,
    /// Bare filename (e.g. `daedalus-6.0.1-mainnet-x86_64-linux.deb`).
    pub filename: String,
    pub platform: Platform,
}

impl Installer {
    /// Path to the GPG detached-signature file (filename + ".asc").
    pub fn sig_path(&self) -> PathBuf {
        self.path
            .parent()
            .unwrap_or(Path::new("."))
            .join(format!("{}.asc", self.filename))
    }

    /// Path to the unsigned-original companion file (`stem-unsigned.ext`).
    pub fn unsigned_path(&self) -> PathBuf {
        let unsigned_name = match self.filename.rsplit_once('.') {
            Some((stem, ext)) => format!("{stem}-unsigned.{ext}"),
            None => format!("{}-unsigned", self.filename),
        };
        self.path
            .parent()
            .unwrap_or(Path::new("."))
            .join(unsigned_name)
    }

    /// Returns true if the `-unsigned.` companion already exists, meaning this
    /// installer was already code-signed in a previous run.
    pub fn is_already_code_signed(&self) -> bool {
        self.unsigned_path().exists()
    }

    /// Read the signature file contents, returning `None` if absent.
    pub fn read_signature(&self) -> Result<Option<String>> {
        let p = self.sig_path();
        if p.exists() {
            Ok(Some(
                std::fs::read_to_string(&p).with_context(|| format!("reading {}", p.display()))?,
            ))
        } else {
            Ok(None)
        }
    }
}

/// Metadata written by `drt fetch-installers` and read by `drt sign` / `drt release`.
/// Stored as `meta.json` in the installer directory.
/// Falls back to reading a plain `version` file if `meta.json` is absent.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Meta {
    pub version: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub gitrev: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub nar_hash: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub env: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub eval_url: Option<String>,
}

/// Everything in an installer directory.
pub struct InstallerDir {
    pub dir: PathBuf,
    pub version: String,
    pub meta: Meta,
    pub installers: Vec<Installer>,
}

impl InstallerDir {
    /// Scan `dir` for installer files (.deb / .rpm / .pkg / .exe) and read
    /// metadata. Tries `meta.json` first; falls back to a plain `version` file.
    pub fn load(dir: &Path) -> Result<Self> {
        let meta_path = dir.join("meta.json");
        let meta = if meta_path.exists() {
            let data = std::fs::read_to_string(&meta_path)
                .with_context(|| format!("reading {}", meta_path.display()))?;
            serde_json::from_str::<Meta>(&data)
                .with_context(|| format!("parsing {}", meta_path.display()))?
        } else {
            let version = std::fs::read_to_string(dir.join("version"))
                .with_context(|| {
                    format!(
                        "neither 'meta.json' nor 'version' file found in {}",
                        dir.display()
                    )
                })?
                .trim()
                .to_string();
            Meta {
                version,
                gitrev: None,
                nar_hash: None,
                env: None,
                eval_url: None,
            }
        };

        Self::from_meta(dir, meta)
    }

    /// Validate downloaded artifacts against metadata before `meta.json` is
    /// written as the handoff to signing and release commands.
    pub(crate) fn from_meta(dir: &Path, meta: Meta) -> Result<Self> {
        let version = meta.version.clone();
        let mut installers = Vec::new();
        let mut seen: HashMap<Platform, String> = HashMap::new();

        for entry in std::fs::read_dir(dir)
            .with_context(|| format!("reading directory {}", dir.display()))?
        {
            let entry = entry.with_context(|| format!("reading directory {}", dir.display()))?;
            let path = entry.path();
            if !path.is_file() {
                continue;
            }
            let Some(filename) = path.file_name().and_then(|name| name.to_str()) else {
                continue;
            };
            let ext = path.extension().and_then(|ext| ext.to_str());
            if ext == Some("bin") {
                anyhow::bail!(
                    "portable Linux .bin installer '{}' is retired; release Linux as a matched .deb/.rpm pair",
                    filename
                );
            }
            // Only macOS/Windows remote code signing creates unsigned
            // companions. Linux system packages must remain byte-identical to
            // their Hydra products.
            if filename.contains("-unsigned.") {
                anyhow::ensure!(
                    !matches!(ext, Some("deb" | "rpm")),
                    "Linux package unsigned companion '{}' is forbidden",
                    filename
                );
                continue;
            }
            let Some(platform) = Platform::from_filename(filename) else {
                continue;
            };

            if let Some(previous) = seen.insert(platform, filename.to_string()) {
                anyhow::bail!(
                    "duplicate {} installers found: '{}' and '{}'",
                    platform.display_name(),
                    previous,
                    filename
                );
            }

            let artifact_version = release_version_from_filename(filename).ok_or_else(|| {
                anyhow::anyhow!("could not extract a release version from installer '{filename}'")
            })?;
            anyhow::ensure!(
                artifact_version == version,
                "installer '{}' reports version {}, but metadata reports {}",
                filename,
                artifact_version,
                version
            );

            let filename = filename.to_string();
            installers.push(Installer {
                path,
                filename,
                platform,
            });
        }

        installers.sort_by(|a, b| a.filename.cmp(&b.filename));

        anyhow::ensure!(
            !installers.is_empty(),
            "no installer files (.deb, .rpm, .pkg, .exe) found in {}",
            dir.display()
        );

        let has_deb = seen.contains_key(&Platform::LinuxDeb);
        let has_rpm = seen.contains_key(&Platform::LinuxRpm);
        anyhow::ensure!(
            has_deb == has_rpm,
            "Linux releases require a matched .deb/.rpm pair; found {} only",
            if has_deb { ".deb" } else { ".rpm" }
        );
        if has_deb {
            let deb_identity = linux_artifact_identity(&seen[&Platform::LinuxDeb])?;
            let rpm_identity = linux_artifact_identity(&seen[&Platform::LinuxRpm])?;
            anyhow::ensure!(
                deb_identity == rpm_identity,
                "Linux .deb/.rpm identity mismatch: .deb is {:?}, .rpm is {:?}",
                deb_identity,
                rpm_identity
            );
            let expected_env = meta.env.as_deref().ok_or_else(|| {
                anyhow::anyhow!("Linux releases require meta.json to declare the target env")
            })?;
            anyhow::ensure!(
                deb_identity.cluster == expected_env,
                "Linux artifact cluster '{}' does not match metadata env '{}'",
                deb_identity.cluster,
                expected_env
            );
        }

        Ok(InstallerDir {
            dir: dir.to_path_buf(),
            version,
            meta,
            installers,
        })
    }
}

/// Extract the dotted release version from a Daedalus installer filename.
pub(crate) fn release_version_from_filename(filename: &str) -> Option<&str> {
    filename
        .split('-')
        .skip(1)
        .find(|part| part.chars().next().is_some_and(|c| c.is_ascii_digit()) && part.contains('.'))
}

#[derive(Debug, PartialEq, Eq)]
struct LinuxArtifactIdentity<'a> {
    version: &'a str,
    build: &'a str,
    cluster: &'a str,
    revision: &'a str,
    architecture: &'a str,
}

fn linux_artifact_identity(filename: &str) -> Result<LinuxArtifactIdentity<'_>> {
    let extension = Path::new(filename)
        .extension()
        .and_then(|extension| extension.to_str());
    anyhow::ensure!(
        matches!(extension, Some("deb" | "rpm")),
        "unsupported Linux package filename '{filename}'"
    );
    let stem = filename
        .strip_suffix(".deb")
        .or_else(|| filename.strip_suffix(".rpm"))
        .and_then(|stem| stem.strip_suffix("-linux"))
        .ok_or_else(|| anyhow::anyhow!("invalid Linux package filename '{filename}'"))?;
    let (identity, architecture) = stem
        .rsplit_once('-')
        .ok_or_else(|| anyhow::anyhow!("Linux package '{filename}' has no architecture"))?;
    anyhow::ensure!(
        architecture == "x86_64",
        "unsupported Linux package architecture '{architecture}' in '{filename}'; expected x86_64"
    );
    let mut parts = identity
        .strip_prefix("daedalus-")
        .ok_or_else(|| anyhow::anyhow!("invalid Linux package filename '{filename}'"))?
        .split('-');
    let version = parts.next();
    let build = parts.next();
    let cluster = parts.next();
    let revision = parts.next();
    anyhow::ensure!(
        parts.next().is_none()
            && version.is_some()
            && build.is_some()
            && cluster.is_some()
            && revision.is_some(),
        "Linux package '{filename}' does not encode version/build/cluster/revision"
    );
    Ok(LinuxArtifactIdentity {
        version: version.unwrap(),
        build: build.unwrap(),
        cluster: cluster.unwrap(),
        revision: revision.unwrap(),
        architecture,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU64, Ordering};

    static FIXTURE_ID: AtomicU64 = AtomicU64::new(0);

    struct Fixture(PathBuf);

    impl Fixture {
        fn new(version: &str, filenames: &[&str]) -> Self {
            Self::new_with_env(version, filenames, Some("mainnet"))
        }

        fn new_with_env(version: &str, filenames: &[&str], env: Option<&str>) -> Self {
            let id = FIXTURE_ID.fetch_add(1, Ordering::Relaxed);
            let dir = std::env::temp_dir()
                .join(format!("drt-installers-test-{}-{id}", std::process::id()));
            std::fs::create_dir_all(&dir).expect("create installer fixture");
            let meta = Meta {
                version: version.to_string(),
                gitrev: None,
                nar_hash: None,
                env: env.map(str::to_string),
                eval_url: None,
            };
            std::fs::write(
                dir.join("meta.json"),
                serde_json::to_vec(&meta).expect("serialize fixture metadata"),
            )
            .expect("write fixture metadata");
            for filename in filenames {
                std::fs::write(dir.join(filename), b"installer").expect("write installer fixture");
            }
            Self(dir)
        }

        fn load_error(&self) -> String {
            match InstallerDir::load(&self.0) {
                Ok(_) => panic!("fixture unexpectedly loaded"),
                Err(error) => format!("{error:#}"),
            }
        }
    }

    impl Drop for Fixture {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    #[test]
    fn classifies_distinct_release_artifacts() {
        assert_eq!(
            Platform::from_filename("daedalus-6.0.1-mainnet-x86_64-linux.deb"),
            Some(Platform::LinuxDeb)
        );
        assert_eq!(
            Platform::from_filename("daedalus-6.0.1-mainnet-x86_64-linux.rpm"),
            Some(Platform::LinuxRpm)
        );
        assert_eq!(
            Platform::from_filename("daedalus-6.0.1-mainnet-aarch64-darwin.pkg"),
            Some(Platform::DarwinArm)
        );
        assert_eq!(
            Platform::from_filename("daedalus-6.0.1-mainnet-x86_64-darwin.pkg"),
            Some(Platform::DarwinX86)
        );
        assert_eq!(
            Platform::from_filename("daedalus-6.0.1-mainnet-x86_64-windows.exe"),
            Some(Platform::Windows)
        );
        assert_eq!(
            Platform::from_filename("daedalus-6.0.1-mainnet-x86_64-linux.bin"),
            None
        );
    }

    #[test]
    fn rejects_retired_portable_bin_explicitly() {
        let fixture = Fixture::new("6.0.1", &["daedalus-6.0.1-mainnet-x86_64-linux.bin"]);
        let error = fixture.load_error();
        assert!(error.contains("portable Linux .bin installer"));
        assert!(error.contains("retired"));
    }

    #[test]
    fn requires_linux_deb_and_rpm_pair() {
        for (filename, expected) in [
            (
                "daedalus-6.0.1-100-mainnet-abcdef0-x86_64-linux.deb",
                "found .deb only",
            ),
            (
                "daedalus-6.0.1-100-mainnet-abcdef0-x86_64-linux.rpm",
                "found .rpm only",
            ),
        ] {
            let fixture = Fixture::new("6.0.1", &[filename]);
            assert!(fixture.load_error().contains(expected));
        }
    }

    #[test]
    fn accepts_linux_pair_as_independent_artifacts() {
        let fixture = Fixture::new(
            "6.0.1",
            &[
                "daedalus-6.0.1-100-mainnet-abcdef0-x86_64-linux.deb",
                "daedalus-6.0.1-100-mainnet-abcdef0-x86_64-linux.rpm",
            ],
        );
        let dir = InstallerDir::load(&fixture.0).expect("load Linux package pair");
        let platforms: Vec<_> = dir
            .installers
            .iter()
            .map(|installer| installer.platform)
            .collect();
        assert_eq!(platforms, vec![Platform::LinuxDeb, Platform::LinuxRpm]);
    }

    #[test]
    fn rejects_duplicate_artifacts_for_one_platform() {
        let fixture = Fixture::new(
            "6.0.1",
            &[
                "daedalus-6.0.1-100-mainnet-abcdef0-x86_64-linux.deb",
                "daedalus-6.0.1-100-preview-abcdef0-x86_64-linux.deb",
                "daedalus-6.0.1-100-mainnet-abcdef0-x86_64-linux.rpm",
            ],
        );
        assert!(fixture
            .load_error()
            .contains("duplicate Linux (.deb) installers"));
    }

    #[test]
    fn rejects_artifact_version_mismatch() {
        let fixture = Fixture::new(
            "6.0.1",
            &[
                "daedalus-6.0.1-100-mainnet-abcdef0-x86_64-linux.deb",
                "daedalus-6.0.2-100-mainnet-abcdef0-x86_64-linux.rpm",
            ],
        );
        let error = fixture.load_error();
        assert!(error.contains("reports version 6.0.2"));
        assert!(error.contains("metadata reports 6.0.1"));
    }

    #[test]
    fn rejects_linux_pair_identity_mismatch() {
        let fixture = Fixture::new(
            "6.0.1",
            &[
                "daedalus-6.0.1-100-mainnet-abcdef0-x86_64-linux.deb",
                "daedalus-6.0.1-101-mainnet-abcdef0-x86_64-linux.rpm",
            ],
        );
        assert!(fixture.load_error().contains("identity mismatch"));
    }

    #[test]
    fn rejects_matched_unsupported_linux_architecture() {
        let fixture = Fixture::new(
            "6.0.1",
            &[
                "daedalus-6.0.1-100-mainnet-abcdef0-aarch64-linux.deb",
                "daedalus-6.0.1-100-mainnet-abcdef0-aarch64-linux.rpm",
            ],
        );
        assert!(fixture
            .load_error()
            .contains("unsupported Linux package architecture 'aarch64'"));
    }

    #[test]
    fn rejects_linux_unsigned_companion() {
        let fixture = Fixture::new(
            "6.0.1",
            &[
                "daedalus-6.0.1-100-mainnet-abcdef0-x86_64-linux.deb",
                "daedalus-6.0.1-100-mainnet-abcdef0-x86_64-linux-unsigned.deb",
                "daedalus-6.0.1-100-mainnet-abcdef0-x86_64-linux.rpm",
            ],
        );
        assert!(fixture
            .load_error()
            .contains("Linux package unsigned companion"));
    }

    #[test]
    fn rejects_linux_cluster_that_differs_from_metadata_env() {
        let fixture = Fixture::new(
            "6.0.1",
            &[
                "daedalus-6.0.1-100-preview-abcdef0-x86_64-linux.deb",
                "daedalus-6.0.1-100-preview-abcdef0-x86_64-linux.rpm",
            ],
        );
        assert!(fixture
            .load_error()
            .contains("does not match metadata env 'mainnet'"));
    }

    #[test]
    fn requires_metadata_env_for_linux_pair() {
        let fixture = Fixture::new_with_env(
            "6.0.1",
            &[
                "daedalus-6.0.1-100-mainnet-abcdef0-x86_64-linux.deb",
                "daedalus-6.0.1-100-mainnet-abcdef0-x86_64-linux.rpm",
            ],
            None,
        );
        assert!(fixture
            .load_error()
            .contains("require meta.json to declare the target env"));
    }

    #[test]
    fn preserves_non_linux_installer_flow() {
        let fixture = Fixture::new(
            "6.0.1",
            &[
                "daedalus-6.0.1-mainnet-aarch64-darwin.pkg",
                "daedalus-6.0.1-mainnet-x86_64-darwin.pkg",
                "daedalus-6.0.1-mainnet-x86_64-windows.exe",
            ],
        );
        let dir = InstallerDir::load(&fixture.0).expect("load non-Linux installers");
        assert_eq!(dir.installers.len(), 3);
    }
}
