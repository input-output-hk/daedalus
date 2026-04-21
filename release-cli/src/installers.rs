use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// Which OS an installer targets, inferred from file extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Platform {
    Linux,
    Darwin,
    Windows,
}

impl Platform {
    pub fn from_extension(ext: &str) -> Option<Self> {
        match ext {
            "bin" => Some(Self::Linux),
            "pkg" => Some(Self::Darwin),
            "exe" => Some(Self::Windows),
            _ => None,
        }
    }

    /// Key used in the platforms map of daedalus-latest-version.json.
    pub fn json_key(self) -> &'static str {
        match self {
            Platform::Linux => "linux",
            Platform::Darwin => "darwin",
            Platform::Windows => "windows",
        }
    }

    pub fn display_name(self) -> &'static str {
        match self {
            Platform::Linux => "Linux",
            Platform::Darwin => "macOS",
            Platform::Windows => "Windows",
        }
    }

    /// Key used in the softwareUpdate / platforms fields of newsfeed JSON.
    /// Note: Windows is "win32" in the newsfeed, not "windows".
    pub fn newsfeed_key(self) -> &'static str {
        match self {
            Platform::Linux => "linux",
            Platform::Darwin => "darwin",
            Platform::Windows => "win32",
        }
    }
}

pub struct Installer {
    pub path: PathBuf,
    /// Bare filename (e.g. `daedalus-6.0.1-mainnet-x86_64-linux.bin`).
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
    /// Scan `dir` for installer files (.bin / .pkg / .exe) and read metadata.
    /// Tries `meta.json` first; falls back to a plain `version` file.
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
        let version = meta.version.clone();

        let mut installers: Vec<Installer> = std::fs::read_dir(dir)
            .with_context(|| format!("reading directory {}", dir.display()))?
            .filter_map(|entry| {
                let entry = entry.ok()?;
                let path = entry.path();
                if !path.is_file() {
                    return None;
                }
                let filename = path.file_name()?.to_str()?.to_string();
                let ext = path.extension()?.to_str()?;
                let platform = Platform::from_extension(ext)?;
                Some(Installer {
                    path,
                    filename,
                    platform,
                })
            })
            .collect();

        // Stable order for deterministic output
        installers.sort_by(|a, b| a.filename.cmp(&b.filename));

        anyhow::ensure!(
            !installers.is_empty(),
            "no installer files (.bin, .pkg, .exe) found in {}",
            dir.display()
        );

        Ok(InstallerDir {
            dir: dir.to_path_buf(),
            version,
            meta,
            installers,
        })
    }
}
