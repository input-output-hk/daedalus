//! Generate `daedalus-latest-version.json`.
//!
//! Format:
//!
//! ```json
//! {
//!   "platforms": {
//!     "linux": {
//!       "bin":  { "version": "6.0.1", "URL": "https://...", "hash": "<blake2b-cbor-hex>", "SHA256": "<sha256-hex>", "signature": "..." },
//!       "deb":  { ... },
//!       "rpm":  { ... },
//!       "arch": { ... }
//!     },
//!     "macos": {
//!       "aarch64": { ... },
//!       "x86_64":  { ... }
//!     },
//!     "windows": {
//!       "exe": { ... }
//!     }
//!   },
//!   "release_notes": null
//! }
//! ```
//!
//! The `hash` field is the Blake2b-256 of the CBOR-encoded file bytes
//! (see `hash.rs`).  The `signature` field is the full ASCII-armoured
//! GPG detached signature, or `null` if no `.asc` file was present.

use crate::hash::Hashes;
use crate::installers::Platform;
use serde::Serialize;
use std::collections::HashMap;

#[derive(Serialize)]
pub struct PlatformEntry {
    pub version: String,
    #[serde(rename = "URL")]
    pub url: String,
    /// Blake2b-256 of CBOR-encoded file bytes, hex-encoded.
    pub hash: String,
    #[serde(rename = "SHA256")]
    pub sha256: String,
    pub signature: Option<String>,
}

#[derive(Serialize, Default)]
pub struct LinuxPlatforms {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bin: Option<PlatformEntry>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub deb: Option<PlatformEntry>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rpm: Option<PlatformEntry>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub arch: Option<PlatformEntry>,
}

#[derive(Serialize, Default)]
pub struct MacOsPlatforms {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub aarch64: Option<PlatformEntry>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub x86_64: Option<PlatformEntry>,
}

#[derive(Serialize, Default)]
pub struct WindowsPlatforms {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub exe: Option<PlatformEntry>,
}

#[derive(Serialize, Default)]
pub struct Platforms {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub linux: Option<LinuxPlatforms>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub macos: Option<MacOsPlatforms>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub windows: Option<WindowsPlatforms>,
}

#[derive(Serialize)]
pub struct VersionJson {
    pub platforms: Platforms,
    pub release_notes: Option<String>,
}

impl VersionJson {
    pub fn build(
        version: &str,
        hashes: &HashMap<Platform, Hashes>,
        urls: &HashMap<Platform, String>,
        signatures: &HashMap<Platform, Option<String>>,
        release_notes: Option<String>,
    ) -> Self {
        let mut linux = LinuxPlatforms::default();
        let mut macos = MacOsPlatforms::default();
        let mut windows = WindowsPlatforms::default();
        let mut has_linux = false;
        let mut has_macos = false;
        let mut has_windows = false;

        for (platform, h) in hashes {
            let entry = PlatformEntry {
                version: version.to_string(),
                url: urls[platform].clone(),
                hash: h.blake2b_cbor.clone(),
                sha256: h.sha256.clone(),
                signature: signatures.get(platform).and_then(|s| s.clone()),
            };
            match platform {
                Platform::LinuxBin => {
                    linux.bin = Some(entry);
                    has_linux = true;
                }
                Platform::LinuxDeb => {
                    linux.deb = Some(entry);
                    has_linux = true;
                }
                Platform::LinuxRpm => {
                    linux.rpm = Some(entry);
                    has_linux = true;
                }
                Platform::LinuxArch => {
                    linux.arch = Some(entry);
                    has_linux = true;
                }
                Platform::MacOsArm => {
                    macos.aarch64 = Some(entry);
                    has_macos = true;
                }
                Platform::MacOsX86 => {
                    macos.x86_64 = Some(entry);
                    has_macos = true;
                }
                Platform::Windows => {
                    windows.exe = Some(entry);
                    has_windows = true;
                }
            }
        }

        VersionJson {
            platforms: Platforms {
                linux: has_linux.then_some(linux),
                macos: has_macos.then_some(macos),
                windows: has_windows.then_some(windows),
            },
            release_notes,
        }
    }
}
