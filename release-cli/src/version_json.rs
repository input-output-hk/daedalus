//! Generate `daedalus-latest-version.json`.
//!
//! Format (matches the legacy proposal-ui output):
//!
//! ```json
//! {
//!   "platforms": {
//!     "linux-deb": { "version": "6.0.1", "URL": "https://...deb", "hash": "<blake2b-cbor-hex>", "SHA256": "<sha256-hex>", "signature": "..." },
//!     "linux-rpm": { "version": "6.0.1", "URL": "https://...rpm", "hash": "<blake2b-cbor-hex>", "SHA256": "<sha256-hex>", "signature": "..." },
//!     "darwin":    { ... },
//!     "windows":   { ... }
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

#[derive(Serialize)]
pub struct VersionJson {
    pub platforms: HashMap<String, PlatformEntry>,
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
        let platforms = hashes
            .iter()
            .map(|(platform, h)| {
                let entry = PlatformEntry {
                    version: version.to_string(),
                    url: urls[platform].clone(),
                    hash: h.blake2b_cbor.clone(),
                    sha256: h.sha256.clone(),
                    signature: signatures.get(platform).and_then(|s| s.clone()),
                };
                (platform.json_key().to_string(), entry)
            })
            .collect();

        VersionJson {
            platforms,
            release_notes,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn hashes(blake2b_cbor: &str, sha256: &str) -> Hashes {
        Hashes {
            blake2b_cbor: blake2b_cbor.to_string(),
            sha256: sha256.to_string(),
        }
    }

    #[test]
    fn emits_independent_linux_deb_and_rpm_entries() {
        let artifact_hashes = HashMap::from([
            (Platform::LinuxDeb, hashes("deb-blake", "deb-sha")),
            (Platform::LinuxRpm, hashes("rpm-blake", "rpm-sha")),
        ]);
        let urls = HashMap::from([
            (
                Platform::LinuxDeb,
                "https://updates.example/daedalus.deb".to_string(),
            ),
            (
                Platform::LinuxRpm,
                "https://updates.example/daedalus.rpm".to_string(),
            ),
        ]);
        let signatures = HashMap::from([
            (Platform::LinuxDeb, Some("deb-signature".to_string())),
            (Platform::LinuxRpm, Some("rpm-signature".to_string())),
        ]);

        let value = serde_json::to_value(VersionJson::build(
            "6.0.1",
            &artifact_hashes,
            &urls,
            &signatures,
            None,
        ))
        .expect("serialize version manifest");

        assert!(value["platforms"].get("linux").is_none());
        assert_eq!(
            value["platforms"]["linux-deb"]["URL"],
            "https://updates.example/daedalus.deb"
        );
        assert_eq!(value["platforms"]["linux-deb"]["hash"], "deb-blake");
        assert_eq!(value["platforms"]["linux-deb"]["SHA256"], "deb-sha");
        assert_eq!(
            value["platforms"]["linux-deb"]["signature"],
            "deb-signature"
        );
        assert_eq!(
            value["platforms"]["linux-rpm"]["URL"],
            "https://updates.example/daedalus.rpm"
        );
        assert_eq!(value["platforms"]["linux-rpm"]["hash"], "rpm-blake");
        assert_eq!(value["platforms"]["linux-rpm"]["SHA256"], "rpm-sha");
        assert_eq!(
            value["platforms"]["linux-rpm"]["signature"],
            "rpm-signature"
        );
        assert_eq!(value["platforms"]["linux-deb"]["version"], "6.0.1");
        assert_eq!(value["platforms"]["linux-rpm"]["version"], "6.0.1");
    }

    #[test]
    fn preserves_non_linux_manifest_keys_and_optional_signature() {
        let artifact_hashes = HashMap::from([
            (Platform::DarwinX86, hashes("darwin-blake", "darwin-sha")),
            (Platform::Windows, hashes("windows-blake", "windows-sha")),
        ]);
        let urls = HashMap::from([
            (
                Platform::DarwinX86,
                "https://updates.example/app.pkg".into(),
            ),
            (Platform::Windows, "https://updates.example/app.exe".into()),
        ]);
        let signatures = HashMap::from([
            (Platform::DarwinX86, None),
            (Platform::Windows, Some("windows-signature".into())),
        ]);

        let value = serde_json::to_value(VersionJson::build(
            "6.0.1",
            &artifact_hashes,
            &urls,
            &signatures,
            None,
        ))
        .expect("serialize version manifest");

        assert!(value["platforms"].get("darwin").is_some());
        assert!(value["platforms"].get("windows").is_some());
        assert!(value["platforms"]["darwin"]["signature"].is_null());
        assert_eq!(value["platforms"]["windows"]["version"], "6.0.1");
    }
}
