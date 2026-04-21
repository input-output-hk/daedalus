//! Generate `daedalus-latest-version.json`.
//!
//! Format (matches the legacy proposal-ui output):
//!
//! ```json
//! {
//!   "platforms": {
//!     "linux":   { "version": "6.0.1", "URL": "https://...", "hash": "<blake2b-cbor-hex>", "SHA256": "<sha256-hex>", "signature": "..." },
//!     "darwin":  { ... },
//!     "windows": { ... }
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
