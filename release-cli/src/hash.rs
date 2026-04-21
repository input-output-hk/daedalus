//! Installer hashing.
//!
//! The Daedalus updater uses two hashes for each installer:
//!
//! * **blake2b-cbor** (`hash` field in the JSON) — Blake2b-256 of the
//!   CBOR-encoded byte string of the file.  The CBOR encoding prepends a
//!   definite-length header (major type 2) before the raw bytes.  This
//!   matches the `installerHash` function in the legacy proposal-ui Haskell
//!   code (Temp.hs).
//!
//! * **sha256** (`SHA256` field) — plain SHA-256 of the raw file bytes.
//!
//! Both digests are returned as lowercase hex strings.
//!
//! Files are read in streaming 1 MiB chunks; they are never fully loaded
//! into memory.

use anyhow::{Context, Result};
use blake2::digest::consts::U32;
use blake2::{Blake2b, Digest as _};
use sha2::Sha256;
use std::io::Read;
use std::path::Path;

type Blake2b256 = Blake2b<U32>;

/// Build the CBOR definite-length byte-string header for a payload of
/// `len` bytes (CBOR major type 2).
fn cbor_bytes_header(len: usize) -> Vec<u8> {
    if len <= 23 {
        vec![0x40 | len as u8]
    } else if len <= 0xFF {
        vec![0x58, len as u8]
    } else if len <= 0xFFFF {
        vec![0x59, (len >> 8) as u8, len as u8]
    } else if len <= 0xFFFF_FFFF {
        vec![
            0x5A,
            (len >> 24) as u8,
            (len >> 16) as u8,
            (len >> 8) as u8,
            len as u8,
        ]
    } else {
        let mut v = vec![0x5B];
        v.extend_from_slice(&(len as u64).to_be_bytes());
        v
    }
}

pub struct Hashes {
    /// Lowercase hex of Blake2b-256( CBOR-encode-bytes( file ) ).
    pub blake2b_cbor: String,
    /// Lowercase hex of SHA-256( file ).
    pub sha256: String,
}

/// Compute both hashes for `path` using streaming I/O.
pub fn hash_file(path: &Path) -> Result<Hashes> {
    let meta = std::fs::metadata(path).with_context(|| format!("stat {}", path.display()))?;
    let file_size = meta.len() as usize;

    // CBOR header is fed only to the Blake2b hasher (SHA-256 covers raw bytes).
    let header = cbor_bytes_header(file_size);

    let mut blake2b = Blake2b256::new();
    let mut sha256 = Sha256::new();

    blake2b.update(&header);

    let file = std::fs::File::open(path).with_context(|| format!("open {}", path.display()))?;
    let mut reader = std::io::BufReader::with_capacity(1 << 20, file);
    let mut buf = vec![0u8; 1 << 20];

    loop {
        let n = reader.read(&mut buf)?;
        if n == 0 {
            break;
        }
        blake2b.update(&buf[..n]);
        sha256.update(&buf[..n]);
    }

    Ok(Hashes {
        blake2b_cbor: hex::encode(blake2b.finalize()),
        sha256: hex::encode(sha256.finalize()),
    })
}
