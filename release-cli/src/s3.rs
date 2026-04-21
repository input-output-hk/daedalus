//! S3 upload helpers.
//!
//! All objects are uploaded with `public-read` ACL, matching the original
//! proposal-ui behaviour.  AWS credentials are resolved through the standard
//! SDK chain (env vars → ~/.aws/credentials → instance profile).
//!
//! Upload strategy for each installer (replicating the Haskell logic):
//!   1. Upload the file under the blake2b-cbor hex hash as the S3 key.
//!   2. Copy that object to a second key equal to the bare filename.
//!   3. The CDN URL of the filename key is what goes into the version JSON.

use anyhow::{Context, Result};
use aws_config::BehaviorVersion;
use aws_sdk_s3::{Client, primitives::ByteStream, types::ObjectCannedAcl};
use bytes::Bytes;
use std::path::Path;

pub struct S3Client {
    client: Client,
    pub bucket: String,
    pub bucket_url: String,
}

impl S3Client {
    pub async fn new(bucket: String, bucket_url: String) -> Result<Self> {
        let config = aws_config::load_defaults(BehaviorVersion::latest()).await;
        let client = Client::new(&config);
        Ok(Self {
            client,
            bucket,
            bucket_url,
        })
    }

    /// `https://<bucket_url>/<key>`
    pub fn cdn_url(&self, key: &str) -> String {
        format!("https://{}/{}", self.bucket_url, key)
    }

    /// Upload the installer at `path`:
    ///   - first under the blake2b-cbor `hash` key
    ///   - then copy to the `filename` key (public-read)
    ///
    /// Returns the CDN URL of the filename-keyed object (used in the JSON).
    pub async fn upload_installer(
        &self,
        path: &Path,
        hash: &str,
        filename: &str,
    ) -> Result<String> {
        println!("  [{filename}] uploading as hash key {hash}");
        let body = ByteStream::from_path(path)
            .await
            .with_context(|| format!("opening {}", path.display()))?;

        self.client
            .put_object()
            .bucket(&self.bucket)
            .key(hash)
            .acl(ObjectCannedAcl::PublicRead)
            .body(body)
            .send()
            .await
            .with_context(|| format!("S3 PutObject {hash}"))?;

        println!("  [{filename}] copying {hash} → {filename}");
        let copy_source = format!("{}/{}", self.bucket, hash);
        self.client
            .copy_object()
            .copy_source(&copy_source)
            .bucket(&self.bucket)
            .key(filename)
            .acl(ObjectCannedAcl::PublicRead)
            .send()
            .await
            .with_context(|| format!("S3 CopyObject {hash} → {filename}"))?;

        Ok(self.cdn_url(filename))
    }

    /// Upload a GPG detached-signature file.
    /// Stored under the bare filename (no path prefix), no explicit ACL.
    pub async fn upload_signature(&self, path: &Path, filename: &str) -> Result<()> {
        println!("  [sig] uploading {filename}");
        let body = ByteStream::from_path(path)
            .await
            .with_context(|| format!("opening {}", path.display()))?;

        self.client
            .put_object()
            .bucket(&self.bucket)
            .key(filename)
            .body(body)
            .send()
            .await
            .with_context(|| format!("S3 PutObject {filename}"))?;

        Ok(())
    }

    /// Upload `daedalus-latest-version.json` with public-read ACL.
    /// Returns the CDN URL.
    pub async fn upload_version_json(&self, json: &[u8]) -> Result<String> {
        println!("  [version] uploading daedalus-latest-version.json");
        self.client
            .put_object()
            .bucket(&self.bucket)
            .key("daedalus-latest-version.json")
            .acl(ObjectCannedAcl::PublicRead)
            .content_type("application/json")
            .body(ByteStream::from(Bytes::copy_from_slice(json)))
            .send()
            .await
            .context("S3 PutObject daedalus-latest-version.json")?;

        Ok(self.cdn_url("daedalus-latest-version.json"))
    }
}
