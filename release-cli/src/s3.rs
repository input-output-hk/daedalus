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
use aws_credential_types::Credentials;
use aws_sdk_s3::{Client, primitives::ByteStream, types::ObjectCannedAcl};
use bytes::Bytes;
use std::path::Path;

pub struct S3Client {
    client: Client,
    /// The bare S3 bucket name (no slashes).
    pub bucket: String,
    /// Optional key prefix (the path after the first `/` in the original --bucket arg).
    /// All object keys are stored under `<prefix>/<key>` when non-empty.
    pub key_prefix: String,
    /// CDN base URL used to build public download links.
    pub bucket_url: String,
    /// When true, omit S3 ACL from all PutObject/CopyObject requests.
    /// Required for Cloudflare R2 buckets that have not enabled per-object ACLs.
    pub use_acl: bool,
}

impl S3Client {
    /// `bucket_arg` may be `"my-bucket"` or `"my-bucket/some/prefix"`.
    /// The part before the first `/` is the S3 bucket name; the rest is the key prefix.
    ///
    /// `endpoint_url` overrides the S3 endpoint (e.g. Cloudflare R2).
    /// `use_acl` — set to false for R2 or any S3-compatible store that does not
    /// support per-object ACLs; set to true for AWS S3 where public-read ACL
    /// controls object visibility.
    /// `credentials` — when Some, bypasses the standard AWS credential chain and
    /// injects the key/secret directly. Used by `publish-linux-repos` so R2 creds
    /// (`REPO_ACCESS_KEY_ID` / `REPO_SECRET_ACCESS_KEY`) don't collide with the S3
    /// creds (`AWS_ACCESS_KEY_ID` / `AWS_SECRET_ACCESS_KEY`) used by `drt release`.
    pub async fn new(
        bucket_arg: String,
        bucket_url: String,
        endpoint_url: Option<String>,
        use_acl: bool,
        credentials: Option<(String, String)>,
    ) -> Result<Self> {
        let (bucket, key_prefix) = match bucket_arg.find('/') {
            Some(i) => (bucket_arg[..i].to_string(), bucket_arg[i + 1..].to_string()),
            None => (bucket_arg, String::new()),
        };
        let config = {
            let mut builder = aws_config::from_env().behavior_version(BehaviorVersion::latest());
            if let Some((key_id, secret)) = credentials {
                builder = builder
                    .credentials_provider(Credentials::new(key_id, secret, None, None, "r2"));
                // R2 requires one of its own region names; override whatever
                // ~/.aws/config says. REPO_REGION defaults to "auto".
                let region = std::env::var("REPO_REGION").unwrap_or_else(|_| "auto".to_string());
                builder = builder.region(aws_config::Region::new(region));
            }
            builder.load().await
        };
        let mut s3_cfg = aws_sdk_s3::config::Builder::from(&config);
        if let Some(url) = endpoint_url {
            s3_cfg = s3_cfg.endpoint_url(url);
        }
        let client = Client::from_conf(s3_cfg.build());
        Ok(Self {
            client,
            bucket,
            key_prefix,
            bucket_url,
            use_acl,
        })
    }

    /// Return `true` if an object at `key` (prefixed) already exists in the bucket.
    async fn object_exists(&self, key: &str) -> Result<bool> {
        let prefixed = self.prefixed_key(key);
        match self
            .client
            .head_object()
            .bucket(&self.bucket)
            .key(&prefixed)
            .send()
            .await
        {
            Ok(_) => Ok(true),
            Err(e)
                if e.as_service_error()
                    .map(|se| se.is_not_found())
                    .unwrap_or(false) =>
            {
                Ok(false)
            }
            Err(e) => Err(anyhow::anyhow!("HeadObject {prefixed}: {e}")),
        }
    }

    /// Prepend the key prefix when one is configured.
    fn prefixed_key(&self, key: &str) -> String {
        if self.key_prefix.is_empty() {
            key.to_string()
        } else {
            format!("{}/{key}", self.key_prefix)
        }
    }

    /// `https://<bucket_url>/<key>`
    pub fn cdn_url(&self, key: &str) -> String {
        format!("https://{}/{key}", self.bucket_url)
    }

    /// Upload the installer at `path`:
    ///   - first under the blake2b-cbor `hash` key (prefixed)
    ///   - then copy to the `filename` key (prefixed, public-read)
    ///
    /// Returns the CDN URL of the filename-keyed object (used in the JSON).
    pub async fn upload_installer(
        &self,
        path: &Path,
        hash: &str,
        filename: &str,
    ) -> Result<String> {
        let hash_key = self.prefixed_key(hash);
        let filename_key = self.prefixed_key(filename);

        if self.object_exists(hash).await? {
            println!("  [{filename}] already uploaded (hash key exists), skipping");
        } else {
            println!("  [{filename}] uploading as hash key {hash_key}");
            let body = ByteStream::from_path(path)
                .await
                .with_context(|| format!("opening {}", path.display()))?;

            let mut req = self
                .client
                .put_object()
                .bucket(&self.bucket)
                .key(&hash_key)
                .body(body);
            if self.use_acl {
                req = req.acl(ObjectCannedAcl::PublicRead);
            }
            req.send()
                .await
                .with_context(|| format!("S3 PutObject {hash_key}"))?;
        }

        if self.object_exists(filename).await? {
            println!("  [{filename}] filename key already present, skipping copy");
        } else {
            println!("  [{filename}] copying {hash_key} → {filename_key}");
            let copy_source = format!("{}/{hash_key}", self.bucket);
            let mut req = self
                .client
                .copy_object()
                .copy_source(&copy_source)
                .bucket(&self.bucket)
                .key(&filename_key);
            if self.use_acl {
                req = req.acl(ObjectCannedAcl::PublicRead);
            }
            req.send()
                .await
                .with_context(|| format!("S3 CopyObject {hash_key} → {filename_key}"))?;
        }

        Ok(self.cdn_url(filename))
    }

    /// Upload a GPG detached-signature file.
    pub async fn upload_signature(&self, path: &Path, filename: &str) -> Result<()> {
        if self.object_exists(filename).await? {
            println!("  [sig] {filename} already uploaded, skipping");
            return Ok(());
        }
        let key = self.prefixed_key(filename);
        println!("  [sig] uploading {key}");
        let body = ByteStream::from_path(path)
            .await
            .with_context(|| format!("opening {}", path.display()))?;

        let mut req = self
            .client
            .put_object()
            .bucket(&self.bucket)
            .key(&key)
            .body(body);
        if self.use_acl {
            req = req.acl(ObjectCannedAcl::PublicRead);
        }
        req.send()
            .await
            .with_context(|| format!("S3 PutObject {key}"))?;

        Ok(())
    }

    /// Upload arbitrary bytes under `key` (prefixed).
    /// Pass `cache_control = Some("no-store")` to prevent CDN caching.
    pub async fn upload_bytes(
        &self,
        key: &str,
        bytes: &[u8],
        content_type: &str,
        cache_control: Option<&str>,
    ) -> Result<()> {
        let prefixed = self.prefixed_key(key);
        println!("  [upload] {prefixed}");
        let mut req = self
            .client
            .put_object()
            .bucket(&self.bucket)
            .key(&prefixed)
            .content_type(content_type)
            .body(ByteStream::from(Bytes::copy_from_slice(bytes)));
        if self.use_acl {
            req = req.acl(ObjectCannedAcl::PublicRead);
        }
        if let Some(cc) = cache_control {
            req = req.cache_control(cc);
        }
        req.send()
            .await
            .with_context(|| format!("S3 PutObject {prefixed}"))?;
        Ok(())
    }

    /// Upload `daedalus-latest-version.json`. Returns the CDN URL.
    pub async fn upload_version_json(&self, json: &[u8]) -> Result<String> {
        let key = self.prefixed_key("daedalus-latest-version.json");
        println!("  [version] uploading {key}");
        let mut req = self
            .client
            .put_object()
            .bucket(&self.bucket)
            .key(&key)
            .content_type("application/json")
            .body(ByteStream::from(Bytes::copy_from_slice(json)));
        if self.use_acl {
            req = req.acl(ObjectCannedAcl::PublicRead);
        }
        req.send()
            .await
            .with_context(|| format!("S3 PutObject {key}"))?;

        Ok(self.cdn_url("daedalus-latest-version.json"))
    }
}
