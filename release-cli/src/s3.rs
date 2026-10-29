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
use aws_sdk_s3::{
    Client,
    operation::head_object::HeadObjectOutput,
    primitives::ByteStream,
    types::{MetadataDirective, ObjectCannedAcl},
};
use bytes::Bytes;
use sha2::{Digest, Sha256};
use std::path::Path;

fn repo_content_matches(head: &HeadObjectOutput, bytes: &[u8], sha256: &str) -> bool {
    if head.content_length().and_then(|n| usize::try_from(n).ok()) != Some(bytes.len()) {
        return false;
    }
    if let Some(checksum) = head.metadata().and_then(|metadata| metadata.get("sha256")) {
        return checksum == sha256;
    }
    // Older single-PUT uploads often have an MD5 ETag. An opaque or multipart
    // ETag is not proof of equality; upload once to establish SHA-256 metadata.
    let Some(etag) = head.e_tag() else {
        return false;
    };
    let etag = etag.trim_matches('"');
    etag.len() == 32
        && etag.bytes().all(|byte| byte.is_ascii_hexdigit())
        && etag.eq_ignore_ascii_case(&hex::encode(md5::Md5::digest(bytes)))
}

fn copy_source(bucket: &str, key: &str) -> String {
    use std::fmt::Write;
    let mut encoded = String::new();
    for byte in format!("{bucket}/{key}").bytes() {
        if byte.is_ascii_alphanumeric() || b"/-_.~".contains(&byte) {
            encoded.push(char::from(byte));
        } else {
            write!(&mut encoded, "%{byte:02X}").expect("writing into a String");
        }
    }
    encoded
}

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

    /// Upload repository bytes only when the remote content differs. Use HEAD
    /// rather than downloading packages, and refresh matching objects' headers
    /// with a conditional server-side copy when necessary.
    pub async fn upload_repo_bytes(
        &self,
        key: &str,
        bytes: &[u8],
        content_type: &str,
        cache_control: Option<&str>,
    ) -> Result<()> {
        let prefixed = self.prefixed_key(key);
        let sha256 = hex::encode(Sha256::digest(bytes));
        let remote = self
            .client
            .head_object()
            .bucket(&self.bucket)
            .key(&prefixed)
            .send()
            .await;
        match remote {
            Ok(head) if repo_content_matches(&head, bytes, &sha256) => {
                let has_sha256 =
                    head.metadata().and_then(|metadata| metadata.get("sha256")) == Some(&sha256);
                if has_sha256
                    && head.content_type() == Some(content_type)
                    && head.cache_control() == cache_control
                {
                    println!("  [skip] {prefixed} (size and SHA-256 match)");
                    return Ok(());
                }
                if let Some(etag) = head.e_tag() {
                    println!("  [metadata] {prefixed} (contents match; server-side copy)");
                    let mut metadata = head.metadata().cloned().unwrap_or_default();
                    metadata.insert("sha256".to_owned(), sha256);
                    let mut req = self
                        .client
                        .copy_object()
                        .bucket(&self.bucket)
                        .key(&prefixed)
                        .copy_source(copy_source(&self.bucket, &prefixed))
                        .copy_source_if_match(etag)
                        .metadata_directive(MetadataDirective::Replace)
                        .set_metadata(Some(metadata))
                        .content_type(content_type)
                        .set_cache_control(cache_control.map(str::to_owned))
                        .set_content_encoding(head.content_encoding().map(str::to_owned))
                        .set_content_disposition(head.content_disposition().map(str::to_owned))
                        .set_content_language(head.content_language().map(str::to_owned));
                    if self.use_acl {
                        req = req.acl(ObjectCannedAcl::PublicRead);
                    }
                    req.send()
                        .await
                        .with_context(|| format!("S3 metadata refresh {prefixed}"))?;
                    return Ok(());
                }
            }
            Ok(_) => {}
            Err(e)
                if e.as_service_error()
                    .map(|e| e.is_not_found())
                    .unwrap_or(false) => {}
            Err(e) => return Err(anyhow::anyhow!("S3 HeadObject {prefixed}: {e}")),
        }
        self.put_bytes(key, bytes, content_type, cache_control, Some(&sha256))
            .await
    }

    /// Verify the origin's size and checksum against the prepared metadata.
    pub async fn verify_repo_bytes(&self, key: &str, bytes: &[u8]) -> Result<()> {
        let prefixed = self.prefixed_key(key);
        let head = self
            .client
            .head_object()
            .bucket(&self.bucket)
            .key(&prefixed)
            .send()
            .await
            .with_context(|| format!("verifying S3 HeadObject {prefixed}"))?;
        anyhow::ensure!(
            repo_content_matches(&head, bytes, &hex::encode(Sha256::digest(bytes))),
            "published repository checksum mismatch: {prefixed}"
        );
        Ok(())
    }

    /// Delete only an explicitly owned inventory object; callers never enumerate
    /// the bucket or use broad prefix deletion. Missing objects are idempotent.
    pub async fn delete_repo_object(&self, key: &str) -> Result<()> {
        let prefixed = self.prefixed_key(key);
        println!("  [collect] {prefixed}");
        self.client
            .delete_object()
            .bucket(&self.bucket)
            .key(&prefixed)
            .send()
            .await
            .with_context(|| format!("S3 DeleteObject {prefixed}"))?;
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
        self.put_bytes(key, bytes, content_type, cache_control, None)
            .await
    }

    async fn put_bytes(
        &self,
        key: &str,
        bytes: &[u8],
        content_type: &str,
        cache_control: Option<&str>,
        sha256: Option<&str>,
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
        if let Some(checksum) = sha256 {
            req = req.metadata("sha256", checksum);
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

#[cfg(test)]
mod tests {
    use super::*;
    use axum::{
        Router,
        body::Body,
        extract::State,
        http::{Request, Response, StatusCode},
    };
    use std::{
        collections::HashMap,
        sync::{Arc, Mutex},
    };

    #[derive(Clone)]
    struct Object {
        bytes: Vec<u8>,
        etag: String,
        metadata: HashMap<String, String>,
        content_type: String,
        cache_control: Option<String>,
    }

    impl Object {
        fn new(bytes: &[u8], sha256: bool) -> Self {
            let mut metadata = HashMap::new();
            if sha256 {
                metadata.insert("sha256".to_owned(), hex::encode(Sha256::digest(bytes)));
            }
            Self {
                bytes: bytes.to_vec(),
                etag: format!("\"{}\"", hex::encode(md5::Md5::digest(bytes))),
                metadata,
                content_type: "application/octet-stream".to_owned(),
                cache_control: Some("no-store".to_owned()),
            }
        }
    }

    #[derive(Default)]
    struct Store {
        object: Option<Object>,
        requests: Vec<(String, String, axum::http::HeaderMap, usize)>,
        head_forbidden: bool,
        copy_conflict: bool,
        delete_forbidden: bool,
    }

    struct Fixture {
        client: S3Client,
        state: Arc<Mutex<Store>>,
        server: tokio::task::JoinHandle<()>,
    }

    impl Drop for Fixture {
        fn drop(&mut self) {
            self.server.abort();
        }
    }

    impl Fixture {
        async fn new(object: Option<Object>) -> Self {
            let state = Arc::new(Mutex::new(Store {
                object,
                ..Store::default()
            }));
            let app = Router::new().fallback(handle).with_state(state.clone());
            let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
            let address = listener.local_addr().unwrap();
            let server = tokio::spawn(async move {
                axum::serve(listener, app).await.unwrap();
            });
            let config = aws_sdk_s3::config::Builder::new()
                .behavior_version(BehaviorVersion::latest())
                .region(aws_sdk_s3::config::Region::new("us-east-1"))
                .credentials_provider(Credentials::new("test", "test", None, None, "fixture"))
                .endpoint_url(format!("http://{address}"))
                .force_path_style(true)
                .request_checksum_calculation(
                    aws_sdk_s3::config::RequestChecksumCalculation::WhenRequired,
                )
                .build();
            let client = S3Client {
                client: Client::from_conf(config),
                bucket: "test-bucket".to_owned(),
                key_prefix: "prefix".to_owned(),
                bucket_url: "unused.invalid".to_owned(),
                use_acl: false,
            };
            Self {
                client,
                state,
                server,
            }
        }

        async fn upload(&self, bytes: &[u8]) -> Result<()> {
            self.client
                .upload_repo_bytes(
                    "arch/package.pkg.tar.zst",
                    bytes,
                    "application/octet-stream",
                    Some("no-store"),
                )
                .await
        }

        fn methods(&self) -> Vec<String> {
            self.state
                .lock()
                .unwrap()
                .requests
                .iter()
                .map(|r| r.0.clone())
                .collect()
        }
    }

    async fn handle(
        State(state): State<Arc<Mutex<Store>>>,
        request: Request<Body>,
    ) -> Response<Body> {
        let (parts, body) = request.into_parts();
        let body = axum::body::to_bytes(body, 1024 * 1024).await.unwrap();
        let mut store = state.lock().unwrap();
        store.requests.push((
            parts.method.to_string(),
            parts.uri.path().to_owned(),
            parts.headers.clone(),
            body.len(),
        ));
        if parts.method == "HEAD" {
            if store.head_forbidden {
                return Response::builder()
                    .status(StatusCode::FORBIDDEN)
                    .body(Body::empty())
                    .unwrap();
            }
            let Some(object) = &store.object else {
                return Response::builder()
                    .status(StatusCode::NOT_FOUND)
                    .body(Body::empty())
                    .unwrap();
            };
            let mut response = Response::builder()
                .header("content-length", object.bytes.len())
                .header("content-type", &object.content_type)
                .header("etag", &object.etag);
            if let Some(cache) = &object.cache_control {
                response = response.header("cache-control", cache);
            }
            for (key, value) in &object.metadata {
                response = response.header(format!("x-amz-meta-{key}"), value);
            }
            return response.body(Body::empty()).unwrap();
        }
        if parts.method == "DELETE" {
            if store.delete_forbidden {
                return Response::builder()
                    .status(StatusCode::FORBIDDEN)
                    .body(Body::from("<Error><Code>AccessDenied</Code></Error>"))
                    .unwrap();
            }
            store.object = None;
            return Response::builder()
                .status(StatusCode::NO_CONTENT)
                .body(Body::empty())
                .unwrap();
        }
        assert_eq!(parts.method, "PUT");
        let copying = parts.headers.contains_key("x-amz-copy-source");
        if copying && store.copy_conflict {
            return Response::builder()
                .status(StatusCode::PRECONDITION_FAILED)
                .body(Body::from("<Error><Code>PreconditionFailed</Code></Error>"))
                .unwrap();
        }
        let bytes = if copying {
            let object = store.object.as_ref().unwrap();
            assert_eq!(
                parts.headers.get("x-amz-copy-source-if-match").unwrap(),
                object.etag.as_str()
            );
            assert_eq!(
                parts.headers.get("x-amz-metadata-directive").unwrap(),
                "REPLACE"
            );
            assert!(body.is_empty());
            object.bytes.clone()
        } else {
            body.to_vec()
        };
        let mut object = Object::new(&bytes, false);
        object.metadata = parts
            .headers
            .iter()
            .filter_map(|(name, value)| {
                name.as_str()
                    .strip_prefix("x-amz-meta-")
                    .map(|name| (name.to_owned(), value.to_str().unwrap().to_owned()))
            })
            .collect();
        object.content_type = parts
            .headers
            .get("content-type")
            .unwrap()
            .to_str()
            .unwrap()
            .to_owned();
        object.cache_control = parts
            .headers
            .get("cache-control")
            .map(|h| h.to_str().unwrap().to_owned());
        let etag = object.etag.clone();
        store.object = Some(object);
        if copying {
            Response::builder()
                .header("content-type", "application/xml")
                .body(Body::from(format!(
                    "<CopyObjectResult><ETag>{etag}</ETag></CopyObjectResult>"
                )))
                .unwrap()
        } else {
            Response::builder()
                .header("etag", etag)
                .body(Body::empty())
                .unwrap()
        }
    }

    #[tokio::test]
    async fn new_upload_records_checksum_and_second_run_only_heads() {
        let fixture = Fixture::new(None).await;
        fixture.upload(b"package").await.unwrap();
        fixture.upload(b"package").await.unwrap();
        assert_eq!(fixture.methods(), ["HEAD", "PUT", "HEAD"]);
        let store = fixture.state.lock().unwrap();
        assert!(
            store
                .requests
                .iter()
                .all(|request| request.1 == "/test-bucket/prefix/arch/package.pkg.tar.zst")
        );
        assert!(
            store
                .requests
                .iter()
                .all(|request| !request.2.contains_key("x-amz-acl"))
        );
        let object = store.object.as_ref().unwrap();
        assert_eq!(object.bytes, b"package");
        assert_eq!(
            object.metadata["sha256"],
            hex::encode(Sha256::digest(b"package"))
        );
    }

    #[tokio::test]
    async fn origin_verification_rejects_changed_or_missing_metadata() {
        let fixture = Fixture::new(Some(Object::new(b"metadata", true))).await;
        fixture
            .client
            .verify_repo_bytes("apt/dists/stable/InRelease", b"metadata")
            .await
            .unwrap();
        assert!(
            fixture
                .client
                .verify_repo_bytes("apt/dists/stable/InRelease", b"changed!")
                .await
                .is_err()
        );
        fixture.state.lock().unwrap().object = None;
        assert!(
            fixture
                .client
                .verify_repo_bytes("apt/dists/stable/InRelease", b"metadata")
                .await
                .is_err()
        );
        assert_eq!(fixture.methods(), ["HEAD", "HEAD", "HEAD"]);
        assert!(
            fixture
                .state
                .lock()
                .unwrap()
                .requests
                .iter()
                .all(|request| request.1 == "/test-bucket/prefix/apt/dists/stable/InRelease")
        );
    }

    #[tokio::test]
    async fn garbage_collection_honors_prefix_and_retries_missing_objects() {
        let fixture = Fixture::new(Some(Object::new(b"old package", true))).await;
        fixture
            .client
            .delete_repo_object("yum/packages/old.rpm")
            .await
            .unwrap();
        fixture
            .client
            .delete_repo_object("yum/packages/old.rpm")
            .await
            .unwrap();
        assert_eq!(fixture.methods(), ["DELETE", "DELETE"]);
        assert!(fixture.state.lock().unwrap().object.is_none());
        assert!(
            fixture
                .state
                .lock()
                .unwrap()
                .requests
                .iter()
                .all(|request| request.1 == "/test-bucket/prefix/yum/packages/old.rpm")
        );
    }

    #[tokio::test]
    async fn garbage_collection_propagates_permission_errors() {
        let fixture = Fixture::new(Some(Object::new(b"old package", true))).await;
        fixture.state.lock().unwrap().delete_forbidden = true;
        assert!(
            fixture
                .client
                .delete_repo_object("arch/old.pkg.tar.zst")
                .await
                .is_err()
        );
        assert!(fixture.state.lock().unwrap().object.is_some());
    }

    #[tokio::test]
    async fn changed_content_is_uploaded_even_when_size_matches() {
        let fixture = Fixture::new(Some(Object::new(b"old", true))).await;
        fixture.upload(b"new").await.unwrap();
        assert_eq!(fixture.methods(), ["HEAD", "PUT"]);
        assert_eq!(
            fixture.state.lock().unwrap().object.as_ref().unwrap().bytes,
            b"new"
        );
    }

    #[tokio::test]
    async fn legacy_md5_match_upgrades_metadata_without_uploading_bytes() {
        let mut object = Object::new(b"package", false);
        object
            .metadata
            .insert("keep".to_owned(), "existing".to_owned());
        let fixture = Fixture::new(Some(object)).await;
        fixture.upload(b"package").await.unwrap();
        fixture.upload(b"package").await.unwrap();
        assert_eq!(fixture.methods(), ["HEAD", "PUT", "HEAD"]);
        let store = fixture.state.lock().unwrap();
        assert_eq!(store.requests[1].3, 0);
        assert_eq!(
            store.requests[1].2["x-amz-copy-source"],
            "test-bucket/prefix/arch/package.pkg.tar.zst"
        );
        assert_eq!(store.object.as_ref().unwrap().metadata["keep"], "existing");
    }

    #[tokio::test]
    async fn matching_content_refreshes_headers_with_server_side_copy() {
        let mut object = Object::new(b"package", true);
        object.cache_control = None;
        object.content_type = "text/plain".to_owned();
        let mut fixture = Fixture::new(Some(object)).await;
        fixture.client.use_acl = true;
        fixture.upload(b"package").await.unwrap();
        let store = fixture.state.lock().unwrap();
        assert_eq!(store.requests[1].3, 0);
        assert_eq!(store.requests[1].2["x-amz-acl"], "public-read");
        let object = store.object.as_ref().unwrap();
        assert_eq!(object.cache_control.as_deref(), Some("no-store"));
        assert_eq!(object.content_type, "application/octet-stream");
    }

    #[tokio::test]
    async fn ambiguous_etag_or_wrong_sha256_does_not_skip_upload() {
        for (etag, sha256) in [
            ("\"multipart-2\"", None),
            ("opaque", None),
            ("\"321c3cf486ed509164edec1e1981fec8\"", Some("wrong")),
        ] {
            let mut object = Object::new(b"package", false);
            if sha256.is_none() {
                object.etag = etag.to_owned();
            }
            if let Some(checksum) = sha256 {
                object
                    .metadata
                    .insert("sha256".to_owned(), checksum.to_owned());
            }
            let fixture = Fixture::new(Some(object)).await;
            fixture.upload(b"package").await.unwrap();
            let store = fixture.state.lock().unwrap();
            assert!(!store.requests[1].2.contains_key("x-amz-copy-source"));
            assert_eq!(store.requests[1].3, b"package".len());
        }
    }

    #[tokio::test]
    async fn head_errors_and_copy_conflicts_abort_without_uploading_body() {
        let fixture = Fixture::new(None).await;
        fixture.state.lock().unwrap().head_forbidden = true;
        assert!(fixture.upload(b"package").await.is_err());
        assert_eq!(fixture.methods(), ["HEAD"]);
        let fixture = Fixture::new(Some(Object::new(b"package", false))).await;
        fixture.state.lock().unwrap().copy_conflict = true;
        assert!(fixture.upload(b"package").await.is_err());
        assert_eq!(fixture.methods(), ["HEAD", "PUT"]);
        assert_eq!(fixture.state.lock().unwrap().requests[1].3, 0);
    }

    #[tokio::test]
    async fn matching_checksum_with_inconsistent_length_does_not_skip() {
        let mut object = Object::new(b"short", true);
        object
            .metadata
            .insert("sha256".to_owned(), hex::encode(Sha256::digest(b"package")));
        let fixture = Fixture::new(Some(object)).await;
        fixture.upload(b"package").await.unwrap();
        assert_eq!(fixture.methods(), ["HEAD", "PUT"]);
        assert_eq!(
            fixture.state.lock().unwrap().requests[1].3,
            b"package".len()
        );
    }

    #[test]
    fn copy_source_encodes_keys_without_losing_prefix_separators() {
        assert_eq!(
            copy_source("bucket", "prefix/a b+%.sig"),
            "bucket/prefix/a%20b%2B%25.sig"
        );
    }
}
