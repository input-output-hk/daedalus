//! One locked deployment transaction: import, select, prepare, publish, then GC.

use anyhow::{Context, Result, ensure};
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use crate::linux_repo_cmd::{self, Format, LinuxPackage, PublishLinuxReposOpts};
use crate::linux_repo_state::{Inventory, Selection, Target};
use crate::s3::S3Client;

pub async fn publish(opts: PublishLinuxReposOpts<'_>) -> Result<()> {
    let enabled: Vec<_> = [
        (Format::Deb, opts.skip_apt),
        (Format::Rpm, opts.skip_yum),
        (Format::ArchPkg, opts.skip_arch),
    ]
    .into_iter()
    .filter_map(|(format, skip)| (!skip).then_some(format))
    .collect();
    ensure!(!enabled.is_empty(), "all repository formats are skipped");
    let grace = opts
        .gc_grace_days
        .checked_mul(86_400)
        .context("GC grace period is too large")?;
    let now = SystemTime::now().duration_since(UNIX_EPOCH)?.as_secs();
    let endpoint = std::env::var("REPO_ENDPOINT_URL")
        .ok()
        .or(opts.endpoint_url)
        .or_else(|| std::env::var("AWS_ENDPOINT_URL_S3").ok())
        .or_else(|| std::env::var("AWS_ENDPOINT_URL").ok());
    let target = Target {
        bucket: opts.bucket.to_owned(),
        endpoint: endpoint.clone(),
    };
    let mut inventory = Inventory::open(opts.state_dir, target, opts.initialize, opts.dry_run)?;
    let incoming: Vec<_> = linux_repo_cmd::scan_linux_packages(opts.installers_dir)?
        .into_iter()
        .filter(|p| enabled.contains(&p.format))
        .collect();
    ensure!(
        !incoming.is_empty() || !inventory.packages().is_empty(),
        "no packages in incoming directory or inventory"
    );
    inventory.import(&incoming)?;
    let selection = inventory.select(&enabled, now, grace)?;
    ensure!(
        !selection.retained.is_empty(),
        "no packages for enabled repository formats"
    );
    print_plan(&inventory, &selection, opts.gc_grace_days);
    if opts.dry_run {
        println!("Dry run: no persistent imports, signing, S3 requests or deletions.");
        return Ok(());
    }

    let packages: Vec<_> = selection
        .retained
        .iter()
        .map(|filename| {
            let package = inventory
                .packages()
                .iter()
                .find(|p| p.filename() == filename)
                .expect("selected inventory package");
            LinuxPackage {
                path: inventory.package_path(filename),
                filename: filename.clone(),
                format: package.identity.format,
            }
        })
        .collect();
    let gpg_env = std::env::var("GPG_USER").ok();
    let gpg = opts.gpg_user.or(gpg_env.as_deref());
    let generation = tempfile::tempdir()?;
    linux_repo_cmd::prepare_publication(&packages, gpg, generation.path(), now)?;

    // No credentials are resolved until after dry run and complete preparation.
    let credentials = match (
        std::env::var("REPO_ACCESS_KEY_ID").ok(),
        std::env::var("REPO_SECRET_ACCESS_KEY").ok(),
    ) {
        (Some(key), Some(secret)) => Some((key, secret)),
        (None, None) => None,
        _ => anyhow::bail!("set both REPO_ACCESS_KEY_ID and REPO_SECRET_ACCESS_KEY"),
    };
    let s3 = S3Client::new(
        opts.bucket.to_owned(),
        opts.bucket_url.to_owned(),
        endpoint,
        !opts.no_acl,
        credentials,
    )
    .await?;
    publish_generation(&mut inventory, &selection, generation.path(), &s3, now).await?;
    println!("Done: repositories published and verified; automatic collection complete.");
    Ok(())
}

fn print_plan(inventory: &Inventory, selection: &Selection, grace_days: u64) {
    println!(
        "Retention: current and previous per native package/format/architecture; GC grace {grace_days} day(s)."
    );
    for filename in &selection.retained {
        println!("  [retain] {filename}");
    }
    for filename in &selection.retired {
        let package = inventory
            .packages()
            .iter()
            .find(|p| p.filename() == filename)
            .expect("selected inventory package");
        let action = if selection.delete.contains(filename) {
            "collect after verified publication"
        } else if package.retired_at.is_none() {
            "withdraw; start grace after verified publication"
        } else {
            "withdrawn; waiting for grace"
        };
        println!("  [{action}] {filename}");
    }
}

// Generic boundary lets failure/retry tests exercise the real lifecycle without
// deployment credentials or replacing the production signing identity.
trait RepositoryStore {
    async fn upload(&self, key: &str, path: &Path) -> Result<()>;
    async fn verify(&self, key: &str, path: &Path) -> Result<()>;
    async fn delete(&self, key: &str) -> Result<()>;
}

impl RepositoryStore for S3Client {
    async fn upload(&self, key: &str, path: &Path) -> Result<()> {
        let content_type = if key.ends_with(".sig") {
            "application/pgp-signature"
        } else if key.ends_with(".gpg") {
            "application/pgp-keys"
        } else if key.ends_with(".gz") {
            "application/gzip"
        } else if key.ends_with(".asc")
            || key.ends_with(".xml")
            || key.ends_with("Packages")
            || key.ends_with("Release")
        {
            "text/plain"
        } else {
            "application/octet-stream"
        };
        let cache = if key.starts_with("arch/") {
            Some(crate::arch_repo::NO_CACHE)
        } else if is_metadata(key) {
            Some("no-store")
        } else {
            None
        };
        self.upload_repo_bytes(key, &std::fs::read(path)?, content_type, cache)
            .await
    }

    async fn verify(&self, key: &str, path: &Path) -> Result<()> {
        self.verify_repo_bytes(key, &std::fs::read(path)?).await
    }

    async fn delete(&self, key: &str) -> Result<()> {
        self.delete_repo_object(key).await
    }
}

fn is_metadata(key: &str) -> bool {
    !(key.starts_with("apt/pool/")
        || key.starts_with("yum/packages/")
        || (key.starts_with("arch/")
            && (key.contains(".pkg.tar.zst") || key.contains(".pkg.tar.gz"))))
}

fn generation_files(root: &Path, current: &Path, files: &mut Vec<(String, PathBuf)>) -> Result<()> {
    for entry in std::fs::read_dir(current)? {
        let path = entry?.path();
        if path.is_dir() {
            generation_files(root, &path, files)?;
        } else {
            let key = path
                .strip_prefix(root)?
                .to_str()
                .context("non-UTF-8 generation path")?
                .to_owned();
            files.push((key, path));
        }
    }
    Ok(())
}

async fn publish_generation(
    inventory: &mut Inventory,
    selection: &Selection,
    generation: &Path,
    store: &impl RepositoryStore,
    now: u64,
) -> Result<()> {
    let mut files = Vec::new();
    generation_files(generation, generation, &mut files)?;
    // Every enabled repository's packages and signatures precede any metadata.
    files.sort_by(|a, b| (is_metadata(&a.0), &a.0).cmp(&(is_metadata(&b.0), &b.0)));
    ensure!(
        files.iter().any(|(key, _)| is_metadata(key)),
        "generation has no repository metadata"
    );
    for (key, path) in &files {
        store.upload(key, path).await?;
    }
    for (key, path) in &files {
        if is_metadata(key) {
            store.verify(key, path).await?;
        }
    }
    // Retirement starts at completion, not at the beginning of a long upload.
    // Bound the completion clock by the invocation time if the clock moves back.
    let completed_at = SystemTime::now()
        .duration_since(UNIX_EPOCH)?
        .as_secs()
        .max(now);
    inventory.commit_retirement(selection, completed_at)?;
    for filename in &selection.delete {
        let package = inventory
            .packages()
            .iter()
            .find(|p| p.filename() == filename)
            .context("unknown GC package")?;
        for key in package.remote_keys() {
            store.delete(&key).await?;
        }
        inventory.finish_collection(filename)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::linux_repo_state::tests::{fixture, target};
    use std::cell::RefCell;
    use std::collections::BTreeMap;
    use std::process::Command;

    #[derive(Default)]
    struct FakeStore {
        objects: RefCell<BTreeMap<String, Vec<u8>>>,
        events: RefCell<Vec<String>>,
        fail_upload: bool,
        fail_verify: bool,
        fail_delete: Option<String>,
    }

    impl RepositoryStore for FakeStore {
        async fn upload(&self, key: &str, path: &Path) -> Result<()> {
            self.events.borrow_mut().push(format!("upload {key}"));
            ensure!(!self.fail_upload, "fixture upload failure");
            self.objects
                .borrow_mut()
                .insert(key.to_owned(), std::fs::read(path)?);
            Ok(())
        }
        async fn verify(&self, key: &str, path: &Path) -> Result<()> {
            self.events.borrow_mut().push(format!("verify {key}"));
            ensure!(!self.fail_verify, "fixture verification failure");
            ensure!(
                self.objects.borrow().get(key) == Some(&std::fs::read(path)?),
                "fixture checksum mismatch"
            );
            Ok(())
        }
        async fn delete(&self, key: &str) -> Result<()> {
            self.events.borrow_mut().push(format!("delete {key}"));
            ensure!(
                self.fail_delete.as_deref() != Some(key),
                "fixture deletion failure"
            );
            self.objects.borrow_mut().remove(key);
            Ok(())
        }
    }

    fn three_releases(state: &Path, incoming: &Path) -> Inventory {
        let mut inventory = Inventory::open(state, target(), true, false).unwrap();
        for version in ["1", "2", "3"] {
            inventory
                .import(&[fixture(
                    incoming,
                    Format::Deb,
                    "daedalus-mainnet",
                    version,
                    &format!("mainnet-{version}.deb"),
                )])
                .unwrap();
        }
        inventory
    }

    fn dummy_generation(path: &Path) {
        for (key, bytes) in [
            ("apt/dists/stable/Release", "signed generation fixture"),
            (
                "apt/pool/main/d/daedalus-mainnet/mainnet-3.deb",
                "package fixture",
            ),
        ] {
            let file = path.join(key);
            std::fs::create_dir_all(file.parent().unwrap()).unwrap();
            std::fs::write(file, bytes).unwrap();
        }
    }

    #[tokio::test]
    async fn publication_failures_do_not_start_retirement_or_collect() {
        for fail_verify in [false, true] {
            let state = tempfile::tempdir().unwrap();
            let incoming = tempfile::tempdir().unwrap();
            let generation = tempfile::tempdir().unwrap();
            let mut inventory = three_releases(state.path(), incoming.path());
            let selection = inventory.select(&[Format::Deb], 1000, 0).unwrap();
            dummy_generation(generation.path());
            let store = FakeStore {
                fail_upload: !fail_verify,
                fail_verify,
                ..FakeStore::default()
            };
            assert!(
                publish_generation(&mut inventory, &selection, generation.path(), &store, 1000)
                    .await
                    .is_err()
            );
            assert!(inventory.packages().iter().all(|p| p.retired_at.is_none()));
            assert!(inventory.package_path("mainnet-1.deb").is_file());
            assert!(
                !store
                    .events
                    .borrow()
                    .iter()
                    .any(|event| event.starts_with("delete "))
            );
            drop(inventory);
            let inventory = Inventory::open(state.path(), target(), false, false).unwrap();
            assert_eq!(
                inventory.packages().len(),
                3,
                "successful import survives publication failure"
            );
            assert!(inventory.packages().iter().all(|p| p.retired_at.is_none()));
        }
    }

    #[tokio::test]
    async fn automatic_gc_follows_verification_and_partial_deletion_is_retryable() {
        let state = tempfile::tempdir().unwrap();
        let incoming = tempfile::tempdir().unwrap();
        let generation = tempfile::tempdir().unwrap();
        let mut inventory = three_releases(state.path(), incoming.path());
        let selection = inventory.select(&[Format::Deb], 1000, 0).unwrap();
        dummy_generation(generation.path());
        let key = inventory.packages()[0].remote_key();
        let store = FakeStore {
            fail_delete: Some(format!("{key}.asc")),
            ..FakeStore::default()
        };
        assert!(
            publish_generation(&mut inventory, &selection, generation.path(), &store, 1000)
                .await
                .is_err()
        );
        assert!(inventory.packages()[0].retired_at.is_some());
        assert!(inventory.package_path("mainnet-1.deb").is_file());
        let events = store.events.borrow().clone();
        assert!(
            events
                .iter()
                .position(|event| event.starts_with("verify "))
                .unwrap()
                < events
                    .iter()
                    .position(|event| event.starts_with("delete "))
                    .unwrap()
        );
        assert!(events[0].contains("apt/pool/"), "packages precede metadata");
        drop(inventory);
        let mut inventory = Inventory::open(state.path(), target(), false, false).unwrap();
        let store = FakeStore::default();
        let selection = inventory.select(&[Format::Deb], u64::MAX, 0).unwrap();
        publish_generation(&mut inventory, &selection, generation.path(), &store, 1000)
            .await
            .unwrap();
        assert_eq!(inventory.packages().len(), 2);
        assert!(!inventory.package_path("mainnet-1.deb").exists());
        assert!(inventory.package_path("mainnet-2.deb").exists());
        assert!(inventory.package_path("mainnet-3.deb").exists());
    }

    #[tokio::test]
    async fn nonzero_grace_delays_automatic_deletion() {
        let state = tempfile::tempdir().unwrap();
        let incoming = tempfile::tempdir().unwrap();
        let generation = tempfile::tempdir().unwrap();
        let mut inventory = three_releases(state.path(), incoming.path());
        dummy_generation(generation.path());
        let store = FakeStore::default();
        let selection = inventory.select(&[Format::Deb], 1000, 86400).unwrap();
        publish_generation(&mut inventory, &selection, generation.path(), &store, 1000)
            .await
            .unwrap();
        let retired_at = inventory.packages()[0].retired_at.unwrap();
        assert!(
            inventory
                .select(&[Format::Deb], retired_at + 86399, 86400)
                .unwrap()
                .delete
                .is_empty()
        );
        let selection = inventory
            .select(&[Format::Deb], retired_at + 86400, 86400)
            .unwrap();
        publish_generation(
            &mut inventory,
            &selection,
            generation.path(),
            &store,
            retired_at + 86400,
        )
        .await
        .unwrap();
        assert_eq!(inventory.packages().len(), 2);
    }

    #[tokio::test]
    async fn dry_run_honors_skips_without_credentials_signing_or_state_mutations() {
        let parent = tempfile::tempdir().unwrap();
        let state = parent.path().join("absent");
        let incoming = tempfile::tempdir().unwrap();
        fixture(
            incoming.path(),
            Format::Deb,
            "daedalus-pilot",
            "1",
            "pilot.deb",
        );
        std::fs::write(incoming.path().join("ignored.rpm"), b"not an RPM").unwrap();
        publish(PublishLinuxReposOpts {
            installers_dir: incoming.path(),
            state_dir: &state,
            initialize: true,
            gc_grace_days: 7,
            bucket: "fixture",
            bucket_url: "unused.invalid",
            endpoint_url: Some("http://unused.invalid".to_owned()),
            gpg_user: Some("nonexistent signing key"),
            no_acl: true,
            dry_run: true,
            skip_apt: false,
            skip_yum: true,
            skip_arch: true,
        })
        .await
        .unwrap();
        assert!(!state.exists());
    }

    const KEY: &str = "Retention fixture <retention@example.invalid>";

    #[test]
    fn signed_apt_yum_deployments() {
        let home = tempfile::tempdir().unwrap();
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(home.path(), std::fs::Permissions::from_mode(0o700)).unwrap();
        let output = Command::new("gpg2")
            .arg("--homedir")
            .arg(home.path())
            .args([
                "--batch",
                "--pinentry-mode",
                "loopback",
                "--passphrase",
                "",
                "--quick-generate-key",
                KEY,
                "ed25519",
                "sign",
                "0",
            ])
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        let output = Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "linux_repo_publish::tests::signed_apt_yum_deployments_child",
                "--nocapture",
            ])
            .env("GNUPGHOME", home.path())
            .env("DRT_RETENTION_SIGNED_CHILD", "1")
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        let _ = Command::new("gpgconf")
            .arg("--homedir")
            .arg(home.path())
            .args(["--kill", "gpg-agent"])
            .status();
    }

    #[tokio::test]
    async fn signed_apt_yum_deployments_child() {
        if std::env::var_os("DRT_RETENTION_SIGNED_CHILD").is_none() {
            return;
        }
        for (format, extension, index) in [
            (
                Format::Deb,
                "deb",
                "apt/dists/stable/main/binary-amd64/Packages",
            ),
            (Format::Rpm, "rpm", "yum/repodata/primary.xml.gz"),
        ] {
            let state = tempfile::tempdir().unwrap();
            let incoming = tempfile::tempdir().unwrap();
            let mut inventory = Inventory::open(state.path(), target(), true, false).unwrap();
            let store = FakeStore::default();
            for (name, version) in [
                ("daedalus-mainnet", "1.0-1"),
                ("daedalus-pilot", "2.0-1"),
                ("daedalus-mainnet", "1.1-1"),
                ("daedalus-mainnet", "1.2-1"),
            ] {
                inventory
                    .import(&[fixture(
                        incoming.path(),
                        format,
                        name,
                        version,
                        &format!("{name}-{version}.{extension}"),
                    )])
                    .unwrap();
                let selection = inventory.select(&[format], 1000, 0).unwrap();
                let packages: Vec<_> = selection
                    .retained
                    .iter()
                    .map(|filename| LinuxPackage {
                        path: inventory.package_path(filename),
                        filename: filename.clone(),
                        format,
                    })
                    .collect();
                let generation = tempfile::tempdir().unwrap();
                linux_repo_cmd::prepare_publication(&packages, Some(KEY), generation.path(), 1000)
                    .unwrap();
                publish_generation(&mut inventory, &selection, generation.path(), &store, 1000)
                    .await
                    .unwrap();
            }
            let objects = store.objects.borrow();
            let content = if format == Format::Rpm {
                use std::io::Read;
                let mut xml = String::new();
                flate2::read::GzDecoder::new(objects[index].as_slice())
                    .read_to_string(&mut xml)
                    .unwrap();
                xml
            } else {
                String::from_utf8(objects[index].clone()).unwrap()
            };
            assert!(content.contains("daedalus-mainnet-1.1-1"));
            assert!(content.contains("daedalus-mainnet-1.2-1"));
            assert!(content.contains("daedalus-pilot-2.0-1"));
            assert!(!content.contains("daedalus-mainnet-1.0-1"));
            assert!(
                !objects
                    .keys()
                    .any(|key| key.contains("daedalus-mainnet-1.0-1"))
            );
            assert!(
                objects
                    .keys()
                    .any(|key| key.contains("daedalus-pilot-2.0-1"))
            );
            drop(objects);
            let packages: Vec<_> = inventory
                .packages()
                .iter()
                .map(|package| LinuxPackage {
                    path: inventory.package_path(package.filename()),
                    filename: package.filename().to_owned(),
                    format,
                })
                .collect();
            let generation = tempfile::tempdir().unwrap();
            assert!(
                linux_repo_cmd::prepare_publication(
                    &packages,
                    Some("nonexistent-retention-key@example.invalid"),
                    generation.path(),
                    1000
                )
                .is_err()
            );
            assert!(
                inventory
                    .packages()
                    .iter()
                    .all(|package| package.retired_at.is_none())
            );
        }
    }
}
