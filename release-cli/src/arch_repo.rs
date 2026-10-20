//! Prepare a complete Arch generation before changing the published repository.
//! `repo-add` owns the database format, including package signatures and file lists.

use anyhow::{Context, Result, bail, ensure};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use tempfile::TempDir;

use crate::sign;

pub const PRIMARY: &str = "31B3F22FA0CCE2E8382C04A2807F2ACD0171BC90";
pub const SIGNING_SUBKEY: &str = "CD1F21F62477EB42D7C4F3DEB3AD9541F4DBD0FE";
pub const NO_CACHE: &str = "no-store, no-cache, max-age=0, must-revalidate";

#[derive(Clone, Copy)]
pub enum Signing<'a> {
    Unsigned,
    Local(&'a str),
    Iohk(Option<&'a str>),
}

impl Signing<'_> {
    fn key(self) -> Option<String> {
        match self {
            Self::Unsigned => None,
            Self::Local(key) => Some(key.to_owned()),
            Self::Iohk(key) => Some(key.unwrap_or(SIGNING_SUBKEY).to_owned()),
        }
    }
}

pub struct PreparedRepo {
    dir: TempDir,
    pub packages: Vec<String>,
    pub metadata: Vec<String>,
}

impl PreparedRepo {
    pub fn path(&self, name: &str) -> PathBuf {
        self.dir.path().join(name)
    }

    /// Copy contents, never symlinks: object storage cannot resolve repo-add aliases.
    pub fn write_to(&self, dest: &Path) -> Result<()> {
        std::fs::create_dir_all(dest)?;
        for name in self.packages.iter().chain(&self.metadata) {
            let destination = dest.join(name);
            // Replace an old alias rather than following it into an earlier generation.
            if std::fs::symlink_metadata(&destination).is_ok() {
                std::fs::remove_file(&destination)?;
            }
            std::fs::copy(self.path(name), destination)?;
        }
        Ok(())
    }
}

struct Verifier {
    home: TempDir,
    iohk: bool,
}

impl Verifier {
    fn new(public_key: &Path, iohk: bool) -> Result<Self> {
        let home = tempfile::tempdir()?;
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(home.path(), std::fs::Permissions::from_mode(0o700))?;
        let output = Command::new("gpg2")
            .args(["--no-options", "--batch", "--homedir"])
            .arg(home.path())
            .arg("--import")
            .arg(public_key)
            .output()
            .context("importing Arch verification public key")?;
        ensure!(
            output.status.success(),
            "public key import failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        Ok(Self { home, iohk })
    }

    fn verify(&self, signature: &Path, data: &Path) -> Result<()> {
        let output = Command::new("gpg2")
            .args([
                "--no-options",
                "--batch",
                "--no-auto-key-retrieve",
                "--homedir",
            ])
            .arg(self.home.path())
            .args(["--status-fd", "1", "--verify"])
            .arg(signature)
            .arg(data)
            .output()
            .context("verifying Arch detached signature")?;
        ensure!(
            output.status.success(),
            "signature verification failed for {}: {}",
            data.display(),
            String::from_utf8_lossy(&output.stderr)
        );
        let status = String::from_utf8_lossy(&output.stdout);
        let valid = status.lines().any(|line| {
            let fields: Vec<_> = line.split_whitespace().collect();
            fields.get(1) == Some(&"VALIDSIG")
                && (!self.iohk
                    || (fields.get(2) == Some(&SIGNING_SUBKEY) && fields.get(11) == Some(&PRIMARY)))
        });
        ensure!(
            valid,
            "signature for {} does not have the required signing identity",
            data.display()
        );
        Ok(())
    }
}

fn sign_binary(path: &Path, output: &Path, key: &str) -> Result<()> {
    let status = Command::new("gpg2")
        .args(["--detach-sign", "--no-armor", "--local-user"])
        .arg(key)
        .arg("--output")
        .arg(output)
        .arg(path)
        .status()
        .context("signing Arch artifact with gpg2")?;
    ensure!(
        status.success(),
        "GPG signing failed for {}",
        path.display()
    );
    Ok(())
}

fn dearmor(source: &Path, output: &Path) -> Result<()> {
    let result = Command::new("gpg2")
        .args(["--batch", "--yes", "--output"])
        .arg(output)
        .arg("--dearmor")
        .arg(source)
        .output()
        .context("dearmoring Arch package signature")?;
    ensure!(
        result.status.success(),
        "dearmoring {} failed: {}",
        source.display(),
        String::from_utf8_lossy(&result.stderr)
    );
    ensure!(
        std::fs::metadata(output)?.len() > 0,
        "empty package signature"
    );
    Ok(())
}

/// Stage exact package bytes and verified signatures, then build and sign both
/// databases. Any invalid existing signature aborts; it is never silently replaced.
pub fn prepare(packages: &[(&Path, &str)], signing: Signing<'_>) -> Result<PreparedRepo> {
    ensure!(
        !packages.is_empty(),
        "cannot build an empty Arch repository"
    );
    let dir = tempfile::tempdir()?;
    let mut repo = PreparedRepo {
        dir,
        packages: Vec::new(),
        metadata: Vec::new(),
    };
    let key = signing.key();
    let verifier = if let Some(key) = &key {
        let public_key = repo.path("daedalus-release.gpg");
        // Export only the intended identity; production never exports unrelated keys.
        let export_key = if matches!(signing, Signing::Iohk(_)) {
            PRIMARY
        } else {
            key
        };
        std::fs::write(&public_key, sign::export_public_key(Some(export_key))?)?;
        repo.metadata.push("daedalus-release.gpg".to_owned());
        Some(Verifier::new(
            &public_key,
            matches!(signing, Signing::Iohk(_)),
        )?)
    } else {
        None
    };

    let mut package_paths = Vec::new();
    for (source, filename) in packages {
        ensure!(
            Path::new(filename).file_name().and_then(|s| s.to_str()) == Some(filename)
                && !filename.starts_with('-'),
            "invalid Arch package filename: {filename}"
        );
        ensure!(
            !repo.packages.iter().any(|name| name == filename),
            "duplicate package filename: {filename}"
        );
        let staged = repo.path(filename);
        std::fs::copy(source, &staged).with_context(|| format!("staging {}", source.display()))?;
        repo.packages.push((*filename).to_owned());
        package_paths.push(staged.clone());
        if let (Some(key), Some(verifier)) = (&key, &verifier) {
            let sig_name = format!("{filename}.sig");
            let signature = repo.path(&sig_name);
            let existing_sig = source.with_file_name(&sig_name);
            let existing_asc = source.with_file_name(format!("{filename}.asc"));
            if existing_sig.exists() {
                std::fs::copy(existing_sig, &signature)?;
                ensure!(
                    !std::fs::read(&signature)?.starts_with(b"-----BEGIN"),
                    "{} must be a binary signature",
                    signature.display()
                );
            } else if existing_asc.exists() {
                dearmor(&existing_asc, &signature)?;
            } else {
                sign_binary(&staged, &signature, key)?;
            }
            verifier.verify(&signature, &staged)?;
            repo.packages.push(sig_name);
        }
    }

    let output = Command::new("repo-add")
        .args(["--quiet", "--include-sigs"])
        .arg(repo.path("daedalus.db.tar.gz"))
        .args(&package_paths)
        .stdin(Stdio::null())
        .output()
        .context("building Arch databases (repo-add must be in PATH)")?;
    if !output.status.success() {
        bail!(
            "repo-add failed: {}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    for kind in ["db", "files"] {
        let archive = format!("daedalus.{kind}.tar.gz");
        let alias = format!("daedalus.{kind}");
        // Read through repo-add's aliases, then replace them with actual contents.
        let bytes = std::fs::read(repo.path(&archive))?;
        let alias_path = repo.path(&alias);
        if std::fs::symlink_metadata(&alias_path).is_ok() {
            std::fs::remove_file(&alias_path)?;
        }
        std::fs::write(&alias_path, &bytes)?;
        repo.metadata.extend([archive.clone(), alias.clone()]);
        if let (Some(key), Some(verifier)) = (&key, &verifier) {
            let signature_name = format!("{archive}.sig");
            let signature = repo.path(&signature_name);
            sign_binary(&repo.path(&archive), &signature, key)?;
            verifier.verify(&signature, &repo.path(&archive))?;
            verifier.verify(&signature, &alias_path)?;
            let alias_signature = format!("{alias}.sig");
            std::fs::copy(&signature, repo.path(&alias_signature))?;
            repo.metadata.extend([signature_name, alias_signature]);
        }
    }
    Ok(repo)
}

#[cfg(test)]
mod tests {
    use super::*;

    const KEY: &str = "Arch fixture <arch-fixture@example.invalid>";
    const FILENAME: &str = "daedalus-11.5.0-87825-mainnet-example-x86_64-linux.pkg.tar.zst";

    fn run(command: &mut Command) -> std::process::Output {
        let output = command.output().expect("start test command");
        assert!(
            output.status.success(),
            "{command:?}: {}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        output
    }

    fn package(directory: &Path) -> PathBuf {
        let root = directory.join("root");
        std::fs::create_dir_all(root.join("usr/share")).unwrap();
        std::fs::write(
            root.join("usr/share/daedalus-fixture.txt"),
            b"signed package bytes\n",
        )
        .unwrap();
        std::fs::write(root.join(".PKGINFO"), "pkgname = daedalus-mainnet\npkgbase = daedalus-mainnet\npkgver = 11.5.0-1\npkgdesc = signature integration fixture\nurl = https://daedaluswallet.io\nbuilddate = 1791847794\npackager = Arch fixture\nsize = 21\narch = x86_64\nlicense = custom\n").unwrap();
        let path = directory.join(FILENAME);
        run(Command::new("bsdtar")
            .args(["--zstd", "-cf"])
            .arg(&path)
            .arg("-C")
            .arg(root)
            .args([".PKGINFO", "usr"]));
        path
    }

    #[test]
    fn unsigned_generation_has_real_aliases_and_file_list() {
        let dir = tempfile::tempdir().unwrap();
        let path = package(dir.path());
        let repo = prepare(&[(&path, FILENAME)], Signing::Unsigned).unwrap();
        for kind in ["db", "files"] {
            assert_eq!(
                std::fs::read(repo.path(&format!("daedalus.{kind}"))).unwrap(),
                std::fs::read(repo.path(&format!("daedalus.{kind}.tar.gz"))).unwrap()
            );
            assert!(!repo.path(&format!("daedalus.{kind}")).is_symlink());
        }
        let files = run(Command::new("bsdtar")
            .arg("-xOf")
            .arg(repo.path("daedalus.files")));
        assert!(String::from_utf8_lossy(&files.stdout).contains("usr/share/daedalus-fixture.txt"));
        assert!(!repo.path("daedalus.db.sig").exists());
    }

    #[test]
    fn signed_generation() {
        run_signed_child(false);
    }

    #[test]
    #[ignore = "requires sudo for pacman in an isolated temporary root"]
    fn pacman_required_signatures() {
        run_signed_child(true);
    }

    fn run_signed_child(pacman: bool) {
        // Isolate GNUPGHOME in a subprocess rather than mutating process-wide
        // environment while Rust's other tests are running.
        let home = tempfile::tempdir().unwrap();
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(home.path(), std::fs::Permissions::from_mode(0o700)).unwrap();
        run(Command::new("gpg2")
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
            ]));
        run(Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "arch_repo::tests::signed_generation_child",
                "--nocapture",
            ])
            .env("GNUPGHOME", home.path())
            .env("DRT_ARCH_TEST_PACMAN", if pacman { "1" } else { "0" })
            .env("DRT_ARCH_TEST_CHILD", "1"));
        run(Command::new("gpgconf")
            .arg("--homedir")
            .arg(home.path())
            .args(["--kill", "gpg-agent"]));
    }

    #[test]
    fn published_iohk_signature_matches_both_required_fingerprints() {
        let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/arch");
        let dir = tempfile::tempdir().unwrap();
        let sig = dir.path().join("database.sig");
        dearmor(&fixtures.join("daedalus.db.asc"), &sig).unwrap();
        let verifier = Verifier::new(&fixtures.join("daedalus-release.gpg"), true).unwrap();
        verifier
            .verify(&sig, &fixtures.join("daedalus.db"))
            .unwrap();
    }

    #[test]
    fn signed_generation_child() {
        if std::env::var("DRT_ARCH_TEST_CHILD").as_deref() != Ok("1") {
            return;
        }
        let dir = tempfile::tempdir().unwrap();
        let path = package(dir.path());
        // Reuse an armored signature produced by the existing drt sign path.
        sign::sign_file(&path, Some(KEY)).unwrap();
        let asc = path.with_file_name(format!("{FILENAME}.asc"));
        let expected_sig = dir.path().join("expected.sig");
        dearmor(&asc, &expected_sig).unwrap();
        let repo = prepare(&[(&path, FILENAME)], Signing::Local(KEY)).unwrap();
        let sig_name = format!("{FILENAME}.sig");
        assert_eq!(
            std::fs::read(&expected_sig).unwrap(),
            std::fs::read(repo.path(&sig_name)).unwrap()
        );
        assert!(
            !std::fs::read(repo.path(&sig_name))
                .unwrap()
                .starts_with(b"-----BEGIN")
        );
        let verifier = Verifier::new(&repo.path("daedalus-release.gpg"), false).unwrap();
        for kind in ["db", "files"] {
            for suffix in ["", ".tar.gz"] {
                let name = format!("daedalus.{kind}{suffix}");
                verifier
                    .verify(&repo.path(&format!("{name}.sig")), &repo.path(&name))
                    .unwrap();
            }
        }
        let pinned = Verifier::new(&repo.path("daedalus-release.gpg"), true).unwrap();
        assert!(
            pinned
                .verify(&repo.path(&sig_name), &repo.path(FILENAME))
                .is_err()
        );

        // Adjacent binary signatures survive byte-for-byte without re-signing.
        std::fs::copy(&expected_sig, dir.path().join(&sig_name)).unwrap();
        std::fs::remove_file(&asc).unwrap();
        let reused = prepare(&[(&path, FILENAME)], Signing::Local(KEY)).unwrap();
        assert_eq!(
            std::fs::read(&expected_sig).unwrap(),
            std::fs::read(reused.path(&sig_name)).unwrap()
        );

        // Published aliases contain bytes, not object-store symlink payloads.
        let published = dir.path().join("published");
        repo.write_to(&published).unwrap();
        for name in &repo.metadata {
            assert!(!published.join(name).is_symlink());
        }
        if std::env::var("DRT_ARCH_TEST_PACMAN").as_deref() == Ok("1") {
            check_pacman(dir.path(), &published, &repo.path("daedalus-release.gpg"));
        }

        // Never silently replace an invalid signature with a fresh signature.
        std::fs::write(&path, b"different artifact").unwrap();
        assert!(prepare(&[(&path, FILENAME)], Signing::Local(KEY)).is_err());
        // Mutating the source never changes the verified staged generation.
        verifier
            .verify(&repo.path(&sig_name), &repo.path(FILENAME))
            .unwrap();
        std::fs::write(repo.path("daedalus.db"), b"changed database").unwrap();
        assert!(
            verifier
                .verify(&repo.path("daedalus.db.sig"), &repo.path("daedalus.db"))
                .is_err()
        );
    }

    fn check_pacman(directory: &Path, repository: &Path, public_key: &Path) {
        let db = directory.join("client-db");
        let cache = directory.join("client-cache");
        let gpg = directory.join("client-gpg");
        let root = directory.join("client-root");
        for path in [&db, &cache, &gpg, &root] {
            std::fs::create_dir_all(path).unwrap();
        }
        run(Command::new("gpg2")
            .arg("--homedir")
            .arg(&gpg)
            .args(["--batch", "--import"])
            .arg(public_key));
        let fingerprints = run(Command::new("gpg2")
            .arg("--homedir")
            .arg(&gpg)
            .args(["--with-colons", "--fingerprint"]));
        let fingerprint = String::from_utf8(fingerprints.stdout)
            .unwrap()
            .lines()
            .find_map(|line| {
                line.strip_prefix("fpr:::::::::")
                    .and_then(|s| s.split(':').next())
            })
            .unwrap()
            .to_owned();
        let trust = directory.join("ownertrust");
        std::fs::write(&trust, format!("{fingerprint}:6:\n")).unwrap();
        run(Command::new("gpg2")
            .arg("--homedir")
            .arg(&gpg)
            .args(["--batch", "--import-ownertrust"])
            .arg(trust));
        let config = directory.join("pacman.conf");
        std::fs::write(&config, format!("[options]\nArchitecture = x86_64\nRootDir = {}\nDBPath = {}\nCacheDir = {}\nGPGDir = {}\nLogFile = {}\n[daedalus]\nServer = file://{}\nSigLevel = Required\n", root.display(), db.display(), cache.display(), gpg.display(), directory.join("pacman.log").display(), repository.display())).unwrap();
        let pacman = run(Command::new("bash").args(["-c", "command -v pacman"]));
        let pacman = String::from_utf8(pacman.stdout).unwrap();
        for args in [
            vec!["-Syu", "--noconfirm"],
            vec!["-S", "--noconfirm", "daedalus-mainnet"],
            vec!["-Fy"],
            vec!["-Fl", "daedalus-mainnet"],
        ] {
            // Opt-in integration only; the root, database, keyring, cache and log
            // all live under this test's temporary directory.
            let output = run(Command::new("sudo")
                .arg("-n")
                .arg(pacman.trim())
                .arg("--config")
                .arg(&config)
                .args(&args));
            if args[0] == "-Fl" {
                assert!(
                    String::from_utf8_lossy(&output.stdout)
                        .contains("usr/share/daedalus-fixture.txt")
                );
            }
        }
        assert!(root.join("usr/share/daedalus-fixture.txt").exists());
        run(Command::new("sudo")
            .args(["-n", "rm", "-rf"])
            .arg(&root)
            .arg(&db)
            .arg(&cache));
    }
}
