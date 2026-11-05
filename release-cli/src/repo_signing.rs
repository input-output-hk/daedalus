//! Shared repository signing policy and isolated signature verification.
use anyhow::{Context, Result, ensure};
use std::path::Path;
use std::process::Command;
use tempfile::TempDir;

pub const PRIMARY: &str = "31B3F22FA0CCE2E8382C04A2807F2ACD0171BC90";
pub const SIGNING_SUBKEY: &str = "CD1F21F62477EB42D7C4F3DEB3AD9541F4DBD0FE";

#[derive(Clone, Copy)]
pub enum Signing<'a> {
    Unsigned,
    Local(&'a str),
    Iohk(Option<&'a str>),
}

impl Signing<'_> {
    pub(crate) fn key(self) -> Option<String> {
        match self {
            Self::Unsigned => None,
            Self::Local(key) => Some(key.to_owned()),
            Self::Iohk(key) => Some(key.unwrap_or(SIGNING_SUBKEY).to_owned()),
        }
    }
}

pub(crate) struct Verifier {
    home: TempDir,
    iohk: bool,
}

impl Verifier {
    pub(crate) fn new(public_key: &Path, iohk: bool) -> Result<Self> {
        let home = tempfile::tempdir()?;
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(home.path(), std::fs::Permissions::from_mode(0o700))?;
        let output = Command::new("gpg2")
            .args(["--no-options", "--batch", "--homedir"])
            .arg(home.path())
            .arg("--import")
            .arg(public_key)
            .output()
            .context("importing repository verification public key")?;
        ensure!(
            output.status.success(),
            "public key import failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        Ok(Self { home, iohk })
    }

    pub(crate) fn verify(&self, signature: &Path, data: &Path) -> Result<()> {
        self.verify_signature(signature, Some(data))
    }

    pub(crate) fn verify_clearsigned(&self, signature: &Path) -> Result<()> {
        self.verify_signature(signature, None)
    }

    fn verify_signature(&self, signature: &Path, data: Option<&Path>) -> Result<()> {
        let mut command = Command::new("gpg2");
        command
            .args([
                "--no-options",
                "--batch",
                "--no-auto-key-retrieve",
                "--homedir",
            ])
            .arg(self.home.path())
            .args(["--status-fd", "1", "--verify"])
            .arg(signature);
        if let Some(data) = data {
            command.arg(data);
        }
        let output = command.output().context("verifying repository signature")?;
        ensure!(
            output.status.success(),
            "signature verification failed for {}: {}",
            signature.display(),
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
            signature.display()
        );
        Ok(())
    }
}
