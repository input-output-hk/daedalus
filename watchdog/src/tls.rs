use anyhow::Result;
use rcgen::{
    BasicConstraints, CertificateParams, CertifiedIssuer, DistinguishedName, DnType,
    ExtendedKeyUsagePurpose, IsCa, KeyPair, KeyUsagePurpose,
};
use std::path::{Path, PathBuf};
use time::{Duration, OffsetDateTime};

pub struct TlsPaths {
    pub server_ca: PathBuf,
    pub server_cert: PathBuf,
    pub server_key: PathBuf,
    pub client_ca: PathBuf,
    pub client_cert: PathBuf,
    pub client_key: PathBuf,
}

/// Write `data` to `path` with 0600 permissions (owner-read/write only).
/// On non-Unix platforms falls back to a plain write.
fn write_private(path: &Path, data: &[u8]) -> Result<()> {
    #[cfg(unix)]
    {
        use std::io::Write;
        use std::os::unix::fs::OpenOptionsExt;
        let mut f = std::fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .mode(0o600)
            .open(path)?;
        f.write_all(data)?;
        Ok(())
    }
    #[cfg(not(unix))]
    {
        std::fs::write(path, data)?;
        Ok(())
    }
}

/// Generate a CA + server cert + client cert into `tls_dir/server/` and
/// `tls_dir/client/`. Always regenerates so certs are always fresh.
pub fn generate_certs(tls_dir: &Path) -> Result<TlsPaths> {
    let server_dir = tls_dir.join("server");
    let client_dir = tls_dir.join("client");

    let paths = TlsPaths {
        server_ca: server_dir.join("ca.crt"),
        server_cert: server_dir.join("server.crt"),
        server_key: server_dir.join("server.key"),
        client_ca: client_dir.join("ca.crt"),
        client_cert: client_dir.join("client.pem"),
        client_key: client_dir.join("client.key"),
    };

    std::fs::create_dir_all(&server_dir)?;
    std::fs::create_dir_all(&client_dir)?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&server_dir, std::fs::Permissions::from_mode(0o700))?;
        std::fs::set_permissions(&client_dir, std::fs::Permissions::from_mode(0o700))?;
    }

    let now = OffsetDateTime::now_utc();
    let ten_years = now + Duration::days(3650);
    let one_year = now + Duration::days(365);

    // CA — self-signed, used to sign server and client certs
    let ca_key = KeyPair::generate()?;
    let mut ca_params = CertificateParams::default();
    ca_params.not_before = now;
    ca_params.not_after = ten_years;
    ca_params.is_ca = IsCa::Ca(BasicConstraints::Unconstrained);
    ca_params.key_usages = vec![KeyUsagePurpose::KeyCertSign, KeyUsagePurpose::CrlSign];
    let mut ca_dn = DistinguishedName::new();
    ca_dn.push(DnType::CommonName, "Daedalus Self-Signed Root CA");
    ca_dn.push(DnType::OrganizationName, "Daedalus");
    ca_params.distinguished_name = ca_dn;
    let ca = CertifiedIssuer::self_signed(ca_params, ca_key)?;

    // Server cert — presented by cardano-wallet; SANs match what the wallet listens on
    let server_key = KeyPair::generate()?;
    let mut server_params = CertificateParams::new(vec![
        "localhost".to_string(),
        "localhost.localdomain".to_string(),
        "127.0.0.1".to_string(),
        "::1".to_string(),
    ])?;
    server_params.not_before = now;
    server_params.not_after = one_year;
    server_params.extended_key_usages = vec![ExtendedKeyUsagePurpose::ServerAuth];
    server_params.key_usages = vec![
        KeyUsagePurpose::DigitalSignature,
        KeyUsagePurpose::KeyEncipherment,
    ];
    let mut server_dn = DistinguishedName::new();
    server_dn.push(DnType::CommonName, "Daedalus Wallet Backend");
    server_dn.push(DnType::OrganizationName, "Daedalus");
    server_params.distinguished_name = server_dn;
    let server_cert = server_params.signed_by(&server_key, &ca)?;

    // Client cert — presented by Daedalus frontend to authenticate to the wallet API
    let client_key = KeyPair::generate()?;
    let mut client_params = CertificateParams::default();
    client_params.not_before = now;
    client_params.not_after = one_year;
    client_params.extended_key_usages = vec![ExtendedKeyUsagePurpose::ClientAuth];
    client_params.key_usages = vec![KeyUsagePurpose::DigitalSignature];
    let mut client_dn = DistinguishedName::new();
    client_dn.push(DnType::CommonName, "Daedalus Frontend");
    client_dn.push(DnType::OrganizationName, "Daedalus");
    client_params.distinguished_name = client_dn;
    let client_cert = client_params.signed_by(&client_key, &ca)?;

    let ca_pem = ca.pem();
    std::fs::write(&paths.server_ca, &ca_pem)?;
    std::fs::write(&paths.client_ca, &ca_pem)?;
    std::fs::write(&paths.server_cert, server_cert.pem())?;
    write_private(&paths.server_key, server_key.serialize_pem().as_bytes())?;
    std::fs::write(&paths.client_cert, client_cert.pem())?;
    write_private(&paths.client_key, client_key.serialize_pem().as_bytes())?;

    tracing::info!("TLS certs generated in {}", tls_dir.display());
    Ok(paths)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn tmp_tls_dir(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("watchdog-tls-test-{name}"));
        let _ = fs::remove_dir_all(&dir);
        dir
    }

    #[cfg(unix)]
    fn unix_mode(path: &std::path::Path) -> u32 {
        use std::os::unix::fs::PermissionsExt;
        fs::metadata(path).unwrap().permissions().mode() & 0o7777
    }

    #[test]
    fn generate_creates_all_cert_files() {
        let tls_dir = tmp_tls_dir("create");
        let paths = generate_certs(&tls_dir).expect("generate_certs failed");
        assert!(paths.server_ca.exists(), "server/ca.crt missing");
        assert!(paths.server_cert.exists(), "server/server.crt missing");
        assert!(paths.server_key.exists(), "server/server.key missing");
        assert!(paths.client_ca.exists(), "client/ca.crt missing");
        assert!(paths.client_cert.exists(), "client/client.pem missing");
        assert!(paths.client_key.exists(), "client/client.key missing");
        #[cfg(unix)]
        {
            assert_eq!(
                unix_mode(&tls_dir.join("server")),
                0o700,
                "server/ must be 0700"
            );
            assert_eq!(
                unix_mode(&tls_dir.join("client")),
                0o700,
                "client/ must be 0700"
            );
            assert_eq!(
                unix_mode(&paths.server_key),
                0o600,
                "server.key must be 0600"
            );
            assert_eq!(
                unix_mode(&paths.client_key),
                0o600,
                "client.key must be 0600"
            );
        }
    }

    #[test]
    fn cert_files_are_valid_pem() {
        let tls_dir = tmp_tls_dir("pem");
        let paths = generate_certs(&tls_dir).unwrap();
        for path in [
            &paths.server_ca,
            &paths.server_cert,
            &paths.server_key,
            &paths.client_ca,
            &paths.client_cert,
            &paths.client_key,
        ] {
            let content = fs::read_to_string(path).unwrap();
            assert!(
                content.contains("-----BEGIN "),
                "{} is not PEM",
                path.display()
            );
        }
    }

    #[test]
    fn ca_cert_identical_in_server_and_client_dirs() {
        let tls_dir = tmp_tls_dir("ca-copy");
        let paths = generate_certs(&tls_dir).unwrap();
        let server_ca = fs::read_to_string(&paths.server_ca).unwrap();
        let client_ca = fs::read_to_string(&paths.client_ca).unwrap();
        assert_eq!(
            server_ca, client_ca,
            "CA cert must be identical in both dirs"
        );
    }

    #[test]
    fn second_call_overwrites_existing_certs() {
        let tls_dir = tmp_tls_dir("overwrite");
        generate_certs(&tls_dir).unwrap();
        let ca_before = fs::read_to_string(tls_dir.join("server/ca.crt")).unwrap();
        // Tiny sleep so clock can advance if the filesystem has coarse mtime resolution.
        std::thread::sleep(std::time::Duration::from_millis(10));
        generate_certs(&tls_dir).unwrap();
        let ca_after = fs::read_to_string(tls_dir.join("server/ca.crt")).unwrap();
        // New CA is self-signed with a fresh key each time, so PEM content must differ.
        assert_ne!(
            ca_before, ca_after,
            "certs must be regenerated on every call"
        );
    }
}
