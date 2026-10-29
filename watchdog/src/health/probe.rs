//! One HTTP GET to a local endpoint, classified by how it ended.
//!
//! Each probe opens a new connection. A server that handles every connection
//! in its own thread can keep answering on an old connection after the loop
//! that accepts new ones has ended, so a kept-alive connection would miss
//! exactly the fault the probe is for.

use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;

use anyhow::{Context, Result};
use rustls::pki_types::{CertificateDer, PrivateKeyDer, PrivatePkcs8KeyDer, ServerName};
use rustls::{AlertDescription, ClientConfig, RootCertStore};
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt};
use tokio::net::TcpStream;
use tokio_rustls::TlsConnector;

use crate::tls::TlsClientMaterial;

/// The name the TLS handshake verifies. The generated server certificate
/// carries it, and the probe always connects to the loopback address.
const TLS_SERVER_NAME: &str = "localhost";

/// Longest status line read before the reply is taken for something other
/// than HTTP.
const MAX_STATUS_LINE_BYTES: usize = 8 * 1024;

/// How one probe ended.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Outcome {
    /// A complete HTTP status line arrived. Any status code counts: a 404 or
    /// a 500 still proves that the server accepts and serves requests.
    Answered { status: u16 },
    /// The connection was refused: nothing listens on the port.
    Refused,
    /// No status line arrived within the probe's timeout.
    Timeout,
    /// The connection was reset or closed, or the reply was not HTTP.
    IoError(String),
    /// The TLS peer failed the certificate check in either direction. This is
    /// a configuration fault, not a fault of the server, so the caller must not
    /// act on it as it would on an unanswered request.
    TlsRejected(String),
}

impl Outcome {
    /// A short description for logs and events, `None` for an answer.
    pub fn describe(&self, timeout: Duration) -> Option<String> {
        match self {
            Outcome::Answered { .. } => None,
            Outcome::Refused => Some("connect: connection refused".to_string()),
            Outcome::Timeout => Some(format!("timeout after {} ms", timeout.as_millis())),
            Outcome::IoError(e) => Some(e.clone()),
            Outcome::TlsRejected(e) => Some(format!("tls: {e}")),
        }
    }
}

/// Builds the TLS client configuration for `material`: the generated CA is the
/// only root, and the generated client certificate is presented. The `ring`
/// provider is named explicitly, so no process-wide default is installed.
pub fn client_config(material: &TlsClientMaterial) -> Result<Arc<ClientConfig>> {
    let mut roots = RootCertStore::empty();
    roots
        .add(CertificateDer::from(material.ca_der.clone()))
        .context("the generated CA certificate is not valid DER")?;
    let key = PrivateKeyDer::from(PrivatePkcs8KeyDer::from(material.key_der.clone()));
    let config =
        ClientConfig::builder_with_provider(Arc::new(rustls::crypto::ring::default_provider()))
            .with_safe_default_protocol_versions()
            .context("TLS protocol versions")?
            .with_root_certificates(roots)
            .with_client_auth_cert(vec![CertificateDer::from(material.cert_der.clone())], key)
            .context("the generated client certificate and key do not match")?;
    Ok(Arc::new(config))
}

/// A GET request to one local endpoint.
#[derive(Clone)]
pub struct HttpProbe {
    addr: SocketAddr,
    path: String,
    timeout: Duration,
    tls: Option<TlsConnector>,
}

impl HttpProbe {
    /// A probe of `path` on `addr`, speaking TLS when `tls` is given and plain
    /// HTTP otherwise. `timeout` bounds the whole exchange: connect,
    /// handshake, request and the reply's status line.
    pub fn new(
        addr: SocketAddr,
        path: &str,
        timeout: Duration,
        tls: Option<Arc<ClientConfig>>,
    ) -> Self {
        Self {
            addr,
            path: path.to_string(),
            timeout,
            tls: tls.map(TlsConnector::from),
        }
    }

    pub fn timeout(&self) -> Duration {
        self.timeout
    }

    /// Sends one request on a new connection and classifies the result.
    pub async fn run(&self) -> Outcome {
        match tokio::time::timeout(self.timeout, self.exchange()).await {
            Ok(outcome) => outcome,
            Err(_) => Outcome::Timeout,
        }
    }

    async fn exchange(&self) -> Outcome {
        let tcp = match TcpStream::connect(self.addr).await {
            Ok(s) => s,
            Err(e) => return classify_io(&e, "connect"),
        };
        let Some(connector) = &self.tls else {
            return self.request(tcp).await;
        };
        let name = ServerName::try_from(TLS_SERVER_NAME).expect("a valid DNS name");
        match connector.connect(name, tcp).await {
            Ok(stream) => self.request(stream).await,
            Err(e) => classify_io(&e, "handshake"),
        }
    }

    async fn request<S: AsyncRead + AsyncWrite + Unpin>(&self, mut stream: S) -> Outcome {
        let request = format!(
            "GET {} HTTP/1.1\r\nHost: {}\r\nAccept: application/json\r\nConnection: close\r\n\r\n",
            self.path, self.addr
        );
        if let Err(e) = stream.write_all(request.as_bytes()).await {
            return classify_io(&e, "write");
        }
        if let Err(e) = stream.flush().await {
            return classify_io(&e, "write");
        }
        let mut buf = Vec::with_capacity(256);
        let mut chunk = [0u8; 256];
        loop {
            if let Some(end) = buf.windows(2).position(|w| w == b"\r\n") {
                return parse_status_line(&buf[..end]);
            }
            if buf.len() > MAX_STATUS_LINE_BYTES {
                return Outcome::IoError("reply is not HTTP: status line too long".to_string());
            }
            match stream.read(&mut chunk).await {
                Ok(0) => {
                    return Outcome::IoError("closed before a status line".to_string());
                }
                Ok(n) => buf.extend_from_slice(&chunk[..n]),
                Err(e) => return classify_io(&e, "read"),
            }
        }
    }
}

fn parse_status_line(line: &[u8]) -> Outcome {
    let text = String::from_utf8_lossy(line);
    let mut parts = text.split(' ');
    let version = parts.next().unwrap_or("");
    let code = parts.next().and_then(|c| c.parse::<u16>().ok());
    match code {
        Some(status) if version.starts_with("HTTP/1.") && (100..=599).contains(&status) => {
            Outcome::Answered { status }
        }
        _ => Outcome::IoError(format!(
            "reply is not HTTP: {:?}",
            text.chars().take(40).collect::<String>()
        )),
    }
}

fn classify_io(e: &std::io::Error, stage: &str) -> Outcome {
    if e.kind() == std::io::ErrorKind::ConnectionRefused {
        return Outcome::Refused;
    }
    let tls = e
        .get_ref()
        .and_then(|inner| inner.downcast_ref::<rustls::Error>());
    if let Some(tls) = tls {
        if is_certificate_fault(tls) {
            return Outcome::TlsRejected(tls.to_string());
        }
    }
    Outcome::IoError(format!("{stage}: {e}"))
}

fn is_certificate_fault(e: &rustls::Error) -> bool {
    use rustls::Error as E;
    match e {
        E::InvalidCertificate(_) | E::NoCertificatesPresented | E::InvalidCertRevocationList(_) => {
            true
        }
        E::AlertReceived(alert) => matches!(
            alert,
            AlertDescription::BadCertificate
                | AlertDescription::UnsupportedCertificate
                | AlertDescription::CertificateRevoked
                | AlertDescription::CertificateExpired
                | AlertDescription::CertificateUnknown
                | AlertDescription::UnknownCA
                | AlertDescription::AccessDenied
                | AlertDescription::CertificateRequired
                | AlertDescription::HandshakeFailure
                // What rustls sends when a certificate's signature does not
                // verify, as when two generated CAs share a subject name.
                | AlertDescription::DecryptError
                | AlertDescription::BadCertificateStatusResponse
        ),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tls::generate_certs;
    use rustls::ServerConfig;
    use rustls::pki_types::pem::PemObject;
    use rustls::server::WebPkiClientVerifier;
    use std::path::{Path, PathBuf};
    use tokio::net::TcpListener;
    use tokio_rustls::TlsAcceptor;

    const SHORT: Duration = Duration::from_millis(400);

    fn plain(addr: SocketAddr) -> HttpProbe {
        HttpProbe::new(addr, "/v2/network/information", SHORT, None)
    }

    /// A plain HTTP server that replies to each connection with `reply`.
    async fn serve_plain(reply: &'static str) -> SocketAddr {
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        tokio::spawn(async move {
            loop {
                let Ok((mut s, _)) = listener.accept().await else {
                    return;
                };
                tokio::spawn(async move {
                    let mut buf = [0u8; 1024];
                    let _ = s.read(&mut buf).await;
                    let _ = s.write_all(reply.as_bytes()).await;
                });
            }
        });
        addr
    }

    #[tokio::test]
    async fn run_counts_any_status_as_an_answer() {
        for (reply, status) in [
            ("HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n", 200),
            ("HTTP/1.1 404 Not Found\r\n\r\n", 404),
            ("HTTP/1.1 500 Internal Server Error\r\n\r\n", 500),
        ] {
            let addr = serve_plain(reply).await;
            assert_eq!(plain(addr).run().await, Outcome::Answered { status });
        }
    }

    #[tokio::test]
    async fn run_needs_only_the_status_line() {
        // The body never arrives and the connection stays open.
        let addr = serve_plain("HTTP/1.1 200 OK\r\n").await;
        assert_eq!(plain(addr).run().await, Outcome::Answered { status: 200 });
    }

    #[tokio::test]
    async fn run_times_out_on_a_server_that_never_writes() {
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let held = tokio::spawn(async move {
            let mut conns = Vec::new();
            while let Ok((s, _)) = listener.accept().await {
                conns.push(s);
            }
        });
        assert_eq!(plain(addr).run().await, Outcome::Timeout);
        held.abort();
    }

    #[tokio::test]
    async fn run_reports_a_refused_connection() {
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        drop(listener);
        assert_eq!(plain(addr).run().await, Outcome::Refused);
    }

    #[tokio::test]
    async fn run_reports_a_connection_closed_before_a_status_line() {
        let addr = serve_plain("").await;
        assert!(matches!(plain(addr).run().await, Outcome::IoError(_)));
    }

    #[tokio::test]
    async fn run_reports_a_reply_that_is_not_http() {
        let addr = serve_plain("SSH-2.0-OpenSSH\r\n").await;
        assert!(matches!(plain(addr).run().await, Outcome::IoError(_)));
    }

    #[test]
    fn describe_names_the_failure() {
        let t = Duration::from_millis(10_000);
        assert_eq!(Outcome::Answered { status: 200 }.describe(t), None);
        assert_eq!(
            Outcome::Timeout.describe(t).as_deref(),
            Some("timeout after 10000 ms")
        );
        assert_eq!(
            Outcome::Refused.describe(t).as_deref(),
            Some("connect: connection refused")
        );
    }

    // ── TLS ──────────────────────────────────────────────────────────────

    struct Certs {
        dir: PathBuf,
        paths: crate::tls::TlsPaths,
    }

    impl Certs {
        fn new(name: &str) -> Self {
            let dir =
                std::env::temp_dir().join(format!("watchdog-probe-{name}-{}", std::process::id()));
            let _ = std::fs::remove_dir_all(&dir);
            let paths = generate_certs(&dir).unwrap();
            Certs { dir, paths }
        }
    }

    impl Drop for Certs {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.dir);
        }
    }

    fn pem_certs(path: &Path) -> Vec<CertificateDer<'static>> {
        CertificateDer::pem_file_iter(path)
            .unwrap()
            .map(|c| c.unwrap())
            .collect()
    }

    /// A TLS server like cardano-wallet's: it presents `certs`' server
    /// certificate and requires a client certificate signed by `certs`' CA.
    async fn serve_tls(certs: &Certs, reply: &'static str) -> SocketAddr {
        let provider = Arc::new(rustls::crypto::ring::default_provider());
        let mut roots = RootCertStore::empty();
        for ca in pem_certs(&certs.paths.server_ca) {
            roots.add(ca).unwrap();
        }
        let verifier =
            WebPkiClientVerifier::builder_with_provider(Arc::new(roots), provider.clone())
                .build()
                .unwrap();
        let key = PrivateKeyDer::from_pem_file(&certs.paths.server_key).unwrap();
        let config = ServerConfig::builder_with_provider(provider)
            .with_safe_default_protocol_versions()
            .unwrap()
            .with_client_cert_verifier(verifier)
            .with_single_cert(pem_certs(&certs.paths.server_cert), key)
            .unwrap();
        let acceptor = TlsAcceptor::from(Arc::new(config));
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        tokio::spawn(async move {
            loop {
                let Ok((tcp, _)) = listener.accept().await else {
                    return;
                };
                let acceptor = acceptor.clone();
                tokio::spawn(async move {
                    let Ok(mut s) = acceptor.accept(tcp).await else {
                        return;
                    };
                    let mut buf = [0u8; 1024];
                    let _ = s.read(&mut buf).await;
                    let _ = s.write_all(reply.as_bytes()).await;
                    let _ = s.shutdown().await;
                });
            }
        });
        addr
    }

    fn tls_probe(addr: SocketAddr, material: &TlsClientMaterial) -> HttpProbe {
        HttpProbe::new(
            addr,
            "/v2/network/information",
            Duration::from_secs(5),
            Some(client_config(material).unwrap()),
        )
    }

    #[tokio::test]
    async fn run_answers_through_mutual_tls_with_the_generated_certificates() {
        let certs = Certs::new("good");
        let addr = serve_tls(&certs, "HTTP/1.1 200 OK\r\n\r\n").await;
        assert_eq!(
            tls_probe(addr, &certs.paths.client).run().await,
            Outcome::Answered { status: 200 }
        );
    }

    #[tokio::test]
    async fn run_rejects_a_server_certificate_from_another_ca() {
        let server = Certs::new("server-other");
        let ours = Certs::new("client-own");
        let addr = serve_tls(&server, "HTTP/1.1 200 OK\r\n\r\n").await;
        let outcome = tls_probe(addr, &ours.paths.client).run().await;
        assert!(matches!(outcome, Outcome::TlsRejected(_)), "{outcome:?}");
    }

    #[tokio::test]
    async fn run_is_rejected_when_the_server_refuses_our_client_certificate() {
        let server = Certs::new("server-strict");
        let stranger = Certs::new("stranger");
        let addr = serve_tls(&server, "HTTP/1.1 200 OK\r\n\r\n").await;
        // Trusts the server's CA but presents a certificate from another CA.
        let material = TlsClientMaterial {
            ca_der: server.paths.client.ca_der.clone(),
            cert_der: stranger.paths.client.cert_der.clone(),
            key_der: stranger.paths.client.key_der.clone(),
        };
        let outcome = tls_probe(addr, &material).run().await;
        assert!(matches!(outcome, Outcome::TlsRejected(_)), "{outcome:?}");
    }

    #[tokio::test]
    async fn run_reports_plain_tcp_on_a_tls_probe_as_an_io_error_not_a_certificate_fault() {
        let certs = Certs::new("plain-peer");
        let addr = serve_plain("").await;
        let outcome = tls_probe(addr, &certs.paths.client).run().await;
        assert!(!matches!(outcome, Outcome::TlsRejected(_)), "{outcome:?}");
    }
}
