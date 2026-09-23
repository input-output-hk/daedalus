// On Windows, build as a GUI app so no console window appears when launched
// from a shortcut or the installer. Pipe-based IPC with Electron still works
// (STARTF_USESTDHANDLES overrides stdio regardless of subsystem), and the
// absence of an inherited console handle table keeps Electron's libuv stdio
// initialization clean.
#![cfg_attr(windows, windows_subsystem = "windows")]

mod chain_validation;
mod config;
mod mithril;
mod protocol;
mod supervisor;
mod tls;

use anyhow::Result;
use clap::Parser;
use file_rotate::compression::Compression;
use file_rotate::suffix::AppendCount;
use file_rotate::{ContentLimit, FileRotate};
use std::sync::Mutex;
use tokio::io::{AsyncBufRead, AsyncBufReadExt, AsyncReadExt, BufReader};
use tokio::sync::mpsc;
use tracing_subscriber::Layer;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

#[derive(Parser)]
#[command(about = "Process supervisor for cardano-node and cardano-wallet")]
struct Args {
    /// Path to the JSON config file
    #[arg(long)]
    config: String,

    /// Directory for public log files (watchdog.log, node.log, cardano-wallet.log).
    /// Overrides the value in the config file. Set by bin/daedalus at runtime.
    #[arg(long)]
    pub_logs_dir: Option<String>,

    /// Directory in which TLS certs are generated.
    /// Overrides the value in the config file. Set by bin/daedalus at runtime.
    #[arg(long)]
    tls_dir: Option<String>,
}

// Per-line bound so a wedged Electron process that writes without newlines
// can't grow the read buffer indefinitely. This must be per line, not a
// lifetime total: commands trickle in for the whole session (e.g. the 30s
// probe_mithril heartbeat), and a cumulative cap would eventually read as
// EOF and shut the whole stack down mid-session.
// Normal lines are << 1 MB (one config line + small JSON commands).
const MAX_LINE_BYTES: u64 = 4 * 1024 * 1024; // 4 MB

/// Read one newline-terminated line with a bounded buffer. Returns Ok(None)
/// on EOF. A line longer than MAX_LINE_BYTES is discarded (drained up to the
/// next newline) and reading continues with the following line, mirroring how
/// malformed JSON lines are ignored.
async fn read_bounded_line<R>(reader: &mut R) -> std::io::Result<Option<String>>
where
    R: AsyncBufRead + Unpin,
{
    loop {
        let mut buf = Vec::new();
        let n = reader
            .take(MAX_LINE_BYTES)
            .read_until(b'\n', &mut buf)
            .await?;
        if n == 0 {
            return Ok(None); // EOF
        }
        if buf.last() == Some(&b'\n') {
            buf.pop();
            if buf.last() == Some(&b'\r') {
                buf.pop();
            }
            return Ok(Some(String::from_utf8_lossy(&buf).into_owned()));
        }
        if (n as u64) < MAX_LINE_BYTES {
            // Final line without a trailing newline.
            return Ok(Some(String::from_utf8_lossy(&buf).into_owned()));
        }
        // Oversized line: drain the remainder, then read the next line.
        tracing::warn!("stdin line exceeded {MAX_LINE_BYTES} bytes; discarded");
        loop {
            let mut rest = Vec::new();
            let m = reader
                .take(MAX_LINE_BYTES)
                .read_until(b'\n', &mut rest)
                .await?;
            if m == 0 {
                return Ok(None);
            }
            if rest.last() == Some(&b'\n') {
                break;
            }
        }
    }
}

/// Expand `${VAR_NAME}` placeholders in `text` using the current environment.
/// Unrecognised or unset variable names expand to the empty string.
fn substitute_env_vars(text: &str) -> String {
    let mut result = String::with_capacity(text.len());
    let mut chars = text.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '$' && chars.peek() == Some(&'{') {
            chars.next(); // consume '{'
            let mut name = String::new();
            for c in chars.by_ref() {
                if c == '}' {
                    break;
                }
                name.push(c);
            }
            let val = std::env::var(&name).unwrap_or_default();
            // JSON-encode so backslashes in Windows paths don't produce invalid escapes.
            if let Ok(encoded) = serde_json::to_string(&val) {
                result.push_str(&encoded[1..encoded.len() - 1]); // strip surrounding quotes
            } else {
                result.push_str(&val);
            }
        } else {
            result.push(c);
        }
    }
    result
}

#[tokio::main]
async fn main() -> Result<()> {
    // SIGPIPE default action (terminate) races with emit()'s EPIPE handler on
    // macOS. Ignore it so write() returns EPIPE instead, which emit() catches.
    #[cfg(unix)]
    unsafe {
        libc::signal(libc::SIGPIPE, libc::SIG_IGN);
    }

    let args = Args::parse();

    // Kill-on-close job object: children must not survive watchdog death.
    #[cfg(windows)]
    supervisor::init_job_object();

    // On non-Linux platforms the watchdog binary is the install-directory anchor.
    // Set DAEDALUS_INSTALL_DIRECTORY from our own path so config placeholders resolve.
    // On macOS this is also set by darwin-launcher before exec; on Windows this is
    // the primary mechanism (no wrapper script). Called before substitute_env_vars.
    #[cfg(not(target_os = "linux"))]
    if std::env::var("DAEDALUS_INSTALL_DIRECTORY").is_err() {
        if let Ok(exe) = std::env::current_exe() {
            if let Some(dir) = exe.parent() {
                // Safety: no other threads yet; tokio worker threads haven't been scheduled.
                unsafe { std::env::set_var("DAEDALUS_INSTALL_DIRECTORY", dir) };
            }
        }
    }

    let config_text = tokio::fs::read_to_string(&args.config)
        .await
        .map_err(|e| anyhow::anyhow!("Failed to read config file '{}': {e}", args.config))?;

    let config_text = substitute_env_vars(&config_text);

    let mut config: config::WatchdogConfig =
        serde_json::from_str(&config_text).map_err(|e| anyhow::anyhow!("Invalid config: {e}"))?;

    // CLI flags override config-file values for runtime-variable paths.
    if let Some(d) = args.pub_logs_dir {
        config.pub_logs_dir = Some(d);
    }
    if let Some(d) = args.tls_dir {
        config.tls_dir = Some(d);
    }

    // Stderr layer — always on, useful for interactive debugging.
    let stderr_layer = tracing_subscriber::fmt::layer()
        .with_writer(std::io::stderr)
        .with_target(false);

    // File layer — only when pub_logs_dir is configured.  Uses the same
    // file-rotate setup as the node/wallet logs so the file stays bounded.
    // ANSI colour codes are stripped so the file is plain text.
    let file_layer = config.pub_logs_dir.as_deref().map(|logs_dir| {
        let path = format!("{logs_dir}/watchdog.log");
        let file = Mutex::new(FileRotate::new(
            &path,
            AppendCount::new(4),
            ContentLimit::Bytes(5 * 1024 * 1024),
            Compression::None,
            None,
        ));
        tracing_subscriber::fmt::layer()
            .with_writer(file)
            .with_ansi(false)
            .with_target(false)
            .boxed()
    });

    tracing_subscriber::registry()
        .with(stderr_layer)
        .with(file_layer)
        .init();

    let (cmd_tx, cmd_rx) = mpsc::channel::<protocol::Command>(8);

    // SIGTERM/SIGINT handler: treat as an orderly stop so children are not orphaned.
    // SIGINT fires on Ctrl-C when watchdog is the foreground process (nix run / direct launch).
    #[cfg(unix)]
    {
        let sigterm_tx = cmd_tx.clone();
        let sigint_tx = cmd_tx.clone();
        tokio::spawn(async move {
            use tokio::signal::unix::{SignalKind, signal};
            if let Ok(mut stream) = signal(SignalKind::terminate()) {
                stream.recv().await;
                tracing::info!("received SIGTERM; initiating graceful shutdown");
                let _ = sigterm_tx.send(protocol::Command::Stop).await;
            }
        });
        tokio::spawn(async move {
            use tokio::signal::unix::{SignalKind, signal};
            if let Ok(mut stream) = signal(SignalKind::interrupt()) {
                stream.recv().await;
                tracing::info!("received SIGINT; initiating graceful shutdown");
                let _ = sigint_tx.send(protocol::Command::Stop).await;
            }
        });
    }

    // Generate TLS certs when tls_dir is configured; inject paths into wallet args
    // and (if in parent mode) into Electron's environment.
    if let Some(ref tls_dir_str) = config.tls_dir.clone() {
        let tls_dir = std::path::Path::new(tls_dir_str);
        let tls = tls::generate_certs(tls_dir)?;
        config.wallet.args.extend([
            "--tls-ca-cert".to_string(),
            tls.server_ca.to_string_lossy().into_owned(),
            "--tls-sv-cert".to_string(),
            tls.server_cert.to_string_lossy().into_owned(),
            "--tls-sv-key".to_string(),
            tls.server_key.to_string_lossy().into_owned(),
        ]);
        if let Some(ref mut el) = config.electron {
            el.env.insert(
                "TLS_CA_CERT".to_string(),
                tls.client_ca.to_string_lossy().into_owned(),
            );
            el.env.insert(
                "TLS_CLIENT_CERT".to_string(),
                tls.client_cert.to_string_lossy().into_owned(),
            );
            el.env.insert(
                "TLS_CLIENT_KEY".to_string(),
                tls.client_key.to_string_lossy().into_owned(),
            );
        }
    }

    if let Some(electron_cfg) = config.electron.take() {
        // ── Event channel (parent mode only) ─────────────────────────────────
        // In standalone mode we leave EVENT_TX unset so emit() falls back to its
        // original direct-to-stdout path, which avoids the race where the tokio
        // runtime shuts down before the drain task flushes the last event.
        let (event_tx, mut event_rx) = mpsc::unbounded_channel::<String>();
        protocol::init_event_sink(event_tx);
        // ── Parent mode ──────────────────────────────────────────────────────
        // Watchdog is PID 1: spawn Electron as a child, wire pipes for IPC.
        use std::process::Stdio;
        use tokio::io::AsyncWriteExt;
        use tokio::process::Command as TokioCommand;

        let logs_dir = config.pub_logs_dir.clone();
        let electron_log_path = logs_dir
            .as_deref()
            .map(|d| format!("{d}/electron.log"))
            .unwrap_or_else(|| "/dev/null".to_string());

        let electron_stderr: Stdio = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&electron_log_path)
            .map(Stdio::from)
            .unwrap_or_else(|_| Stdio::null());

        // ── Windows: named-pipe IPC ──────────────────────────────────────────
        // Chromium may reassign or close the inherited stdin/stdout handles
        // during browser-process startup (before Node.js/libuv claims them),
        // making pipe-based IPC unreliable on Windows. A named pipe is a
        // dedicated OS object that Chromium never touches, so it is the
        // correct IPC transport on Windows.
        //
        // We create the server before spawning Electron so the pipe name
        // exists in the kernel when Electron starts. Electron connects to it
        // via the DAEDALUS_IPC_PIPE env var. Electron's stdin/stdout are set
        // to null so Chromium has nothing to interfere with.
        //
        // ── Non-Windows: stdin/stdout IPC ───────────────────────────────────
        // On Linux and macOS the standard handles are inherited cleanly and
        // Chromium does not redirect them, so the original pipe-based approach
        // works without modification.
        #[cfg(windows)]
        let ipc_pipe_name = format!(r"\\.\pipe\daedalus-ipc-{}", std::process::id());

        #[cfg(windows)]
        let ipc_server = {
            use tokio::net::windows::named_pipe::ServerOptions;
            ServerOptions::new()
                .first_pipe_instance(true)
                .create(&ipc_pipe_name)
                .map_err(|e| anyhow::anyhow!("Failed to create IPC named pipe: {e}"))?
        };

        let mut electron_cmd = TokioCommand::new(&electron_cfg.exe);
        electron_cmd
            .args(&electron_cfg.args)
            .envs(&electron_cfg.env)
            .kill_on_drop(true);

        #[cfg(windows)]
        electron_cmd
            .env("DAEDALUS_IPC_PIPE", &ipc_pipe_name)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(electron_stderr);

        #[cfg(not(windows))]
        electron_cmd
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(electron_stderr);

        supervisor::tether_to_watchdog(&mut electron_cmd);

        let mut electron = electron_cmd
            .spawn()
            .map_err(|e| anyhow::anyhow!("Failed to spawn Electron '{}': {e}", electron_cfg.exe))?;

        let electron_pid = electron.id().unwrap_or(0);
        tracing::info!("Electron started (PID {electron_pid})");
        protocol::emit(&protocol::Event::ElectronStarted { pid: electron_pid });

        // ── Windows: accept the named-pipe connection ────────────────────────
        // Electron connects when its Node.js main-process code runs. We wait
        // up to 60 s to allow for slow Windows startup and AV scanning. Once
        // connected the pipe is split into a write half (events → Electron)
        // and a read half (commands ← Electron).
        #[cfg(windows)]
        {
            use tokio::time::{Duration, timeout};

            tracing::info!("Waiting for Electron to connect to IPC pipe: {ipc_pipe_name}");
            match timeout(Duration::from_secs(60), ipc_server.connect()).await {
                Ok(Ok(_)) => tracing::info!("Electron connected to IPC named pipe"),
                Ok(Err(e)) => return Err(anyhow::anyhow!("IPC named pipe accept failed: {e}")),
                Err(_) => {
                    return Err(anyhow::anyhow!(
                        "Electron did not connect to IPC pipe within 60 s"
                    ));
                }
            }

            let (ipc_read, mut ipc_write) = tokio::io::split(ipc_server);

            // Events → Electron via named pipe
            tokio::spawn(async move {
                while let Some(line) = event_rx.recv().await {
                    let mut buf = line.into_bytes();
                    buf.push(b'\n');
                    if ipc_write.write_all(&buf).await.is_err() || ipc_write.flush().await.is_err()
                    {
                        tracing::warn!("IPC pipe write failed; discarding remaining events");
                        while event_rx.recv().await.is_some() {}
                        return;
                    }
                }
            });

            // Commands ← Electron via named pipe.
            // EOF on the pipe means Electron closed its end (e.g. process exit).
            // The Electron process monitor task below handles the actual Stop.
            let mut reader = BufReader::new(ipc_read);
            let ipc_cmd_tx = cmd_tx.clone();
            tokio::spawn(async move {
                while let Ok(Some(line)) = read_bounded_line(&mut reader).await {
                    if let Ok(cmd) = serde_json::from_str::<protocol::Command>(&line) {
                        if ipc_cmd_tx.send(cmd).await.is_err() {
                            break;
                        }
                    }
                }
                tracing::info!("IPC pipe reader: Electron disconnected");
            });
        }

        // ── Non-Windows: stdin/stdout IPC ────────────────────────────────────
        #[cfg(not(windows))]
        {
            // Drain event channel → Electron's stdin.
            let mut electron_stdin = electron.stdin.take().unwrap();
            let drain_cmd_tx = cmd_tx.clone();
            tokio::spawn(async move {
                while let Some(line) = event_rx.recv().await {
                    let mut buf = line.into_bytes();
                    buf.push(b'\n');
                    if electron_stdin.write_all(&buf).await.is_err()
                        || electron_stdin.flush().await.is_err()
                    {
                        tracing::warn!("Electron stdin closed (broken pipe); stopping");
                        let _ = drain_cmd_tx.send(protocol::Command::Stop).await;
                        return;
                    }
                }
            });

            // Read commands from Electron's stdout.
            let electron_stdout = electron.stdout.take().unwrap();
            let mut reader = BufReader::new(electron_stdout);
            let stdout_cmd_tx = cmd_tx.clone();
            tokio::spawn(async move {
                while let Ok(Some(line)) = read_bounded_line(&mut reader).await {
                    if let Ok(cmd) = serde_json::from_str::<protocol::Command>(&line) {
                        if stdout_cmd_tx.send(cmd).await.is_err() {
                            break;
                        }
                    }
                }
                let _ = stdout_cmd_tx.send(protocol::Command::Stop).await;
            });
        }

        // Monitor Electron exit → stop the stack.
        let exit_cmd_tx = cmd_tx.clone();
        tokio::spawn(async move {
            let status = electron.wait().await;
            let (code, signal) = match status {
                Ok(s) => {
                    #[cfg(unix)]
                    {
                        use std::os::unix::process::ExitStatusExt;
                        if let Some(sig) = s.signal() {
                            use nix::sys::signal::Signal;
                            let name = Signal::try_from(sig).ok().map(|s| s.as_str().to_string());
                            (None, name)
                        } else {
                            (s.code(), None)
                        }
                    }
                    #[cfg(not(unix))]
                    (s.code(), None)
                }
                Err(_) => (None, None),
            };
            tracing::info!(
                "Electron exited (code={:?}, signal={:?}); stopping",
                code,
                signal
            );
            protocol::emit(&protocol::Event::ElectronExited {
                code,
                signal: signal.clone(),
            });
            let _ = exit_cmd_tx.send(protocol::Command::Stop).await;
        });
    } else {
        // ── Standalone mode ──────────────────────────────────────────────────
        // No Electron section in config: emit() writes directly to stdout (the
        // original behaviour — no channel, no race on shutdown). Read commands
        // from stdin; EOF on stdin triggers Stop.
        let mut reader = BufReader::new(tokio::io::stdin());
        let stdin_cmd_tx = cmd_tx.clone();
        tokio::spawn(async move {
            while let Ok(Some(line)) = read_bounded_line(&mut reader).await {
                if let Ok(cmd) = serde_json::from_str::<protocol::Command>(&line) {
                    if stdin_cmd_tx.send(cmd).await.is_err() {
                        break;
                    }
                }
            }
            // stdin EOF — treat as stop
            let _ = stdin_cmd_tx.send(protocol::Command::Stop).await;
        });
    }

    supervisor::run(config, cmd_rx).await
}

#[cfg(test)]
mod tests {
    use super::substitute_env_vars;

    #[test]
    fn known_var_is_expanded() {
        std::env::set_var("_TEST_WATCHDOG_SUB_A", "hello");
        assert_eq!(
            substitute_env_vars("prefix/${_TEST_WATCHDOG_SUB_A}/suffix"),
            "prefix/hello/suffix"
        );
    }

    #[test]
    fn unknown_var_expands_to_empty() {
        std::env::remove_var("_TEST_WATCHDOG_SUB_MISSING");
        assert_eq!(
            substitute_env_vars("a/${_TEST_WATCHDOG_SUB_MISSING}/b"),
            "a//b"
        );
    }

    #[test]
    fn dollar_without_brace_is_literal() {
        assert_eq!(substitute_env_vars("cost: $10"), "cost: $10");
    }

    #[test]
    fn multiple_vars_in_sequence() {
        std::env::set_var("_TEST_WATCHDOG_SUB_X", "foo");
        std::env::set_var("_TEST_WATCHDOG_SUB_Y", "bar");
        assert_eq!(
            substitute_env_vars("${_TEST_WATCHDOG_SUB_X}/${_TEST_WATCHDOG_SUB_Y}"),
            "foo/bar"
        );
    }

    #[test]
    fn no_placeholders_returns_input_unchanged() {
        let input = r#"{"exe": "/usr/bin/electron", "args": []}"#;
        assert_eq!(substitute_env_vars(input), input);
    }
}
