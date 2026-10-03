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
mod state;
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

    // Load persisted user overrides (chain path, node RTS args, electron flags).
    // These override the static daedalus-config.json values at runtime.
    let base_node_args = config.node.args.clone();
    let watchdog_state = state::load(&config.node.state_dir).await;
    state::apply_to_config(&mut config, &base_node_args, &watchdog_state);
    // Capture base electron args before any flag extension so the manager can
    // reconstruct the full arg list (base + current_flags) on each Electron spawn.
    let base_electron_args: Vec<String> = config
        .electron
        .as_ref()
        .map(|e| e.args.clone())
        .unwrap_or_default();
    let initial_electron_flags = watchdog_state.electron_flags.clone();
    // Restart channel: supervisor sends (new_flags, wallet_port) on SetElectronFlags;
    // the Electron manager task performs the kill-and-respawn. In standalone mode
    // the receiver is dropped, so sends silently fail (which is correct).
    let (el_restart_tx, mut el_restart_rx) =
        mpsc::unbounded_channel::<protocol::ElectronRestartPayload>();

    if let Some(electron_cfg) = config.electron.take() {
        // ── Event channel (parent mode only) ─────────────────────────────────
        // In standalone mode we leave EVENT_TX unset so emit() falls back to its
        // original direct-to-stdout path, which avoids the race where the tokio
        // runtime shuts down before the drain task flushes the last event.
        let (event_tx, mut event_rx) = mpsc::unbounded_channel::<String>();
        protocol::init_event_sink(event_tx);
        // ── Parent mode ──────────────────────────────────────────────────────
        // Watchdog is PID 1: spawn Electron as a child, wire pipes for IPC.
        // Electron may be restarted in-place (blank-screen-fix toggle) without
        // stopping cardano-node or cardano-wallet.
        use std::process::Stdio;
        use std::sync::Arc;
        use std::sync::atomic::{AtomicBool, Ordering};
        use tokio::io::AsyncWriteExt;
        use tokio::process::Command as TokioCommand;

        let logs_dir = config.pub_logs_dir.clone();
        let electron_log_path = logs_dir
            .as_deref()
            .map(|d| format!("{d}/electron.log"))
            .unwrap_or_else(|| "/dev/null".to_string());
        let watchdog_pid = std::process::id();

        // Per-instance forwarding: the long-lived drain task forwards from
        // event_rx to whichever Electron instance is current. current_tx is
        // swapped on each Electron respawn so queued events go to the new pipe.
        let (initial_per_tx, initial_per_rx) = mpsc::unbounded_channel::<String>();
        let current_tx: Arc<tokio::sync::Mutex<mpsc::UnboundedSender<String>>> =
            Arc::new(tokio::sync::Mutex::new(initial_per_tx));
        {
            let current_tx_drain = Arc::clone(&current_tx);
            tokio::spawn(async move {
                while let Some(line) = event_rx.recv().await {
                    let guard = current_tx_drain.lock().await;
                    let _ = guard.send(line);
                }
            });
        }

        // is_restarting: suppresses a spurious Stop when we intentionally kill
        // Electron to respawn it with new flags.
        let is_restarting = Arc::new(AtomicBool::new(false));

        let exit_cmd_tx = cmd_tx.clone();
        let is_restarting_mgr = Arc::clone(&is_restarting);
        let current_tx_mgr = Arc::clone(&current_tx);
        let reader_cmd_tx_base = cmd_tx.clone();
        let electron_exe = electron_cfg.exe.clone();
        let electron_env = electron_cfg.env.clone();

        // Windows: restart counter for unique named-pipe names per Electron instance.
        #[cfg(windows)]
        let mut el_instance = 0u32;

        enum ManagerAction {
            Exit,
            Restart {
                payload: protocol::ElectronRestartPayload,
            },
        }

        tokio::spawn(async move {
            let mut current_electron_flags = initial_electron_flags;
            let mut per_rx = initial_per_rx;

            'manager: loop {
                // Reopen the log file in append mode so every spawn continues
                // into the same log rather than creating a new one.
                let electron_stderr: Stdio = std::fs::OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(&electron_log_path)
                    .map(Stdio::from)
                    .unwrap_or_else(|_| Stdio::null());

                let mut electron_cmd = TokioCommand::new(&electron_exe);
                electron_cmd
                    .args(&base_electron_args)
                    .args(&current_electron_flags)
                    .envs(&electron_env)
                    .kill_on_drop(true);

                // ── Windows: named-pipe IPC ──────────────────────────────────
                // A new pipe name is used per instance so there is no conflict
                // if the old Electron process lingers momentarily after the kill.
                #[cfg(windows)]
                {
                    use tokio::net::windows::named_pipe::ServerOptions;
                    use tokio::time::{Duration, timeout};
                    el_instance += 1;
                    let ipc_pipe_name =
                        format!(r"\\.\pipe\daedalus-ipc-{}-{}", watchdog_pid, el_instance);
                    let ipc_server = match ServerOptions::new()
                        .first_pipe_instance(true)
                        .create(&ipc_pipe_name)
                    {
                        Ok(s) => s,
                        Err(e) => {
                            tracing::error!("Failed to create IPC named pipe: {e}");
                            let _ = exit_cmd_tx.send(protocol::Command::Stop).await;
                            break 'manager;
                        }
                    };
                    electron_cmd
                        .env("DAEDALUS_IPC_PIPE", &ipc_pipe_name)
                        .stdin(Stdio::null())
                        .stdout(Stdio::null())
                        .stderr(electron_stderr);
                    supervisor::tether_to_watchdog(&mut electron_cmd);
                    let mut electron = match electron_cmd.spawn() {
                        Ok(e) => e,
                        Err(err) => {
                            tracing::error!("Failed to spawn Electron '{}': {err}", electron_exe);
                            let _ = exit_cmd_tx.send(protocol::Command::Stop).await;
                            break 'manager;
                        }
                    };
                    let electron_pid = electron.id().unwrap_or(0);
                    tracing::info!("Electron started (PID {electron_pid})");
                    protocol::emit(&protocol::Event::ElectronStarted { pid: electron_pid });

                    tracing::info!("Waiting for Electron to connect to IPC pipe: {ipc_pipe_name}");
                    match timeout(Duration::from_secs(60), ipc_server.connect()).await {
                        Ok(Ok(_)) => {
                            tracing::info!("Electron connected to IPC named pipe")
                        }
                        Ok(Err(e)) => {
                            tracing::error!("IPC named pipe accept failed: {e}");
                            let _ = exit_cmd_tx.send(protocol::Command::Stop).await;
                            break 'manager;
                        }
                        Err(_) => {
                            tracing::error!("Electron did not connect to IPC pipe within 60 s");
                            let _ = exit_cmd_tx.send(protocol::Command::Stop).await;
                            break 'manager;
                        }
                    }
                    let (ipc_read, mut ipc_write) = tokio::io::split(ipc_server);

                    // Per-instance writer: per_rx → named pipe
                    tokio::spawn(async move {
                        while let Some(line) = per_rx.recv().await {
                            let mut buf = line.into_bytes();
                            buf.push(b'\n');
                            if ipc_write.write_all(&buf).await.is_err()
                                || ipc_write.flush().await.is_err()
                            {
                                break;
                            }
                        }
                    });

                    // Per-instance reader: named pipe → cmd_tx
                    let mut reader = BufReader::new(ipc_read);
                    let is_restarting_pipe = Arc::clone(&is_restarting_mgr);
                    let ipc_cmd_tx = reader_cmd_tx_base.clone();
                    let reader_handle = tokio::spawn(async move {
                        while let Ok(Some(line)) = read_bounded_line(&mut reader).await {
                            if let Ok(cmd) = serde_json::from_str::<protocol::Command>(&line) {
                                if ipc_cmd_tx.send(cmd).await.is_err() {
                                    break;
                                }
                            }
                        }
                        if !is_restarting_pipe.load(Ordering::SeqCst) {
                            let _ = ipc_cmd_tx.send(protocol::Command::Stop).await;
                        }
                        tracing::info!("IPC pipe reader: Electron disconnected");
                    });

                    let action = tokio::select! {
                        msg = el_restart_rx.recv() => match msg {
                            Some(p) => ManagerAction::Restart { payload: p },
                            None => ManagerAction::Exit,
                        },
                        _ = electron.wait() => ManagerAction::Exit,
                    };
                    match action {
                        ManagerAction::Exit => {
                            if !is_restarting_mgr.load(Ordering::SeqCst) {
                                let _ = exit_cmd_tx.send(protocol::Command::Stop).await;
                            }
                            break 'manager;
                        }
                        ManagerAction::Restart { payload } => {
                            is_restarting_mgr.store(true, Ordering::SeqCst);
                            tracing::info!(
                                "Electron restart (PID {electron_pid}): applying new flags"
                            );
                            let _ = electron.kill().await;
                            let _ = electron.wait().await;
                            // Await the old reader so it sees is_restarting=true before we
                            // clear it. Without this, a delayed reader task can check the
                            // flag after it is reset and send a spurious Stop.
                            let _ = reader_handle.await;
                            is_restarting_mgr.store(false, Ordering::SeqCst);
                            current_electron_flags = payload.flags;
                            let (per_tx_next, per_rx_next) = mpsc::unbounded_channel::<String>();
                            *current_tx_mgr.lock().await = per_tx_next;
                            per_rx = per_rx_next;
                            protocol::emit(&protocol::Event::WatchdogStarted {
                                pid: watchdog_pid,
                                node_extra_args: payload.node_extra_args,
                            });
                            if let Some(phase) = payload.startup_phase {
                                protocol::emit(&protocol::Event::NodeStartupStatus { phase });
                            }
                            if let Some(port) = payload.wallet_port {
                                protocol::emit(&protocol::Event::WalletReady {
                                    port,
                                    waited_ms: 0,
                                });
                            }
                            continue 'manager;
                        }
                    }
                }

                // ── Non-Windows: stdin/stdout IPC ────────────────────────────
                #[cfg(not(windows))]
                {
                    electron_cmd
                        .stdin(Stdio::piped())
                        .stdout(Stdio::piped())
                        .stderr(electron_stderr);
                    supervisor::tether_to_watchdog(&mut electron_cmd);
                    let mut electron = match electron_cmd.spawn() {
                        Ok(e) => e,
                        Err(err) => {
                            tracing::error!("Failed to spawn Electron '{}': {err}", electron_exe);
                            let _ = exit_cmd_tx.send(protocol::Command::Stop).await;
                            break 'manager;
                        }
                    };
                    let electron_pid = electron.id().unwrap_or(0);
                    tracing::info!("Electron started (PID {electron_pid})");
                    protocol::emit(&protocol::Event::ElectronStarted { pid: electron_pid });

                    // Per-instance writer: per_rx → Electron stdin
                    let mut stdin = electron.stdin.take().unwrap();
                    tokio::spawn(async move {
                        while let Some(line) = per_rx.recv().await {
                            let mut buf = line.into_bytes();
                            buf.push(b'\n');
                            if stdin.write_all(&buf).await.is_err() || stdin.flush().await.is_err()
                            {
                                break;
                            }
                        }
                    });

                    // Per-instance reader: Electron stdout → cmd_tx
                    let stdout = electron.stdout.take().unwrap();
                    let mut reader = BufReader::new(stdout);
                    let is_restarting_reader = Arc::clone(&is_restarting_mgr);
                    let reader_cmd_tx = reader_cmd_tx_base.clone();
                    let reader_handle = tokio::spawn(async move {
                        while let Ok(Some(line)) = read_bounded_line(&mut reader).await {
                            if let Ok(cmd) = serde_json::from_str::<protocol::Command>(&line) {
                                if reader_cmd_tx.send(cmd).await.is_err() {
                                    break;
                                }
                            }
                        }
                        if !is_restarting_reader.load(Ordering::SeqCst) {
                            let _ = reader_cmd_tx.send(protocol::Command::Stop).await;
                        }
                    });

                    let action = tokio::select! {
                        msg = el_restart_rx.recv() => match msg {
                            Some(p) => ManagerAction::Restart { payload: p },
                            None => ManagerAction::Exit,
                        },
                        _ = electron.wait() => ManagerAction::Exit,
                    };
                    match action {
                        ManagerAction::Exit => {
                            if !is_restarting_mgr.load(Ordering::SeqCst) {
                                let _ = exit_cmd_tx.send(protocol::Command::Stop).await;
                            }
                            break 'manager;
                        }
                        ManagerAction::Restart { payload } => {
                            is_restarting_mgr.store(true, Ordering::SeqCst);
                            tracing::info!(
                                "Electron restart (PID {electron_pid}): applying new flags"
                            );
                            let _ = electron.kill().await;
                            let _ = electron.wait().await;
                            // Await the old reader so it sees is_restarting=true before we
                            // clear it. Without this, a delayed reader task can check the
                            // flag after it is reset and send a spurious Stop.
                            let _ = reader_handle.await;
                            is_restarting_mgr.store(false, Ordering::SeqCst);
                            current_electron_flags = payload.flags;
                            let (per_tx_next, per_rx_next) = mpsc::unbounded_channel::<String>();
                            *current_tx_mgr.lock().await = per_tx_next;
                            per_rx = per_rx_next;
                            protocol::emit(&protocol::Event::WatchdogStarted {
                                pid: watchdog_pid,
                                node_extra_args: payload.node_extra_args,
                            });
                            if let Some(phase) = payload.startup_phase {
                                protocol::emit(&protocol::Event::NodeStartupStatus { phase });
                            }
                            if let Some(port) = payload.wallet_port {
                                protocol::emit(&protocol::Event::WalletReady {
                                    port,
                                    waited_ms: 0,
                                });
                            }
                            continue 'manager;
                        }
                    }
                }
            }
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

    supervisor::run(
        config,
        cmd_rx,
        base_node_args,
        watchdog_state,
        Some(el_restart_tx),
    )
    .await
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
