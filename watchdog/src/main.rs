mod chain_validation;
mod config;
mod mithril;
mod protocol;
mod supervisor;

use anyhow::Result;
use clap::Parser;
use file_rotate::compression::Compression;
use file_rotate::suffix::AppendCount;
use file_rotate::{ContentLimit, FileRotate};
use std::sync::Mutex;
use tokio::io::{AsyncBufRead, AsyncBufReadExt, AsyncReadExt, BufReader};
use tokio::sync::mpsc;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

#[derive(Parser)]
#[command(about = "Process supervisor for cardano-node and cardano-wallet")]
struct Args {
    /// Path to the JSON config file
    #[arg(long)]
    config: String,
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

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();

    // Kill-on-close job object: children must not survive watchdog death.
    #[cfg(windows)]
    supervisor::init_job_object();

    let config_text = tokio::fs::read_to_string(&args.config)
        .await
        .map_err(|e| anyhow::anyhow!("Failed to read config file '{}': {e}", args.config))?;

    let config: config::WatchdogConfig =
        serde_json::from_str(&config_text).map_err(|e| anyhow::anyhow!("Invalid config: {e}"))?;

    // Stderr layer — always on, useful for interactive debugging.
    let stderr_layer = tracing_subscriber::fmt::layer()
        .with_writer(std::io::stderr)
        .with_target(false);

    // File layer — only when watchdog_log_file is configured.  Uses the same
    // file-rotate setup as the node/wallet logs so the file stays bounded.
    // ANSI colour codes are stripped so the file is plain text.
    let watchdog_log_path = format!("{}/watchdog.log", config.pub_logs_dir);
    let file_layer = {
        let file = Mutex::new(FileRotate::new(
            &watchdog_log_path,
            AppendCount::new(4),
            ContentLimit::Bytes(10 * 1024 * 1024),
            Compression::None,
            None,
        ));
        tracing_subscriber::fmt::layer()
            .with_writer(file)
            .with_ansi(false)
            .with_target(false)
    };

    tracing_subscriber::registry()
        .with(stderr_layer)
        .with(file_layer)
        .init();

    let (cmd_tx, cmd_rx) = mpsc::channel::<protocol::Command>(8);

    // SIGTERM handler: treat as an orderly stop so children are not orphaned.
    #[cfg(unix)]
    {
        let sigterm_tx = cmd_tx.clone();
        tokio::spawn(async move {
            use tokio::signal::unix::{SignalKind, signal};
            if let Ok(mut stream) = signal(SignalKind::terminate()) {
                stream.recv().await;
                tracing::info!("received SIGTERM; initiating graceful shutdown");
                let _ = sigterm_tx.send(protocol::Command::Stop).await;
            }
        });
    }

    // Read commands from stdin lines; EOF on stdin triggers Stop.
    let mut reader = BufReader::new(tokio::io::stdin());
    tokio::spawn(async move {
        while let Ok(Some(line)) = read_bounded_line(&mut reader).await {
            if let Ok(cmd) = serde_json::from_str::<protocol::Command>(&line) {
                if cmd_tx.send(cmd).await.is_err() {
                    break;
                }
            }
        }
        // stdin EOF — treat as stop
        let _ = cmd_tx.send(protocol::Command::Stop).await;
    });

    supervisor::run(config, cmd_rx).await
}
