mod config;
mod mithril;
mod protocol;
mod supervisor;

use anyhow::Result;
use tokio::io::{AsyncBufRead, AsyncBufReadExt, AsyncReadExt, BufReader};
use tokio::sync::mpsc;

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
    tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .without_time()
        .with_target(false)
        .init();

    let mut reader = BufReader::new(tokio::io::stdin());

    let config_line = read_bounded_line(&mut reader)
        .await?
        .ok_or_else(|| anyhow::anyhow!("Expected config JSON on first stdin line"))?;

    let config: config::WatchdogConfig =
        serde_json::from_str(&config_line).map_err(|e| anyhow::anyhow!("Invalid config: {e}"))?;

    let (cmd_tx, cmd_rx) = mpsc::channel::<protocol::Command>(8);

    // Read commands from subsequent stdin lines; EOF on stdin triggers Stop.
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
