mod chain_validation;
mod config;
mod mithril;
mod protocol;
mod supervisor;

use anyhow::Result;
use tokio::io::{AsyncBufReadExt, AsyncReadExt, BufReader};
use tokio::sync::mpsc;

// Bound total stdin consumption so a wedged Electron process that writes
// without newlines can't grow the BufReader buffer indefinitely.
// Normal sessions use << 1 MB (one config line + small JSON commands).
const MAX_STDIN_BYTES: u64 = 4 * 1024 * 1024; // 4 MB

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .without_time()
        .with_target(false)
        .init();

    let stdin = tokio::io::stdin();
    let mut lines = BufReader::new(stdin.take(MAX_STDIN_BYTES)).lines();

    let config_line = lines
        .next_line()
        .await?
        .ok_or_else(|| anyhow::anyhow!("Expected config JSON on first stdin line"))?;

    let config: config::WatchdogConfig =
        serde_json::from_str(&config_line).map_err(|e| anyhow::anyhow!("Invalid config: {e}"))?;

    let (cmd_tx, cmd_rx) = mpsc::channel::<protocol::Command>(8);

    // Read commands from subsequent stdin lines; EOF on stdin triggers Stop.
    tokio::spawn(async move {
        while let Ok(Some(line)) = lines.next_line().await {
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
