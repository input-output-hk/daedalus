mod config;
mod protocol;
mod supervisor;

use anyhow::Result;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::sync::mpsc;

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .without_time()
        .with_target(false)
        .init();

    let stdin = tokio::io::stdin();
    let mut lines = BufReader::new(stdin).lines();

    let config_line = lines
        .next_line()
        .await?
        .ok_or_else(|| anyhow::anyhow!("Expected config JSON on first stdin line"))?;

    let config: config::WatchdogConfig = serde_json::from_str(&config_line)
        .map_err(|e| anyhow::anyhow!("Invalid config: {e}"))?;

    let (cmd_tx, cmd_rx) = mpsc::channel::<protocol::Command>(8);

    // Read commands from subsequent stdin lines
    tokio::spawn(async move {
        while let Ok(Some(line)) = lines.next_line().await {
            if let Ok(cmd) = serde_json::from_str::<protocol::Command>(&line) {
                if cmd_tx.send(cmd).await.is_err() {
                    break;
                }
            }
        }
    });

    supervisor::run(config, cmd_rx).await
}
