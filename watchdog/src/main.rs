mod chain_validation;
mod config;
mod mithril;
mod protocol;
mod supervisor;

use anyhow::Result;
use clap::Parser;
use tokio::io::{AsyncBufReadExt, AsyncReadExt, BufReader};
use tokio::sync::mpsc;

#[derive(Parser)]
#[command(about = "Process supervisor for cardano-node and cardano-wallet")]
struct Args {
    /// Path to the JSON config file
    #[arg(long)]
    config: String,
}

// Bound total stdin consumption so a wedged Electron process that writes
// without newlines can't grow the BufReader buffer indefinitely.
const MAX_STDIN_BYTES: u64 = 4 * 1024 * 1024; // 4 MB

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .without_time()
        .with_target(false)
        .init();

    let args = Args::parse();

    let config_text = tokio::fs::read_to_string(&args.config)
        .await
        .map_err(|e| anyhow::anyhow!("Failed to read config file '{}': {e}", args.config))?;

    let config: config::WatchdogConfig =
        serde_json::from_str(&config_text).map_err(|e| anyhow::anyhow!("Invalid config: {e}"))?;

    let (cmd_tx, cmd_rx) = mpsc::channel::<protocol::Command>(8);

    // Read commands from stdin lines; EOF on stdin triggers Stop.
    let stdin = tokio::io::stdin();
    let mut lines = BufReader::new(stdin.take(MAX_STDIN_BYTES)).lines();
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
