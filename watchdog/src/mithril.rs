use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use anyhow::Result;
use serde::Deserialize;
use tokio::io::AsyncBufReadExt;
use tokio::process::Command;
use tokio::sync::mpsc;
use tracing::{info, warn};

use crate::config::MithrilConfig;
use crate::protocol::{Command as Cmd, Event, emit};
use crate::supervisor::tether_to_watchdog;

const PROGRESS_THROTTLE_MS: u128 = 500;
// Duration the cutover gate holds open for an in-flight cancel after the
// converter exits successfully.  Wide enough for inter-process pipe delivery
// on a loaded CI machine; negligible relative to the overall pipeline time.
const CUTOVER_GATE_MS: u64 = 100;

pub enum PipelineResult {
    Installed,
    NotNeeded,
    Cancelled,
    /// The user explicitly cancelled via `cancel_mithril`; the supervisor should
    /// wait for a recovery action rather than restarting the node immediately.
    UserCancelled,
    /// Post-cutover install failure: the `cutover-in-progress` marker was written
    /// but `install_staged` failed. Chain data may be partially modified; the node
    /// must not restart. Only wipe-and-full-sync can recover.
    Failed,
    Stopped,
}

#[derive(Deserialize)]
struct SnapshotMeta {
    beacon: Beacon,
}

#[derive(Deserialize)]
struct Beacon {
    immutable_file_number: u64,
}

#[derive(Deserialize)]
struct ProgressLine {
    files_downloaded: Option<u64>,
    files_total: Option<u64>,
    bytes_downloaded: Option<u64>,
    bytes_total: Option<u64>,
    seconds_elapsed: Option<f64>,
    step_num: Option<u32>,
    total_steps: Option<u32>,
}

pub async fn read_marker(state_dir: &str) -> Option<String> {
    let path = Path::new(state_dir)
        .join("Logs")
        .join("mithril-partial-sync.lock");
    let text = tokio::fs::read_to_string(&path).await.ok()?;
    let v: serde_json::Value = serde_json::from_str(&text).ok()?;
    v.get("state")?.as_str().map(|s| s.to_string())
}

pub async fn write_marker(state_dir: &str, state: &str) -> Result<()> {
    let logs = Path::new(state_dir).join("Logs");
    tokio::fs::create_dir_all(&logs).await?;
    let path = logs.join("mithril-partial-sync.lock");
    let json = serde_json::json!({ "state": state });
    tokio::fs::write(&path, serde_json::to_string_pretty(&json)?).await?;
    Ok(())
}

/// Returns the highest immutable chunk number found in chain_path/immutable/,
/// by parsing the numeric prefix of each filename (e.g. "09170.chunk" → 9170).
/// Returns None if the directory is missing or empty.
async fn highest_local_immutable(chain_path: &str) -> Option<u64> {
    let dir = Path::new(chain_path).join("immutable");
    let mut entries = tokio::fs::read_dir(dir).await.ok()?;
    let mut max_num: Option<u64> = None;
    while let Ok(Some(entry)) = entries.next_entry().await {
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        if let Some(stem) = name_str.split('.').next() {
            if let Ok(n) = stem.parse::<u64>() {
                max_num = Some(max_num.unwrap_or(0).max(n));
            }
        }
    }
    max_num
}

pub(crate) async fn probe(cfg: &MithrilConfig) -> Result<(Option<u64>, u64)> {
    let mut cmd = Command::new(&cfg.mithril_bin);
    cmd.args([
        "--origin-tag",
        "DAEDALUS",
        "--json",
        "cardano-db",
        "snapshot",
        "show",
        "latest",
    ])
    .env("AGGREGATOR_ENDPOINT", &cfg.aggregator_url);
    tether_to_watchdog(&mut cmd);
    let output = cmd.output().await?;
    if !output.status.success() {
        anyhow::bail!(
            "mithril-client snapshot show failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
    let meta: SnapshotMeta = serde_json::from_slice(&output.stdout)?;
    let local_highest = highest_local_immutable(&cfg.chain_path).await;
    Ok((local_highest, meta.beacon.immutable_file_number))
}

enum ProcResult {
    Success,
    Failed(String),
    Cancelled,
    Stopped,
}

async fn run_download(
    cfg: &MithrilConfig,
    staging_db: &Path,
    cmd_rx: &mut mpsc::Receiver<Cmd>,
    local_highest: Option<u64>,
    certified: u64,
) -> ProcResult {
    let mut cmd = Command::new(&cfg.mithril_bin);
    cmd.args([
        "--origin-tag",
        "DAEDALUS",
        "--json",
        "cardano-db",
        "download",
        "latest",
        "--download-dir",
    ])
    .arg(staging_db)
    .arg("--include-ancillary")
    .env("AGGREGATOR_ENDPOINT", &cfg.aggregator_url)
    .env("GENESIS_VERIFICATION_KEY", &cfg.genesis_vkey);
    if let Some(local) = local_highest {
        // When at or ahead of the certified tip, download just the certified chunk
        // to get the ledger state; --allow-override handles the overlap harmlessly.
        let start = if local >= certified {
            certified
        } else {
            local + 1
        };
        cmd.args([
            "--start",
            &start.to_string(),
            "--end",
            &certified.to_string(),
            "--allow-override",
        ]);
    }
    if let Some(ref key) = cfg.ancillary_vkey {
        cmd.env("ANCILLARY_VERIFICATION_KEY", key);
    }
    cmd.stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .kill_on_drop(true);
    tether_to_watchdog(&mut cmd);

    let mut proc = match cmd.spawn() {
        Ok(p) => p,
        Err(e) => return ProcResult::Failed(e.to_string()),
    };

    // mithril-client writes JSON progress to both stdout and stderr depending on
    // its version; read both streams and feed them into the same progress channel.
    let stdout = proc.stdout.take().unwrap();
    let stderr = proc.stderr.take().unwrap();
    let (prog_tx, mut prog_rx) = mpsc::channel::<Event>(16);

    fn spawn_progress_reader(
        reader: impl tokio::io::AsyncRead + Unpin + Send + 'static,
        tx: mpsc::Sender<Event>,
    ) {
        tokio::spawn(async move {
            let mut lines = tokio::io::BufReader::new(reader).lines();
            // Start PROGRESS_THROTTLE_MS in the past so the first event always fires.
            let mut last = Instant::now() - Duration::from_millis(PROGRESS_THROTTLE_MS as u64);
            while let Ok(Some(line)) = lines.next_line().await {
                if let Ok(p) = serde_json::from_str::<ProgressLine>(&line) {
                    let is_ledger = p.step_num.unwrap_or(1) > 1;
                    if last.elapsed().as_millis() >= PROGRESS_THROTTLE_MS {
                        last = Instant::now();
                        let _ = tx
                            .send(Event::MithrilProgress {
                                files_downloaded: p.files_downloaded.unwrap_or(0),
                                files_total: p.files_total.unwrap_or(0),
                                bytes_downloaded: p.bytes_downloaded.unwrap_or(0),
                                bytes_total: p.bytes_total.unwrap_or(0),
                                seconds_elapsed: p.seconds_elapsed.unwrap_or(0.0),
                                step_num: p.step_num.unwrap_or(1),
                                total_steps: p.total_steps.unwrap_or(4),
                                phase: if is_ledger {
                                    "ledger".to_string()
                                } else {
                                    "snapshot".to_string()
                                },
                            })
                            .await;
                        if is_ledger {
                            let _ = tx
                                .send(Event::MithrilStatus {
                                    phase: "verifying".to_string(),
                                })
                                .await;
                        }
                    }
                }
            }
        });
    }

    spawn_progress_reader(stdout, prog_tx.clone());
    spawn_progress_reader(stderr, prog_tx);

    let mut downloading_announced = false;
    let mut announce_downloading = |ev: &Event| {
        if !downloading_announced {
            if let Event::MithrilProgress {
                files_total, phase, ..
            } = ev
            {
                if *files_total > 0 && phase == "snapshot" {
                    emit(&Event::MithrilStatus {
                        phase: "downloading".to_string(),
                    });
                    downloading_announced = true;
                }
            }
        }
    };

    loop {
        tokio::select! {
            result = proc.wait() => {
                // Drain remaining progress events
                prog_rx.close();
                while let Some(ev) = prog_rx.recv().await {
                    announce_downloading(&ev);
                    emit(&ev);
                }
                match result {
                    Ok(s) if s.success() => return ProcResult::Success,
                    Ok(s) => return ProcResult::Failed(format!("mithril-client exited with {s}")),
                    Err(e) => return ProcResult::Failed(e.to_string()),
                }
            }
            Some(ev) = prog_rx.recv() => {
                announce_downloading(&ev);
                emit(&ev);
            }
            cmd = cmd_rx.recv() => {
                match cmd {
                    Some(Cmd::CancelMithril) => {
                        let _ = proc.start_kill();
                        let _ = proc.wait().await;
                        return ProcResult::Cancelled;
                    }
                    Some(Cmd::Stop) | None => {
                        let _ = proc.start_kill();
                        let _ = proc.wait().await;
                        return ProcResult::Stopped;
                    }
                    _ => {}
                }
            }
        }
    }
}

async fn validate_staged(staging_db: &Path) -> Result<()> {
    for (name, is_dir) in [
        ("clean", false),
        ("immutable", true),
        ("ledger", true),
        ("protocolMagicId", false),
    ] {
        let p = staging_db.join(name);
        let meta = tokio::fs::metadata(&p)
            .await
            .map_err(|_| anyhow::anyhow!("Missing {name} in staged db"))?;
        if is_dir != meta.is_dir() {
            anyhow::bail!("{name} has wrong type in staged db");
        }
    }
    Ok(())
}

// Move a file, falling back to copy+delete if src and dst are on different devices.
async fn move_file(src: &Path, dst: &Path) -> Result<()> {
    if tokio::fs::rename(src, dst).await.is_ok() {
        return Ok(());
    }
    tokio::fs::copy(src, dst)
        .await
        .map_err(|e| anyhow::anyhow!("copy {}: {e}", src.display()))?;
    let _ = tokio::fs::remove_file(src).await;
    Ok(())
}

// Move a directory tree, falling back to copy+delete if cross-device.
async fn move_dir(src: &Path, dst: &Path) -> Result<()> {
    if tokio::fs::rename(src, dst).await.is_ok() {
        return Ok(());
    }
    copy_dir_recursive(src, dst).await?;
    let _ = tokio::fs::remove_dir_all(src).await;
    Ok(())
}

async fn copy_dir_recursive(src: &Path, dst: &Path) -> Result<()> {
    let mut stack: Vec<(PathBuf, PathBuf)> = vec![(src.to_path_buf(), dst.to_path_buf())];
    while let Some((src_dir, dst_dir)) = stack.pop() {
        tokio::fs::create_dir_all(&dst_dir).await?;
        let mut entries = tokio::fs::read_dir(&src_dir).await?;
        while let Some(entry) = entries.next_entry().await? {
            let from = entry.path();
            let to = dst_dir.join(entry.file_name());
            if entry.file_type().await?.is_dir() {
                stack.push((from, to));
            } else {
                tokio::fs::copy(&from, &to).await?;
            }
        }
    }
    Ok(())
}

// For bootstrap (local_highest = None): replace the entire chain directory.
// For partial sync (local_highest = Some): merge new immutables in and replace ledger/lsm.
async fn install_staged(staging_db: &Path, chain_path: &Path, is_partial: bool) -> Result<()> {
    if !is_partial {
        // Detect symlinks/junctions (Windows): if chain_path is a link, install
        // inside the real target so the link entry point is preserved.
        let is_link = tokio::fs::symlink_metadata(chain_path)
            .await
            .ok()
            .map(|m| m.file_type().is_symlink())
            .unwrap_or(false);

        if is_link {
            let raw_target = tokio::fs::read_link(chain_path)
                .await
                .map_err(|e| anyhow::anyhow!("read chain_path symlink target: {e}"))?;
            let target = if raw_target.is_absolute() {
                raw_target
            } else {
                chain_path
                    .parent()
                    .unwrap_or(Path::new("."))
                    .join(raw_target)
            };
            let _ = tokio::fs::remove_dir_all(&target).await;
            move_dir(staging_db, &target)
                .await
                .map_err(|e| anyhow::anyhow!("install staged db to symlink target: {e}"))?;
        } else {
            let _ = tokio::fs::remove_dir_all(chain_path).await;
            // move_dir, not rename: staging and chain can live on different
            // filesystems (EXDEV), and the old chain is already gone at this
            // point — a bare rename failure would leave no chain at all.
            move_dir(staging_db, chain_path)
                .await
                .map_err(|e| anyhow::anyhow!("install staged db to chain path: {e}"))?;
        }
        return Ok(());
    }

    // Partial sync: move new immutable files into the existing chain directory.
    let src_immutable = staging_db.join("immutable");
    let dst_immutable = chain_path.join("immutable");
    let mut entries = tokio::fs::read_dir(&src_immutable).await?;
    while let Some(entry) = entries.next_entry().await? {
        let src = entry.path();
        let dst = dst_immutable.join(entry.file_name());
        move_file(&src, &dst).await?;
    }

    // Replace ledger directory.
    let src_ledger = staging_db.join("ledger");
    let dst_ledger = chain_path.join("ledger");
    let _ = tokio::fs::remove_dir_all(&dst_ledger).await;
    move_dir(&src_ledger, &dst_ledger).await?;

    // Replace lsm database if the converter produced one.
    let src_lsm = staging_db.join("lsm");
    if tokio::fs::metadata(&src_lsm).await.is_ok() {
        let dst_lsm = chain_path.join("lsm");
        let _ = tokio::fs::remove_dir_all(&dst_lsm).await;
        move_dir(&src_lsm, &dst_lsm).await?;
    }

    Ok(())
}

async fn find_highest_slot(ledger_dir: &Path) -> Result<u64> {
    let mut dir = tokio::fs::read_dir(ledger_dir).await?;
    let mut slots: Vec<u64> = Vec::new();
    while let Some(entry) = dir.next_entry().await? {
        if entry.file_type().await?.is_dir() {
            if let Ok(n) = entry.file_name().to_string_lossy().parse::<u64>() {
                slots.push(n);
            }
        }
    }
    slots
        .into_iter()
        .max()
        .ok_or_else(|| anyhow::anyhow!("No slot directories found in ledger dir"))
}

async fn run_converter(
    cfg: &MithrilConfig,
    staging_db: &Path,
    cmd_rx: &mut mpsc::Receiver<Cmd>,
) -> ProcResult {
    let ledger_dir = staging_db.join("ledger");
    let slot = match find_highest_slot(&ledger_dir).await {
        Ok(s) => s,
        Err(e) => return ProcResult::Failed(e.to_string()),
    };
    let slot_str = slot.to_string();

    let input_mem = ledger_dir.join(&slot_str);
    let temp_input = staging_db.join(&slot_str);
    let output_lsm_snapshot = ledger_dir.join(&slot_str);
    let output_lsm_database = staging_db.join("lsm");

    if let Err(e) = tokio::fs::rename(&input_mem, &temp_input).await {
        return ProcResult::Failed(e.to_string());
    }

    let _ = tokio::fs::remove_dir_all(&output_lsm_database).await;

    let mut converter_cmd = Command::new(&cfg.snapshot_converter_bin);
    converter_cmd
        .arg("--input-mem")
        .arg(&temp_input)
        .arg("--output-lsm-snapshot")
        .arg(&output_lsm_snapshot)
        .arg("--output-lsm-database")
        .arg(&output_lsm_database)
        .arg("--config")
        .arg(&cfg.converter_config)
        .stderr(std::process::Stdio::null())
        .kill_on_drop(true);
    tether_to_watchdog(&mut converter_cmd);
    let mut proc = match converter_cmd.spawn() {
        Ok(p) => p,
        Err(e) => return ProcResult::Failed(e.to_string()),
    };

    let result = loop {
        tokio::select! {
            result = proc.wait() => {
                break match result {
                    Ok(s) if s.success() => ProcResult::Success,
                    Ok(s) => ProcResult::Failed(format!("snapshot-converter exited with {s}")),
                    Err(e) => ProcResult::Failed(e.to_string()),
                };
            }
            cmd = cmd_rx.recv() => {
                match cmd {
                    Some(Cmd::CancelMithril) => {
                        let _ = proc.start_kill();
                        let _ = proc.wait().await;
                        break ProcResult::Cancelled;
                    }
                    Some(Cmd::Stop) | None => {
                        let _ = proc.start_kill();
                        let _ = proc.wait().await;
                        break ProcResult::Stopped;
                    }
                    _ => {}
                }
            }
        }
    };

    let _ = tokio::fs::remove_dir_all(&temp_input).await;
    result
}

pub async fn run_pipeline(
    cfg: &MithrilConfig,
    cmd_rx: &mut mpsc::Receiver<Cmd>,
    force: bool,
    wipe_chain: bool,
) -> PipelineResult {
    // The pipeline downloads with --include-ancillary (the converter needs the
    // ledger state it brings), and mithril-client refuses that flag without an
    // ANCILLARY_VERIFICATION_KEY — or, on older versions, skips signature
    // verification of the ancillary data. Fail fast, before any chain wipe,
    // instead of failing mid-download or downloading unverified data.
    if cfg.ancillary_vkey.is_none() {
        emit(&Event::MithrilError {
            code: "ANCILLARY_VKEY_MISSING".to_string(),
            message: "Mithril is configured without an ancillary verification key".to_string(),
        });
        return PipelineResult::Cancelled;
    }

    // If wipe_chain is requested, delete the existing chain directory so the
    // download is treated as a full bootstrap rather than an incremental sync.
    if wipe_chain {
        let chain_path = PathBuf::from(&cfg.chain_path);
        let state_path = PathBuf::from(&cfg.state_dir);
        // Safety: chain_path must be absolute and must live inside state_dir to
        // prevent a misconfigured path (e.g. "/") from deleting the user's filesystem.
        let chain_is_safe = chain_path.is_absolute()
            && chain_path
                .canonicalize()
                .ok()
                .or_else(|| {
                    // chain_path may not exist yet; check the parent instead.
                    chain_path
                        .parent()
                        .and_then(|p| p.canonicalize().ok())
                        .map(|canon_parent| {
                            canon_parent.join(chain_path.file_name().unwrap_or_default())
                        })
                })
                .map(|canon| {
                    canon.starts_with(state_path.canonicalize().unwrap_or(state_path.clone()))
                })
                .unwrap_or(false);
        if !chain_is_safe {
            warn!(
                "mithril: chain_path '{}' is not inside state_dir '{}'",
                cfg.chain_path, cfg.state_dir
            );
            emit(&Event::MithrilError {
                code: "INVALID_CHAIN_PATH".to_string(),
                message: format!(
                    "chain_path '{}' is not inside state_dir '{}'",
                    cfg.chain_path, cfg.state_dir
                ),
            });
            return PipelineResult::Cancelled;
        }
        let _ = tokio::fs::remove_dir_all(&chain_path).await;
    }

    info!("mithril: preparing");
    emit(&Event::MithrilStatus {
        phase: "preparing".to_string(),
    });

    // 1. Behind-ness probe
    let (local_highest, certified) = match probe(cfg).await {
        Ok(r) => r,
        Err(e) => {
            warn!("mithril probe failed: {e}");
            emit(&Event::MithrilError {
                code: "PROBE_FAILED".to_string(),
                message: e.to_string(),
            });
            return PipelineResult::Cancelled;
        }
    };

    let local_num = local_highest.unwrap_or(0);
    let behind = certified.saturating_sub(local_num);
    if !force && behind < cfg.behind_threshold {
        info!(
            "Mithril not needed: local={local_num}, certified={certified}, behind={behind}, threshold={}",
            cfg.behind_threshold
        );
        emit(&Event::MithrilNotNeeded {
            local_immutable_count: local_num,
            latest_certified_immutable: certified,
        });
        return PipelineResult::NotNeeded;
    }

    if force {
        info!(
            "Mithril forced by caller: local={local_num}, certified={certified}; skipping not-needed check"
        );
    } else {
        info!(
            "Mithril needed: local={local_num}, certified={certified}, behind={behind}; starting download"
        );
    }

    // 2. Prepare staging
    let staging_root = PathBuf::from(&cfg.state_dir).join("mithril-partial-sync");
    // mithril-client creates a `db/` subdirectory inside --download-dir, so we
    // pass `download_dir` to the client and then work with `download_dir/db`.
    let download_dir = staging_root.join("download");
    let staging_db = download_dir.join("db");
    let _ = tokio::fs::remove_dir_all(&staging_root).await;
    if let Err(e) = tokio::fs::create_dir_all(&download_dir).await {
        warn!("mithril: failed to create staging directory: {e}");
        emit(&Event::MithrilError {
            code: "STAGING_FAILED".to_string(),
            message: e.to_string(),
        });
        return PipelineResult::Cancelled;
    }

    // 3. Download — "downloading" status is emitted by run_download on first real progress event.
    match run_download(cfg, &download_dir, cmd_rx, local_highest, certified).await {
        ProcResult::Success => {}
        ProcResult::Failed(msg) => {
            let _ = tokio::fs::remove_dir_all(&staging_root).await;
            warn!("mithril download failed: {msg}");
            emit(&Event::MithrilError {
                code: "PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED".to_string(),
                message: msg,
            });
            return PipelineResult::Cancelled;
        }
        ProcResult::Cancelled => {
            let _ = tokio::fs::remove_dir_all(&staging_root).await;
            info!("mithril: cancelled by user");
            emit(&Event::MithrilStatus {
                phase: "cancelled".to_string(),
            });
            return PipelineResult::UserCancelled;
        }
        ProcResult::Stopped => {
            let _ = tokio::fs::remove_dir_all(&staging_root).await;
            return PipelineResult::Stopped;
        }
    }

    // 4. Validate
    if let Err(e) = validate_staged(&staging_db).await {
        let _ = tokio::fs::remove_dir_all(&staging_root).await;
        warn!("mithril staged db invalid: {e}");
        emit(&Event::MithrilError {
            code: "PARTIAL_SYNC_STAGED_DB_INVALID".to_string(),
            message: e.to_string(),
        });
        return PipelineResult::Cancelled;
    }

    // 5. Convert
    info!("mithril: converting");
    emit(&Event::MithrilStatus {
        phase: "converting".to_string(),
    });
    match run_converter(cfg, &staging_db, cmd_rx).await {
        ProcResult::Success => {}
        ProcResult::Failed(msg) => {
            let _ = tokio::fs::remove_dir_all(&staging_root).await;
            warn!("mithril conversion failed: {msg}");
            emit(&Event::MithrilError {
                code: "PARTIAL_SYNC_CONVERSION_FAILED".to_string(),
                message: msg,
            });
            return PipelineResult::Cancelled;
        }
        ProcResult::Cancelled => {
            let _ = tokio::fs::remove_dir_all(&staging_root).await;
            info!("mithril: cancelled by user (post-convert)");
            emit(&Event::MithrilStatus {
                phase: "cancelled".to_string(),
            });
            return PipelineResult::UserCancelled;
        }
        ProcResult::Stopped => {
            let _ = tokio::fs::remove_dir_all(&staging_root).await;
            return PipelineResult::Stopped;
        }
    }

    // Cutover gate: hold at the last safe cancellation boundary for
    // CUTOVER_GATE_MS so any CancelMithril/Stop that raced the converter exit
    // can be delivered before chain mutation begins.  Unrelated commands that
    // are queued ahead of a cancel are drained rather than silently consumed.
    // If the window closes with no cancel the loop is interrupted by timeout
    // and the pipeline proceeds.
    let gate = async {
        loop {
            match cmd_rx.recv().await {
                Some(Cmd::CancelMithril) => return ProcResult::Cancelled,
                Some(Cmd::Stop) | None => return ProcResult::Stopped,
                Some(_) => {} // discard unrelated; keep the window open
            }
        }
    };
    if let Ok(abort) = tokio::time::timeout(Duration::from_millis(CUTOVER_GATE_MS), gate).await {
        let _ = tokio::fs::remove_dir_all(&staging_root).await;
        return match abort {
            ProcResult::Cancelled => {
                info!("mithril: cancelled by user (cutover gate)");
                emit(&Event::MithrilStatus {
                    phase: "cancelled".to_string(),
                });
                PipelineResult::UserCancelled
            }
            ProcResult::Stopped => PipelineResult::Stopped,
            _ => unreachable!(),
        };
    }

    // 6. Install
    info!("mithril: installing");
    emit(&Event::MithrilStatus {
        phase: "installing".to_string(),
    });

    // Write cutover-in-progress marker before rename
    if let Err(e) = write_marker(&cfg.state_dir, "cutover-in-progress").await {
        let _ = tokio::fs::remove_dir_all(&staging_root).await;
        warn!("mithril: failed to write cutover-in-progress marker: {e}");
        emit(&Event::MithrilError {
            code: "MARKER_FAILED".to_string(),
            message: e.to_string(),
        });
        return PipelineResult::Cancelled;
    }

    let chain_path = PathBuf::from(&cfg.chain_path);
    let is_partial = local_highest.is_some();
    if let Err(e) = install_staged(&staging_db, &chain_path, is_partial).await {
        // The cutover-in-progress marker is already written and chain may be
        // partially modified. Retain staging material for potential recovery;
        // return Failed so the supervisor does not restart the node against
        // a partially installed chain.
        warn!("mithril install failed (chain may be partially modified): {e}");
        emit(&Event::MithrilError {
            code: "INSTALL_FAILED".to_string(),
            message: e.to_string(),
        });
        return PipelineResult::Failed;
    }

    // Clean up staging root (download dir and any remnants)
    let _ = tokio::fs::remove_dir_all(&staging_root).await;

    // Write installed-awaiting-node-start marker
    if let Err(e) = write_marker(&cfg.state_dir, "installed-awaiting-node-start").await {
        warn!("Failed to write installed-awaiting-node-start marker: {e}");
    }

    info!("mithril: installed successfully");
    emit(&Event::MithrilStatus {
        phase: "finalizing".to_string(),
    });

    PipelineResult::Installed
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    // Real NTFS junction tests.
    //
    // The parallel TypeScript suite (chainStorageWindows.realfs.spec.ts) covers
    // the lstat premise and the TypeScript-side junction handling.  These tests
    // cover install_staged, which is the Rust-side junction handler: when
    // chain_path is a junction the function must install into the junction
    // *target* rather than replacing the junction entry point itself.
    //
    // Junctions are created with `mklink /J` (no elevation required) rather
    // than std::os::windows::fs::symlink_dir (requires Developer Mode or UAC).
    #[cfg(windows)]
    mod junction_install {
        use super::super::install_staged;
        use std::fs;
        use std::path::Path;

        fn temp_dir(label: &str) -> std::path::PathBuf {
            let p = std::env::temp_dir().join(format!(
                "wdg-jn-{}-{}-{}",
                label,
                std::process::id(),
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .subsec_nanos()
            ));
            fs::create_dir_all(&p).unwrap();
            p
        }

        fn make_junction(link: &Path, target: &Path) {
            let status = std::process::Command::new("cmd")
                .args([
                    "/c",
                    "mklink",
                    "/J",
                    link.to_str().unwrap(),
                    target.to_str().unwrap(),
                ])
                .status()
                .expect("mklink /J: failed to spawn cmd");
            assert!(
                status.success(),
                "mklink /J failed for {link:?} → {target:?}"
            );
        }

        fn make_staging(root: &Path) -> std::path::PathBuf {
            let staging = root.join("staging");
            fs::create_dir_all(&staging).unwrap();
            fs::write(staging.join("probe.txt"), b"staged").unwrap();
            staging
        }

        // install_staged with a junction chain_path must install into the
        // junction *target* and leave the junction entry point intact.
        #[tokio::test]
        async fn installs_into_junction_target_preserving_entry_point() {
            let root = temp_dir("basic");
            let target = root.join("target");
            fs::create_dir_all(&target).unwrap();
            let chain_path = root.join("chain");
            make_junction(&chain_path, &target);

            let staging = make_staging(&root);
            install_staged(&staging, &chain_path, false).await.unwrap();

            // Junction entry point still reports as a symlink
            let meta = fs::symlink_metadata(&chain_path).unwrap();
            assert!(
                meta.file_type().is_symlink(),
                "chain entry point should remain a junction after install"
            );

            // Staged content is readable through the junction
            let via_link = fs::read_to_string(chain_path.join("probe.txt")).unwrap();
            assert_eq!(via_link, "staged");

            // Staged content is present in the real target
            let in_target = fs::read_to_string(target.join("probe.txt")).unwrap();
            assert_eq!(in_target, "staged");
        }

        // install_staged with a plain directory must rename staging → chain_path
        // directly without creating a junction.
        #[tokio::test]
        async fn plain_directory_install_replaces_contents() {
            let root = temp_dir("plain");
            let chain_path = root.join("chain");
            fs::create_dir_all(&chain_path).unwrap();
            fs::write(chain_path.join("old.txt"), b"old").unwrap();

            let staging = make_staging(&root);
            install_staged(&staging, &chain_path, false).await.unwrap();

            // chain_path must not have become a junction
            let meta = fs::symlink_metadata(&chain_path).unwrap();
            assert!(!meta.file_type().is_symlink());

            // New content present, old content gone
            assert_eq!(
                fs::read_to_string(chain_path.join("probe.txt")).unwrap(),
                "staged"
            );
            assert!(!chain_path.join("old.txt").exists());
        }

        // A dangling junction (target deleted after the junction was created)
        // must not silently produce a corrupt state.  install_staged reads the
        // raw link target, and move_dir creates the target directory if missing,
        // so the install must succeed by recreating the target.
        #[tokio::test]
        async fn dangling_junction_installs_by_recreating_target() {
            let root = temp_dir("dangling");
            let target = root.join("doomed");
            fs::create_dir_all(&target).unwrap();
            let chain_path = root.join("chain");
            make_junction(&chain_path, &target);

            // Delete the target to make a dangling junction
            fs::remove_dir_all(&target).unwrap();
            assert!(
                !target.exists(),
                "target must not exist before install_staged"
            );

            let staging = make_staging(&root);
            install_staged(&staging, &chain_path, false)
                .await
                .expect("install_staged should succeed by recreating junction target");

            // Junction still points to the (now recreated) target
            assert!(
                chain_path.join("probe.txt").exists(),
                "probe file must be reachable through the junction after install"
            );
        }

        // install_staged uses read_link to get the raw target, then resolves
        // relative targets against chain_path's parent.  Verify the relative
        // branch by creating a junction whose target path, as returned by
        // read_link, will be absolute (the common Windows case) so the absolute
        // branch is exercised; the relative branch is covered by the code path
        // but cannot be triggered by mklink /J in practice.
        //
        // This test is therefore a belt-and-suspenders check that the absolute
        // branch handles a junction whose target is in a sibling directory.
        #[tokio::test]
        async fn junction_with_sibling_target_installs_correctly() {
            let root = temp_dir("sibling");
            let target = root.join("sibling-target").join("chain");
            fs::create_dir_all(&target).unwrap();
            let state_dir = root.join("state");
            fs::create_dir_all(&state_dir).unwrap();
            let chain_path = state_dir.join("chain");
            make_junction(&chain_path, &target);

            let staging = make_staging(&state_dir);
            install_staged(&staging, &chain_path, false).await.unwrap();

            assert_eq!(
                fs::read_to_string(chain_path.join("probe.txt")).unwrap(),
                "staged"
            );
            assert_eq!(
                fs::read_to_string(target.join("probe.txt")).unwrap(),
                "staged"
            );
        }
    }
}
