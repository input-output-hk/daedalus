use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use anyhow::Result;
use serde::{Deserialize, Serialize};
use tokio::io::AsyncBufReadExt;
use tokio::process::Command;
use tokio::sync::mpsc;
use tracing::{info, warn};

use crate::chain_validation;
use crate::config::MithrilConfig;
use crate::protocol::{Command as Cmd, Event, emit};
use crate::state;
use crate::supervisor::tether_to_watchdog;

const PROGRESS_THROTTLE_MS: u128 = 500;
// Duration the cutover gate holds open for an in-flight cancel after the
// converter exits successfully.  Wide enough for inter-process pipe delivery
// on a loaded CI machine; negligible relative to the overall pipeline time.
const CUTOVER_GATE_MS: u64 = 100;
// How much of a converter's stdout and stderr is kept for the log. Its error
// messages are a few lines; anything longer is repeated progress output.
const SUBPROCESS_OUTPUT_TAIL: usize = 16 * 1024;

pub enum PipelineResult {
    /// A database was installed. `rebuild` is true when the pipeline was a
    /// rebuild, asked for or forced by one that did not finish.
    Installed {
        rebuild: bool,
    },
    NotNeeded,
    Cancelled,
    /// The user explicitly cancelled via `cancel_mithril`; the supervisor should
    /// wait for a recovery action rather than restarting the node immediately.
    UserCancelled,
    /// Post-cutover install failure: the `cutover-in-progress` marker and the
    /// record of the install were written but `install_staged` failed. Chain
    /// data may be partially modified; the node must not restart, in this
    /// session or the next. Only wipe-and-full-sync can recover.
    Failed,
    /// A rebuild (`wipe_chain`) started deleting the old chain and installed
    /// nothing: the delete or a later step failed, was refused, or was
    /// cancelled by the user. A node started now would sync from genesis
    /// without asking, so the supervisor reports no chain and waits for the
    /// user's choice, as on first run.
    FailedAfterWipe,
    Stopped,
}

#[derive(Deserialize)]
struct SnapshotMeta {
    beacon: Beacon,
    /// Size of the snapshot's database once unpacked, in bytes.
    #[serde(default)]
    total_db_size_uncompressed: Option<u64>,
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

/// File in the state directory that records a rebuild in progress. It is
/// written before the rebuild deletes anything and deleted once a database is
/// installed, so a rebuild cut short by a failure, a crash or the user quitting
/// is still known at the next start. What a part-failed delete leaves would
/// otherwise look like a chain, and the node would replay it from genesis.
/// Every other install writes it too, before it changes the database, so an
/// install cut short leaves no database that counts as a chain.
const REBUILD_MARKER: &str = "chain-rebuild.json";

/// Where the rebuild marker is written before it is renamed into place, so
/// the marker is never seen half-written.
const REBUILD_MARKER_TEMP: &str = "chain-rebuild.json.tmp";

/// What the record of an unfinished change to the chain directory is for.
/// A record keeps the kind it was first written with until it is removed.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum RecordKind {
    /// A rebuild began deleting the old chain. Records written before kinds
    /// existed name none, and were all written by a rebuild or for a full
    /// install, so they read as this kind.
    #[default]
    Rebuild,
    /// An install that was not a rebuild began changing the database: a
    /// full install moving the download in, or a partial sync replacing
    /// chunks and the ledger.
    Install,
}

#[derive(Serialize, Deserialize)]
struct RebuildMarker {
    /// The chain directory being rebuilt, as Mithril's chain_path names it.
    chain_path: String,
    #[serde(default)]
    kind: RecordKind,
}

/// `path` without its trailing separators, unless nothing else is left.
fn without_trailing_separators(path: &str) -> &str {
    let trimmed = path.trim_end_matches(std::path::is_separator);
    if trimmed.is_empty() { path } else { trimmed }
}

/// Whether two spellings of a chain directory name the same directory, as
/// the platform compares paths (without regard to case on Windows), with
/// trailing separators ignored.
pub(crate) fn same_chain_path(a: &str, b: &str) -> bool {
    chain_validation::is_same_path(
        Path::new(without_trailing_separators(a)),
        Path::new(without_trailing_separators(b)),
    )
}

/// The kind of the record of an unfinished rebuild or install of the
/// configured chain directory, or None when there is none. A rebuild of it
/// began deleting it, or an install began changing its database, and no
/// database has been installed since. A marker for another chain directory
/// does not count.
pub async fn pending_record(cfg: &MithrilConfig) -> Option<RecordKind> {
    let path = Path::new(&cfg.state_dir).join(REBUILD_MARKER);
    let text = match tokio::fs::read_to_string(&path).await {
        Ok(text) => text,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return None,
        Err(e) => {
            warn!("mithril: cannot read {}: {e}", path.display());
            return unreadable_marker_pending(cfg, &path).await;
        }
    };
    match serde_json::from_str::<RebuildMarker>(&text) {
        Ok(marker) => same_chain_path(&marker.chain_path, &cfg.chain_path).then_some(marker.kind),
        Err(e) => {
            warn!("mithril: {} is not a rebuild marker: {e}", path.display());
            unreadable_marker_pending(cfg, &path).await
        }
    }
}

/// Whether a marker that cannot be read, and so names no directory, still
/// counts as a pending rebuild of the configured chain directory. A rebuild
/// deletes the `clean` marker before anything else and an install moves it in
/// last, so while the directory has no `clean` the marker may be its own and
/// counts, as a rebuild's, since its kind cannot be read either. A directory
/// with `clean` holds a database no unfinished rebuild has touched, and the
/// marker is deleted.
async fn unreadable_marker_pending(cfg: &MithrilConfig, path: &Path) -> Option<RecordKind> {
    let clean = Path::new(&cfg.chain_path).join("clean");
    if !tokio::fs::metadata(&clean).await.is_ok_and(|m| m.is_file()) {
        warn!(
            "mithril: {} has no clean marker, so the rebuild the unreadable {} recorded counts as pending",
            cfg.chain_path,
            path.display()
        );
        return Some(RecordKind::Rebuild);
    }
    match tokio::fs::remove_file(path).await {
        Ok(()) => warn!(
            "mithril: deleted the unreadable {}; {} holds a clean database",
            path.display(),
            cfg.chain_path
        ),
        Err(e) => warn!("mithril: could not delete {}: {e}", path.display()),
    }
    None
}

/// Writes the rebuild marker for the configured chain directory. It is
/// written in full to a temporary file in the same directory and then renamed
/// over the marker, so a crash or a full disk never leaves a partial marker.
async fn write_rebuild_marker(cfg: &MithrilConfig, kind: RecordKind) -> Result<()> {
    use tokio::io::AsyncWriteExt;
    let marker = RebuildMarker {
        chain_path: cfg.chain_path.clone(),
        kind,
    };
    let text = serde_json::to_string_pretty(&marker)?;
    tokio::fs::create_dir_all(&cfg.state_dir).await?;
    let temp = Path::new(&cfg.state_dir).join(REBUILD_MARKER_TEMP);
    let written = async {
        let mut file = tokio::fs::File::create(&temp).await?;
        file.write_all(text.as_bytes()).await?;
        file.sync_all().await?;
        drop(file);
        tokio::fs::rename(&temp, Path::new(&cfg.state_dir).join(REBUILD_MARKER)).await
    }
    .await;
    if written.is_err() {
        let _ = tokio::fs::remove_file(&temp).await;
    }
    Ok(written?)
}

/// Deletes the rebuild marker when it is for the configured chain directory:
/// a database was installed there, or the user chose to sync it from genesis.
pub async fn clear_rebuild_marker(cfg: &MithrilConfig) {
    if pending_record(cfg).await.is_none() {
        return;
    }
    let path = Path::new(&cfg.state_dir).join(REBUILD_MARKER);
    match tokio::fs::remove_file(&path).await {
        Ok(()) => info!(
            "mithril: the rebuild of {} is no longer pending",
            cfg.chain_path
        ),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(e) => warn!("mithril: could not delete {}: {e}", path.display()),
    }
}

/// Parses the chunk number from an immutable file name ("09170.chunk" → 9170).
fn immutable_file_number(file_name: &str) -> Option<u64> {
    file_name.split('.').next()?.parse().ok()
}

/// Returns the highest immutable chunk number found in chain_path/immutable/,
/// by parsing the numeric prefix of each filename (e.g. "09170.chunk" → 9170).
/// Returns None if the directory is missing or empty.
async fn highest_local_immutable(chain_path: &str) -> Option<u64> {
    let dir = Path::new(chain_path).join("immutable");
    let mut entries = tokio::fs::read_dir(dir).await.ok()?;
    let mut max_num: Option<u64> = None;
    while let Ok(Some(entry)) = entries.next_entry().await {
        if let Some(n) = immutable_file_number(&entry.file_name().to_string_lossy()) {
            max_num = Some(max_num.unwrap_or(0).max(n));
        }
    }
    max_num
}

/// The inclusive `--start`/`--end` immutable file range for a partial sync, or
/// None for a bootstrap (no local immutable files), which downloads everything.
///
/// The highest local chunk is the one cardano-node was still appending to, so
/// it is usually a strict prefix of the certified chunk with the same number.
/// The range therefore starts at that chunk, not after it: skipping it would
/// install the following chunks after an incomplete one and leave the blocks
/// missing from its tail permanently absent from the ImmutableDB.
///
/// When the local chain is at or ahead of the certified tip, only the
/// certified chunk is downloaded, because `--include-ancillary` requires the
/// range to contain it.
fn download_range(local_highest: Option<u64>, certified: u64) -> Option<(u64, u64)> {
    local_highest.map(|local| (local.min(certified), certified))
}

pub(crate) async fn probe(cfg: &MithrilConfig) -> Result<(Option<u64>, u64)> {
    let (local_highest, certified, _) = probe_snapshot(cfg).await?;
    Ok((local_highest, certified))
}

/// The highest local chunk, the certified chunk and, when the aggregator
/// states it, the size of the snapshot's database once unpacked.
async fn probe_snapshot(cfg: &MithrilConfig) -> Result<(Option<u64>, u64, Option<u64>)> {
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
    Ok((
        local_highest,
        meta.beacon.immutable_file_number,
        meta.total_db_size_uncompressed,
    ))
}

/// `path` as an external program is given it. On Windows, `read_link` on a
/// junction returns its target in the verbatim form `\\?\D:\...`, and the
/// staging directory inside the chain directory inherits that form. Not every
/// program accepts it, so a verbatim drive path is passed in its plain form,
/// `D:\...`. Any other path, a verbatim network share `\\?\UNC\...`
/// included, is passed as it is.
fn for_external_program(path: &Path) -> PathBuf {
    #[cfg(windows)]
    if let Some(plain) = path.to_str().and_then(plain_drive_path) {
        return PathBuf::from(plain);
    }
    path.to_path_buf()
}

/// The plain form of a verbatim drive path, `\\?\D:\data` for `D:\data`,
/// or None when `path` is not one.
#[cfg(any(windows, test))]
fn plain_drive_path(path: &str) -> Option<&str> {
    let rest = path.strip_prefix(r"\\?\")?;
    let drive = rest.as_bytes();
    let is_drive_root =
        drive.len() >= 3 && drive[0].is_ascii_alphabetic() && drive[1] == b':' && drive[2] == b'\\';
    is_drive_root.then_some(rest)
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
    rebuild: bool,
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
    .arg(for_external_program(staging_db))
    .arg("--include-ancillary")
    .env("AGGREGATOR_ENDPOINT", &cfg.aggregator_url)
    .env("GENESIS_VERIFICATION_KEY", &cfg.genesis_vkey);
    if let Some((start, end)) = download_range(local_highest, certified) {
        cmd.args([
            "--start",
            &start.to_string(),
            "--end",
            &end.to_string(),
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
        rebuild: bool,
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
                                    rebuild,
                                })
                                .await;
                        }
                    }
                }
            }
        });
    }

    spawn_progress_reader(stdout, prog_tx.clone(), rebuild);
    spawn_progress_reader(stderr, prog_tx, rebuild);

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
                        rebuild,
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

/// How many times in all a rename that fails without crossing volumes is
/// tried before an install falls back to copying, and the pause between
/// tries. On Windows a file that another program, such as a virus scanner,
/// briefly holds open cannot be renamed. The download is staged on the
/// chain's own volume, so a copy there needs as much free space again as the
/// space check reserved for the database.
const RENAME_ATTEMPTS: u32 = 5;
const RENAME_RETRY_DELAY: Duration = Duration::from_millis(200);

/// Renames `src` to `dst`. A rename between volumes cannot succeed and fails
/// at once; any other failure is tried again, RENAME_ATTEMPTS times in all.
async fn rename_with_retry(src: &Path, dst: &Path) -> std::io::Result<()> {
    let mut attempt = 1;
    loop {
        match tokio::fs::rename(src, dst).await {
            Ok(()) => return Ok(()),
            Err(e)
                if e.kind() == std::io::ErrorKind::CrossesDevices || attempt >= RENAME_ATTEMPTS =>
            {
                return Err(e);
            }
            Err(e) => {
                warn!(
                    "mithril: could not rename {} (attempt {attempt}): {e}; trying again",
                    src.display()
                );
                tokio::time::sleep(RENAME_RETRY_DELAY).await;
                attempt += 1;
            }
        }
    }
}

// Move a file, falling back to copy+delete when it cannot be renamed.
async fn move_file(src: &Path, dst: &Path) -> Result<()> {
    match rename_with_retry(src, dst).await {
        Ok(()) => return Ok(()),
        Err(e) => warn!("mithril: copying {} instead: {e}", src.display()),
    }
    tokio::fs::copy(src, dst)
        .await
        .map_err(|e| anyhow::anyhow!("copy {}: {e}", src.display()))?;
    let _ = tokio::fs::remove_file(src).await;
    Ok(())
}

// Move a directory tree, falling back to copy+delete when it cannot be renamed.
async fn move_dir(src: &Path, dst: &Path) -> Result<()> {
    match rename_with_retry(src, dst).await {
        Ok(()) => return Ok(()),
        Err(e) => warn!("mithril: copying {} instead: {e}", src.display()),
    }
    copy_dir_recursive(src, dst).await?;
    let _ = tokio::fs::remove_dir_all(src).await;
    Ok(())
}

/// Copies the tree at `src` into `dst`. When `src` is a database, its `clean`
/// marker is copied after every other entry: cardano-node opens a database
/// marked clean without validating its chunks, so a copy that fails part-way
/// must not leave the marker on a partial database.
async fn copy_dir_recursive(src: &Path, dst: &Path) -> Result<()> {
    let mut stack: Vec<(PathBuf, PathBuf)> = vec![(src.to_path_buf(), dst.to_path_buf())];
    let mut clean: Option<(PathBuf, PathBuf)> = None;
    while let Some((src_dir, dst_dir)) = stack.pop() {
        tokio::fs::create_dir_all(&dst_dir).await?;
        let at_top = src_dir == src;
        let mut entries = tokio::fs::read_dir(&src_dir).await?;
        while let Some(entry) = entries.next_entry().await? {
            let from = entry.path();
            let to = dst_dir.join(entry.file_name());
            if entry.file_type().await?.is_dir() {
                stack.push((from, to));
            } else if at_top && entry.file_name() == "clean" {
                clean = Some((from, to));
            } else {
                tokio::fs::copy(&from, &to).await?;
            }
        }
    }
    if let Some((from, to)) = clean {
        tokio::fs::copy(&from, &to).await?;
    }
    Ok(())
}

/// Top-level entries of a cardano-node database directory. The ChainDB mounts
/// `immutable`, `volatile`, `ledger` and `gsm`; the node adds `lock`, `clean`
/// and `protocolMagicId`; the LSM ledger backend keeps `lsm`.
const DATABASE_ENTRIES: &[&str] = &[
    "immutable",
    "volatile",
    "ledger",
    "gsm",
    "lsm",
    "lock",
    "clean",
    "protocolMagicId",
];

/// Files a desktop file browser leaves in any directory it has displayed.
const OS_METADATA_ENTRIES: &[&str] = &[".DS_Store", "Thumbs.db", "desktop.ini"];

/// The entry in the chain directory that holds a Mithril download until it is
/// installed. It lies inside the directory a link or junction at the chain
/// path resolves to, so it is on the chain's own volume and the install is a
/// rename. It is never part of the database: check_replaceable allows it, the
/// wipe leaves it alone, and the has-chain check does not look at it.
pub(crate) const STAGING_ENTRY: &str = "mithril-staging";

/// Where earlier versions staged a Mithril download: in the state
/// directory, which can be on another volume than the chain.
const LEGACY_STAGING_DIR: &str = "mithril-partial-sync";

/// Free space a full install needs beyond the snapshot's database size, in
/// percent: the safety margin mithril-client adds to a partial download,
/// here also covering the ledger conversion.
const FREE_SPACE_MARGIN_PERCENT: u64 = 10;

/// The directory a full install replaces: `chain_path` itself, or the target
/// of `chain_path` when it is a symlink or junction, so the link is kept.
async fn full_install_target(chain_path: &Path) -> Result<PathBuf> {
    let is_link = tokio::fs::symlink_metadata(chain_path)
        .await
        .is_ok_and(|m| m.file_type().is_symlink());
    if !is_link {
        return Ok(chain_path.to_path_buf());
    }
    let raw_target = tokio::fs::read_link(chain_path)
        .await
        .map_err(|e| anyhow::anyhow!("read chain_path symlink target: {e}"))?;
    Ok(if raw_target.is_absolute() {
        raw_target
    } else {
        chain_path
            .parent()
            .unwrap_or(Path::new("."))
            .join(raw_target)
    })
}

/// A full install deletes the database in the directory it installs into, and
/// that directory can sit inside a folder the user picked. Allow it only when
/// the directory is missing, empty, or holds nothing but cardano-node database
/// entries, file-browser metadata and the Mithril staging entry.
async fn check_replaceable(dir: &Path) -> Result<()> {
    match tokio::fs::metadata(dir).await {
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(e) => anyhow::bail!("Cannot read {}: {e}", dir.display()),
        Ok(m) if !m.is_dir() => anyhow::bail!(
            "{} is a file, not a folder. Daedalus did not change it.",
            dir.display()
        ),
        Ok(_) => {}
    }
    let mut entries = tokio::fs::read_dir(dir).await?;
    while let Some(entry) = entries.next_entry().await? {
        let name = entry.file_name().to_string_lossy().into_owned();
        if !DATABASE_ENTRIES.contains(&name.as_str())
            && !OS_METADATA_ENTRIES.contains(&name.as_str())
            && name != STAGING_ENTRY
        {
            anyhow::bail!(
                "{} contains {name}, which is not part of a Cardano node database. Daedalus did not change the folder. Move its contents elsewhere or choose an empty folder.",
                dir.display()
            );
        }
    }
    Ok(())
}

/// Whether a full install may replace the chain directory. Emits a
/// `CHAIN_DIR_NOT_REPLACEABLE` error when it may not; `wipe_chain` tells the
/// renderer whether that install was a rebuild.
async fn full_install_allowed(chain_path: &Path, wipe_chain: bool) -> bool {
    let checked = match full_install_target(chain_path).await {
        Ok(target) => check_replaceable(&target).await,
        Err(e) => Err(e),
    };
    match checked {
        Ok(()) => true,
        Err(e) => {
            warn!("mithril: refusing to replace the chain directory: {e}");
            emit(&Event::MithrilError {
                code: "CHAIN_DIR_NOT_REPLACEABLE".to_string(),
                message: e.to_string(),
                rebuild: wipe_chain,
            });
            false
        }
    }
}

/// Why a rebuild may not delete the database in `chain_path`, if it may not.
/// The chain directory, and the directory a symlink or junction there points
/// to, must be absolute paths and must not be a filesystem root, as given and
/// as resolved.
async fn check_rebuild_path(chain_path: &Path) -> Result<()> {
    fn check(dir: &Path) -> Result<()> {
        if !dir.is_absolute() || dir.parent().is_none() {
            anyhow::bail!(
                "{} is a relative path or the root of a drive. Daedalus did not change it.",
                dir.display()
            );
        }
        Ok(())
    }
    check(chain_path)?;
    let target = full_install_target(chain_path).await?;
    check(&target)?;
    if let Ok(resolved) = tokio::fs::canonicalize(&target).await {
        check(&resolved)?;
    }
    Ok(())
}

/// Removes a symlink or junction itself, never what it points to. Windows
/// removes a link to a directory as a directory.
async fn remove_link(path: &Path) -> std::io::Result<()> {
    match tokio::fs::remove_file(path).await {
        Ok(()) => Ok(()),
        Err(e) => tokio::fs::remove_dir(path).await.map_err(|_| e),
    }
}

/// Deletes `path`, whatever it is, without following it if it is a link.
/// Returns false when there was nothing to delete.
async fn remove_entry(path: &Path) -> std::io::Result<bool> {
    let meta = match tokio::fs::symlink_metadata(path).await {
        Ok(meta) => meta,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(false),
        Err(e) => return Err(e),
    };
    if meta.file_type().is_symlink() {
        remove_link(path).await?;
    } else if meta.is_dir() {
        tokio::fs::remove_dir_all(path).await?;
    } else {
        tokio::fs::remove_file(path).await?;
    }
    Ok(true)
}

/// Deletes the cardano-node database a rebuild replaces. It lies in the
/// directory `full_install_target` resolves: `chain_path` itself, or the
/// target of a symlink or junction there. That directory and the link are
/// kept, so a custom location is rebuilt where it is.
async fn wipe_database(chain_path: &Path) -> Result<()> {
    let target = full_install_target(chain_path).await?;
    if target == chain_path {
        info!("mithril: deleting the database in {}", target.display());
    } else {
        info!(
            "mithril: deleting the database in {}, the target of {}, which is kept",
            target.display(),
            chain_path.display()
        );
    }
    delete_database(&target).await
}

/// Deletes the cardano-node database entries in `dir`. A link among them is
/// deleted without following it. check_replaceable refuses a directory that
/// holds anything else before this runs, and the file-browser metadata and
/// the staging entry it allows are left in place. `clean` goes first, so a
/// node never trusts what a failed delete leaves, and `immutable` next, so no
/// chunk is left to seed a partial sync.
async fn delete_database(dir: &Path) -> Result<()> {
    let first = ["clean", "immutable"];
    let rest = DATABASE_ENTRIES
        .iter()
        .copied()
        .filter(|name| !first.contains(name));
    for name in first.into_iter().chain(rest) {
        let path = dir.join(name);
        remove_entry(&path)
            .await
            .map_err(|e| anyhow::anyhow!("Could not delete {}: {e}", path.display()))?;
    }
    Ok(())
}

/// Moves every entry of the staged database `staging_db` into `dir`, the
/// `clean` marker last, so an install that stops part-way never leaves a
/// partial database that cardano-node opens without validating it. Staging
/// and `dir` are on one volume, so each move is a rename.
async fn install_entries(staging_db: &Path, dir: &Path) -> Result<()> {
    let mut names = Vec::new();
    let mut entries = tokio::fs::read_dir(staging_db).await?;
    while let Some(entry) = entries.next_entry().await? {
        names.push(entry.file_name());
    }
    names.sort_by_key(|name| name == "clean");
    for name in names {
        let (src, dst) = (staging_db.join(&name), dir.join(&name));
        if tokio::fs::symlink_metadata(&src).await?.is_dir() {
            move_dir(&src, &dst).await?;
        } else {
            move_file(&src, &dst).await?;
        }
    }
    Ok(())
}

/// The size of the files under `path`, read without following links: deleting
/// a link frees nothing on this volume.
fn size_on_disk(path: &Path) -> u64 {
    let Ok(meta) = std::fs::symlink_metadata(path) else {
        return 0;
    };
    if meta.file_type().is_symlink() {
        0
    } else if meta.is_dir() {
        std::fs::read_dir(path)
            .map(|entries| entries.flatten().map(|e| size_on_disk(&e.path())).sum())
            .unwrap_or(0)
    } else {
        meta.len()
    }
}

/// The space a full install into `dir` frees before it downloads: a staging
/// leftover, which is deleted before every download, and for a rebuild the
/// database entries the wipe deletes.
async fn space_freed_before_download(dir: &Path, wipe_chain: bool) -> u64 {
    let mut names = vec![STAGING_ENTRY];
    if wipe_chain {
        names.extend_from_slice(DATABASE_ENTRIES);
    }
    let dir = dir.to_path_buf();
    tokio::task::spawn_blocking(move || names.iter().map(|n| size_on_disk(&dir.join(n))).sum())
        .await
        .unwrap_or(0)
}

/// Bytes a full install of a snapshot whose database is `db_size` bytes needs
/// free on the chain's volume.
fn required_space(db_size: u64) -> u64 {
    db_size.saturating_add(db_size / 100 * FREE_SPACE_MARGIN_PERCENT)
}

/// Bytes shown as gigabytes, for messages.
fn gigabytes(bytes: u64) -> String {
    format!("{:.1} GB", bytes as f64 / 1e9)
}

/// Whether the chain's volume has room for a full install into `dir` of a
/// snapshot whose database is `db_size` bytes, counting what is deleted before
/// the download. Emits an `INSUFFICIENT_SPACE` error when it has not. When the
/// size or the free space cannot be read, the install goes ahead, as before.
async fn space_allowed(dir: &Path, db_size: Option<u64>, wipe_chain: bool) -> bool {
    let Some(db_size) = db_size else {
        info!("mithril: the snapshot does not state its size; free space is not checked");
        return true;
    };
    // The directory may not exist yet; its nearest existing ancestor is on
    // the volume it will be created on.
    let mut existing = dir.to_path_buf();
    while !tokio::fs::try_exists(&existing).await.unwrap_or(false) {
        match existing.parent() {
            Some(parent) => existing = parent.to_path_buf(),
            None => break,
        }
    }
    let Some(free) = chain_validation::read_free_space(existing).await else {
        warn!(
            "mithril: could not read the free space for {}; it is not checked",
            dir.display()
        );
        return true;
    };
    let freed = space_freed_before_download(dir, wipe_chain).await;
    let required = required_space(db_size);
    let available = free.saturating_add(freed);
    info!(
        "mithril: {} free on the volume of {}, {} more once old data is deleted; the install needs {}",
        gigabytes(free),
        dir.display(),
        gigabytes(freed),
        gigabytes(required)
    );
    if available >= required {
        return true;
    }
    let message = format!(
        "The blockchain data needs {} of free space on the drive that holds {}, and {} is available{}. Daedalus did not delete or download anything. Free up space on that drive, or choose another location, and try again.",
        gigabytes(required),
        dir.display(),
        gigabytes(available),
        if wipe_chain {
            ", counting the blockchain data the rebuild would delete"
        } else {
            ""
        }
    );
    warn!("mithril: refusing to install: {message}");
    emit(&Event::MithrilError {
        code: "INSUFFICIENT_SPACE".to_string(),
        message,
        rebuild: wipe_chain,
    });
    false
}

/// Deletes what an interrupted Mithril run left behind: the staging entry in
/// the chain directory, and the staging directory earlier versions kept
/// in the state directory. Called at startup, when no pipeline of this
/// Daedalus runs. The staging entry is kept when the chain directory, or the
/// database staged in it, belongs to another network: every network keeps
/// its database in the chain directory of the storage folder, so the
/// Daedalus of that network may be downloading into it now.
pub async fn remove_stale_staging(cfg: &MithrilConfig) {
    let mut leftovers = vec![Path::new(&cfg.state_dir).join(LEGACY_STAGING_DIR)];
    if let Ok(target) = full_install_target(Path::new(&cfg.chain_path)).await {
        if target.is_absolute() {
            let staging = target.join(STAGING_ENTRY);
            let staged_db = staging.join("download").join("db");
            let other = match state::other_network_magic(&target, cfg.network_magic).await {
                Some(found) => Some(found),
                None => state::other_network_magic(&staged_db, cfg.network_magic).await,
            };
            match other {
                Some(found) => info!(
                    "mithril: {} belongs to network magic {found}, not this network's; it is left as it is",
                    staging.display()
                ),
                None => leftovers.push(staging),
            }
        }
    }
    for path in leftovers {
        match remove_entry(&path).await {
            Ok(true) => info!(
                "mithril: deleted {}, left by an interrupted sync",
                path.display()
            ),
            Ok(false) => {}
            Err(e) => warn!("mithril: could not delete {}: {e}", path.display()),
        }
    }
}

/// Whether Mithril may change the chain directory at all. Never when it holds
/// another network's database: a partial sync would rewrite its chunks and
/// ledger, and a full install or a wipe would delete it, although it holds
/// nothing but database entries. Emits a `CHAIN_DIR_OTHER_NETWORK` error when
/// it may not; `wipe_chain` tells the renderer whether the sync was a rebuild.
async fn network_allowed(chain_path: &Path, network_magic: Option<u32>, wipe_chain: bool) -> bool {
    let (Some(expected), Some(found)) = (
        network_magic,
        state::other_network_magic(chain_path, network_magic).await,
    ) else {
        return true;
    };
    let message = state::other_network_message(chain_path, found, expected);
    warn!("mithril: refusing to change the chain directory: {message}");
    emit(&Event::MithrilError {
        code: "CHAIN_DIR_OTHER_NETWORK".to_string(),
        message,
        rebuild: wipe_chain,
    });
    false
}

// For bootstrap (local_highest = None): replace the entire chain directory.
// For partial sync (local_highest = Some): delete the clean marker, merge new
// immutables in, replace ledger/lsm, and move the new clean marker in last.
async fn install_staged(staging_db: &Path, chain_path: &Path, is_partial: bool) -> Result<()> {
    if !is_partial {
        // If chain_path is a symlink or junction, install inside the real
        // target so the link entry point is preserved. The directory itself
        // is kept: it holds the staging entry, and any file-browser metadata
        // check_replaceable allows.
        let target = full_install_target(chain_path).await?;
        check_replaceable(&target).await?;
        delete_database(&target).await?;
        tokio::fs::create_dir_all(&target).await?;
        install_entries(staging_db, &target)
            .await
            .map_err(|e| anyhow::anyhow!("install staged db to {}: {e}", target.display()))?;
        return Ok(());
    }

    // Nothing in the database changes while it is marked clean: cardano-node
    // opens a clean database without validating its chunks, so an install
    // that stops part-way must not leave the old marker on a mixed database.
    let dst_clean = chain_path.join("clean");
    match tokio::fs::remove_file(&dst_clean).await {
        Ok(()) => {}
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(e) => anyhow::bail!("delete {}: {e}", dst_clean.display()),
    }

    // Partial sync: move new immutable files into the existing chain directory.
    // A staged file replaces the local file of the same name, which is how the
    // incomplete highest local chunk is swapped for the certified one.
    let src_immutable = staging_db.join("immutable");
    let dst_immutable = chain_path.join("immutable");
    let mut highest_staged: Option<u64> = None;
    let mut entries = tokio::fs::read_dir(&src_immutable).await?;
    while let Some(entry) = entries.next_entry().await? {
        if let Some(n) = immutable_file_number(&entry.file_name().to_string_lossy()) {
            highest_staged = Some(highest_staged.unwrap_or(0).max(n));
        }
        let src = entry.path();
        let dst = dst_immutable.join(entry.file_name());
        move_file(&src, &dst).await?;
    }

    // The highest staged chunk comes from the ancillary archive and is not
    // finalized, so it can be shorter than a local chunk with the same number.
    // Any local chunk after it would then follow a gap; remove those so the
    // ImmutableDB ends at the installed tip and the node syncs the rest.
    if let Some(highest_staged) = highest_staged {
        let mut entries = tokio::fs::read_dir(&dst_immutable).await?;
        while let Some(entry) = entries.next_entry().await? {
            let above_tip = immutable_file_number(&entry.file_name().to_string_lossy())
                .is_some_and(|n| n > highest_staged);
            if above_tip {
                tokio::fs::remove_file(entry.path()).await?;
            }
        }
    }

    // Replace ledger directory.
    let src_ledger = staging_db.join("ledger");
    let dst_ledger = chain_path.join("ledger");
    let _ = tokio::fs::remove_dir_all(&dst_ledger).await;
    move_dir(&src_ledger, &dst_ledger).await?;

    // Always remove the old lsm — it belongs to the old ledger and is
    // stale the moment we install a new one.  Install the new lsm if the
    // converter produced one; a missing lsm is safer than a mismatched one.
    let dst_lsm = chain_path.join("lsm");
    let _ = tokio::fs::remove_dir_all(&dst_lsm).await;
    let src_lsm = staging_db.join("lsm");
    if tokio::fs::metadata(&src_lsm).await.is_ok() {
        move_dir(&src_lsm, &dst_lsm).await?;
    }

    // Install the clean sentinel, last, so cardano-node skips chunk
    // validation on next startup. Without this, node treats the partial-sync
    // state as an unclean shutdown and re-validates every immutable chunk.
    let src_clean = staging_db.join("clean");
    move_file(&src_clean, &dst_clean).await?;

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

/// Reads `reader` to end of stream and returns its last `cap` bytes.
async fn read_tail(mut reader: impl tokio::io::AsyncRead + Unpin, cap: usize) -> Vec<u8> {
    use tokio::io::AsyncReadExt;
    let mut tail = Vec::new();
    let mut chunk = [0u8; 8192];
    loop {
        match reader.read(&mut chunk).await {
            Ok(0) | Err(_) => break,
            Ok(n) => {
                tail.extend_from_slice(&chunk[..n]);
                if tail.len() > cap {
                    tail.drain(..tail.len() - cap);
                }
            }
        }
    }
    tail
}

/// Wait for `proc` to exit, honouring cancel/stop commands from `cmd_rx`.
/// Reads its piped stdout and stderr while it runs and logs the last
/// SUBPROCESS_OUTPUT_TAIL bytes of each after it exits. A pipe that nobody
/// reads fills up (64 KiB on Windows) and blocks the child's next write, so
/// the child would never exit.
async fn run_subprocess(
    label: &str,
    proc: &mut tokio::process::Child,
    cmd_rx: &mut mpsc::Receiver<Cmd>,
) -> ProcResult {
    let stdout = proc
        .stdout
        .take()
        .map(|s| tokio::spawn(read_tail(s, SUBPROCESS_OUTPUT_TAIL)));
    let stderr = proc
        .stderr
        .take()
        .map(|s| tokio::spawn(read_tail(s, SUBPROCESS_OUTPUT_TAIL)));
    let result = loop {
        tokio::select! {
            result = proc.wait() => {
                break match result {
                    Ok(s) if s.success() => ProcResult::Success,
                    Ok(s) => ProcResult::Failed(format!("{label} exited with {s}")),
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
    for (stream, reader) in [("stdout", stdout), ("stderr", stderr)] {
        let Some(reader) = reader else { continue };
        let Ok(tail) = reader.await else { continue };
        let text = String::from_utf8_lossy(&tail);
        let trimmed = text.trim();
        if !trimmed.is_empty() {
            info!("mithril: {label} {stream}:\n{trimmed}");
        }
    }
    result
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
    let lsm_export_dir = staging_db.join("lsm-export");
    let lsm_database = staging_db.join("lsm");

    if let Err(e) = tokio::fs::rename(&input_mem, &temp_input).await {
        return ProcResult::Failed(e.to_string());
    }

    let _ = tokio::fs::remove_dir_all(&lsm_export_dir).await;
    let _ = tokio::fs::remove_dir_all(&lsm_database).await;

    // The paths as the converter is given them; see for_external_program.
    let snapshot_in_arg = for_external_program(&temp_input);
    let snapshot_out_arg = for_external_program(&output_lsm_snapshot);
    let lsm_export_arg = for_external_program(&lsm_export_dir);
    let lsm_database_arg = for_external_program(&lsm_database);
    let config_arg = for_external_program(Path::new(&cfg.converter_config));

    // Step 1: convert Mem snapshot → LSM snapshot + export tables
    info!(
        "mithril: {} convert --snapshot-in {} --snapshot-out {} --lsm-export-to {} --config {}",
        cfg.snapshot_converter_bin,
        snapshot_in_arg.display(),
        snapshot_out_arg.display(),
        lsm_export_arg.display(),
        config_arg.display(),
    );

    let mut convert_cmd = Command::new(&cfg.snapshot_converter_bin);
    convert_cmd
        .arg("convert")
        .arg("--snapshot-in")
        .arg(&snapshot_in_arg)
        .arg("--snapshot-out")
        .arg(&snapshot_out_arg)
        .arg("--lsm-export-to")
        .arg(&lsm_export_arg)
        .arg("--config")
        .arg(&config_arg)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .kill_on_drop(true);
    tether_to_watchdog(&mut convert_cmd);

    let mut proc = match convert_cmd.spawn() {
        Ok(p) => p,
        Err(e) => {
            let _ = tokio::fs::remove_dir_all(&temp_input).await;
            return ProcResult::Failed(e.to_string());
        }
    };
    let convert_result = run_subprocess("snapshot-converter convert", &mut proc, cmd_rx).await;
    let _ = tokio::fs::remove_dir_all(&temp_input).await;

    match convert_result {
        ProcResult::Success => {}
        other => return other,
    }

    // Step 2: import exported tables into a fresh LSM database
    info!(
        "mithril: {} lsm import --lsm-database {} --lsm-import-from {} --snapshot {}",
        cfg.snapshot_converter_bin,
        lsm_database_arg.display(),
        lsm_export_arg.display(),
        slot_str,
    );

    let mut import_cmd = Command::new(&cfg.snapshot_converter_bin);
    import_cmd
        .arg("lsm")
        .arg("import")
        .arg("--lsm-database")
        .arg(&lsm_database_arg)
        .arg("--lsm-import-from")
        .arg(&lsm_export_arg)
        .arg("--snapshot")
        .arg(&slot_str)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .kill_on_drop(true);
    tether_to_watchdog(&mut import_cmd);

    let mut proc = match import_cmd.spawn() {
        Ok(p) => p,
        Err(e) => return ProcResult::Failed(e.to_string()),
    };
    let import_result = run_subprocess("snapshot-converter lsm import", &mut proc, cmd_rx).await;
    let _ = tokio::fs::remove_dir_all(&lsm_export_dir).await;

    import_result
}

pub async fn run_pipeline(
    cfg: &MithrilConfig,
    cmd_rx: &mut mpsc::Receiver<Cmd>,
    force: bool,
    wipe_chain: bool,
) -> PipelineResult {
    // Until a rebuild that began deleting this chain directory, or an install
    // that began changing its database, has installed a database, every sync
    // of it is a rebuild: a partial sync would build on what was left.
    let pending = pending_record(cfg).await;
    let wipe_chain = if !wipe_chain && pending.is_some() {
        info!(
            "mithril: a rebuild or install of {} is pending; this sync rebuilds it",
            cfg.chain_path
        );
        true
    } else {
        wipe_chain
    };

    // The pipeline downloads with --include-ancillary (the converter needs the
    // ledger state it brings), and mithril-client refuses that flag without an
    // ANCILLARY_VERIFICATION_KEY — or, on older versions, skips signature
    // verification of the ancillary data. Fail fast, before any chain wipe,
    // instead of failing mid-download or downloading unverified data.
    if cfg.ancillary_vkey.is_none() {
        emit(&Event::MithrilError {
            code: "ANCILLARY_VKEY_MISSING".to_string(),
            message: "Mithril is configured without an ancillary verification key".to_string(),
            rebuild: wipe_chain,
        });
        return PipelineResult::Cancelled;
    }

    // Nothing is wiped from, downloaded for or installed over another
    // network's database.
    let chain_path = PathBuf::from(&cfg.chain_path);
    if !network_allowed(&chain_path, cfg.network_magic, wipe_chain).await {
        return PipelineResult::Cancelled;
    }

    // A rebuild (wipe_chain) deletes the database in the chain directory and
    // installs a full bootstrap in its place. The path is checked here; nothing
    // is deleted until the aggregator has answered the probe below.
    if wipe_chain {
        if let Err(e) = check_rebuild_path(&chain_path).await {
            warn!("mithril: refusing to rebuild the chain: {e}");
            emit(&Event::MithrilError {
                code: "INVALID_CHAIN_PATH".to_string(),
                message: e.to_string(),
                rebuild: wipe_chain,
            });
            return PipelineResult::Cancelled;
        }
    }

    info!("mithril: preparing");
    emit(&Event::MithrilStatus {
        phase: "preparing".to_string(),
        rebuild: wipe_chain,
    });

    // 1. Behind-ness probe. It runs before a rebuild deletes anything, so an
    //    aggregator that does not answer leaves the chain as it is.
    let (probed_local, certified, snapshot_size) = match probe_snapshot(cfg).await {
        Ok(r) => r,
        Err(e) => {
            warn!("mithril probe failed: {e}");
            emit(&Event::MithrilError {
                code: "PROBE_FAILED".to_string(),
                message: e.to_string(),
                rebuild: wipe_chain,
            });
            return PipelineResult::Cancelled;
        }
    };

    // A rebuild replaces the local chain with a full bootstrap, so its chunks
    // neither decide whether a sync is needed nor narrow the download.
    let local_highest = if wipe_chain { None } else { probed_local };
    let local_num = local_highest.unwrap_or(0);
    let behind = certified.saturating_sub(local_num);
    if !force && !wipe_chain && behind < cfg.behind_threshold {
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

    if wipe_chain {
        info!(
            "Mithril rebuild requested: local={}, certified={certified}; the local chain is replaced by a full bootstrap",
            probed_local.unwrap_or(0)
        );
    } else if force {
        info!(
            "Mithril forced by caller: local={local_num}, certified={certified}; skipping not-needed check"
        );
    } else {
        info!(
            "Mithril needed: local={local_num}, certified={certified}, behind={behind}; starting download"
        );
    }

    // A full install replaces the chain directory. Refuse before downloading
    // anything if that would delete files that are not a node database.
    let is_partial = local_highest.is_some();
    if !is_partial && !full_install_allowed(&chain_path, wipe_chain).await {
        return PipelineResult::Cancelled;
    }

    // The download is staged inside the directory the database lives in, on
    // its volume, so installing it is a rename and the volume that must hold
    // the database is the one that holds the download.
    let target = match full_install_target(&chain_path).await {
        Ok(target) => target,
        Err(e) => {
            warn!("mithril: cannot resolve the chain directory: {e}");
            emit(&Event::MithrilError {
                code: "STAGING_FAILED".to_string(),
                message: e.to_string(),
                rebuild: wipe_chain,
            });
            return PipelineResult::Cancelled;
        }
    };

    // A full install needs room for the whole database. Refuse before a
    // rebuild deletes anything when the volume cannot hold it.
    if !is_partial && !space_allowed(&target, snapshot_size, wipe_chain).await {
        return PipelineResult::Cancelled;
    }

    // The aggregator has answered and the directory may be replaced: delete
    // the old chain now, before the download, so the disk never has to hold
    // two full databases. The marker goes first, so a restart after a delete
    // or download that did not finish still knows the chain is being rebuilt.
    // A rebuild that continues an unfinished install keeps that install's
    // record kind: what it deletes is what the install left.
    if wipe_chain {
        let kind = pending.unwrap_or(RecordKind::Rebuild);
        if let Err(e) = write_rebuild_marker(cfg, kind).await {
            let message = format!("Could not record the rebuild in {}: {e}", cfg.state_dir);
            warn!("mithril: {message}");
            emit(&Event::MithrilError {
                code: "REBUILD_MARKER_FAILED".to_string(),
                message,
                rebuild: wipe_chain,
            });
            return PipelineResult::Cancelled;
        }
        if let Err(e) = wipe_database(&chain_path).await {
            warn!("mithril: could not delete the old chain: {e}");
            emit(&Event::MithrilError {
                code: "WIPE_FAILED".to_string(),
                message: e.to_string(),
                rebuild: wipe_chain,
            });
            return PipelineResult::FailedAfterWipe;
        }
    }

    // From here on a rebuild has deleted the old chain. Its failures and
    // cancels must not end in a node started on the empty directory.
    let failed = || {
        if wipe_chain {
            PipelineResult::FailedAfterWipe
        } else {
            PipelineResult::Cancelled
        }
    };
    let cancelled_by_user = || {
        if wipe_chain {
            PipelineResult::FailedAfterWipe
        } else {
            PipelineResult::UserCancelled
        }
    };

    // 2. Prepare staging
    let staging_root = target.join(STAGING_ENTRY);
    // mithril-client creates a `db/` subdirectory inside --download-dir, so we
    // pass `download_dir` to the client and then work with `download_dir/db`.
    let download_dir = staging_root.join("download");
    let staging_db = download_dir.join("db");
    let _ = remove_entry(&staging_root).await;
    if let Err(e) = tokio::fs::create_dir_all(&download_dir).await {
        warn!("mithril: failed to create staging directory: {e}");
        emit(&Event::MithrilError {
            code: "STAGING_FAILED".to_string(),
            message: e.to_string(),
            rebuild: wipe_chain,
        });
        return failed();
    }

    // 3. Download — "downloading" status is emitted by run_download on first real progress event.
    match run_download(
        cfg,
        &download_dir,
        cmd_rx,
        local_highest,
        certified,
        wipe_chain,
    )
    .await
    {
        ProcResult::Success => {}
        ProcResult::Failed(msg) => {
            let _ = tokio::fs::remove_dir_all(&staging_root).await;
            warn!("mithril download failed: {msg}");
            emit(&Event::MithrilError {
                code: "PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED".to_string(),
                message: msg,
                rebuild: wipe_chain,
            });
            return failed();
        }
        ProcResult::Cancelled => {
            let _ = tokio::fs::remove_dir_all(&staging_root).await;
            info!("mithril: cancelled by user");
            emit(&Event::MithrilStatus {
                phase: "cancelled".to_string(),
                rebuild: wipe_chain,
            });
            return cancelled_by_user();
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
            rebuild: wipe_chain,
        });
        return failed();
    }

    // 5. Convert
    info!("mithril: converting");
    emit(&Event::MithrilStatus {
        phase: "converting".to_string(),
        rebuild: wipe_chain,
    });
    match run_converter(cfg, &staging_db, cmd_rx).await {
        ProcResult::Success => {}
        ProcResult::Failed(msg) => {
            let _ = tokio::fs::remove_dir_all(&staging_root).await;
            warn!("mithril conversion failed: {msg}");
            emit(&Event::MithrilError {
                code: "PARTIAL_SYNC_CONVERSION_FAILED".to_string(),
                message: msg,
                rebuild: wipe_chain,
            });
            return failed();
        }
        ProcResult::Cancelled => {
            let _ = tokio::fs::remove_dir_all(&staging_root).await;
            info!("mithril: cancelled by user (post-convert)");
            emit(&Event::MithrilStatus {
                phase: "cancelled".to_string(),
                rebuild: wipe_chain,
            });
            return cancelled_by_user();
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
                    rebuild: wipe_chain,
                });
                cancelled_by_user()
            }
            ProcResult::Stopped => PipelineResult::Stopped,
            _ => unreachable!(),
        };
    }

    // 6. Install
    info!("mithril: installing");
    emit(&Event::MithrilStatus {
        phase: "installing".to_string(),
        rebuild: wipe_chain,
    });

    // The download takes minutes; check again before anything is changed.
    if !network_allowed(&chain_path, cfg.network_magic, wipe_chain).await
        || (!is_partial && !full_install_allowed(&chain_path, wipe_chain).await)
    {
        let _ = tokio::fs::remove_dir_all(&staging_root).await;
        return failed();
    }

    // Write cutover-in-progress marker before rename
    if let Err(e) = write_marker(&cfg.state_dir, "cutover-in-progress").await {
        let _ = tokio::fs::remove_dir_all(&staging_root).await;
        warn!("mithril: failed to write cutover-in-progress marker: {e}");
        emit(&Event::MithrilError {
            code: "MARKER_FAILED".to_string(),
            message: e.to_string(),
            rebuild: wipe_chain,
        });
        return failed();
    }

    // A full install deletes the database in the chain directory and moves
    // the download in entry by entry, the clean marker last. A partial sync
    // deletes the clean marker, replaces chunks, the ledger and the LSM
    // tables, and moves the new clean marker in last. A rebuild recorded
    // itself before it deleted the old chain; any other install, a first
    // bootstrap and a partial sync included, records itself here. An install
    // that fails or is cut short by a crash then leaves the record, and the
    // next start reports no chain and installs again instead of starting the
    // node on part of a database.
    if !wipe_chain {
        if let Err(e) = write_rebuild_marker(cfg, RecordKind::Install).await {
            let _ = tokio::fs::remove_dir_all(&staging_root).await;
            let message = format!("Could not record the install in {}: {e}", cfg.state_dir);
            warn!("mithril: {message}");
            emit(&Event::MithrilError {
                code: "MARKER_FAILED".to_string(),
                message,
                rebuild: wipe_chain,
            });
            return failed();
        }
    }

    if let Err(e) = install_staged(&staging_db, &chain_path, is_partial).await {
        // The cutover-in-progress marker is already written and chain may be
        // partially modified. Retain staging material for potential recovery;
        // return Failed so the supervisor does not restart the node against
        // a partially installed chain.
        warn!("mithril install failed (chain may be partially modified): {e}");
        emit(&Event::MithrilError {
            code: "INSTALL_FAILED".to_string(),
            message: e.to_string(),
            rebuild: wipe_chain,
        });
        return PipelineResult::Failed;
    }

    // Clean up staging root (download dir and any remnants)
    let _ = tokio::fs::remove_dir_all(&staging_root).await;

    // A database is installed: a rebuild of this directory is over.
    clear_rebuild_marker(cfg).await;

    // Write installed-awaiting-node-start marker
    if let Err(e) = write_marker(&cfg.state_dir, "installed-awaiting-node-start").await {
        warn!("Failed to write installed-awaiting-node-start marker: {e}");
    }

    info!("mithril: installed successfully");
    emit(&Event::MithrilStatus {
        phase: "finalizing".to_string(),
        rebuild: wipe_chain,
    });

    PipelineResult::Installed {
        rebuild: wipe_chain,
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    mod rebuild_marker {
        use super::super::{
            REBUILD_MARKER, REBUILD_MARKER_TEMP, RecordKind, clear_rebuild_marker, pending_record,
            same_chain_path, write_rebuild_marker,
        };
        use crate::config::MithrilConfig;
        use std::fs;
        use std::path::PathBuf;

        struct TempDir(PathBuf);

        impl TempDir {
            fn new(label: &str) -> Self {
                let p = std::env::temp_dir().join(format!(
                    "wdg-marker-{label}-{}-{}",
                    std::process::id(),
                    std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap()
                        .subsec_nanos()
                ));
                fs::create_dir_all(&p).unwrap();
                TempDir(p)
            }
        }

        impl Drop for TempDir {
            fn drop(&mut self) {
                let _ = fs::remove_dir_all(&self.0);
            }
        }

        fn config(state_dir: &std::path::Path, chain: &str) -> MithrilConfig {
            serde_json::from_value(serde_json::json!({
                "mithril_bin": "m",
                "snapshot_converter_bin": "c",
                "converter_config": "cfg",
                "aggregator_url": "u",
                "genesis_vkey": "g",
                "state_dir": state_dir.to_str().unwrap(),
                "chain_path": chain,
            }))
            .unwrap()
        }

        #[tokio::test]
        async fn a_marker_for_this_chain_is_pending_until_cleared() {
            let t = TempDir::new("this");
            let cfg = config(&t.0, "/data/chain");
            assert_eq!(pending_record(&cfg).await, None);
            write_rebuild_marker(&cfg, RecordKind::Rebuild)
                .await
                .unwrap();
            assert_eq!(pending_record(&cfg).await, Some(RecordKind::Rebuild));
            clear_rebuild_marker(&cfg).await;
            assert_eq!(pending_record(&cfg).await, None);
            assert!(!t.0.join(REBUILD_MARKER).exists());
        }

        #[tokio::test]
        async fn a_marker_says_whether_it_is_for_a_rebuild_or_an_install() {
            let t = TempDir::new("kind");
            let cfg = config(&t.0, "/data/chain");
            write_rebuild_marker(&cfg, RecordKind::Install)
                .await
                .unwrap();
            let text = fs::read_to_string(t.0.join(REBUILD_MARKER)).unwrap();
            let marker: serde_json::Value = serde_json::from_str(&text).unwrap();
            assert_eq!(marker["kind"], "install");
            assert_eq!(pending_record(&cfg).await, Some(RecordKind::Install));
            write_rebuild_marker(&cfg, RecordKind::Rebuild)
                .await
                .unwrap();
            assert_eq!(pending_record(&cfg).await, Some(RecordKind::Rebuild));
        }

        #[tokio::test]
        async fn a_marker_without_a_kind_is_a_rebuilds() {
            let t = TempDir::new("no-kind");
            fs::write(t.0.join(REBUILD_MARKER), br#"{"chain_path":"/data/chain"}"#).unwrap();
            assert_eq!(
                pending_record(&config(&t.0, "/data/chain")).await,
                Some(RecordKind::Rebuild)
            );
        }

        #[tokio::test]
        async fn a_marker_for_another_chain_is_not_pending_and_is_kept() {
            let t = TempDir::new("other");
            write_rebuild_marker(&config(&t.0, "/data/chain"), RecordKind::Rebuild)
                .await
                .unwrap();
            let cfg = config(&t.0, "/elsewhere/chain");
            assert_eq!(pending_record(&cfg).await, None);
            clear_rebuild_marker(&cfg).await;
            assert!(t.0.join(REBUILD_MARKER).exists());
        }

        #[tokio::test]
        async fn a_marker_names_its_chain_with_or_without_a_trailing_separator() {
            let t = TempDir::new("separator");
            write_rebuild_marker(&config(&t.0, "/data/chain/"), RecordKind::Rebuild)
                .await
                .unwrap();
            assert!(pending_record(&config(&t.0, "/data/chain")).await.is_some());
            write_rebuild_marker(&config(&t.0, "/data/chain"), RecordKind::Rebuild)
                .await
                .unwrap();
            assert!(
                pending_record(&config(&t.0, "/data/chain//"))
                    .await
                    .is_some()
            );
            assert!(
                pending_record(&config(&t.0, "/data/chain-2"))
                    .await
                    .is_none()
            );
        }

        #[test]
        fn chain_paths_compare_as_the_platform_compares_them() {
            assert!(same_chain_path("/data/chain", "/data/chain/"));
            assert!(!same_chain_path("/data/chain", "/data"));
            assert!(same_chain_path("/", "/"));
            // Windows paths do not depend on case; other platforms' do.
            assert_eq!(same_chain_path("/Data/Chain", "/data/chain"), cfg!(windows));
        }

        #[cfg(windows)]
        #[test]
        fn windows_chain_paths_ignore_case_and_a_trailing_backslash() {
            assert!(same_chain_path(r"D:\Daedalus\Chain\", r"d:\daedalus\chain"));
        }

        #[tokio::test]
        async fn the_marker_is_renamed_into_place_whole() {
            let t = TempDir::new("atomic");
            let cfg = config(&t.0, "/data/chain");
            // A temporary file left by an earlier write that did not finish.
            fs::write(t.0.join(REBUILD_MARKER_TEMP), b"{\"chain_pa").unwrap();
            write_rebuild_marker(&cfg, RecordKind::Rebuild)
                .await
                .unwrap();
            assert!(!t.0.join(REBUILD_MARKER_TEMP).exists());
            let text = fs::read_to_string(t.0.join(REBUILD_MARKER)).unwrap();
            let marker: serde_json::Value = serde_json::from_str(&text).unwrap();
            assert_eq!(marker["chain_path"], "/data/chain");
        }

        #[tokio::test]
        async fn a_corrupt_marker_is_pending_while_the_chain_has_no_clean_marker() {
            let t = TempDir::new("garbled");
            let chain = t.0.join("chain");
            fs::create_dir_all(chain.join("immutable")).unwrap();
            fs::write(t.0.join(REBUILD_MARKER), b"{\"chain_pa").unwrap();
            assert_eq!(
                pending_record(&config(&t.0, chain.to_str().unwrap())).await,
                Some(RecordKind::Rebuild)
            );
            assert!(t.0.join(REBUILD_MARKER).exists());
        }

        #[tokio::test]
        async fn an_empty_marker_beside_a_clean_chain_is_removed() {
            let t = TempDir::new("empty-clean");
            let chain = t.0.join("chain");
            fs::create_dir_all(chain.join("immutable")).unwrap();
            fs::write(chain.join("clean"), b"").unwrap();
            fs::write(t.0.join(REBUILD_MARKER), b"").unwrap();
            assert_eq!(
                pending_record(&config(&t.0, chain.to_str().unwrap())).await,
                None
            );
            assert!(!t.0.join(REBUILD_MARKER).exists());
        }
    }

    mod external_paths {
        use super::super::{for_external_program, plain_drive_path};
        use std::path::Path;

        #[test]
        fn a_verbatim_drive_path_loses_its_prefix() {
            assert_eq!(
                plain_drive_path(r"\\?\D:\Daedalus\chain\mithril-staging"),
                Some(r"D:\Daedalus\chain\mithril-staging")
            );
            assert_eq!(plain_drive_path(r"\\?\c:\"), Some(r"c:\"));
        }

        #[test]
        fn other_paths_are_not_drive_paths() {
            for path in [
                r"\\?\UNC\server\share\chain",
                r"\\?\Volume{0b1c2d3e-0000-0000-0000-100000000000}\chain",
                r"\\?\D:",
                r"D:\Daedalus\chain",
                r"\\server\share\chain",
                "/data/chain",
            ] {
                assert_eq!(plain_drive_path(path), None, "{path}");
            }
        }

        #[test]
        fn programs_get_the_plain_form_on_windows_only() {
            let verbatim = Path::new(r"\\?\D:\chain");
            let expected = if cfg!(windows) {
                Path::new(r"D:\chain")
            } else {
                verbatim
            };
            assert_eq!(for_external_program(verbatim), expected);
            assert_eq!(
                for_external_program(Path::new("/data/chain")),
                Path::new("/data/chain")
            );
        }
    }

    mod download_range {
        use super::super::download_range;

        #[test]
        fn starts_at_the_highest_local_chunk_when_behind() {
            assert_eq!(download_range(Some(9170), 9172), Some((9170, 9172)));
        }

        #[test]
        fn starts_at_chunk_zero_when_only_chunk_zero_exists() {
            assert_eq!(download_range(Some(0), 5), Some((0, 5)));
        }

        #[test]
        fn downloads_only_the_certified_chunk_when_level() {
            assert_eq!(download_range(Some(9172), 9172), Some((9172, 9172)));
        }

        #[test]
        fn downloads_only_the_certified_chunk_when_ahead() {
            assert_eq!(download_range(Some(9180), 9172), Some((9172, 9172)));
        }

        #[test]
        fn downloads_everything_without_local_chunks() {
            assert_eq!(download_range(None, 9172), None);
        }
    }

    mod partial_install {
        use super::super::install_staged;
        use std::fs;
        use std::path::{Path, PathBuf};

        struct TempDir(PathBuf);

        impl TempDir {
            fn new(label: &str) -> Self {
                let p = std::env::temp_dir().join(format!(
                    "wdg-partial-{}-{}-{}",
                    label,
                    std::process::id(),
                    std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap()
                        .subsec_nanos()
                ));
                fs::create_dir_all(&p).unwrap();
                TempDir(p)
            }
        }

        impl Drop for TempDir {
            fn drop(&mut self) {
                let _ = fs::remove_dir_all(&self.0);
            }
        }

        const TRIO: [&str; 3] = ["chunk", "primary", "secondary"];

        fn write_trio(immutable: &Path, n: u64, content: &str) {
            fs::create_dir_all(immutable).unwrap();
            for ext in TRIO {
                fs::write(immutable.join(format!("{n:05}.{ext}")), content).unwrap();
            }
        }

        fn read(immutable: &Path, n: u64, ext: &str) -> Option<String> {
            fs::read_to_string(immutable.join(format!("{n:05}.{ext}"))).ok()
        }

        /// A staged DB as mithril-client lays it out for `--start first --end
        /// certified --include-ancillary`: certified chunks first..=certified
        /// plus the unfinalized chunk certified + 1 from the ancillary archive.
        fn make_staging(root: &Path, first: u64, certified: u64) -> PathBuf {
            let staging = root.join("staging");
            let immutable = staging.join("immutable");
            for n in first..=certified {
                write_trio(&immutable, n, &format!("certified {n}"));
            }
            write_trio(&immutable, certified + 1, "ancillary");
            fs::create_dir_all(staging.join("ledger").join("12345")).unwrap();
            fs::write(staging.join("clean"), b"").unwrap();
            staging
        }

        #[tokio::test]
        async fn replaces_the_incomplete_highest_local_chunk() {
            let root = TempDir::new("behind");
            let chain = root.0.join("chain");
            let immutable = chain.join("immutable");
            for n in 0..5 {
                write_trio(&immutable, n, &format!("local {n}"));
            }
            write_trio(&immutable, 5, "local 5 prefix");

            let staging = make_staging(&root.0, 5, 8);
            install_staged(&staging, &chain, true).await.unwrap();

            for ext in TRIO {
                assert_eq!(read(&immutable, 4, ext).as_deref(), Some("local 4"));
                assert_eq!(read(&immutable, 5, ext).as_deref(), Some("certified 5"));
                assert_eq!(read(&immutable, 8, ext).as_deref(), Some("certified 8"));
                assert_eq!(read(&immutable, 9, ext).as_deref(), Some("ancillary"));
            }
        }

        #[cfg(unix)]
        #[tokio::test]
        async fn deletes_the_clean_marker_before_it_changes_anything() {
            use std::os::unix::fs::PermissionsExt;
            let root = TempDir::new("clean-first");
            let chain = root.0.join("chain");
            let immutable = chain.join("immutable");
            for n in 0..5 {
                write_trio(&immutable, n, &format!("local {n}"));
            }
            fs::create_dir_all(chain.join("ledger").join("999")).unwrap();
            fs::write(chain.join("clean"), b"").unwrap();
            let staging = make_staging(&root.0, 4, 8);
            // The first change, moving a chunk in, cannot be made.
            fs::set_permissions(&immutable, fs::Permissions::from_mode(0o555)).unwrap();
            if fs::File::create(immutable.join("probe")).is_ok() {
                fs::set_permissions(&immutable, fs::Permissions::from_mode(0o755)).unwrap();
                eprintln!("skipped: directory permissions are not enforced for this user");
                return;
            }

            let installed = install_staged(&staging, &chain, true).await;
            fs::set_permissions(&immutable, fs::Permissions::from_mode(0o755)).unwrap();

            assert!(installed.is_err());
            assert!(
                !chain.join("clean").exists(),
                "a changed database is marked clean"
            );
            for n in 0..5 {
                assert_eq!(read(&immutable, n, "chunk"), Some(format!("local {n}")));
            }
            assert!(chain.join("ledger").join("999").is_dir());
        }

        #[tokio::test]
        async fn a_completed_partial_install_is_marked_clean() {
            let root = TempDir::new("clean-last");
            let chain = root.0.join("chain");
            write_trio(&chain.join("immutable"), 4, "local 4");
            fs::write(chain.join("clean"), b"old").unwrap();
            let staging = make_staging(&root.0, 4, 8);

            install_staged(&staging, &chain, true).await.unwrap();

            assert_eq!(fs::read(chain.join("clean")).unwrap(), b"");
            assert!(chain.join("ledger").join("12345").is_dir());
        }

        #[tokio::test]
        async fn removes_local_chunks_after_the_installed_tip() {
            let root = TempDir::new("ahead");
            let chain = root.0.join("chain");
            let immutable = chain.join("immutable");
            for n in 0..=12 {
                write_trio(&immutable, n, &format!("local {n}"));
            }

            let staging = make_staging(&root.0, 8, 8);
            install_staged(&staging, &chain, true).await.unwrap();

            for ext in TRIO {
                assert_eq!(read(&immutable, 7, ext).as_deref(), Some("local 7"));
                assert_eq!(read(&immutable, 8, ext).as_deref(), Some("certified 8"));
                assert_eq!(read(&immutable, 9, ext).as_deref(), Some("ancillary"));
                for n in 10..=12 {
                    assert_eq!(read(&immutable, n, ext), None, "{n:05}.{ext} kept");
                }
            }
        }
    }

    mod rebuild {
        use super::super::{
            STAGING_ENTRY, check_rebuild_path, required_space, space_allowed,
            space_freed_before_download, wipe_database,
        };
        use std::fs;
        use std::path::{Path, PathBuf};

        struct TempDir(PathBuf);

        impl TempDir {
            fn new(label: &str) -> Self {
                let p = std::env::temp_dir().join(format!(
                    "wdg-rebuild-{label}-{}-{}",
                    std::process::id(),
                    std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap()
                        .subsec_nanos()
                ));
                fs::create_dir_all(&p).unwrap();
                TempDir(p)
            }
        }

        impl Drop for TempDir {
            fn drop(&mut self) {
                let _ = fs::remove_dir_all(&self.0);
            }
        }

        /// Every top-level entry of a node database, with some content.
        fn make_db(dir: &Path) {
            for sub in ["immutable", "volatile", "ledger", "gsm", "lsm"] {
                fs::create_dir_all(dir.join(sub)).unwrap();
                fs::write(dir.join(sub).join("data"), b"old").unwrap();
            }
            for file in ["lock", "clean", "protocolMagicId"] {
                fs::write(dir.join(file), b"old").unwrap();
            }
        }

        fn entries(dir: &Path) -> Vec<String> {
            let mut names: Vec<String> = fs::read_dir(dir)
                .unwrap()
                .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
                .collect();
            names.sort();
            names
        }

        #[tokio::test]
        async fn deletes_the_database_and_keeps_the_directory() {
            let t = TempDir::new("plain");
            let chain = t.0.join("chain");
            make_db(&chain);
            fs::write(chain.join("desktop.ini"), b"folder settings").unwrap();

            wipe_database(&chain).await.unwrap();

            assert!(chain.is_dir());
            assert_eq!(entries(&chain), vec!["desktop.ini"]);
        }

        #[tokio::test]
        async fn leaves_the_staging_entry_alone() {
            let t = TempDir::new("staging");
            let chain = t.0.join("chain");
            make_db(&chain);
            fs::create_dir_all(chain.join(STAGING_ENTRY).join("download")).unwrap();

            wipe_database(&chain).await.unwrap();

            assert_eq!(entries(&chain), vec![STAGING_ENTRY]);
        }

        #[tokio::test]
        async fn counts_the_space_deleted_before_the_download() {
            let t = TempDir::new("freed");
            let chain = t.0.join("chain");
            fs::create_dir_all(chain.join("immutable")).unwrap();
            fs::write(chain.join("immutable").join("00000.chunk"), vec![0u8; 3000]).unwrap();
            fs::write(chain.join("protocolMagicId"), vec![0u8; 10]).unwrap();
            fs::create_dir_all(chain.join(STAGING_ENTRY)).unwrap();
            fs::write(chain.join(STAGING_ENTRY).join("left"), vec![0u8; 500]).unwrap();
            fs::write(chain.join("desktop.ini"), vec![0u8; 7]).unwrap();

            assert_eq!(space_freed_before_download(&chain, true).await, 3510);
            // Without a rebuild only the staging leftover is deleted first.
            assert_eq!(space_freed_before_download(&chain, false).await, 500);
        }

        #[cfg(unix)]
        #[tokio::test]
        async fn a_link_in_the_database_frees_nothing() {
            let t = TempDir::new("freed-link");
            let elsewhere = t.0.join("other-disk");
            fs::create_dir_all(&elsewhere).unwrap();
            fs::write(elsewhere.join("00000.chunk"), vec![0u8; 3000]).unwrap();
            let chain = t.0.join("chain");
            fs::create_dir_all(&chain).unwrap();
            std::os::unix::fs::symlink(&elsewhere, chain.join("immutable")).unwrap();

            assert_eq!(space_freed_before_download(&chain, true).await, 0);
        }

        #[tokio::test]
        async fn a_folder_not_created_yet_is_checked_on_the_volume_it_will_be_on() {
            let t = TempDir::new("not-yet");
            let chain = t.0.join("picked").join("chain");
            assert!(space_allowed(&chain, Some(1000), false).await);
            // Refused rather than waved through, so the free space was read
            // from the nearest folder that exists.
            assert!(!space_allowed(&chain, Some(u64::MAX / 2), false).await);
            assert!(!chain.exists());
        }

        // The old chain is a sparse file: it counts as its full length, and
        // takes no space. Sparse files cost real space on Windows.
        #[cfg(unix)]
        #[tokio::test]
        async fn a_rebuild_fits_once_the_space_its_wipe_frees_is_counted() {
            let t = TempDir::new("fits-after-wipe");
            let free = crate::chain_validation::read_free_space(t.0.clone())
                .await
                .expect("free space of the test volume");
            let chain = t.0.join("chain");
            fs::create_dir_all(chain.join("immutable")).unwrap();
            let old_chain = free / 2 + 1_000_000_000;
            fs::File::create(chain.join("immutable").join("00000.chunk"))
                .unwrap()
                .set_len(old_chain)
                .unwrap();
            // More than the volume has free, less than it will have once the
            // old chain is deleted.
            let db_size = free + old_chain / 4;

            assert!(space_allowed(&chain, Some(db_size), true).await);
            assert!(!space_allowed(&chain, Some(db_size), false).await);
        }

        #[test]
        fn a_full_install_needs_a_tenth_more_than_the_database() {
            assert_eq!(required_space(200_000), 220_000);
            assert_eq!(required_space(u64::MAX), u64::MAX);
        }

        #[tokio::test]
        async fn a_missing_directory_has_nothing_to_delete() {
            let t = TempDir::new("missing");
            wipe_database(&t.0.join("chain")).await.unwrap();
            assert!(!t.0.join("chain").exists());
        }

        #[cfg(unix)]
        #[tokio::test]
        async fn keeps_a_link_and_empties_its_target() {
            let t = TempDir::new("link");
            let target = t.0.join("elsewhere");
            make_db(&target);
            let chain = t.0.join("state").join("chain");
            fs::create_dir_all(chain.parent().unwrap()).unwrap();
            std::os::unix::fs::symlink(&target, &chain).unwrap();

            wipe_database(&chain).await.unwrap();

            assert!(
                fs::symlink_metadata(&chain)
                    .unwrap()
                    .file_type()
                    .is_symlink()
            );
            assert_eq!(fs::read_link(&chain).unwrap(), target);
            assert!(target.is_dir());
            assert!(entries(&target).is_empty());
        }

        #[cfg(unix)]
        #[tokio::test]
        async fn deletes_a_link_in_the_database_without_following_it() {
            let t = TempDir::new("inner-link");
            let chain = t.0.join("chain");
            make_db(&chain);
            let elsewhere = t.0.join("other-disk").join("immutable");
            fs::create_dir_all(&elsewhere).unwrap();
            fs::write(elsewhere.join("00000.chunk"), b"kept").unwrap();
            fs::remove_dir_all(chain.join("immutable")).unwrap();
            std::os::unix::fs::symlink(&elsewhere, chain.join("immutable")).unwrap();

            wipe_database(&chain).await.unwrap();

            assert!(fs::symlink_metadata(chain.join("immutable")).is_err());
            assert_eq!(fs::read(elsewhere.join("00000.chunk")).unwrap(), b"kept");
        }

        #[tokio::test]
        async fn accepts_an_absolute_directory() {
            let t = TempDir::new("absolute");
            assert!(check_rebuild_path(&t.0.join("chain")).await.is_ok());
        }

        #[tokio::test]
        async fn refuses_a_relative_path() {
            let err = check_rebuild_path(Path::new("chain")).await.unwrap_err();
            assert!(err.to_string().contains("relative path"), "{err}");
        }

        #[tokio::test]
        async fn refuses_a_filesystem_root() {
            let root = std::env::temp_dir()
                .ancestors()
                .last()
                .unwrap()
                .to_path_buf();
            assert!(check_rebuild_path(&root).await.is_err());
        }

        #[cfg(unix)]
        #[tokio::test]
        async fn refuses_a_link_to_a_filesystem_root() {
            let t = TempDir::new("link-root");
            let chain = t.0.join("chain");
            std::os::unix::fs::symlink("/", &chain).unwrap();
            assert!(check_rebuild_path(&chain).await.is_err());
        }

        #[cfg(unix)]
        #[tokio::test]
        async fn refuses_a_link_that_resolves_to_a_filesystem_root() {
            let t = TempDir::new("link-root-via");
            let hop = t.0.join("hop");
            std::os::unix::fs::symlink("/", &hop).unwrap();
            let chain = t.0.join("chain");
            std::os::unix::fs::symlink(&hop, &chain).unwrap();
            assert!(check_rebuild_path(&chain).await.is_err());
        }
    }

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
    mod full_install {
        use super::super::{
            RENAME_ATTEMPTS, RENAME_RETRY_DELAY, STAGING_ENTRY, check_replaceable,
            copy_dir_recursive, install_entries, install_staged, rename_with_retry,
        };
        use std::fs;
        use std::path::{Path, PathBuf};

        struct TempDir(PathBuf);

        impl TempDir {
            fn new(label: &str) -> Self {
                let p = std::env::temp_dir().join(format!(
                    "wdg-full-{label}-{}-{}",
                    std::process::id(),
                    std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap()
                        .subsec_nanos()
                ));
                fs::create_dir_all(&p).unwrap();
                TempDir(p)
            }
        }

        impl Drop for TempDir {
            fn drop(&mut self) {
                let _ = fs::remove_dir_all(&self.0);
            }
        }

        fn make_staging(root: &Path) -> PathBuf {
            let staging = root.join("staging");
            fs::create_dir_all(staging.join("immutable")).unwrap();
            fs::write(staging.join("protocolMagicId"), b"1").unwrap();
            staging
        }

        #[tokio::test]
        async fn refuses_a_directory_with_unrelated_files_and_leaves_it_alone() {
            let t = TempDir::new("unrelated");
            let chain = t.0.join("chain");
            fs::create_dir_all(chain.join("immutable")).unwrap();
            fs::write(chain.join("notes.txt"), b"user data").unwrap();
            let staging = make_staging(&t.0);

            let err = install_staged(&staging, &chain, false).await.unwrap_err();

            assert!(err.to_string().contains("notes.txt"), "{err}");
            assert_eq!(fs::read(chain.join("notes.txt")).unwrap(), b"user data");
            assert!(chain.join("immutable").is_dir());
            assert!(
                staging.join("protocolMagicId").exists(),
                "staging was consumed"
            );
        }

        #[tokio::test]
        async fn replaces_a_directory_that_holds_only_a_node_database() {
            let t = TempDir::new("db");
            let chain = t.0.join("chain");
            for dir in ["immutable", "volatile", "ledger", "gsm", "lsm"] {
                fs::create_dir_all(chain.join(dir)).unwrap();
            }
            for file in ["lock", "clean", "protocolMagicId", ".DS_Store"] {
                fs::write(chain.join(file), b"old").unwrap();
            }
            let staging = make_staging(&t.0);

            install_staged(&staging, &chain, false).await.unwrap();

            assert!(chain.join("immutable").is_dir());
            assert_eq!(fs::read(chain.join("protocolMagicId")).unwrap(), b"1");
            assert!(!chain.join("lock").exists());
        }

        #[tokio::test]
        async fn installs_into_a_missing_directory() {
            let t = TempDir::new("missing");
            let chain = t.0.join("chain");
            let staging = make_staging(&t.0);
            install_staged(&staging, &chain, false).await.unwrap();
            assert!(chain.join("protocolMagicId").exists());
        }

        /// A staged database with a `clean` marker and two chunks.
        fn make_clean_db(root: &Path) -> PathBuf {
            let db = root.join("staged");
            fs::create_dir_all(db.join("immutable")).unwrap();
            for n in 0..2 {
                fs::write(db.join("immutable").join(format!("{n:05}.chunk")), b"chunk").unwrap();
            }
            fs::write(db.join("protocolMagicId"), b"1").unwrap();
            fs::write(db.join("clean"), b"").unwrap();
            db
        }

        #[tokio::test]
        async fn a_copy_that_fails_part_way_leaves_no_clean_marker() {
            let t = TempDir::new("copy-fail");
            let db = make_clean_db(&t.0);
            let dst = t.0.join("chain");
            // A directory where a chunk is to be copied makes that copy fail.
            fs::create_dir_all(dst.join("immutable").join("00001.chunk")).unwrap();

            assert!(copy_dir_recursive(&db, &dst).await.is_err());

            assert!(
                !dst.join("clean").exists(),
                "a partial copy is marked clean"
            );
        }

        #[tokio::test]
        async fn a_complete_copy_ends_with_the_clean_marker() {
            let t = TempDir::new("copy-ok");
            let db = make_clean_db(&t.0);
            let dst = t.0.join("chain");

            copy_dir_recursive(&db, &dst).await.unwrap();

            assert!(dst.join("clean").is_file());
            assert!(dst.join("protocolMagicId").is_file());
            assert!(dst.join("immutable").join("00001.chunk").is_file());
        }

        #[tokio::test]
        async fn accepts_a_database_with_a_staging_leftover() {
            let t = TempDir::new("staging-leftover");
            let chain = t.0.join("chain");
            for dir in ["immutable", "ledger"] {
                fs::create_dir_all(chain.join(dir)).unwrap();
            }
            fs::write(chain.join("protocolMagicId"), b"1").unwrap();
            fs::create_dir_all(chain.join(STAGING_ENTRY).join("download").join("db")).unwrap();
            assert!(check_replaceable(&chain).await.is_ok());
        }

        #[tokio::test]
        async fn installs_beside_the_staging_entry_and_keeps_the_directory() {
            let t = TempDir::new("in-place");
            let chain = t.0.join("chain");
            fs::create_dir_all(chain.join("immutable")).unwrap();
            fs::write(chain.join("immutable").join("00000.chunk"), b"old").unwrap();
            fs::write(chain.join("clean"), b"old").unwrap();
            let staging = chain.join(STAGING_ENTRY).join("download").join("db");
            fs::create_dir_all(staging.join("immutable")).unwrap();
            fs::write(staging.join("immutable").join("00001.chunk"), b"new").unwrap();
            fs::write(staging.join("protocolMagicId"), b"1").unwrap();
            fs::write(staging.join("clean"), b"").unwrap();

            install_staged(&staging, &chain, false).await.unwrap();

            assert!(!chain.join("immutable").join("00000.chunk").exists());
            assert_eq!(
                fs::read(chain.join("immutable").join("00001.chunk")).unwrap(),
                b"new"
            );
            assert_eq!(fs::read(chain.join("protocolMagicId")).unwrap(), b"1");
            assert_eq!(fs::read(chain.join("clean")).unwrap(), b"");
        }

        #[tokio::test]
        async fn an_install_that_stops_part_way_leaves_no_clean_marker() {
            let t = TempDir::new("install-fail");
            let db = make_clean_db(&t.0);
            let dst = t.0.join("chain");
            // A directory where a chunk is to go makes that move fail on every
            // platform: no rename replaces a non-empty directory, and no copy
            // writes the chunk over a directory. Windows renames over a file.
            fs::create_dir_all(dst.join("immutable").join("00001.chunk")).unwrap();

            assert!(install_entries(&db, &dst).await.is_err());

            assert!(
                !dst.join("clean").exists(),
                "a partial install is marked clean"
            );
        }

        #[cfg(unix)]
        #[tokio::test]
        async fn a_rename_refused_at_first_succeeds_when_tried_again() {
            use std::os::unix::fs::PermissionsExt;
            let t = TempDir::new("rename-retry");
            let src = t.0.join("staged");
            fs::write(&src, b"chunk").unwrap();
            let dir = t.0.join("chain");
            fs::create_dir_all(&dir).unwrap();
            fs::set_permissions(&dir, fs::Permissions::from_mode(0o555)).unwrap();
            if fs::File::create(dir.join("probe")).is_ok() {
                fs::set_permissions(&dir, fs::Permissions::from_mode(0o755)).unwrap();
                eprintln!("skipped: directory permissions are not enforced for this user");
                return;
            }
            // Like a file another program holds open for a moment: the first
            // tries fail, and a later one succeeds once it lets go.
            let unlock = {
                let dir = dir.clone();
                tokio::spawn(async move {
                    tokio::time::sleep(RENAME_RETRY_DELAY * 2).await;
                    fs::set_permissions(&dir, fs::Permissions::from_mode(0o755)).unwrap();
                })
            };
            let renamed = rename_with_retry(&src, &dir.join("00000.chunk")).await;
            unlock.await.unwrap();
            renamed.unwrap();
            assert_eq!(fs::read(dir.join("00000.chunk")).unwrap(), b"chunk");
            assert!(!src.exists());
        }

        #[tokio::test]
        async fn a_rename_that_keeps_failing_gives_up_after_its_attempts() {
            let t = TempDir::new("rename-gives-up");
            let src = t.0.join("missing");
            let started = std::time::Instant::now();
            assert!(rename_with_retry(&src, &t.0.join("dst")).await.is_err());
            assert!(started.elapsed() >= RENAME_RETRY_DELAY * (RENAME_ATTEMPTS - 1));
        }

        #[tokio::test]
        async fn accepts_an_empty_directory() {
            let t = TempDir::new("empty");
            assert!(check_replaceable(&t.0).await.is_ok());
        }

        #[tokio::test]
        async fn refuses_a_file_in_place_of_the_directory() {
            let t = TempDir::new("file");
            let chain = t.0.join("chain");
            fs::write(&chain, b"not a folder").unwrap();
            assert!(check_replaceable(&chain).await.is_err());
            assert_eq!(fs::read(&chain).unwrap(), b"not a folder");
        }

        #[cfg(unix)]
        #[tokio::test]
        async fn refuses_a_link_whose_target_holds_unrelated_files() {
            let t = TempDir::new("link-unrelated");
            let target = t.0.join("elsewhere");
            fs::create_dir_all(&target).unwrap();
            fs::write(target.join("photo.jpg"), b"user data").unwrap();
            let chain = t.0.join("chain");
            std::os::unix::fs::symlink(&target, &chain).unwrap();
            let staging = make_staging(&t.0);

            assert!(install_staged(&staging, &chain, false).await.is_err());

            assert_eq!(fs::read(target.join("photo.jpg")).unwrap(), b"user data");
            assert!(
                fs::symlink_metadata(&chain)
                    .unwrap()
                    .file_type()
                    .is_symlink()
            );
        }

        #[cfg(unix)]
        #[tokio::test]
        async fn replaces_a_link_target_that_holds_a_node_database() {
            let t = TempDir::new("link-db");
            let target = t.0.join("elsewhere");
            fs::create_dir_all(target.join("immutable")).unwrap();
            fs::write(target.join("clean"), b"old").unwrap();
            let chain = t.0.join("chain");
            std::os::unix::fs::symlink(&target, &chain).unwrap();
            let staging = make_staging(&t.0);

            install_staged(&staging, &chain, false).await.unwrap();

            assert!(
                fs::symlink_metadata(&chain)
                    .unwrap()
                    .file_type()
                    .is_symlink()
            );
            assert!(target.join("protocolMagicId").exists());
            assert!(!target.join("clean").exists());
        }
    }

    #[cfg(windows)]
    mod junction_install {
        use super::super::{install_staged, wipe_database};
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
            fs::write(chain_path.join("clean"), b"old").unwrap();

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
            assert!(!chain_path.join("clean").exists());
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

        // wipe_database with a junction chain_path must delete the database in
        // the junction target and keep both the junction and the target, so a
        // rebuild installs into the same custom location.
        #[tokio::test]
        async fn rebuild_empties_the_junction_target_and_keeps_the_junction() {
            let root = temp_dir("rebuild");
            let target = root.join("target");
            fs::create_dir_all(target.join("immutable")).unwrap();
            fs::write(target.join("immutable").join("00000.chunk"), b"old").unwrap();
            fs::write(target.join("protocolMagicId"), b"1").unwrap();
            let chain_path = root.join("chain");
            make_junction(&chain_path, &target);

            wipe_database(&chain_path).await.unwrap();

            let meta = fs::symlink_metadata(&chain_path).unwrap();
            assert!(
                meta.file_type().is_symlink(),
                "chain entry point should remain a junction after the wipe"
            );
            assert!(target.is_dir(), "junction target should be kept");
            assert!(!target.join("immutable").exists());
            assert!(!target.join("protocolMagicId").exists());
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
