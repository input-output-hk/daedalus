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
    /// The user explicitly chose to start the node while recovering from a
    /// conversion failure, including a genesis sync after a rebuild.
    StartNodeChosen,
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

impl RecordKind {
    /// What the record is for, in words for the log.
    pub fn describe(self) -> &'static str {
        match self {
            RecordKind::Rebuild => "rebuild",
            RecordKind::Install => "Mithril install",
        }
    }
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

/// The pauses before each further try to delete the rebuild marker. On
/// Windows a file that another program, such as a virus scanner, holds open
/// for a moment cannot be deleted, and a marker left after a database was
/// installed keeps the next start from starting the node on it.
const MARKER_DELETE_RETRY_DELAYS: [Duration; 4] = [
    Duration::from_millis(100),
    Duration::from_millis(250),
    Duration::from_millis(500),
    Duration::from_millis(1000),
];

/// Deletes the file at `path`, trying again after each of `delays` while it
/// fails. A file that is already gone counts as deleted.
async fn remove_marker_file(path: &Path, delays: &[Duration]) -> std::io::Result<()> {
    let mut delays = delays.iter();
    loop {
        match tokio::fs::remove_file(path).await {
            Ok(()) => return Ok(()),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(()),
            Err(e) => match delays.next() {
                Some(delay) => {
                    warn!(
                        "mithril: could not delete {}: {e}; trying again",
                        path.display()
                    );
                    tokio::time::sleep(*delay).await;
                }
                None => return Err(e),
            },
        }
    }
}

/// Deletes the rebuild marker when it is for the configured chain directory:
/// a database was installed there, or the user chose to sync it from genesis.
pub async fn clear_rebuild_marker(cfg: &MithrilConfig) {
    clear_rebuild_marker_trying(cfg, &MARKER_DELETE_RETRY_DELAYS).await;
}

async fn clear_rebuild_marker_trying(cfg: &MithrilConfig, delays: &[Duration]) {
    if pending_record(cfg).await.is_none() {
        return;
    }
    let path = Path::new(&cfg.state_dir).join(REBUILD_MARKER);
    match remove_marker_file(&path, delays).await {
        Ok(()) => info!(
            "mithril: the rebuild or install of {} is no longer pending",
            cfg.chain_path
        ),
        Err(e) => warn!("mithril: could not delete {}: {e}", path.display()),
    }
}

/// Removes, at startup, the record of a rebuild or install of the configured
/// chain directory when that directory has a `clean` marker. Every rebuild
/// and install deletes `clean` before it changes the database, and an install
/// moves it in last, so a database marked clean is whole: the install the
/// record describes finished and the record was not deleted, or the rebuild
/// stopped before it deleted anything. The node then starts on it as usual.
pub async fn remove_finished_record(cfg: &MithrilConfig) {
    let clean = Path::new(&cfg.chain_path).join("clean");
    if !tokio::fs::metadata(&clean).await.is_ok_and(|m| m.is_file()) {
        return;
    }
    let Some(kind) = pending_record(cfg).await else {
        return;
    };
    let path = Path::new(&cfg.state_dir).join(REBUILD_MARKER);
    warn!(
        "mithril: {} is marked clean, so the {} that {} records left it whole; the record is removed",
        cfg.chain_path,
        kind.describe(),
        path.display()
    );
    if let Err(e) = remove_marker_file(&path, &MARKER_DELETE_RETRY_DELAYS).await {
        warn!("mithril: could not delete {}: {e}", path.display());
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

/// `mithril-client cardano-db snapshot show latest` against the configured
/// aggregator. It reaches the aggregator as the download does, through the
/// proxy settings of the environment and the system.
fn snapshot_show_command(cfg: &MithrilConfig) -> Command {
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
    cmd
}

/// The highest local chunk, the certified chunk and, when the aggregator
/// states it, the size of the snapshot's database once unpacked.
async fn probe_snapshot(cfg: &MithrilConfig) -> Result<(Option<u64>, u64, Option<u64>)> {
    let output = snapshot_show_command(cfg).output().await?;
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
/// junction returns its target in the verbatim form, `\\?\D:\...` or
/// `\\?\UNC\server\share\...`, and the staging directory inside the chain
/// directory inherits that form. Not every program accepts it, so a verbatim
/// drive or network path is passed in its plain form, `D:\...` or
/// `\\server\share\...`, unless the plain form is too long for programs
/// bound by the classic Windows path limit, which reach it only in the
/// verbatim form. Any other path is passed as it is.
fn for_external_program(path: &Path) -> PathBuf {
    #[cfg(windows)]
    if let Some(plain) = path.to_str().and_then(plain_form) {
        return PathBuf::from(plain);
    }
    path.to_path_buf()
}

/// The length, in UTF-16 units, from which a directory path no longer fits
/// the classic Windows limit: MAX_PATH, 260, less room for an 8.3 file name.
#[cfg(any(windows, test))]
const PLAIN_PATH_LIMIT: usize = 248;

/// The plain form of a verbatim drive path, `D:\data` for `\\?\D:\data`, or of
/// a verbatim network path, `\\server\share\data` for
/// `\\?\UNC\server\share\data`. None when `path` is neither, or when the
/// plain form would be PLAIN_PATH_LIMIT units long or longer.
#[cfg(any(windows, test))]
fn plain_form(path: &str) -> Option<String> {
    let rest = path.strip_prefix(r"\\?\")?;
    let unc = rest
        .get(..4)
        .filter(|prefix| prefix.eq_ignore_ascii_case(r"UNC\"))
        .map(|_| &rest[4..]);
    let plain = match unc {
        Some(share_path) => {
            let mut parts = share_path.split('\\');
            let server = parts.next().unwrap_or_default();
            let share = parts.next().unwrap_or_default();
            if server.is_empty() || share.is_empty() {
                return None;
            }
            format!(r"\\{share_path}")
        }
        None => {
            let drive = rest.as_bytes();
            let is_drive_root = drive.len() >= 3
                && drive[0].is_ascii_alphabetic()
                && drive[1] == b':'
                && drive[2] == b'\\';
            if !is_drive_root {
                return None;
            }
            rest.to_string()
        }
    };
    (plain.encode_utf16().count() < PLAIN_PATH_LIMIT).then_some(plain)
}

enum ProcResult {
    Success,
    Failed(String),
    Cancelled,
    Stopped,
}

/// Longest a check of whether the aggregator can be reached may take before
/// mithril-client is stopped and the aggregator counts as not reached.
const AGGREGATOR_CHECK_TIMEOUT: Duration = Duration::from_secs(15);

/// How often the download watch looks at the download, at most.
const DOWNLOAD_WATCH_TICK_MAX: Duration = Duration::from_secs(5);

/// Watches a Mithril download for a stall, and for the checks of the
/// aggregator that follow one. mithril-client waits for the network by
/// itself and continues when it returns, without reporting anything, so a
/// stall with an aggregator that cannot be reached is taken for that wait.
///
/// mithril-client reports the immutable files it has downloaded as
/// `files_downloaded` and the ancillary archive as `bytes_downloaded`, one
/// kind per line, with the other read as 0, and announces each step with its
/// number. Progress is the highest of each count growing on its own. The
/// download ends when the file count reaches its total, or when a later
/// step is announced after file or byte progress.
struct DownloadWatch {
    stall_after: Duration,
    recheck_every: Duration,
    files: u64,
    bytes: u64,
    step: u32,
    grew_at: Instant,
    next_check: Option<Instant>,
    done: bool,
    waiting: bool,
}

impl DownloadWatch {
    fn new(stall_after: Duration, recheck_every: Duration, now: Instant) -> Self {
        DownloadWatch {
            stall_after,
            recheck_every,
            files: 0,
            bytes: 0,
            step: 0,
            grew_at: now,
            next_check: None,
            done: false,
            waiting: false,
        }
    }

    /// How often the watch is to be looked at.
    fn tick(&self) -> Duration {
        (self.stall_after / 4).clamp(Duration::from_millis(100), DOWNLOAD_WATCH_TICK_MAX)
    }

    /// Takes in one progress report. Returns `Some(false)` when it ends a
    /// reported wait.
    fn on_progress(
        &mut self,
        files_downloaded: u64,
        files_total: u64,
        bytes_downloaded: u64,
        step: u32,
        now: Instant,
    ) -> Option<bool> {
        let counted = self.files > 0 || self.bytes > 0;
        let grew =
            files_downloaded > self.files || bytes_downloaded > self.bytes || step > self.step;
        if (step > self.step && counted) || (files_total > 0 && files_downloaded >= files_total) {
            self.done = true;
        }
        self.files = self.files.max(files_downloaded);
        self.bytes = self.bytes.max(bytes_downloaded);
        self.step = self.step.max(step);
        if grew {
            self.grew_at = now;
            self.next_check = None;
        }
        self.end_wait_if(grew || self.done)
    }

    /// Whether the aggregator is to be checked at `now`: the download has
    /// stalled, and no check is due later.
    fn check_due(&mut self, now: Instant) -> bool {
        if !self.stalled(now) || self.next_check.is_some_and(|at| now < at) {
            return false;
        }
        self.next_check = Some(now + self.recheck_every);
        true
    }

    /// Takes in the result of a check. A check that finishes after the
    /// download has grown again or ended counts for nothing. Returns the new
    /// waiting state when it changes.
    fn on_check(&mut self, reachable: bool, now: Instant) -> Option<bool> {
        if !self.stalled(now) || reachable != self.waiting {
            return None;
        }
        self.waiting = !reachable;
        Some(self.waiting)
    }

    /// Ends the watch with the download. Returns `Some(false)` when a wait
    /// was reported.
    fn finish(&mut self) -> Option<bool> {
        self.done = true;
        self.end_wait_if(true)
    }

    fn stalled(&self, now: Instant) -> bool {
        !self.done && now.saturating_duration_since(self.grew_at) >= self.stall_after
    }

    fn end_wait_if(&mut self, condition: bool) -> Option<bool> {
        (condition && std::mem::take(&mut self.waiting)).then_some(false)
    }
}

/// Logs and reports a change in whether the download waits for the network.
fn report_network_wait(change: Option<bool>, stall_after: Duration, aggregator: &str) {
    match change {
        Some(true) => {
            warn!(
                "Mithril download: no progress for {} s and the aggregator at {aggregator} cannot \
                 be reached; waiting for the network",
                stall_after.as_secs()
            );
            emit(&Event::MithrilWaitingForNetwork { waiting: true });
        }
        Some(false) => {
            info!("Mithril download: no longer waiting for the network");
            emit(&Event::MithrilWaitingForNetwork { waiting: false });
        }
        None => {}
    }
}

/// Whether mithril-client reaches the aggregator, asked with the same
/// command as the probe before a download. mithril-client downloads through
/// the proxy that the environment or the system sets, which a direct
/// connection from the watchdog would bypass, so only mithril-client can
/// tell whether the download's route to the network works. A client still
/// running after `limit`, as one waiting on name resolution can be, is
/// stopped, and the aggregator counts as not reached.
async fn aggregator_reachable(cfg: &MithrilConfig, limit: Duration) -> bool {
    let mut cmd = snapshot_show_command(cfg);
    cmd.stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .kill_on_drop(true);
    let Ok(mut child) = cmd.spawn() else {
        return false;
    };
    match tokio::time::timeout(limit, child.wait()).await {
        Ok(status) => status.is_ok_and(|s| s.success()),
        Err(_) => {
            let _ = child.start_kill();
            let _ = child.wait().await;
            false
        }
    }
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

    // A download that stalls while mithril-client cannot reach the
    // aggregator is reported as waiting for the network. The download is
    // left running: it continues by itself when the network returns.
    let stall_after = Duration::from_secs(cfg.download_stall_secs);
    let watch_stalls = !stall_after.is_zero();
    let mut watch = DownloadWatch::new(
        stall_after,
        Duration::from_secs(cfg.network_recheck_secs),
        Instant::now(),
    );
    let mut watch_ticks = tokio::time::interval(watch.tick());
    watch_ticks.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
    let (reach_tx, mut reach_rx) = mpsc::channel::<bool>(4);
    // One check at a time: a check that outlasts the recheck interval is not
    // joined by another.
    let mut checking = false;
    let aggregator_label = cfg.aggregator_url.clone();
    let watch_progress = |watch: &mut DownloadWatch, ev: &Event| {
        if let Event::MithrilProgress {
            files_downloaded,
            files_total,
            bytes_downloaded,
            step_num,
            ..
        } = ev
        {
            let change = watch.on_progress(
                *files_downloaded,
                *files_total,
                *bytes_downloaded,
                *step_num,
                Instant::now(),
            );
            report_network_wait(change, stall_after, &aggregator_label);
        }
    };

    let result = loop {
        tokio::select! {
            result = proc.wait() => {
                // Drain remaining progress events
                prog_rx.close();
                while let Some(ev) = prog_rx.recv().await {
                    announce_downloading(&ev);
                    emit(&ev);
                }
                break match result {
                    Ok(s) if s.success() => ProcResult::Success,
                    Ok(s) => ProcResult::Failed(format!("mithril-client exited with {s}")),
                    Err(e) => ProcResult::Failed(e.to_string()),
                };
            }
            Some(ev) = prog_rx.recv() => {
                announce_downloading(&ev);
                watch_progress(&mut watch, &ev);
                emit(&ev);
            }
            _ = watch_ticks.tick(), if watch_stalls => {
                if !checking && watch.check_due(Instant::now()) {
                    checking = true;
                    let tx = reach_tx.clone();
                    let check_cfg = cfg.clone();
                    tokio::spawn(async move {
                        let reachable =
                            aggregator_reachable(&check_cfg, AGGREGATOR_CHECK_TIMEOUT).await;
                        let _ = tx.send(reachable).await;
                    });
                }
            }
            Some(reachable) = reach_rx.recv() => {
                checking = false;
                let change = watch.on_check(reachable, Instant::now());
                report_network_wait(change, stall_after, &aggregator_label);
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
    report_network_wait(watch.finish(), stall_after, &aggregator_label);
    result
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

/// The pause before the second try of a rename that fails without crossing
/// volumes, and the longest pause between later tries: each pause doubles,
/// up to the longest, until the install's rename budget is spent.
const RENAME_FIRST_DELAY: Duration = Duration::from_millis(100);
const RENAME_MAX_DELAY: Duration = Duration::from_secs(5);

/// Renames `src` to `dst`. A rename between volumes cannot succeed and fails
/// at once. Any other failure, such as a file that another program holds
/// open for a moment, is tried again with growing pauses for as long as
/// `budget` allows, and then fails.
async fn rename_with_retry(src: &Path, dst: &Path, budget: Duration) -> std::io::Result<()> {
    let mut waited = Duration::ZERO;
    let mut delay = RENAME_FIRST_DELAY;
    loop {
        match tokio::fs::rename(src, dst).await {
            Ok(()) => return Ok(()),
            Err(e) if e.kind() == std::io::ErrorKind::CrossesDevices || waited >= budget => {
                return Err(e);
            }
            Err(e) => {
                let pause = delay.min(budget - waited);
                warn!(
                    "mithril: could not rename {} after {} ms: {e}; trying again",
                    src.display(),
                    waited.as_millis()
                );
                tokio::time::sleep(pause).await;
                waited += pause;
                delay = (delay * 2).min(RENAME_MAX_DELAY);
            }
        }
    }
}

/// Moves a file. A rename that fails because `dst` is on another volume
/// falls back to copy and delete; any other failure fails the move. A copy
/// within the volume would need as much free space again as the space check
/// reserved for the database.
async fn move_file(src: &Path, dst: &Path, budget: Duration) -> Result<()> {
    match rename_with_retry(src, dst, budget).await {
        Ok(()) => return Ok(()),
        Err(e) if e.kind() == std::io::ErrorKind::CrossesDevices => {
            warn!("mithril: copying {} to another volume: {e}", src.display());
        }
        Err(e) => anyhow::bail!("rename {} to {}: {e}", src.display(), dst.display()),
    }
    tokio::fs::copy(src, dst)
        .await
        .map_err(|e| anyhow::anyhow!("copy {}: {e}", src.display()))?;
    let _ = tokio::fs::remove_file(src).await;
    Ok(())
}

/// Moves a directory tree, as move_file moves a file.
async fn move_dir(src: &Path, dst: &Path, budget: Duration) -> Result<()> {
    match rename_with_retry(src, dst, budget).await {
        Ok(()) => return Ok(()),
        Err(e) if e.kind() == std::io::ErrorKind::CrossesDevices => {
            warn!("mithril: copying {} to another volume: {e}", src.display());
        }
        Err(e) => anyhow::bail!("rename {} to {}: {e}", src.display(), dst.display()),
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
/// wipe leaves it alone, and the has-chain check does not look at it. Its
/// marker names the network it holds a download for.
pub(crate) const STAGING_ENTRY: &str = "mithril-staging";

/// File in the staging entry that names the network its download is for, as
/// the network magic in decimal text, the form of a database's
/// `protocolMagicId`. It is written when the staging entry is created, before
/// mithril-client starts, and lies beside the download, not in it, so it is
/// never installed into the chain. mithril-client 0.13.9 writes
/// `protocolMagicId` into the database it downloads only late in the
/// download, so until then this marker is what tells another network's
/// Daedalus, sharing the storage folder, that the staging entry is not its
/// own.
const STAGING_NETWORK_MARKER: &str = "network-magic";

/// Where the staging marker is written before it is renamed into place, so
/// the marker is never read half-written.
const STAGING_NETWORK_MARKER_TEMP: &str = "network-magic.tmp";

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
async fn install_entries(staging_db: &Path, dir: &Path, rename_budget: Duration) -> Result<()> {
    let mut names = Vec::new();
    let mut entries = tokio::fs::read_dir(staging_db).await?;
    while let Some(entry) = entries.next_entry().await? {
        names.push(entry.file_name());
    }
    names.sort_by_key(|name| name == "clean");
    for name in names {
        let (src, dst) = (staging_db.join(&name), dir.join(&name));
        if tokio::fs::symlink_metadata(&src).await?.is_dir() {
            move_dir(&src, &dst, rename_budget).await?;
        } else {
            move_file(&src, &dst, rename_budget).await?;
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
/// database entries the wipe deletes. A staging entry of another network is
/// never deleted, but network_allowed refuses the install before this
/// counts one.
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

/// The network the staging entry `staging` holds a download for: the network
/// magic in its marker or, in a staging entry written without one, in the
/// `protocolMagicId` of the database staged in it. None when neither names a
/// network.
async fn staging_network_magic(staging: &Path) -> Option<u32> {
    match state::network_magic_in(&staging.join(STAGING_NETWORK_MARKER)).await {
        Some(magic) => Some(magic),
        None => state::database_network_magic(&staging.join("download").join("db")).await,
    }
}

/// The network magic of the staging entry `staging` when it names a network
/// other than `network_magic`, the cluster's: that network's Daedalus may be
/// downloading into it now. None when it names this network or none, or when
/// the cluster's network magic is unknown.
async fn other_network_staging(staging: &Path, network_magic: Option<u32>) -> Option<u32> {
    let expected = network_magic?;
    staging_network_magic(staging)
        .await
        .filter(|found| *found != expected)
}

/// The network magic of another network whose download the staging entry in
/// the chain directory `dir` may hold, which keeps it at startup. The staging
/// entry's own marker decides, or the database staged in it. A staging entry
/// that names no network is taken for the other network's when the chain
/// directory holds that network's database.
async fn staging_kept_for(dir: &Path, network_magic: Option<u32>) -> Option<u32> {
    let expected = network_magic?;
    let owner = match staging_network_magic(&dir.join(STAGING_ENTRY)).await {
        Some(found) => Some(found),
        None => state::database_network_magic(dir).await,
    };
    owner.filter(|found| *found != expected)
}

/// Writes the marker that names the network `magic` into the staging entry
/// `staging`. It is written in full to a temporary file there and renamed
/// over the marker, tried again for up to `rename_budget` as an install's
/// renames are, so a reader never sees a partial value. It is not flushed to
/// the disk first: a marker that a crash leaves empty names no network, as
/// in a staging entry without one.
async fn write_staging_marker(
    staging: &Path,
    magic: u32,
    rename_budget: Duration,
) -> std::io::Result<()> {
    let temp = staging.join(STAGING_NETWORK_MARKER_TEMP);
    let written = async {
        tokio::fs::write(&temp, magic.to_string()).await?;
        rename_with_retry(&temp, &staging.join(STAGING_NETWORK_MARKER), rename_budget).await
    }
    .await;
    if written.is_err() {
        let _ = tokio::fs::remove_file(&temp).await;
    }
    written
}

/// Why the staging entry could not be prepared for a download.
#[derive(Debug)]
enum StagingRefused {
    /// It holds a download for another network, `found`; this network's is
    /// `expected`.
    OtherNetwork {
        found: u32,
        expected: u32,
    },
    Io(std::io::Error),
}

/// Prepares the staging entry `staging` for a download for `network_magic`:
/// deletes what an earlier download of this network left there, creates the
/// download directory, and writes the marker that names the network, all
/// before mithril-client starts. A staging entry of another network is left
/// as it is and refused. Without a known network magic no marker is written,
/// as before staging entries were marked.
async fn prepare_staging(
    staging: &Path,
    network_magic: Option<u32>,
    rename_budget: Duration,
) -> Result<(), StagingRefused> {
    if let (Some(expected), Some(found)) = (
        network_magic,
        other_network_staging(staging, network_magic).await,
    ) {
        return Err(StagingRefused::OtherNetwork { found, expected });
    }
    let _ = remove_entry(staging).await;
    tokio::fs::create_dir_all(staging.join("download"))
        .await
        .map_err(StagingRefused::Io)?;
    if let Some(magic) = network_magic {
        write_staging_marker(staging, magic, rename_budget)
            .await
            .map_err(StagingRefused::Io)?;
    }
    Ok(())
}

/// Deletes what an interrupted Mithril run left behind: the staging entry in
/// the chain directory, and the staging directory earlier versions kept
/// in the state directory. Called at startup, when no pipeline of this
/// Daedalus runs. Every network keeps its database in the chain directory of
/// the storage folder, so a staging entry of another network may hold that
/// network's download in progress, and is kept (`staging_kept_for`).
pub async fn remove_stale_staging(cfg: &MithrilConfig) {
    let mut leftovers = vec![Path::new(&cfg.state_dir).join(LEGACY_STAGING_DIR)];
    if let Ok(target) = full_install_target(Path::new(&cfg.chain_path)).await {
        if target.is_absolute() {
            let staging = target.join(STAGING_ENTRY);
            match staging_kept_for(&target, cfg.network_magic).await {
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
/// nothing but database entries. Nor when its staging entry holds another
/// network's download: every download first deletes what the staging entry
/// holds. Emits a `CHAIN_DIR_OTHER_NETWORK` error when it may not;
/// `wipe_chain` tells the renderer whether the sync was a rebuild.
async fn network_allowed(chain_path: &Path, network_magic: Option<u32>, wipe_chain: bool) -> bool {
    let Some(expected) = network_magic else {
        return true;
    };
    let staging = chain_path.join(STAGING_ENTRY);
    if let Some(found) = state::other_network_magic(chain_path, network_magic).await {
        refuse_other_network(chain_path, found, expected, wipe_chain);
        return false;
    }
    if let Some(found) = other_network_staging(&staging, network_magic).await {
        refuse_other_network(&staging, found, expected, wipe_chain);
        return false;
    }
    true
}

/// Logs and reports that `dir`, in the chain directory, holds data of the
/// network `found` and so is not changed.
fn refuse_other_network(dir: &Path, found: u32, expected: u32, wipe_chain: bool) {
    let message = state::other_network_message(dir, found, expected);
    warn!("mithril: refusing to change the chain directory: {message}");
    emit(&Event::MithrilError {
        code: "CHAIN_DIR_OTHER_NETWORK".to_string(),
        message,
        rebuild: wipe_chain,
    });
}

// For bootstrap (local_highest = None): replace the entire chain directory.
// For partial sync (local_highest = Some): delete the clean marker, merge new
// immutables in, replace ledger/lsm, and move the new clean marker in last.
// A rename that fails without crossing volumes is tried again for up to
// `rename_budget`.
async fn install_staged(
    staging_db: &Path,
    chain_path: &Path,
    is_partial: bool,
    rename_budget: Duration,
) -> Result<()> {
    install_staged_with_replay(staging_db, chain_path, is_partial, rename_budget, false).await
}

async fn install_staged_with_replay(
    staging_db: &Path,
    chain_path: &Path,
    is_partial: bool,
    rename_budget: Duration,
    replay: bool,
) -> Result<()> {
    if !is_partial {
        // If chain_path is a symlink or junction, install inside the real
        // target so the link entry point is preserved. The directory itself
        // is kept: it holds the staging entry, and any file-browser metadata
        // check_replaceable allows.
        let target = full_install_target(chain_path).await?;
        check_replaceable(&target).await?;
        delete_database(&target).await?;
        tokio::fs::create_dir_all(&target).await?;
        install_entries(staging_db, &target, rename_budget)
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
        move_file(&src, &dst, rename_budget).await?;
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
    if replay {
        remove_replay_entry(&dst_ledger).await?;
        remove_replay_entry(&chain_path.join("volatile")).await?;
        remove_replay_entry(&chain_path.join("lsm")).await?;
    } else {
        let _ = tokio::fs::remove_dir_all(&dst_ledger).await;
    }
    move_dir(&src_ledger, &dst_ledger, rename_budget).await?;

    // Always remove the old lsm — it belongs to the old ledger and is
    // stale the moment we install a new one.  Install the new lsm if the
    // converter produced one; a missing lsm is safer than a mismatched one.
    let dst_lsm = chain_path.join("lsm");
    let _ = tokio::fs::remove_dir_all(&dst_lsm).await;
    let src_lsm = staging_db.join("lsm");
    if tokio::fs::metadata(&src_lsm).await.is_ok() {
        move_dir(&src_lsm, &dst_lsm, rename_budget).await?;
    }

    // Install the clean sentinel, last, so cardano-node skips chunk
    // validation on next startup. Without this, node treats the partial-sync
    // state as an unclean shutdown and re-validates every immutable chunk.
    let src_clean = staging_db.join("clean");
    if !replay {
        move_file(&src_clean, &dst_clean, rename_budget).await?;
    }

    Ok(())
}

// Unlike best-effort staging cleanup, a stale ledger or LSM database must
// never survive a replay request. Propagate every error except absence.
async fn remove_replay_entry(path: &Path) -> Result<()> {
    let metadata = match tokio::fs::symlink_metadata(path).await {
        Ok(metadata) => metadata,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(e) => return Err(e.into()),
    };
    if metadata.is_dir() && !metadata.file_type().is_symlink() {
        tokio::fs::remove_dir_all(path).await?;
    } else {
        tokio::fs::remove_file(path).await?;
    }
    Ok(())
}

async fn prepare_ledger_replay(staging_db: &Path) -> Result<()> {
    for name in ["ledger", "lsm", "lsm-export", "volatile", "clean"] {
        remove_replay_entry(&staging_db.join(name)).await?;
    }
    tokio::fs::create_dir(staging_db.join("ledger")).await?;
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
    // network's database, and no download replaces another network's in the
    // staging entry.
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
    let rename_budget = Duration::from_secs(cfg.rename_retry_secs);
    match prepare_staging(&staging_root, cfg.network_magic, rename_budget).await {
        Ok(()) => {}
        // network_allowed refused this before anything was deleted; the
        // other network's download began since.
        Err(StagingRefused::OtherNetwork { found, expected }) => {
            refuse_other_network(&staging_root, found, expected, wipe_chain);
            return failed();
        }
        Err(StagingRefused::Io(e)) => {
            warn!("mithril: failed to create staging directory: {e}");
            emit(&Event::MithrilError {
                code: "STAGING_FAILED".to_string(),
                message: e.to_string(),
                rebuild: wipe_chain,
            });
            return failed();
        }
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
    let mut replay = false;
    match run_converter(cfg, &staging_db, cmd_rx).await {
        ProcResult::Success => {}
        ProcResult::Failed(msg) => {
            warn!("mithril conversion failed: {msg}");
            // The pipeline now holds the recovery prompt itself. Report the
            // removed chain here rather than waiting for the supervisor, so
            // the UI still requires an explicit genesis choice after a wipe.
            if wipe_chain {
                let pending = pending_record(cfg).await;
                emit(&Event::ChainStatus {
                    has_chain: false,
                    rebuild_pending: pending == Some(RecordKind::Rebuild),
                    install_pending: pending == Some(RecordKind::Install),
                });
            }
            emit(&Event::MithrilError {
                code: "PARTIAL_SYNC_CONVERSION_FAILED".to_string(),
                message: msg,
                rebuild: wipe_chain,
            });
            // Verification finished before conversion. Keep those immutable
            // files until the user chooses how to recover; never automatically
            // redownload them or start the node on the old chain.
            loop {
                match cmd_rx.recv().await {
                    Some(Cmd::ReplayMithrilLedger) => {
                        if let Err(e) = prepare_ledger_replay(&staging_db).await {
                            let _ = tokio::fs::remove_dir_all(&staging_root).await;
                            emit(&Event::MithrilError {
                                code: "LEDGER_REPLAY_PREPARATION_FAILED".to_string(),
                                message: e.to_string(),
                                rebuild: wipe_chain,
                            });
                            return failed();
                        }
                        replay = true;
                        break;
                    }
                    Some(Cmd::StartMithril { force, wipe_chain }) => {
                        let _ = tokio::fs::remove_dir_all(&staging_root).await;
                        return Box::pin(run_pipeline(cfg, cmd_rx, force, wipe_chain)).await;
                    }
                    Some(Cmd::StartNode) => {
                        let _ = tokio::fs::remove_dir_all(&staging_root).await;
                        clear_rebuild_marker(cfg).await;
                        return PipelineResult::StartNodeChosen;
                    }
                    Some(Cmd::CancelMithril) => {
                        let _ = tokio::fs::remove_dir_all(&staging_root).await;
                        emit(&Event::MithrilStatus {
                            phase: "cancelled".to_string(),
                            rebuild: wipe_chain,
                        });
                        return cancelled_by_user();
                    }
                    Some(Cmd::Stop) | None => {
                        let _ = tokio::fs::remove_dir_all(&staging_root).await;
                        return PipelineResult::Stopped;
                    }
                    Some(_) => {}
                }
            }
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

    let install = if replay {
        install_staged_with_replay(&staging_db, &chain_path, is_partial, rename_budget, true).await
    } else {
        install_staged(&staging_db, &chain_path, is_partial, rename_budget).await
    };
    if let Err(e) = install {
        // The cutover-in-progress marker and the record are already written
        // and the chain may be partially modified. Return Failed so the
        // supervisor does not restart the node against a partially installed
        // chain. Only a rebuild recovers, and it downloads everything again,
        // so the rest of the download is deleted rather than left to fill the
        // volume.
        let _ = tokio::fs::remove_dir_all(&staging_root).await;
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
            remove_finished_record, same_chain_path, write_rebuild_marker,
        };
        use crate::config::MithrilConfig;
        use std::fs;
        use std::path::PathBuf;
        // Used by the tests that need Unix directory permissions.
        #[cfg(unix)]
        use {super::super::clear_rebuild_marker_trying, std::time::Duration};

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

        #[tokio::test]
        async fn a_record_beside_a_clean_chain_is_removed_at_startup() {
            let t = TempDir::new("finished");
            let chain = t.0.join("chain");
            fs::create_dir_all(chain.join("immutable")).unwrap();
            fs::write(chain.join("clean"), b"").unwrap();
            for kind in [RecordKind::Rebuild, RecordKind::Install] {
                let cfg = config(&t.0, chain.to_str().unwrap());
                write_rebuild_marker(&cfg, kind).await.unwrap();
                remove_finished_record(&cfg).await;
                assert!(!t.0.join(REBUILD_MARKER).exists(), "{kind:?} kept");
                assert_eq!(pending_record(&cfg).await, None);
            }
        }

        #[tokio::test]
        async fn a_record_beside_a_chain_without_clean_is_kept_at_startup() {
            let t = TempDir::new("unfinished");
            let chain = t.0.join("chain");
            fs::create_dir_all(chain.join("immutable")).unwrap();
            let cfg = config(&t.0, chain.to_str().unwrap());
            write_rebuild_marker(&cfg, RecordKind::Install)
                .await
                .unwrap();
            remove_finished_record(&cfg).await;
            assert_eq!(pending_record(&cfg).await, Some(RecordKind::Install));
        }

        #[tokio::test]
        async fn a_record_of_another_chain_is_kept_at_startup() {
            let t = TempDir::new("finished-other");
            let chain = t.0.join("chain");
            fs::create_dir_all(&chain).unwrap();
            fs::write(chain.join("clean"), b"").unwrap();
            write_rebuild_marker(&config(&t.0, "/elsewhere/chain"), RecordKind::Rebuild)
                .await
                .unwrap();
            remove_finished_record(&config(&t.0, chain.to_str().unwrap())).await;
            assert!(t.0.join(REBUILD_MARKER).exists());
        }

        /// Makes `dir` read-only. False when this user may write to it anyway,
        /// and it is left writable.
        #[cfg(unix)]
        fn make_read_only(dir: &std::path::Path) -> bool {
            use std::os::unix::fs::PermissionsExt;
            fs::set_permissions(dir, fs::Permissions::from_mode(0o555)).unwrap();
            if fs::File::create(dir.join("probe")).is_ok() {
                let _ = fs::remove_file(dir.join("probe"));
                make_writable(dir);
                return false;
            }
            true
        }

        #[cfg(unix)]
        fn make_writable(dir: &std::path::Path) {
            use std::os::unix::fs::PermissionsExt;
            fs::set_permissions(dir, fs::Permissions::from_mode(0o755)).unwrap();
        }

        #[cfg(unix)]
        #[tokio::test]
        async fn a_record_that_cannot_be_deleted_at_first_is_deleted_when_tried_again() {
            let t = TempDir::new("delete-retry");
            let state = t.0.join("state");
            fs::create_dir_all(&state).unwrap();
            let cfg = config(&state, "/data/chain");
            write_rebuild_marker(&cfg, RecordKind::Install)
                .await
                .unwrap();
            if !make_read_only(&state) {
                eprintln!("skipped: directory permissions are not enforced for this user");
                return;
            }
            // Like a file another program holds open for a moment.
            let unlock = {
                let state = state.clone();
                tokio::spawn(async move {
                    tokio::time::sleep(Duration::from_millis(60)).await;
                    make_writable(&state);
                })
            };
            let delays = [Duration::from_millis(20); 10];
            clear_rebuild_marker_trying(&cfg, &delays).await;
            unlock.await.unwrap();
            assert!(!state.join(REBUILD_MARKER).exists());
        }

        #[cfg(unix)]
        #[tokio::test]
        async fn a_record_that_cannot_be_deleted_is_kept_after_the_last_try() {
            let t = TempDir::new("delete-gives-up");
            let state = t.0.join("state");
            fs::create_dir_all(&state).unwrap();
            let cfg = config(&state, "/data/chain");
            write_rebuild_marker(&cfg, RecordKind::Rebuild)
                .await
                .unwrap();
            if !make_read_only(&state) {
                eprintln!("skipped: directory permissions are not enforced for this user");
                return;
            }
            clear_rebuild_marker_trying(&cfg, &[Duration::from_millis(1); 2]).await;
            make_writable(&state);
            assert_eq!(pending_record(&cfg).await, Some(RecordKind::Rebuild));
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
        use super::super::{PLAIN_PATH_LIMIT, for_external_program, plain_form};
        use std::path::Path;

        #[test]
        fn a_verbatim_drive_path_loses_its_prefix() {
            assert_eq!(
                plain_form(r"\\?\D:\Daedalus\chain\mithril-staging").as_deref(),
                Some(r"D:\Daedalus\chain\mithril-staging")
            );
            assert_eq!(plain_form(r"\\?\c:\").as_deref(), Some(r"c:\"));
        }

        #[test]
        fn a_verbatim_network_path_becomes_a_plain_share_path() {
            assert_eq!(
                plain_form(r"\\?\UNC\server\share\Daedalus\chain").as_deref(),
                Some(r"\\server\share\Daedalus\chain")
            );
            assert_eq!(
                plain_form(r"\\?\unc\server\share").as_deref(),
                Some(r"\\server\share")
            );
        }

        #[test]
        fn other_paths_have_no_plain_form() {
            for path in [
                r"\\?\UNC\server",
                r"\\?\UNC\\share\chain",
                r"\\?\UNC\server\",
                r"\\?\Volume{0b1c2d3e-0000-0000-0000-100000000000}\chain",
                r"\\?\D:",
                r"D:\Daedalus\chain",
                r"\\server\share\chain",
                "/data/chain",
            ] {
                assert_eq!(plain_form(path), None, "{path}");
            }
        }

        #[test]
        fn a_path_too_long_for_its_plain_form_stays_verbatim() {
            // D:\ and a name filling the rest up to the given length.
            let drive = |len: usize| format!(r"\\?\D:\{}", "a".repeat(len - 3));
            assert!(plain_form(&drive(PLAIN_PATH_LIMIT - 1)).is_some());
            assert_eq!(plain_form(&drive(PLAIN_PATH_LIMIT)), None);
            assert_eq!(plain_form(&drive(400)), None);
            // \\server\share\ is 15 units long.
            let share = |len: usize| format!(r"\\?\UNC\server\share\{}", "a".repeat(len - 15));
            assert!(plain_form(&share(PLAIN_PATH_LIMIT - 1)).is_some());
            assert_eq!(plain_form(&share(PLAIN_PATH_LIMIT)), None);
        }

        #[test]
        fn the_limit_counts_utf16_units() {
            // Each of these characters is two UTF-16 units.
            let name = "\u{1F4BE}".repeat(123);
            assert_eq!(plain_form(&format!(r"\\?\D:\{name}")), None);
            let name = "\u{1F4BE}".repeat(122);
            assert!(plain_form(&format!(r"\\?\D:\{name}")).is_some());
        }

        #[test]
        fn programs_get_the_plain_form_on_windows_only() {
            for (verbatim, plain) in [
                (r"\\?\D:\chain", r"D:\chain"),
                (r"\\?\UNC\server\share\chain", r"\\server\share\chain"),
            ] {
                let verbatim = Path::new(verbatim);
                let expected = if cfg!(windows) {
                    Path::new(plain)
                } else {
                    verbatim
                };
                assert_eq!(for_external_program(verbatim), expected);
            }
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

    mod network_wait {
        use super::super::DownloadWatch;
        use std::time::{Duration, Instant};

        const STALL: Duration = Duration::from_secs(60);
        const RECHECK: Duration = Duration::from_secs(20);

        fn secs(n: u64) -> Duration {
            Duration::from_secs(n)
        }

        /// A watch that has seen files 0 to 10 of 100 at the start.
        fn watch(start: Instant) -> DownloadWatch {
            let mut w = DownloadWatch::new(STALL, RECHECK, start);
            assert_eq!(w.on_progress(10, 100, 0, 1, start), None);
            w
        }

        #[test]
        fn checks_the_aggregator_once_the_download_has_stalled() {
            let start = Instant::now();
            let mut w = watch(start);
            assert!(!w.check_due(start + secs(59)));
            assert!(w.check_due(start + secs(60)));
            // Not again before the recheck interval.
            assert!(!w.check_due(start + secs(75)));
            assert!(w.check_due(start + secs(80)));
        }

        #[test]
        fn waits_for_the_network_only_when_the_aggregator_cannot_be_reached() {
            let start = Instant::now();
            let mut w = watch(start);
            assert_eq!(w.on_check(true, start + secs(61)), None);
            assert_eq!(w.on_check(false, start + secs(81)), Some(true));
            assert_eq!(w.on_check(false, start + secs(101)), None);
            // Reached again with no progress: the wait ends all the same.
            assert_eq!(w.on_check(true, start + secs(121)), Some(false));
        }

        #[test]
        fn progress_ends_the_wait_and_restarts_the_stall() {
            let start = Instant::now();
            let mut w = watch(start);
            assert!(w.check_due(start + secs(60)));
            assert_eq!(w.on_check(false, start + secs(61)), Some(true));
            assert_eq!(w.on_progress(11, 100, 0, 1, start + secs(90)), Some(false));
            assert!(!w.check_due(start + secs(149)));
            assert!(w.check_due(start + secs(150)));
        }

        #[test]
        fn tracks_files_and_bytes_on_their_own() {
            let start = Instant::now();
            let mut w = watch(start);
            // An ancillary line reads files as 0 and a file line reads bytes
            // as 0; neither is a fall, and repeating one is no growth.
            assert_eq!(w.on_progress(0, 0, 500, 1, start + secs(30)), None);
            assert_eq!(w.on_progress(10, 100, 0, 1, start + secs(70)), None);
            assert_eq!(w.on_progress(0, 0, 500, 1, start + secs(80)), None);
            assert!(!w.check_due(start + secs(89)));
            assert!(w.check_due(start + secs(90)));
        }

        #[test]
        fn a_check_finished_after_progress_counts_for_nothing() {
            let start = Instant::now();
            let mut w = watch(start);
            assert!(w.check_due(start + secs(60)));
            assert_eq!(w.on_progress(11, 100, 0, 1, start + secs(62)), None);
            assert_eq!(w.on_check(false, start + secs(63)), None);
        }

        #[test]
        fn the_download_ends_with_its_last_file_or_a_later_step() {
            let start = Instant::now();
            let mut w = watch(start);
            assert_eq!(w.on_progress(100, 100, 0, 1, start + secs(10)), None);
            assert!(!w.check_due(start + secs(600)));

            let mut w = watch(start);
            assert!(w.check_due(start + secs(60)));
            assert_eq!(w.on_check(false, start + secs(61)), Some(true));
            assert_eq!(w.on_progress(0, 0, 0, 4, start + secs(70)), Some(false));
            assert!(!w.check_due(start + secs(600)));
        }

        #[test]
        fn a_step_before_any_download_is_progress_not_the_end() {
            let start = Instant::now();
            let mut w = DownloadWatch::new(STALL, RECHECK, start);
            assert_eq!(w.on_progress(0, 0, 0, 2, start + secs(30)), None);
            assert!(!w.check_due(start + secs(89)));
            assert!(w.check_due(start + secs(90)));
        }

        #[test]
        fn finishing_ends_a_reported_wait() {
            let start = Instant::now();
            let mut w = watch(start);
            assert_eq!(w.finish(), None);
            let mut w = watch(start);
            assert_eq!(w.on_check(false, start + secs(61)), Some(true));
            assert_eq!(w.finish(), Some(false));
            assert!(!w.check_due(start + secs(600)));
        }

        #[test]
        fn the_tick_follows_the_stall() {
            let start = Instant::now();
            assert_eq!(watch(start).tick(), secs(5));
            assert_eq!(
                DownloadWatch::new(secs(1), secs(1), start).tick(),
                Duration::from_millis(250)
            );
        }
    }

    /// The check of the aggregator, run with a stand-in for mithril-client.
    #[cfg(unix)]
    mod network_check {
        use super::super::aggregator_reachable;
        use crate::config::MithrilConfig;
        use std::os::unix::fs::PermissionsExt;
        use std::path::PathBuf;
        use std::time::{Duration, Instant};

        struct TempDir(PathBuf);

        impl Drop for TempDir {
            fn drop(&mut self) {
                let _ = std::fs::remove_dir_all(&self.0);
            }
        }

        /// A configuration whose mithril-client is a script running `body`,
        /// in a directory removed when the returned guard is dropped.
        fn client(label: &str, body: &str) -> (TempDir, MithrilConfig) {
            let dir = std::env::temp_dir().join(format!(
                "wdg-network-check-{label}-{}-{}",
                std::process::id(),
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .subsec_nanos()
            ));
            std::fs::create_dir_all(&dir).unwrap();
            let script = dir.join("mithril-client");
            std::fs::write(&script, format!("#!/bin/sh\n{body}\n")).unwrap();
            std::fs::set_permissions(&script, std::fs::Permissions::from_mode(0o755)).unwrap();
            let cfg = serde_json::from_value(serde_json::json!({
                "mithril_bin": script.to_str().unwrap(),
                "snapshot_converter_bin": "c",
                "converter_config": "cfg",
                "aggregator_url": "https://aggregator.example",
                "genesis_vkey": "g",
                "state_dir": dir.to_str().unwrap(),
                "chain_path": dir.join("chain").to_str().unwrap(),
            }))
            .unwrap();
            (TempDir(dir), cfg)
        }

        #[tokio::test]
        async fn the_aggregator_is_reached_when_the_snapshot_query_succeeds() {
            let (_dir, cfg) = client(
                "ok",
                r#"[ "$AGGREGATOR_ENDPOINT" = https://aggregator.example ] && [ "$7" = latest ]"#,
            );
            assert!(aggregator_reachable(&cfg, Duration::from_secs(10)).await);
        }

        #[tokio::test]
        async fn the_aggregator_is_not_reached_when_the_snapshot_query_fails() {
            let (_dir, cfg) = client("fail", "exit 1");
            assert!(!aggregator_reachable(&cfg, Duration::from_secs(10)).await);
        }

        #[tokio::test]
        async fn a_client_still_running_at_the_limit_is_stopped_and_not_reached() {
            let (dir, cfg) = client("hang", r#"echo $$ > "$(dirname "$0")/pid"; exec sleep 30"#);
            let limit = Duration::from_secs(2);
            let started = Instant::now();
            assert!(!aggregator_reachable(&cfg, limit).await);
            assert!(started.elapsed() < limit * 5);
            // A client that has not run its first line by the limit is stopped
            // before it writes its pid, and the check waits for it to exit.
            let Ok(pid) = std::fs::read_to_string(dir.0.join("pid")) else {
                return;
            };
            let alive = std::process::Command::new("kill")
                .args(["-0", pid.trim()])
                .status()
                .unwrap()
                .success();
            assert!(!alive, "the client is still running");
        }

        #[tokio::test]
        async fn a_client_that_cannot_start_does_not_reach_the_aggregator() {
            let (dir, mut cfg) = client("missing", "exit 0");
            cfg.mithril_bin = dir.0.join("absent").to_str().unwrap().to_string();
            assert!(!aggregator_reachable(&cfg, Duration::from_secs(10)).await);
        }
    }

    mod partial_install {
        use super::super::install_staged;
        use std::fs;
        use std::path::{Path, PathBuf};
        use std::time::Duration;

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
            install_staged(&staging, &chain, true, Duration::ZERO)
                .await
                .unwrap();

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

            let installed = install_staged(&staging, &chain, true, Duration::ZERO).await;
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

            install_staged(&staging, &chain, true, Duration::ZERO)
                .await
                .unwrap();

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
            install_staged(&staging, &chain, true, Duration::ZERO)
                .await
                .unwrap();

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

    /// The staging entry's network marker, and what startup and a download
    /// do with a staging entry of this network, of another, or of none.
    mod staging {
        use super::super::{
            STAGING_ENTRY, STAGING_NETWORK_MARKER, STAGING_NETWORK_MARKER_TEMP, StagingRefused,
            network_allowed, prepare_staging, remove_stale_staging, staging_network_magic,
        };
        use crate::config::MithrilConfig;
        use std::collections::BTreeMap;
        use std::fs;
        use std::path::{Path, PathBuf};
        use std::time::Duration;

        /// The cluster's network magic in these tests.
        const THIS: u32 = 2;
        /// Another network's.
        const OTHER: &str = "1";

        struct TempDir(PathBuf);

        impl TempDir {
            fn new(label: &str) -> Self {
                let p = std::env::temp_dir().join(format!(
                    "wdg-staging-{label}-{}-{}",
                    std::process::id(),
                    std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap()
                        .subsec_nanos()
                ));
                fs::create_dir_all(&p).unwrap();
                TempDir(p)
            }

            fn chain(&self) -> PathBuf {
                self.0.join("chain")
            }

            fn staging(&self) -> PathBuf {
                self.chain().join(STAGING_ENTRY)
            }
        }

        impl Drop for TempDir {
            fn drop(&mut self) {
                let _ = fs::remove_dir_all(&self.0);
            }
        }

        /// A staging entry in `t`'s chain directory holding part of a
        /// download, with a marker naming `marker` and a staged
        /// `protocolMagicId` naming `staged`, each when given.
        fn make_staging(t: &TempDir, marker: Option<&str>, staged: Option<&str>) {
            let staged_db = t.staging().join("download").join("db");
            fs::create_dir_all(staged_db.join("immutable")).unwrap();
            fs::write(
                staged_db.join("immutable").join("00000.chunk"),
                b"in flight",
            )
            .unwrap();
            if let Some(magic) = marker {
                fs::write(t.staging().join(STAGING_NETWORK_MARKER), magic).unwrap();
            }
            if let Some(magic) = staged {
                fs::write(staged_db.join("protocolMagicId"), magic).unwrap();
            }
        }

        /// Every entry under `dir` with the contents of each file.
        fn tree(dir: &Path) -> BTreeMap<PathBuf, Option<Vec<u8>>> {
            let mut entries = BTreeMap::new();
            let mut stack = vec![dir.to_path_buf()];
            while let Some(d) = stack.pop() {
                for entry in fs::read_dir(&d).unwrap() {
                    let path = entry.unwrap().path();
                    let rel = path.strip_prefix(dir).unwrap().to_path_buf();
                    if path.is_dir() {
                        entries.insert(rel, None);
                        stack.push(path);
                    } else {
                        entries.insert(rel, Some(fs::read(&path).unwrap()));
                    }
                }
            }
            entries
        }

        fn config(t: &TempDir, network_magic: Option<u32>) -> MithrilConfig {
            let mut cfg: MithrilConfig = serde_json::from_value(serde_json::json!({
                "mithril_bin": "m",
                "snapshot_converter_bin": "c",
                "converter_config": "cfg",
                "aggregator_url": "u",
                "genesis_vkey": "g",
                "state_dir": t.0.to_str().unwrap(),
                "chain_path": t.chain().to_str().unwrap(),
            }))
            .unwrap();
            cfg.network_magic = network_magic;
            cfg
        }

        /// Whether startup keeps a staging entry with `marker` and `staged`,
        /// in a chain directory whose database names `chain`.
        async fn kept_at_startup(
            marker: Option<&str>,
            staged: Option<&str>,
            chain: Option<&str>,
        ) -> bool {
            let t = TempDir::new("startup");
            make_staging(&t, marker, staged);
            if let Some(magic) = chain {
                fs::write(t.chain().join("protocolMagicId"), magic).unwrap();
            }
            remove_stale_staging(&config(&t, Some(THIS))).await;
            t.staging().exists()
        }

        #[tokio::test]
        async fn a_download_is_prepared_with_the_network_beside_it() {
            let t = TempDir::new("prepare");
            prepare_staging(&t.staging(), Some(THIS), Duration::ZERO)
                .await
                .unwrap();

            assert_eq!(
                fs::read_to_string(t.staging().join(STAGING_NETWORK_MARKER)).unwrap(),
                "2"
            );
            assert!(t.staging().join("download").is_dir());
            assert!(!t.staging().join(STAGING_NETWORK_MARKER_TEMP).exists());
            assert_eq!(
                tree(&t.staging()).into_keys().collect::<Vec<_>>(),
                vec![
                    PathBuf::from("download"),
                    PathBuf::from(STAGING_NETWORK_MARKER)
                ]
            );
        }

        #[tokio::test]
        async fn a_leftover_of_this_network_or_of_none_is_replaced() {
            for (marker, staged) in [
                (Some("2"), None),
                (None, Some("2")),
                (Some("2"), Some("2")),
                (None, None),
            ] {
                let t = TempDir::new("replace");
                make_staging(&t, marker, staged);
                prepare_staging(&t.staging(), Some(THIS), Duration::ZERO)
                    .await
                    .unwrap();
                assert!(
                    !t.staging().join("download").join("db").exists(),
                    "marker {marker:?}, staged {staged:?}: the leftover was kept"
                );
                assert_eq!(
                    fs::read_to_string(t.staging().join(STAGING_NETWORK_MARKER)).unwrap(),
                    "2"
                );
            }
        }

        #[tokio::test]
        async fn a_download_of_another_network_is_refused_and_left_as_it_is() {
            for (marker, staged) in [(Some(OTHER), None), (None, Some(OTHER))] {
                let t = TempDir::new("refuse");
                make_staging(&t, marker, staged);
                let before = tree(&t.staging());

                let refused = prepare_staging(&t.staging(), Some(THIS), Duration::ZERO).await;

                assert!(
                    matches!(
                        refused,
                        Err(StagingRefused::OtherNetwork {
                            found: 1,
                            expected: THIS
                        })
                    ),
                    "marker {marker:?}, staged {staged:?}: {refused:?}"
                );
                assert_eq!(tree(&t.staging()), before);
            }
        }

        #[tokio::test]
        async fn without_a_known_network_a_leftover_is_replaced_and_nothing_marked() {
            let t = TempDir::new("unknown");
            make_staging(&t, Some(OTHER), None);
            prepare_staging(&t.staging(), None, Duration::ZERO)
                .await
                .unwrap();
            assert_eq!(
                tree(&t.staging()).into_keys().collect::<Vec<_>>(),
                vec![PathBuf::from("download")]
            );
        }

        #[tokio::test]
        async fn the_marker_names_the_network_before_the_staged_database() {
            let t = TempDir::new("read");
            assert_eq!(staging_network_magic(&t.staging()).await, None);
            make_staging(&t, None, Some(OTHER));
            assert_eq!(staging_network_magic(&t.staging()).await, Some(1));
            fs::write(t.staging().join(STAGING_NETWORK_MARKER), "2\n").unwrap();
            assert_eq!(staging_network_magic(&t.staging()).await, Some(2));
            // A marker that holds no number names no network.
            fs::write(t.staging().join(STAGING_NETWORK_MARKER), "").unwrap();
            assert_eq!(staging_network_magic(&t.staging()).await, Some(1));
        }

        #[tokio::test]
        async fn startup_deletes_a_leftover_of_this_network() {
            assert!(!kept_at_startup(Some("2"), None, None).await);
            assert!(!kept_at_startup(None, Some("2"), None).await);
        }

        #[tokio::test]
        async fn startup_keeps_a_download_of_another_network() {
            assert!(kept_at_startup(Some(OTHER), None, None).await);
            assert!(kept_at_startup(None, Some(OTHER), None).await);
        }

        #[tokio::test]
        async fn startup_deletes_a_staging_entry_that_names_no_network() {
            assert!(!kept_at_startup(None, None, None).await);
            assert!(!kept_at_startup(None, None, Some("2")).await);
        }

        #[tokio::test]
        async fn startup_keeps_an_unnamed_staging_entry_beside_another_networks_database() {
            assert!(kept_at_startup(None, None, Some(OTHER)).await);
        }

        #[tokio::test]
        async fn the_marker_decides_over_the_chain_directory_at_startup() {
            assert!(!kept_at_startup(Some("2"), None, Some(OTHER)).await);
            assert!(kept_at_startup(Some(OTHER), None, Some("2")).await);
        }

        #[tokio::test]
        async fn startup_without_a_known_network_deletes_any_staging_entry() {
            let t = TempDir::new("startup-unknown");
            make_staging(&t, Some(OTHER), None);
            remove_stale_staging(&config(&t, None)).await;
            assert!(!t.staging().exists());
        }

        #[tokio::test]
        async fn a_sync_is_refused_over_a_download_of_another_network() {
            let t = TempDir::new("allowed");
            make_staging(&t, Some(OTHER), None);
            let before = tree(&t.chain());
            assert!(!network_allowed(&t.chain(), Some(THIS), true).await);
            assert_eq!(tree(&t.chain()), before);

            fs::write(t.staging().join(STAGING_NETWORK_MARKER), "2").unwrap();
            assert!(network_allowed(&t.chain(), Some(THIS), true).await);
            fs::remove_file(t.staging().join(STAGING_NETWORK_MARKER)).unwrap();
            assert!(network_allowed(&t.chain(), Some(THIS), true).await);
            assert!(network_allowed(&t.chain(), None, true).await);
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
            STAGING_ENTRY, check_replaceable, copy_dir_recursive, install_entries, install_staged,
            move_dir, rename_with_retry,
        };
        use std::fs;
        use std::path::{Path, PathBuf};
        use std::time::{Duration, Instant};

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

            let err = install_staged(&staging, &chain, false, Duration::ZERO)
                .await
                .unwrap_err();

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

            install_staged(&staging, &chain, false, Duration::ZERO)
                .await
                .unwrap();

            assert!(chain.join("immutable").is_dir());
            assert_eq!(fs::read(chain.join("protocolMagicId")).unwrap(), b"1");
            assert!(!chain.join("lock").exists());
        }

        #[tokio::test]
        async fn installs_into_a_missing_directory() {
            let t = TempDir::new("missing");
            let chain = t.0.join("chain");
            let staging = make_staging(&t.0);
            install_staged(&staging, &chain, false, Duration::ZERO)
                .await
                .unwrap();
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

            install_staged(&staging, &chain, false, Duration::ZERO)
                .await
                .unwrap();

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

            assert!(install_entries(&db, &dst, Duration::ZERO).await.is_err());

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
                    tokio::time::sleep(Duration::from_millis(250)).await;
                    fs::set_permissions(&dir, fs::Permissions::from_mode(0o755)).unwrap();
                })
            };
            let renamed =
                rename_with_retry(&src, &dir.join("00000.chunk"), Duration::from_secs(10)).await;
            unlock.await.unwrap();
            renamed.unwrap();
            assert_eq!(fs::read(dir.join("00000.chunk")).unwrap(), b"chunk");
            assert!(!src.exists());
        }

        #[tokio::test]
        async fn a_rename_that_keeps_failing_gives_up_once_its_time_is_spent() {
            let t = TempDir::new("rename-gives-up");
            let src = t.0.join("missing");
            let budget = Duration::from_millis(400);
            let started = Instant::now();
            assert!(
                rename_with_retry(&src, &t.0.join("dst"), budget)
                    .await
                    .is_err()
            );
            let spent = started.elapsed();
            assert!(spent >= budget, "gave up after {spent:?}");
            assert!(spent < budget * 5, "kept trying for {spent:?}");
        }

        #[tokio::test]
        async fn a_rename_without_time_to_retry_is_tried_once() {
            let t = TempDir::new("rename-once");
            let started = Instant::now();
            assert!(
                rename_with_retry(&t.0.join("missing"), &t.0.join("dst"), Duration::ZERO)
                    .await
                    .is_err()
            );
            assert!(started.elapsed() < Duration::from_millis(100));
        }

        #[tokio::test]
        async fn a_failed_rename_on_one_volume_is_not_copied() {
            let t = TempDir::new("no-copy");
            let src = t.0.join("staged-ledger");
            fs::create_dir_all(&src).unwrap();
            fs::write(src.join("state"), b"new").unwrap();
            // A directory cannot be renamed over one that is not empty, while
            // a copy into it would succeed.
            let dst = t.0.join("ledger");
            fs::create_dir_all(&dst).unwrap();
            fs::write(dst.join("old"), b"old").unwrap();

            assert!(move_dir(&src, &dst, Duration::ZERO).await.is_err());

            assert!(!dst.join("state").exists(), "the directory was copied");
            assert_eq!(fs::read(src.join("state")).unwrap(), b"new");
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

            assert!(
                install_staged(&staging, &chain, false, Duration::ZERO)
                    .await
                    .is_err()
            );

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

            install_staged(&staging, &chain, false, Duration::ZERO)
                .await
                .unwrap();

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
        use std::time::Duration;

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
            install_staged(&staging, &chain_path, false, Duration::ZERO)
                .await
                .unwrap();

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
            install_staged(&staging, &chain_path, false, Duration::ZERO)
                .await
                .unwrap();

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
            install_staged(&staging, &chain_path, false, Duration::ZERO)
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
            install_staged(&staging, &chain_path, false, Duration::ZERO)
                .await
                .unwrap();

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
