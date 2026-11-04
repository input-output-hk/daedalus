use std::io::Write;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{SystemTime, UNIX_EPOCH};

use file_rotate::compression::Compression;
use file_rotate::suffix::AppendCount;
use file_rotate::{ContentLimit, FileRotate};

use anyhow::Result;
use tokio::io::{AsyncBufReadExt, AsyncReadExt, BufReader};
use tokio::net::TcpStream;
use tokio::process::{Child, ChildStdin, Command};
use tokio::sync::{mpsc, oneshot, watch};
use tokio::time::{Duration, Instant, MissedTickBehavior, interval, interval_at, sleep, timeout};
use tracing::{info, warn};

#[cfg(unix)]
use nix::sys::signal::Signal;

use crate::chain_damage::{ChainDamageDetector, Detection, SharedDetector};
use crate::chain_validation;
use crate::config::WatchdogConfig;
use crate::crash_window::CrashWindow;
use crate::ledger_point::{self, Verdict};
use crate::mithril;
use crate::protocol::{ChainDamageKind, Command as Cmd, ElectronRestartPayload, Event, emit};
use crate::state;

type ExitInfo = (Option<i32>, Option<String>);

enum RunResult {
    Stopped,
    NodeCrashed,
    RestartRequested,
    StartMithril { force: bool, wipe_chain: bool },
}

/// How the Mithril pipeline before a node start ended, when the renderer has
/// still to be told. The Mithril view stays up until the node has started
/// again; then `completed` closes it.
#[derive(Clone, Copy, PartialEq, Eq)]
enum AfterMithril {
    /// No pipeline ran, or the renderer already has its outcome.
    Nothing,
    /// A database was installed; `rebuild` is true when a rebuild installed
    /// it, which `completed` passes on.
    Installed { rebuild: bool },
    /// The local chain is close enough to the certified tip, and nothing was
    /// changed. Nothing else ends the renderer's Mithril view in this case.
    NotNeeded,
}

fn unix_ms() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as u64
}

fn extract_exit(status: Option<std::process::ExitStatus>) -> ExitInfo {
    let Some(s) = status else {
        return (None, None);
    };
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if let Some(sig) = s.signal() {
            let name = Signal::try_from(sig).ok().map(|s| s.as_str().to_string());
            return (None, name);
        }
    }
    (s.code(), None)
}

// Spawns a chain-directory validation task. Non-blocking: emits ChainDirValidation
// when done. Can be called from any command-receive arm without stalling the loop.
fn spawn_validate_chain_dir(
    state_dir: String,
    path: String,
    default_chain_path: String,
    required_space_bytes: u64,
) {
    tokio::spawn(async move {
        let result = chain_validation::validate_chain_storage_directory(
            std::path::Path::new(&path),
            std::path::Path::new(&state_dir),
            std::path::Path::new(&default_chain_path),
            required_space_bytes,
        )
        .await;
        emit(&Event::ChainDirValidation {
            is_valid: result.is_valid,
            path: result.path.map(|p| p.to_string_lossy().into_owned()),
            resolved_path: result
                .resolved_path
                .map(|p| p.to_string_lossy().into_owned()),
            reason: result.reason.map(|s| s.to_owned()),
            available_space_bytes: result.available_space_bytes,
            required_space_bytes: result.required_space_bytes,
        });
    });
}

// Helper: emit an Error event.
fn emit_error(msg: &str) {
    warn!("{msg}");
    emit(&Event::Error {
        message: msg.to_string(),
    });
}

// Ensure a child process cannot outlive the watchdog itself. kill_on_drop
// only covers an orderly runtime teardown; if the watchdog is SIGKILLed the
// children would otherwise keep running (an orphaned cardano-wallet holds the
// wallet DB open, and the next Daedalus start would spawn a second wallet
// against the same database).
//
// Linux: PR_SET_PDEATHSIG delivers SIGKILL to the child when the spawning
// thread dies. Tokio worker threads live for the lifetime of the runtime, so
// in practice this fires on watchdog death. The getppid() check closes the
// race where the watchdog dies between fork and prctl.
//
// Windows: children inherit the watchdog's job object, created in
// init_job_object() with JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE — nothing to do
// per child.
//
// macOS has no PDEATHSIG equivalent; there the TypeScript driver kills the
// child PIDs it has been told about before force-killing the watchdog.
pub(crate) fn tether_to_watchdog(cmd: &mut Command) {
    #[cfg(target_os = "linux")]
    {
        let watchdog_pid = std::process::id() as libc::pid_t;
        unsafe {
            cmd.pre_exec(move || {
                if libc::prctl(libc::PR_SET_PDEATHSIG, libc::SIGKILL) != 0 {
                    return Err(std::io::Error::last_os_error());
                }
                if libc::getppid() != watchdog_pid {
                    return Err(std::io::Error::other("watchdog died before spawn"));
                }
                Ok(())
            });
        }
    }
    // Suppress blank console windows for child processes. Watchdog is a GUI-subsystem
    // app (no console), so any console-app child would otherwise get its own visible
    // console. CREATE_NO_WINDOW keeps them hidden. creation_flags() replaces the
    // previous value rather than adding to it, so a caller that needs further
    // flags must call it after this function and include CREATE_NO_WINDOW itself.
    #[cfg(windows)]
    {
        const CREATE_NO_WINDOW: u32 = 0x0800_0000;
        cmd.creation_flags(CREATE_NO_WINDOW);
    }
    #[cfg(not(any(target_os = "linux", windows)))]
    let _ = cmd;
}

// Put the watchdog into a kill-on-close job object so every child it spawns
// dies with it, even on TerminateProcess. Called once at startup; the job
// handle is intentionally leaked — the OS closes it when the watchdog dies,
// which is exactly the trigger we want.
#[cfg(windows)]
pub(crate) fn init_job_object() {
    use windows_sys::Win32::System::JobObjects::{
        AssignProcessToJobObject, CreateJobObjectW, JOB_OBJECT_LIMIT_BREAKAWAY_OK,
        JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE, JOBOBJECT_EXTENDED_LIMIT_INFORMATION,
        JobObjectExtendedLimitInformation, SetInformationJobObject,
    };
    use windows_sys::Win32::System::Threading::GetCurrentProcess;
    unsafe {
        let job = CreateJobObjectW(std::ptr::null(), std::ptr::null());
        if job.is_null() {
            warn!("CreateJobObjectW failed; children may outlive the watchdog");
            return;
        }
        let mut info: JOBOBJECT_EXTENDED_LIMIT_INFORMATION = std::mem::zeroed();
        // BREAKAWAY_OK lets a process that asks for it (CREATE_BREAKAWAY_FROM_JOB)
        // leave the job: only the update installer does, so it outlives the
        // watchdog. Node, wallet and Electron do not ask and stay tethered.
        info.BasicLimitInformation.LimitFlags =
            JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE | JOB_OBJECT_LIMIT_BREAKAWAY_OK;
        if SetInformationJobObject(
            job,
            JobObjectExtendedLimitInformation,
            &info as *const _ as *const _,
            std::mem::size_of::<JOBOBJECT_EXTENDED_LIMIT_INFORMATION>() as u32,
        ) == 0
        {
            warn!("SetInformationJobObject failed; children may outlive the watchdog");
            return;
        }
        if AssignProcessToJobObject(job, GetCurrentProcess()) == 0 {
            warn!("AssignProcessToJobObject failed; children may outlive the watchdog");
        }
    }
}

/// cardano-wallet `serve` flag that makes the wallet shut down cleanly when its
/// stdin reaches end-of-file. The watchdog holds the write end of that pipe and
/// closes it to stop the wallet. This works on every platform; a console
/// control event cannot, because the watchdog runs without a console on
/// Windows.
const WALLET_SHUTDOWN_HANDLER_FLAG: &str = "--shutdown-handler";

/// `BackendStopProgress` stage while cardano-wallet is being stopped.
const STAGE_STOPPING_WALLET: &str = "stopping_wallet";
/// `BackendStopProgress` stage while cardano-node is being stopped.
const STAGE_STOPPING_NODE: &str = "stopping_node";
/// Interval between `BackendStopProgress` events within one stage.
const STOP_PROGRESS_INTERVAL: Duration = Duration::from_secs(1);

fn emit_stop_progress(stage: &str, started: Instant, bound: Duration) {
    emit(&Event::BackendStopProgress {
        stage: stage.to_string(),
        elapsed_ms: started.elapsed().as_millis() as u64,
        timeout_ms: bound.as_millis() as u64,
    });
}

// Ticks once per STOP_PROGRESS_INTERVAL, the first tick one interval from now:
// each stage reports its start itself.
fn stop_progress_ticks() -> tokio::time::Interval {
    interval_at(
        Instant::now() + STOP_PROGRESS_INTERVAL,
        STOP_PROGRESS_INTERVAL,
    )
}

// Asks cardano-wallet to stop by closing its stdin, waits at most `bound` for
// it to exit, then kills it. Emits stop progress once a second while waiting.
async fn stop_wallet(wallet: &mut Child, wallet_stdin: &mut Option<ChildStdin>, bound: Duration) {
    let started = Instant::now();
    emit_stop_progress(STAGE_STOPPING_WALLET, started, bound);
    drop(wallet_stdin.take());
    let deadline = sleep(bound);
    tokio::pin!(deadline);
    let mut tick = stop_progress_ticks();
    let force_killed = loop {
        tokio::select! {
            _ = wallet.wait() => break false,
            _ = tick.tick() => emit_stop_progress(STAGE_STOPPING_WALLET, started, bound),
            _ = &mut deadline => {
                warn!("cardano-wallet did not exit within {}s; killing it", bound.as_secs());
                let _ = wallet.start_kill();
                let _ = wallet.wait().await;
                break true;
            }
        }
    };
    info!(
        "wallet stopped in {}ms (force_killed={force_killed})",
        started.elapsed().as_millis()
    );
}

// Waits for the node-watcher channel to report that cardano-node has exited,
// for at most `bound`. A node still running then is killed through `kill_tx`.
// Returns true when the node had to be killed. Emits stop progress once a
// second while waiting.
async fn wait_for_node_exit(
    node_rx: &watch::Receiver<Option<ExitInfo>>,
    kill_tx: &mut Option<oneshot::Sender<()>>,
    bound: Duration,
) -> bool {
    let started = Instant::now();
    emit_stop_progress(STAGE_STOPPING_NODE, started, bound);
    let mut rx = node_rx.clone();
    let deadline = sleep(bound);
    tokio::pin!(deadline);
    let mut tick = stop_progress_ticks();
    loop {
        if rx.borrow_and_update().is_some() {
            return false;
        }
        tokio::select! {
            changed = rx.changed() => {
                if changed.is_err() {
                    return false;
                }
            }
            _ = tick.tick() => emit_stop_progress(STAGE_STOPPING_NODE, started, bound),
            _ = &mut deadline => break,
        }
    }
    warn!(
        "cardano-node did not exit within {}s of the stop request; killing it",
        bound.as_secs()
    );
    if let Some(tx) = kill_tx.take() {
        let _ = tx.send(());
    }
    while rx.borrow_and_update().is_none() {
        if rx.changed().await.is_err() {
            break;
        }
    }
    true
}

// The user settings the watchdog persists (chain path, node arguments,
// Electron flags) and the config they are applied to.
struct Settings<'a> {
    config: &'a mut WatchdogConfig,
    base_node_args: &'a [String],
    wstate: &'a mut state::WatchdogState,
}

impl Settings<'_> {
    async fn save_and_apply(&mut self) {
        if let Err(e) = state::save(&self.config.node.state_dir, self.wstate).await {
            warn!("Failed to save watchdog state: {e}");
        }
        state::apply_to_config(self.config, self.base_node_args, self.wstate);
    }
}

// Requests that arrive while the node is stopping, or while the watchdog
// waits to restart it. They are held until the node is gone and then decide
// what happens next, so a stop never lands on a node started after it.
#[derive(Default)]
struct HeldRequests {
    stop: bool,
    /// `force` and `wipe_chain` of the Mithril sync to run, merged over every
    /// request held.
    mithril: Option<(bool, bool)>,
}

/// One Mithril sync for two requests. A later request never turns off a
/// rebuild (`wipe_chain`) or a forced sync that an earlier one asked for: a
/// plain sync in place of a confirmed rebuild would sync over the damage the
/// rebuild was to replace.
fn merge_mithril(earlier: (bool, bool), later: (bool, bool)) -> (bool, bool) {
    (earlier.0 || later.0, earlier.1 || later.1)
}

impl HeldRequests {
    async fn take(&mut self, cmd: Cmd, settings: &mut Settings<'_>) {
        match cmd {
            Cmd::Stop => {
                info!("stop requested; the node will not be started again");
                self.stop = true;
            }
            Cmd::StartMithril { force, wipe_chain } => {
                info!("Mithril sync requested; it runs before the node starts again");
                self.mithril = Some(match self.mithril {
                    Some(earlier) => merge_mithril(earlier, (force, wipe_chain)),
                    None => (force, wipe_chain),
                });
            }
            Cmd::SetChainPath { path } => {
                settings.wstate.chain_path = path;
                info!("chain_path updated: {:?}", settings.wstate.chain_path);
                settings.save_and_apply().await;
            }
            Cmd::SetNodeExtraArgs { args } => {
                settings.wstate.node_extra_args = args;
                info!(
                    "node_extra_args updated (RTS flags): {:?}",
                    settings.wstate.node_extra_args
                );
                settings.save_and_apply().await;
            }
            Cmd::SetElectronFlags { flags } => {
                settings.wstate.electron_flags = flags;
                info!(
                    "electron_flags updated (blank screen fix), applied at the next start: {:?}",
                    settings.wstate.electron_flags
                );
                settings.save_and_apply().await;
            }
            Cmd::ValidateChainDir {
                path,
                default_chain_path,
                required_space_bytes,
            } => {
                spawn_validate_chain_dir(
                    settings.config.node.state_dir.clone(),
                    path,
                    default_chain_path,
                    required_space_bytes,
                );
            }
            // Sent periodically by the UI; there is no running node to probe for.
            Cmd::ProbeMithril => {}
            other => info!("ignoring {other:?} while the node is stopped or stopping"),
        }
    }

    // What a held request asks for instead of the planned next step, if anything.
    fn instead(&self) -> Option<RunResult> {
        if self.stop {
            Some(RunResult::Stopped)
        } else {
            self.mithril
                .map(|(force, wipe_chain)| RunResult::StartMithril { force, wipe_chain })
        }
    }

    // The next step once the node has exited: `planned`, unless a held request
    // replaces it. A planned stop is never replaced, and a planned Mithril
    // sync is merged with a held one.
    fn after(&self, planned: RunResult) -> RunResult {
        match (planned, self.mithril) {
            (RunResult::Stopped, _) => RunResult::Stopped,
            (RunResult::StartMithril { force, wipe_chain }, Some(held)) if !self.stop => {
                let (force, wipe_chain) = merge_mithril((force, wipe_chain), held);
                RunResult::StartMithril { force, wipe_chain }
            }
            (planned, _) => self.instead().unwrap_or(planned),
        }
    }
}

// Asks cardano-node to stop, waits for it to exit within the configured
// bound (killing it after that), and reports how long it took. Commands that
// arrive meanwhile are held and returned, not acted on against a later node.
async fn stop_node(
    reason: &str,
    shutdown_pipe: &mut ShutdownPipe,
    node_rx: &watch::Receiver<Option<ExitInfo>>,
    kill_tx: &mut Option<oneshot::Sender<()>>,
    cmd_rx: &mut mpsc::Receiver<Cmd>,
    settings: &mut Settings<'_>,
) -> HeldRequests {
    info!("stopping node ({reason})");
    let bound = Duration::from_secs(settings.config.node.stop_timeout_secs);
    let shutdown_start = unix_ms();
    shutdown_pipe.close_write();
    let mut held = HeldRequests::default();
    let exited = wait_for_node_exit(node_rx, kill_tx, bound);
    tokio::pin!(exited);
    let force_killed = loop {
        tokio::select! {
            force_killed = &mut exited => break force_killed,
            Some(cmd) = cmd_rx.recv() => held.take(cmd, settings).await,
        }
    };
    while let Ok(cmd) = cmd_rx.try_recv() {
        held.take(cmd, settings).await;
    }
    let shutdown_ms = unix_ms() - shutdown_start;
    info!("node shut down in {shutdown_ms}ms (force_killed={force_killed})");
    emit(&Event::NodeShutdownMs {
        ms: shutdown_ms,
        force_killed,
    });
    held
}

// Crash history kept across node and wallet restarts.
struct CrashCounters {
    // Consecutive node exits before the node reached chainDbReady.
    node_consecutive: u32,
    node_window: CrashWindow,
    wallet_window: CrashWindow,
}

impl CrashCounters {
    fn new(config: &WatchdogConfig) -> Self {
        Self {
            node_consecutive: 0,
            node_window: CrashWindow::new(
                config.node.max_crashes_in_window,
                Duration::from_secs(config.node.crash_window_secs),
            ),
            wallet_window: CrashWindow::new(
                config.wallet.max_crashes_in_window,
                Duration::from_secs(config.wallet.crash_window_secs),
            ),
        }
    }

    // Forgets all crashes, when the user restarts or retries.
    fn clear(&mut self) {
        self.node_consecutive = 0;
        self.node_window.clear();
        self.wallet_window.clear();
    }
}

// Records an unexpected wallet exit. Returns true when the wallet has failed
// too often to be restarted again without the user asking: `attempt`
// consecutive failed starts, or the crash window exhausted.
fn wallet_exhausted(
    attempt: u32,
    crashes: &mut CrashCounters,
    wallet_cfg: &crate::config::WalletConfig,
) -> bool {
    let window_exhausted = crashes.wallet_window.record(std::time::Instant::now());
    if attempt >= wallet_cfg.max_restart_attempts || window_exhausted {
        warn!(
            "wallet unrecoverable: {attempt} consecutive failed starts, {} crashes within {}s",
            crashes.wallet_window.count(),
            wallet_cfg.crash_window_secs
        );
        return true;
    }
    false
}

// Waits, once cardano-node is unrecoverable or while the ledger snapshot
// check holds it back, for the user to retry (restart_node, start_node or
// restart_wallet), start a Mithril sync, or stop. Settings changes are saved
// and apply when the node next starts.
async fn wait_for_retry(
    cmd_rx: &mut mpsc::Receiver<Cmd>,
    settings: &mut Settings<'_>,
) -> HeldRequests {
    let mut held = HeldRequests::default();
    loop {
        match cmd_rx.recv().await {
            Some(Cmd::RestartNode | Cmd::StartNode | Cmd::RestartWallet) => return held,
            Some(cmd) => {
                held.take(cmd, settings).await;
                if held.instead().is_some() {
                    return held;
                }
            }
            None => {
                held.stop = true;
                return held;
            }
        }
    }
}

/// How the wait that follows a failed install ended.
enum InstallRetry {
    /// A Mithril sync installed a database; `rebuild` is true when it was a
    /// rebuild.
    Installed { rebuild: bool },
    /// A rebuild began deleting the chain and installed nothing.
    Wiped,
    /// The watchdog is to stop.
    Stopped,
}

/// Waits after a Mithril install failed part-way. The chain may be partly
/// replaced, so no node starts on it and `start_node` is ignored: the wait
/// ends only when a Mithril sync installs a database, or when a rebuild
/// deletes the chain and installs nothing, which leaves no chain at all.
/// Settings changes are saved and applied, so a chain location chosen
/// meanwhile is the one the next sync installs into.
async fn await_install_retry(
    cmd_rx: &mut mpsc::Receiver<Cmd>,
    settings: &mut Settings<'_>,
) -> InstallRetry {
    use mithril::PipelineResult;
    loop {
        match cmd_rx.recv().await {
            Some(Cmd::StartMithril { force, wipe_chain }) => {
                let Some(mc) = settings.config.mithril.clone() else {
                    continue;
                };
                match mithril::run_pipeline(&mc, cmd_rx, force, wipe_chain).await {
                    PipelineResult::Installed { rebuild } => {
                        return InstallRetry::Installed { rebuild };
                    }
                    PipelineResult::FailedAfterWipe => return InstallRetry::Wiped,
                    PipelineResult::Stopped => return InstallRetry::Stopped,
                    PipelineResult::NotNeeded
                    | PipelineResult::Cancelled
                    | PipelineResult::UserCancelled
                    | PipelineResult::Failed => {}
                }
            }
            Some(Cmd::Stop) | None => return InstallRetry::Stopped,
            Some(Cmd::StartNode) => info!(
                "start_node ignored: the failed install left no chain to start the node on; \
                 waiting for a Mithril sync to install one"
            ),
            Some(cmd) => HeldRequests::default().take(cmd, settings).await,
        }
    }
}

// Waits `delay` before a crashed node is restarted, holding the commands that
// arrive meanwhile. A stop ends the wait at once.
async fn hold_requests_for(
    delay: Duration,
    cmd_rx: &mut mpsc::Receiver<Cmd>,
    settings: &mut Settings<'_>,
) -> HeldRequests {
    let mut held = HeldRequests::default();
    let wait = sleep(delay);
    tokio::pin!(wait);
    while !held.stop {
        tokio::select! {
            _ = &mut wait => break,
            Some(cmd) = cmd_rx.recv() => held.take(cmd, settings).await,
        }
    }
    held
}

type RotatingLog = Arc<Mutex<FileRotate<AppendCount>>>;

fn open_log(path: &str) -> RotatingLog {
    Arc::new(Mutex::new(FileRotate::new(
        path,
        AppendCount::new(4),
        ContentLimit::Bytes(5 * 1024 * 1024),
        Compression::None,
        None,
    )))
}

// Node startup sub-states, in order of expected progression.
// chainDbReady is the terminal state that gates wallet startup.
#[derive(Debug)]
enum NodeStartupState {
    Init,
    OpeningChainDb,
    OpeningImmutableDb,
    OpenedImmutableDb,
    OpeningVolatileDb,
    OpenedVolatileDb,
    OpeningLedgerDb,
    ReplayingLedger,
    OpenedLedgerDb,
}

impl NodeStartupState {
    fn as_phase_str(&self) -> Option<&'static str> {
        match self {
            NodeStartupState::Init => None,
            NodeStartupState::OpeningChainDb => Some("openingChainDb"),
            NodeStartupState::OpeningImmutableDb => Some("openingImmutableDb"),
            NodeStartupState::OpenedImmutableDb => Some("openedImmutableDb"),
            NodeStartupState::OpeningVolatileDb => Some("openingVolatileDb"),
            NodeStartupState::OpenedVolatileDb => Some("openedVolatileDb"),
            NodeStartupState::OpeningLedgerDb => Some("openingLedgerDb"),
            NodeStartupState::ReplayingLedger => Some("replayingLedger"),
            NodeStartupState::OpenedLedgerDb => Some("openedLedgerDb"),
        }
    }
}

/// Parse the ChainDB startup phase from a cardano-node log line.
/// Returns a camelCase phase key matching the TypeScript `NodeStartupPhase` type.
fn try_parse_startup_status(line: &str) -> Option<&'static str> {
    // More-specific event names before shorter ones to avoid accidental substring matches.
    if line.contains("StartedOpeningImmutableDB") {
        Some("openingImmutableDb")
    } else if line.contains("OpenedImmutableDB") {
        Some("openedImmutableDb")
    } else if line.contains("StartedOpeningVolatileDB") {
        Some("openingVolatileDb")
    } else if line.contains("OpenedVolatileDB") {
        Some("openedVolatileDb")
    } else if line.contains("StartedOpeningLgrDB") {
        Some("openingLedgerDb")
    } else if line.contains("OpenedLgrDB") {
        Some("openedLedgerDb")
    } else if line.contains("StartedOpeningDB") {
        Some("openingChainDb")
    } else if line.contains("OpenedDB") {
        Some("chainDbReady")
    } else if line.contains("ReplayFromGenesis") || line.contains("Replaying ledger from genesis") {
        Some("replayingLedger")
    } else {
        None
    }
}

/// Parse block-replay/validation progress from a cardano-node log line.
/// Returns `(kind, percentage)` when the line matches, where `kind` is one of
/// `"replayedBlock"`, `"validatingChunk"`, or `"pushingLedger"` — the same
/// camelCase strings used by the `BlockSyncType` enum on the TypeScript side.
fn try_parse_block_sync(line: &str) -> Option<(&'static str, f64)> {
    let kind = if line.contains("Replayed block") {
        "replayedBlock"
    } else if line.contains("Validating chunk") || line.contains("Validated chunk") {
        "validatingChunk"
    } else if line.contains("Pushing ledger state") {
        "pushingLedger"
    } else {
        return None;
    };

    // Extract the number from "Progress: 0.01%" (commas allowed for European locales)
    let after = line.split_once("Progress:")?.1;
    let pct_end = after.find('%')?;
    let num_str: String = after[..pct_end]
        .chars()
        .filter(|c| c.is_ascii_digit() || *c == '.' || *c == ',')
        .collect::<String>()
        .replace(',', ".");
    let progress: f64 = num_str.parse().ok()?;
    Some((kind, progress))
}

/// Environment variable trace-dispatcher reads for the host field of each trace line.
const TRACE_HOSTNAME_ENV: &str = "TRACE_DISPATCHER_LOGGING_HOSTNAME";

/// Value for `TRACE_HOSTNAME_ENV`. Matches `TraceOptionNodeName` in the shipped
/// node configuration (`perSystem/common.nix`).
const TRACE_HOSTNAME: &str = "daedalus";

/// Longest run of child output held in memory before it is logged. A line
/// longer than this is split into consecutive chunks, each written to the log
/// as it fills, so a child that never writes a newline cannot grow the buffer.
const MAX_LOG_LINE_BYTES: u64 = 1024 * 1024;

/// How long a child may leave a line without its newline before `damage`
/// reads what has arrived. cardano-node writes the missing ledger point error
/// without a newline, running into the text that follows it, so the line
/// would otherwise reach the detector only when the node exits.
const PARTIAL_LINE_WAIT: Duration = Duration::from_millis(500);

/// Bytes a child has written so far, counted as they arrive, for the startup
/// silence watch. Shared by the stdout and stderr readers of one process.
type OutputCount = Arc<AtomicU64>;

/// How often the startup silence watch looks at the node's output, at most.
const SILENCE_TICK_MAX: Duration = Duration::from_secs(5);

/// Ticks of the silence watch that may pass without one arriving before the
/// gap is taken for the machine sleeping: 30 s at the default tick.
const SILENCE_TICKS_BEFORE_SLEEP: u32 = 6;

/// A change in a starting node's output that the silence watch reports.
#[derive(Debug, PartialEq, Eq)]
enum Silence {
    /// No byte has arrived for the limit.
    Began,
    /// Output arrived after a reported silence, which lasted this long.
    Ended(Duration),
}

/// Watches a starting node's output for a silence of `limit`, reported once
/// until output arrives again. The watch runs on the watchdog's own timer: a
/// tick that arrives long after the one before, by the monotonic or the wall
/// clock, means the machine slept, and the silence is counted again from
/// then rather than reported. A clock that does not advance while the
/// machine sleeps does not count the sleep either.
struct SilenceWatch {
    limit: Duration,
    tick: Duration,
    output: u64,
    quiet_since: Instant,
    last_tick: (Instant, SystemTime),
    reported: bool,
}

impl SilenceWatch {
    fn new(limit: Duration, now: Instant, wall: SystemTime) -> Self {
        SilenceWatch {
            limit,
            tick: (limit / 60).clamp(Duration::from_millis(50), SILENCE_TICK_MAX),
            output: 0,
            quiet_since: now,
            last_tick: (now, wall),
            reported: false,
        }
    }

    /// The longest gap between two ticks that the watchdog's timer makes
    /// while the machine is awake.
    fn max_gap(&self) -> Duration {
        (self.tick * SILENCE_TICKS_BEFORE_SLEEP).max(Duration::from_secs(1))
    }

    /// Looks at the output count `output` at a tick of the timer.
    fn on_tick(&mut self, now: Instant, wall: SystemTime, output: u64) -> Option<Silence> {
        let (last, last_wall) = std::mem::replace(&mut self.last_tick, (now, wall));
        let gap = now
            .saturating_duration_since(last)
            .max(wall.duration_since(last_wall).unwrap_or(Duration::MAX));
        if output != self.output {
            self.output = output;
            let quiet = now.saturating_duration_since(self.quiet_since);
            self.quiet_since = now;
            return std::mem::take(&mut self.reported).then_some(Silence::Ended(quiet));
        }
        if gap > self.max_gap() {
            self.quiet_since = now;
            return None;
        }
        if !self.reported && now.saturating_duration_since(self.quiet_since) >= self.limit {
            self.reported = true;
            return Some(Silence::Began);
        }
        None
    }
}

/// Copy a child's output pipe into `log`, feeding startup and sync-progress
/// parsers when `parse_sync_progress` is set, and every line to `damage` when
/// one is given. A line the child leaves unterminated for PARTIAL_LINE_WAIT is
/// also shown to `damage` as it stands; the bytes stay in the buffer, so the
/// line is still logged once, whole, when its newline or the end of the
/// output arrives. Every byte is added to `output` when one is given, a
/// partial line's within PARTIAL_LINE_WAIT. Output is decoded lossily, so
/// bytes that are not valid UTF-8 (a Windows user name in the active code page,
/// for instance) are logged with replacement characters and reading continues:
/// stopping would close the pipe and make the child's next write fail.
async fn pipe_to_log(
    reader: impl tokio::io::AsyncRead + Unpin + Send + 'static,
    log: RotatingLog,
    parse_sync_progress: bool,
    startup_tx: Option<mpsc::UnboundedSender<&'static str>>,
    damage: Option<SharedDetector>,
    output: Option<OutputCount>,
) {
    let count = |bytes: usize| {
        if let Some(ref output) = output {
            output.fetch_add(bytes as u64, Ordering::Relaxed);
        }
    };
    let mut buf = BufReader::new(reader);
    let mut raw: Vec<u8> = Vec::new();
    loop {
        raw.clear();
        // How much of `raw` the detector has already seen as a partial line.
        let mut partial_seen = 0;
        // read_until is cancel safe: bytes read before the timeout stay in
        // `raw`, and the next call appends to them.
        let read = loop {
            let mut limited = (&mut buf).take(MAX_LOG_LINE_BYTES - raw.len() as u64);
            let next = limited.read_until(b'\n', &mut raw);
            match timeout(PARTIAL_LINE_WAIT, next).await {
                Ok(read) => break read,
                Err(_) if raw.len() > partial_seen => {
                    count(raw.len() - partial_seen);
                    partial_seen = raw.len();
                    if let Some(ref detector) = damage {
                        let line = String::from_utf8_lossy(&raw);
                        let detection = detector
                            .lock()
                            .ok()
                            .and_then(|mut d| d.observe_partial(&line));
                        if let Some(detection) = detection {
                            detection.report();
                        }
                    }
                }
                Err(_) => {}
            }
        };
        match read {
            Err(e) => {
                warn!("child output pipe read failed; no longer logging it: {e}");
                break;
            }
            // End of output, with nothing left over from an unterminated line.
            Ok(_) if raw.is_empty() => break,
            Ok(_) => {
                count(raw.len() - partial_seen);
                let line = String::from_utf8_lossy(&raw);
                if parse_sync_progress {
                    if let Some((kind, progress)) = try_parse_block_sync(&line) {
                        emit(&Event::NodeBlockSyncProgress {
                            kind: kind.to_string(),
                            progress,
                        });
                    }
                    if let Some(phase) = try_parse_startup_status(&line) {
                        info!("node startup phase: {phase}");
                        emit(&Event::NodeStartupStatus {
                            phase: phase.to_string(),
                        });
                        if let Some(ref tx) = startup_tx {
                            let _ = tx.send(phase);
                        }
                    }
                }
                if let Some(ref detector) = damage {
                    let detection = detector.lock().ok().and_then(|mut d| d.observe(&line));
                    if let Some(detection) = detection {
                        detection.report();
                    }
                }
                if let Ok(mut w) = log.lock() {
                    let _ = w.write_all(line.as_bytes());
                }
            }
        }
    }
}

fn pick_free_port() -> anyhow::Result<u16> {
    use std::net::TcpListener;
    let listener = TcpListener::bind("127.0.0.1:0")?;
    Ok(listener.local_addr()?.port())
}

async fn wait_for_port(port: u16) {
    loop {
        if TcpStream::connect(("127.0.0.1", port)).await.is_ok() {
            return;
        }
        sleep(Duration::from_millis(500)).await;
    }
}

// Shutdown pipe used to signal cardano-node to exit gracefully.
//
// Unix: anonymous pipe pair; read end is duped to fd 3 in the child via pre_exec;
//       --shutdown-ipc 3 is passed to cardano-node.
// Windows: anonymous pipe pair; read handle is bound as child stdin (fd 0) via
//          Command::stdin(); --shutdown-ipc 0 is passed to cardano-node.
//          Closing the write handle sends EOF to stdin, triggering shutdown.
struct ShutdownPipe {
    /// Value passed to cardano-node as --shutdown-ipc <ipc_arg>.
    pub ipc_arg: String,

    /// Guards against double-close on the write end (close_write + Drop).
    write_closed: bool,

    #[cfg(unix)]
    read_fd: libc::c_int,
    #[cfg(unix)]
    write_fd: libc::c_int,

    #[cfg(windows)]
    read_handle: windows_sys::Win32::Foundation::HANDLE,
    #[cfg(windows)]
    write_handle: windows_sys::Win32::Foundation::HANDLE,
}

impl ShutdownPipe {
    #[cfg(unix)]
    fn new() -> Result<Self> {
        let mut fds: [libc::c_int; 2] = [-1, -1];
        unsafe {
            if libc::pipe(fds.as_mut_ptr()) != 0 {
                return Err(anyhow::anyhow!("pipe() failed"));
            }
            // Write end must not be inherited by child processes
            let flags = libc::fcntl(fds[1], libc::F_GETFD);
            libc::fcntl(fds[1], libc::F_SETFD, flags | libc::FD_CLOEXEC);
        }
        Ok(Self {
            ipc_arg: "3".to_string(),
            write_closed: false,
            read_fd: fds[0],
            write_fd: fds[1],
        })
    }

    #[cfg(windows)]
    fn new() -> Result<Self> {
        use windows_sys::Win32::Foundation::{
            HANDLE, HANDLE_FLAG_INHERIT, INVALID_HANDLE_VALUE, SetHandleInformation,
        };
        use windows_sys::Win32::Security::SECURITY_ATTRIBUTES;
        use windows_sys::Win32::System::Pipes::CreatePipe;

        let mut read_handle: HANDLE = INVALID_HANDLE_VALUE;
        let mut write_handle: HANDLE = INVALID_HANDLE_VALUE;
        let sa = SECURITY_ATTRIBUTES {
            nLength: std::mem::size_of::<SECURITY_ATTRIBUTES>() as u32,
            lpSecurityDescriptor: std::ptr::null_mut(),
            bInheritHandle: 1, // read end inheritable by child
        };
        let ok = unsafe { CreatePipe(&mut read_handle, &mut write_handle, &sa, 0) };
        if ok == 0 {
            return Err(anyhow::anyhow!("CreatePipe failed"));
        }
        // Ensure write end is NOT inherited
        unsafe { SetHandleInformation(write_handle, HANDLE_FLAG_INHERIT, 0) };
        Ok(Self {
            ipc_arg: "0".to_string(),
            write_closed: false,
            read_handle,
            write_handle,
        })
    }

    // Unix: dup the read end to fd 3 in the child via pre_exec.
    // Windows: bind the read handle as child stdin so --shutdown-ipc 0 refers to it.
    fn setup_node_cmd(&self, cmd: &mut Command) {
        #[cfg(unix)]
        {
            let read_fd = self.read_fd;
            unsafe {
                cmd.pre_exec(move || {
                    if read_fd != 3 {
                        libc::dup2(read_fd, 3);
                        libc::close(read_fd);
                    }
                    Ok(())
                });
            }
        }
        #[cfg(windows)]
        {
            use std::os::windows::io::FromRawHandle;
            // Transfer read_handle ownership to the child's stdin. std will pass it via
            // STARTUPINFO.hStdInput and close the parent's copy after CreateProcess.
            let stdio = unsafe {
                std::process::Stdio::from_raw_handle(self.read_handle as *mut std::os::raw::c_void)
            };
            cmd.stdin(stdio);
        }
    }

    fn close_read_in_parent(&self) {
        #[cfg(unix)]
        unsafe {
            libc::close(self.read_fd);
        }
        // Windows: read_handle was transferred to Command::stdin(); std closes it
        // after spawn via OwnedHandle drop. Nothing to do here.
        #[cfg(windows)]
        {}
    }

    // Drop the write end — sends EOF to cardano-node, triggering graceful shutdown.
    // Idempotent: safe to call more than once (second call is a no-op).
    fn close_write(&mut self) {
        if self.write_closed {
            return;
        }
        self.write_closed = true;
        #[cfg(unix)]
        unsafe {
            libc::close(self.write_fd);
        }
        #[cfg(windows)]
        unsafe {
            windows_sys::Win32::Foundation::CloseHandle(self.write_handle);
        }
    }
}

impl Drop for ShutdownPipe {
    fn drop(&mut self) {
        self.close_write();
    }
}

/// Time the client has to answer `migrate_state_request` once it is listening.
/// In parent mode `main` adds Electron's connect timeout on top.
pub(crate) const MIGRATION_REPLY_TIMEOUT: Duration = Duration::from_secs(10);

/// Outcome of waiting for Electron's reply to `migrate_state_request`.
enum Migration {
    Received {
        chain_path: Option<String>,
        electron_flags: Vec<String>,
        node_extra_args: Vec<String>,
    },
    Stopped,
    TimedOut,
}

/// Wait up to `limit` for the `migrate_state` reply. Other commands cannot be
/// acted on before the state is known and are dropped; Electron sends none
/// before it has answered.
async fn await_migration(cmd_rx: &mut mpsc::Receiver<Cmd>, limit: Duration) -> Migration {
    let reply = timeout(limit, async {
        loop {
            match cmd_rx.recv().await {
                Some(Cmd::MigrateState {
                    chain_path,
                    electron_flags,
                    node_extra_args,
                }) => {
                    return Migration::Received {
                        chain_path,
                        electron_flags,
                        node_extra_args,
                    };
                }
                Some(Cmd::Stop) | None => return Migration::Stopped,
                _ => {}
            }
        }
    })
    .await;
    reply.unwrap_or(Migration::TimedOut)
}

/// Returns true if the database directory holds a cardano-node database of
/// this network. Anything else, including a directory of the user's own files
/// or another network's database, counts as no chain, so the user is asked
/// again instead of being left with a node that refuses to open it.
async fn chain_has_data(config: &WatchdogConfig, storage_folder: Option<&str>) -> bool {
    let db = state::database_dir(&config.node.state_dir, storage_folder);
    if let Some(found) = state::other_network_magic(&db, config.node.network_magic).await {
        warn!(
            "{} holds a cardano-node database for network magic {found}, not this network's; \
             it is not used, and the user is asked for another location",
            db.display()
        );
        return false;
    }
    state::holds_node_db(&db).await
}

/// True when the database directory holds another network's database, which
/// the node must never be started on: cardano-node would refuse it, and it
/// belongs to the Daedalus that runs that network. Emits an error naming the
/// directory when it does.
async fn holds_other_network_db(config: &WatchdogConfig, wstate: &state::WatchdogState) -> bool {
    let db = state::database_dir(&config.node.state_dir, wstate.chain_path.as_deref());
    let (Some(expected), Some(found)) = (
        config.node.network_magic,
        state::other_network_magic(&db, config.node.network_magic).await,
    ) else {
        return false;
    };
    emit_error(&state::other_network_message(&db, found, expected));
    true
}

/// The slot of the ledger snapshot the node would stop on, when the check
/// before a node start finds that its block is missing from the
/// ImmutableDB. Any other verdict is logged, and the node starts.
async fn missing_ledger_point(config: &WatchdogConfig) -> Option<u64> {
    match ledger_point::check_before_start(config).await {
        Verdict::BlockMissing { slot } => {
            warn!(
                "ledger snapshot check: the node would replay from the ledger snapshot at \
                 slot {slot}, whose block is missing from the ImmutableDB; not starting the node"
            );
            Some(slot)
        }
        Verdict::Start(reason) => {
            info!("ledger snapshot check: {reason}; starting the node");
            None
        }
        Verdict::Unknown(reason) => {
            warn!("ledger snapshot check could not decide: {reason}; starting the node");
            None
        }
    }
}

/// The chain directory Mithril installs into, when Mithril is configured.
fn mithril_chain_path(config: &WatchdogConfig) -> Option<String> {
    config.mithril.as_ref().map(|mc| mc.chain_path.clone())
}

/// `chain_status` for a chain directory that holds no chain to start the node
/// on. `pending` is the kind of the record of an unfinished rebuild or install
/// that is the reason, if one is.
fn emit_no_chain(pending: Option<mithril::RecordKind>) {
    emit(&Event::ChainStatus {
        has_chain: false,
        rebuild_pending: pending == Some(mithril::RecordKind::Rebuild),
        install_pending: pending == Some(mithril::RecordKind::Install),
    });
}

/// The kind of the record that makes the chain directory wait for a Mithril
/// sync, when `pending` says there is one. A wait that follows a rebuild has
/// written a record before it deleted anything, so a record that cannot be
/// found counts as a rebuild's.
async fn pending_kind(config: &WatchdogConfig, pending: bool) -> Option<mithril::RecordKind> {
    if !pending {
        return None;
    }
    let found = match config.mithril {
        Some(ref mc) => mithril::pending_record(mc).await,
        None => None,
    };
    Some(found.unwrap_or(mithril::RecordKind::Rebuild))
}

/// Waits, while there is no chain to start the node on, for the user to choose
/// between syncing from genesis (`start_node`) and a Mithril bootstrap
/// (`start_mithril`). Settings changes meanwhile are saved and applied.
/// Returns what the renderer is to be told once the node has started, or
/// None, after emitting `stopped`, when the watchdog is to stop.
///
/// `rebuild` is true when the wait follows a rebuild that started deleting the
/// chain and installed nothing, or an install that did not finish, as the
/// record of either says. Until the chain directory changes, a Mithril
/// bootstrap of it is then a rebuild too, so it deletes whatever a failed
/// delete or install left instead of syncing it.
async fn await_chain_choice(
    config: &mut WatchdogConfig,
    cmd_rx: &mut mpsc::Receiver<Cmd>,
    base_node_args: &[String],
    wstate: &mut state::WatchdogState,
    el_restart_tx: &Option<mpsc::UnboundedSender<ElectronRestartPayload>>,
    mut rebuild: bool,
) -> Option<AfterMithril> {
    loop {
        match cmd_rx.recv().await {
            Some(Cmd::StartNode) => {
                // The user chose to sync from genesis on what is there, so
                // no rebuild is pending any more.
                if let Some(ref mc) = config.mithril {
                    mithril::clear_rebuild_marker(mc).await;
                }
                return Some(AfterMithril::Nothing);
            }
            Some(Cmd::StartMithril { wipe_chain, .. }) => {
                // Mithril not configured; ignore and keep waiting.
                let Some(mc) = config.mithril.clone() else {
                    continue;
                };
                use mithril::PipelineResult;
                let wipe_chain = wipe_chain || rebuild;
                let wiped = match mithril::run_pipeline(&mc, cmd_rx, true, wipe_chain).await {
                    PipelineResult::Installed { rebuild } => {
                        return Some(AfterMithril::Installed { rebuild });
                    }
                    PipelineResult::Stopped => {
                        emit(&Event::Stopped);
                        return None;
                    }
                    PipelineResult::FailedAfterWipe => true,
                    // An install that failed part-way may have replaced part
                    // of the chain, so a sync from genesis is not offered on
                    // it: only another Mithril sync ends the wait.
                    PipelineResult::Failed => {
                        let mut settings = Settings {
                            config: &mut *config,
                            base_node_args,
                            wstate: &mut *wstate,
                        };
                        match await_install_retry(cmd_rx, &mut settings).await {
                            InstallRetry::Installed { rebuild } => {
                                return Some(AfterMithril::Installed { rebuild });
                            }
                            InstallRetry::Wiped => true,
                            InstallRetry::Stopped => {
                                emit(&Event::Stopped);
                                return None;
                            }
                        }
                    }
                    PipelineResult::NotNeeded
                    | PipelineResult::Cancelled
                    | PipelineResult::UserCancelled => false,
                };
                if wiped {
                    rebuild = true;
                }
                // Still no chain data: re-prompt.
                emit_no_chain(pending_kind(config, rebuild).await);
            }
            Some(Cmd::Stop) | None => {
                emit(&Event::Stopped);
                return None;
            }
            Some(Cmd::ValidateChainDir {
                path,
                default_chain_path,
                required_space_bytes,
            }) => {
                spawn_validate_chain_dir(
                    config.node.state_dir.clone(),
                    path,
                    default_chain_path,
                    required_space_bytes,
                );
            }
            Some(Cmd::SetChainPath { path }) => {
                let before = mithril_chain_path(config);
                wstate.chain_path = path;
                info!("chain_path updated: {:?}", wstate.chain_path);
                if let Err(e) = state::save(&config.node.state_dir, wstate).await {
                    warn!("Failed to save watchdog state: {e}");
                }
                state::apply_to_config(config, base_node_args, wstate);
                // Another directory is not the one the rebuild deleted.
                let same = match (before.as_deref(), mithril_chain_path(config).as_deref()) {
                    (Some(before), Some(after)) => mithril::same_chain_path(before, after),
                    (before, after) => before == after,
                };
                if !same {
                    rebuild = false;
                }
            }
            Some(Cmd::SetNodeExtraArgs { args }) => {
                wstate.node_extra_args = args;
                info!(
                    "node_extra_args updated (RTS flags): {:?}",
                    wstate.node_extra_args
                );
                if let Err(e) = state::save(&config.node.state_dir, wstate).await {
                    warn!("Failed to save watchdog state: {e}");
                }
                state::apply_to_config(config, base_node_args, wstate);
            }
            Some(Cmd::SetElectronFlags { flags }) => {
                wstate.electron_flags = flags.clone();
                info!(
                    "electron_flags updated (blank screen fix): {:?}",
                    wstate.electron_flags
                );
                if let Err(e) = state::save(&config.node.state_dir, wstate).await {
                    warn!("Failed to save watchdog state: {e}");
                }
                if let Some(tx) = el_restart_tx {
                    let _ = tx.send(ElectronRestartPayload {
                        flags,
                        wallet_port: None,
                        node_extra_args: wstate.node_extra_args.clone(),
                        startup_phase: None,
                    });
                }
                emit(&Event::ElectronRestarting);
            }
            _ => {}
        }
    }
}

pub async fn run(
    mut config: WatchdogConfig,
    mut cmd_rx: mpsc::Receiver<Cmd>,
    base_node_args: Vec<String>,
    mut wstate: state::WatchdogState,
    el_restart_tx: Option<mpsc::UnboundedSender<ElectronRestartPayload>>,
    migration_wait: Duration,
) -> Result<()> {
    let watchdog_pid = std::process::id();
    info!("watchdog started (PID {watchdog_pid})");
    emit(&Event::WatchdogStarted {
        pid: watchdog_pid,
        node_extra_args: wstate.node_extra_args.clone(),
    });
    info!(
        "watchdog state: chain_path={:?} electron_flags={:?} node_extra_args={:?}",
        wstate.chain_path, wstate.electron_flags, wstate.node_extra_args
    );

    // Check for Mithril resume state
    let mut after_mithril = if let Some(ref mc) = config.mithril {
        match mithril::read_marker(&mc.state_dir).await.as_deref() {
            Some("installed-awaiting-node-start") => {
                // Marker left from a previous session where mithril installed but
                // node/wallet hadn't confirmed ready yet. Clear it silently — the
                // mithril UI is only shown when the user explicitly triggers a sync
                // in the current session, never automatically on restart.
                if let Some(ref mc) = config.mithril {
                    let _ = mithril::write_marker(&mc.state_dir, "node-start-verified").await;
                }
                info!("Cleared stale installed-awaiting-node-start marker from previous session");
                AfterMithril::Nothing
            }
            Some("cutover-in-progress") => {
                // remove_stale_staging below deletes what it staged.
                warn!("a Mithril install was interrupted in the previous session");
                AfterMithril::Nothing
            }
            _ => AfterMithril::Nothing,
        }
    } else {
        AfterMithril::Nothing
    };

    // On the first launch after an upgrade from a pre-watchdog-state.json build,
    // ask Electron to return whatever overrides it has in its electron-store so we
    // can seed watchdog-state.json before making decisions that depend on chain_path.
    // The wait covers Electron's whole start-up, so a slow Electron does not
    // make this session start without the user's settings.
    if !state::exists(&config.node.state_dir).await {
        info!(
            "watchdog-state.json absent; requesting migration data from Electron (waiting up to {} s)",
            migration_wait.as_secs()
        );
        emit(&Event::MigrateStateRequest);
        match await_migration(&mut cmd_rx, migration_wait).await {
            Migration::Received {
                chain_path,
                electron_flags,
                node_extra_args,
            } => {
                info!(
                    "migrate_state received: chain_path={chain_path:?} \
                     electron_flags={electron_flags:?} node_extra_args={node_extra_args:?}"
                );
                wstate.chain_path = state::migrated_chain_path(
                    &config.node.state_dir,
                    chain_path,
                    config.node.network_magic,
                )
                .await;
                wstate.electron_flags = electron_flags;
                wstate.node_extra_args = node_extra_args;
                match state::save(&config.node.state_dir, &wstate).await {
                    // Electron deletes the keys it migrated only on this
                    // event, so a failed save is retried on the next launch.
                    Ok(()) => emit(&Event::MigrateStateSaved),
                    Err(e) => warn!("Failed to save migrated watchdog state: {e}"),
                }
                state::apply_to_config(&mut config, &base_node_args, &wstate);
                // Restart Electron if flags differ from the empty default so it
                // launches with the correct flags (e.g. --safe-mode) this session.
                if !wstate.electron_flags.is_empty() {
                    if let Some(ref tx) = el_restart_tx {
                        let _ = tx.send(ElectronRestartPayload {
                            flags: wstate.electron_flags.clone(),
                            wallet_port: None,
                            node_extra_args: wstate.node_extra_args.clone(),
                            startup_phase: None,
                        });
                    }
                }
            }
            Migration::Stopped => {
                emit(&Event::Stopped);
                return Ok(());
            }
            Migration::TimedOut => {
                // Electron is running but did not answer. Electron keeps the
                // settings until watchdog-state.json is saved, so leaving the
                // file absent makes the next launch ask again.
                warn!(
                    "no migrate_state reply within {} s; starting with defaults, watchdog-state.json stays absent",
                    migration_wait.as_secs()
                );
            }
        }
    }

    // No pipeline runs yet, so any Mithril staging is left from an
    // interrupted run. The chain directory is known once the state is.
    if let Some(ref mc) = config.mithril {
        mithril::remove_stale_staging(mc).await;
    }

    // A record beside a chain marked clean is left from an install that
    // finished or a rebuild that deleted nothing: the node starts as usual.
    if let Some(ref mc) = config.mithril {
        mithril::remove_finished_record(mc).await;
    }

    // A rebuild that began deleting the chain in an earlier session and
    // installed nothing, or an install that began changing the database and
    // did not finish, leaves database entries the node would replay from
    // genesis. The user chooses again instead, as after a failed rebuild in
    // this session.
    let pending = match config.mithril {
        Some(ref mc) => mithril::pending_record(mc).await,
        None => None,
    };
    if let Some(kind) = pending {
        warn!(
            "a {} of the chain did not finish in an earlier session; \
             waiting for the user's choice instead of starting the node",
            kind.describe()
        );
    }

    // Emit chain status and, if empty, wait for the user to choose genesis vs Mithril.
    let has_chain = pending.is_none()
        && (chain_has_data(&config, wstate.chain_path.as_deref()).await
            || matches!(after_mithril, AfterMithril::Installed { .. }));
    info!("chain status: has_chain={has_chain} pending={pending:?}");
    if has_chain {
        emit(&Event::ChainStatus {
            has_chain,
            rebuild_pending: false,
            install_pending: false,
        });
    } else {
        emit_no_chain(pending);
    }
    let mut choose_chain = !has_chain;

    let mut crashes = CrashCounters::new(&config);
    // Set when a request held during a wait replaces starting the node again.
    let mut next: Option<RunResult> = None;
    // Set when a rebuild started deleting the chain and installed nothing,
    // for the wait for the user's choice that follows; see await_chain_choice.
    let mut rebuild_chain = pending.is_some();
    loop {
        if choose_chain {
            // Hold here until the UI sends start_node or start_mithril.
            match await_chain_choice(
                &mut config,
                &mut cmd_rx,
                &base_node_args,
                &mut wstate,
                &el_restart_tx,
                std::mem::take(&mut rebuild_chain),
            )
            .await
            {
                Some(after) => after_mithril = after,
                None => return Ok(()),
            }
            choose_chain = false;
        }
        let result = match next.take() {
            Some(result) => result,
            None => {
                // Every start of the node passes here. Another network's
                // database is reported as no chain, so the user chooses again.
                if holds_other_network_db(&config, &wstate).await {
                    emit_no_chain(None);
                    choose_chain = true;
                    continue;
                }
                // A node that would stop on a missing ledger point, and
                // then wait with no output until it is stopped, is not
                // started. The damage is reported, and the watchdog waits
                // for a rebuild, a retry or a stop.
                if let Some(slot) = missing_ledger_point(&config).await {
                    // The Mithril view waits for the node an install was
                    // for. That node is not started, so the view is closed
                    // here, before the damage is reported.
                    if after_mithril != AfterMithril::Nothing {
                        emit(&Event::MithrilStatus {
                            phase: "completed".to_string(),
                            rebuild: matches!(
                                after_mithril,
                                AfterMithril::Installed { rebuild: true }
                            ),
                        });
                        after_mithril = AfterMithril::Nothing;
                    }
                    Detection {
                        kind: ChainDamageKind::LedgerPointMissing,
                        slot: Some(slot),
                        last_good_slot: None,
                    }
                    .report_held();
                    let held = wait_for_retry(
                        &mut cmd_rx,
                        &mut Settings {
                            config: &mut config,
                            base_node_args: &base_node_args,
                            wstate: &mut wstate,
                        },
                    )
                    .await;
                    next = held.instead();
                    if next.is_none() {
                        info!("checking the ledger snapshot again before starting the node");
                    }
                    continue;
                }
                let result = run_node_wallet(
                    &mut config,
                    &mut cmd_rx,
                    after_mithril,
                    &mut crashes,
                    &base_node_args,
                    &mut wstate,
                    &el_restart_tx,
                )
                .await?;
                after_mithril = AfterMithril::Nothing;
                result
            }
        };

        match result {
            RunResult::Stopped => break,
            RunResult::NodeCrashed => {
                crashes.node_consecutive += 1;
                let window_exhausted = crashes.node_window.record(std::time::Instant::now());
                if crashes.node_consecutive >= config.node.max_crash_attempts || window_exhausted {
                    let count = crashes.node_consecutive.max(crashes.node_window.count());
                    emit_error(&format!("cardano-node unrecoverable after {count} crashes"));
                    emit(&Event::NodeUnrecoverable { crashes: count });
                    let held = wait_for_retry(
                        &mut cmd_rx,
                        &mut Settings {
                            config: &mut config,
                            base_node_args: &base_node_args,
                            wstate: &mut wstate,
                        },
                    )
                    .await;
                    next = held.instead();
                    if next.is_none() {
                        info!("retrying cardano-node after it was unrecoverable");
                    }
                    crashes.clear();
                    continue;
                }
                info!(
                    "cardano-node crashed, restarting (attempt {})",
                    crashes.node_consecutive
                );
                let delay = Duration::from_millis(config.node.crash_restart_delay_ms);
                let held = hold_requests_for(
                    delay,
                    &mut cmd_rx,
                    &mut Settings {
                        config: &mut config,
                        base_node_args: &base_node_args,
                        wstate: &mut wstate,
                    },
                )
                .await;
                next = held.instead();
                // otherwise the loop restarts node+wallet
            }
            RunResult::RestartRequested => {
                info!("user-initiated node restart");
                crashes.clear();
                // continue loop immediately — no delay, no crash-count increment
            }
            RunResult::StartMithril { force, wipe_chain } => {
                // A network without Mithril has nothing to sync from. The
                // node and wallet were stopped for the sync, so the loop
                // starts them again on the chain they had.
                let Some(mc) = config.mithril.clone() else {
                    warn!(
                        "start_mithril ignored: Mithril is not configured for this network; \
                         starting the node again"
                    );
                    continue;
                };
                use mithril::PipelineResult;
                // Set when a rebuild started deleting the chain and installed nothing.
                let mut wiped = false;
                // Set when an install failed part-way.
                let mut install_failed = false;
                // Set when the sync changed nothing and the chain directory
                // holds no chain to start the node on.
                let mut no_chain = false;
                match mithril::run_pipeline(&mc, &mut cmd_rx, force, wipe_chain).await {
                    PipelineResult::Installed { rebuild } => {
                        after_mithril = AfterMithril::Installed { rebuild };
                        // loop continues to restart node/wallet
                    }
                    PipelineResult::NotNeeded => {
                        after_mithril = AfterMithril::NotNeeded;
                        // loop continues to restart node/wallet
                    }
                    PipelineResult::Cancelled => {
                        // The sync was refused or failed before it changed
                        // anything, and the loop starts the node again on the
                        // chain it had. A chain location sent with the sync,
                        // as the renderer sends a rebuild in another
                        // location, can hold no chain: a node started there
                        // would sync from genesis without asking.
                        no_chain = !chain_has_data(&config, wstate.chain_path.as_deref()).await;
                    }
                    PipelineResult::Stopped => break,
                    PipelineResult::FailedAfterWipe => wiped = true,
                    PipelineResult::Failed => install_failed = true,
                    PipelineResult::UserCancelled => {
                        // Wait for the user to choose a recovery action before
                        // restarting the node.
                        'recovery: loop {
                            match cmd_rx.recv().await {
                                Some(Cmd::StartNode) => break 'recovery,
                                Some(Cmd::StartMithril {
                                    force: f,
                                    wipe_chain: wc,
                                }) => match mithril::run_pipeline(&mc, &mut cmd_rx, f, wc).await {
                                    PipelineResult::Installed { rebuild } => {
                                        after_mithril = AfterMithril::Installed { rebuild };
                                        break 'recovery;
                                    }
                                    PipelineResult::NotNeeded => {
                                        after_mithril = AfterMithril::NotNeeded;
                                        break 'recovery;
                                    }
                                    PipelineResult::Cancelled => break 'recovery,
                                    PipelineResult::FailedAfterWipe => {
                                        wiped = true;
                                        break 'recovery;
                                    }
                                    PipelineResult::Failed => {
                                        install_failed = true;
                                        break 'recovery;
                                    }
                                    PipelineResult::UserCancelled => continue 'recovery,
                                    PipelineResult::Stopped => {
                                        emit(&Event::Stopped);
                                        return Ok(());
                                    }
                                },
                                Some(Cmd::Stop) | None => {
                                    emit(&Event::Stopped);
                                    return Ok(());
                                }
                                Some(Cmd::ValidateChainDir {
                                    path,
                                    default_chain_path,
                                    required_space_bytes,
                                }) => {
                                    spawn_validate_chain_dir(
                                        config.node.state_dir.clone(),
                                        path,
                                        default_chain_path,
                                        required_space_bytes,
                                    );
                                }
                                _ => {}
                            }
                        }
                        // fall through to restart node/wallet
                    }
                }
                if install_failed {
                    // Post-cutover install failure: the chain may be partly
                    // installed, so the node must not start on it until a
                    // Mithril sync installs a database.
                    let mut settings = Settings {
                        config: &mut config,
                        base_node_args: &base_node_args,
                        wstate: &mut wstate,
                    };
                    match await_install_retry(&mut cmd_rx, &mut settings).await {
                        InstallRetry::Installed { rebuild } => {
                            after_mithril = AfterMithril::Installed { rebuild };
                        }
                        InstallRetry::Wiped => wiped = true,
                        InstallRetry::Stopped => break,
                    }
                }
                if no_chain {
                    warn!(
                        "the chain directory holds no chain after a sync that changed nothing; \
                         waiting for the user's choice instead of starting the node"
                    );
                    emit_no_chain(None);
                    choose_chain = true;
                }
                if wiped {
                    // A node started now would sync from genesis without
                    // asking. Report no chain and wait for the user's
                    // choice, as on first run.
                    warn!(
                        "the rebuild started deleting the chain and installed nothing; \
                         waiting for the user's choice instead of starting the node"
                    );
                    emit_no_chain(pending_kind(&config, true).await);
                    choose_chain = true;
                    rebuild_chain = true;
                }
            }
        }
    }

    emit(&Event::Stopped);
    info!("watchdog stopped");
    Ok(())
}

async fn run_node_wallet(
    config: &mut WatchdogConfig,
    cmd_rx: &mut mpsc::Receiver<Cmd>,
    after_mithril: AfterMithril,
    crashes: &mut CrashCounters,
    base_node_args: &[String],
    wstate: &mut state::WatchdogState,
    el_restart_tx: &Option<mpsc::UnboundedSender<ElectronRestartPayload>>,
) -> Result<RunResult> {
    let logs_dir = config
        .pub_logs_dir
        .as_deref()
        .unwrap_or(&config.node.state_dir);
    let node_log = open_log(&format!("{logs_dir}/node.log"));
    let wallet_stop_bound = Duration::from_secs(config.wallet.stop_timeout_secs);

    let mut shutdown_pipe = match ShutdownPipe::new() {
        Ok(p) => p,
        Err(e) => {
            emit_error(&format!("failed to create shutdown pipe: {e}"));
            return Err(e);
        }
    };

    // Remove any stale socket file left by a previous run. (Unix only; Windows uses Named Pipes.)
    #[cfg(not(windows))]
    if let Err(e) = tokio::fs::remove_file(&config.node.socket_path).await {
        if e.kind() != std::io::ErrorKind::NotFound {
            warn!("failed to remove stale socket file: {e}");
        }
    }

    // Spawn cardano-node
    let mut node_cmd = Command::new(&config.node.exe);
    node_cmd
        .args(&config.node.args)
        .args(["--shutdown-ipc", &shutdown_pipe.ipc_arg])
        .current_dir(&config.node.state_dir)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .kill_on_drop(true);

    // Fixed ASCII host name for trace-dispatcher's host field. The default is
    // the machine host name, which cardano-node writes to stdout in the console
    // code page on Windows: invalid UTF-8 for names inside the code page, a
    // crash for names outside it. Set unconditionally, so an inherited value
    // cannot reintroduce the machine name. The child's environment only; the
    // user's is untouched.
    node_cmd.env(TRACE_HOSTNAME_ENV, TRACE_HOSTNAME);

    shutdown_pipe.setup_node_cmd(&mut node_cmd);
    tether_to_watchdog(&mut node_cmd);

    let node_started_at = unix_ms();
    let mut node_proc = node_cmd.spawn()?;
    let node_pid = node_proc.id().unwrap_or(0);

    shutdown_pipe.close_read_in_parent();

    emit(&Event::NodeStarted {
        pid: node_pid,
        started_at_unix_ms: node_started_at,
    });
    info!("cardano-node started (PID {node_pid})");

    // Dismiss the Mithril overlay now that the node is running so the user
    // sees normal sync/replay progress screens instead of the Mithril UI.
    if after_mithril != AfterMithril::Nothing {
        if let AfterMithril::Installed { .. } = after_mithril {
            if let Some(ref mc) = config.mithril {
                if let Err(e) = mithril::write_marker(&mc.state_dir, "node-start-verified").await {
                    warn!("Failed to write node-start-verified marker: {e}");
                }
            }
        }
        emit(&Event::MithrilStatus {
            phase: "completed".to_string(),
            rebuild: matches!(after_mithril, AfterMithril::Installed { rebuild: true }),
        });
    }

    // Startup phase channel: pipe_to_log feeds phases here so the node_starting
    // loop can gate wallet startup on chainDbReady.
    let (startup_tx, mut startup_rx) = mpsc::unbounded_channel::<&'static str>();

    // One detector and one output count per node process, shared by both
    // output streams.
    let node_damage: SharedDetector = Arc::new(Mutex::new(ChainDamageDetector::node()));
    let node_output: OutputCount = Arc::new(AtomicU64::new(0));

    // Forward node logs
    tokio::spawn(pipe_to_log(
        node_proc.stdout.take().unwrap(),
        Arc::clone(&node_log),
        true,
        Some(startup_tx.clone()),
        Some(Arc::clone(&node_damage)),
        Some(Arc::clone(&node_output)),
    ));
    tokio::spawn(pipe_to_log(
        node_proc.stderr.take().unwrap(),
        node_log,
        true,
        Some(startup_tx),
        Some(node_damage),
        Some(Arc::clone(&node_output)),
    ));

    // Watch node for exit. The kill channel lets us force-kill via start_kill()
    // without the PID-reuse race that kill-by-PID would introduce after wait().
    let (node_kill_tx, node_kill_rx) = oneshot::channel::<()>();
    let mut node_kill_tx = Some(node_kill_tx);
    let (node_tx, node_rx) = watch::channel::<Option<ExitInfo>>(None);
    tokio::spawn(async move {
        tokio::select! {
            status = node_proc.wait() => {
                let _ = node_tx.send(Some(extract_exit(status.ok())));
            }
            _ = node_kill_rx => {
                let _ = node_proc.start_kill();
                let status = node_proc.wait().await;
                let _ = node_tx.send(Some(extract_exit(status.ok())));
            }
        }
    });

    // Wait for chainDbReady, observing early node exit and stop commands
    info!("waiting for chainDbReady: {}", config.node.socket_path);
    let mut node_rx_socket = node_rx.clone();

    let socket_wait_start = unix_ms();

    // Race guard: on a multi-threaded runtime the watch task may have sent
    // Some(...) before the clone above.  clone() initialises the receiver's
    // seen-version to the current channel version, so changed() would never
    // fire.  borrow_and_update() marks whatever is there now as seen AND
    // returns the value, eliminating the race window.
    if let Some(exit) = node_rx_socket.borrow_and_update().clone() {
        warn!(
            "cardano-node exited before socket was ready (code={:?}, signal={:?})",
            exit.0, exit.1
        );
        emit(&Event::NodeExited {
            code: exit.0,
            signal: exit.1,
        });
        return Ok(RunResult::NodeCrashed);
    }

    // State machine: NodeStarting → (chainDbReady) → WalletStarting
    //
    // chainDbReady is the authoritative gate on all platforms. The log parser
    // in pipe_to_log feeds startup phases into startup_rx; when chainDbReady
    // arrives the node is considered ready and wallet startup begins.
    //
    // On Linux the socket file also appears around this time, but we do not
    // use it as the gate — parsing the log phase gives a single consistent
    // signal across platforms.
    let mut startup_state = NodeStartupState::Init;

    // A node that writes nothing for startup_silence_secs while it starts is
    // reported once per silence, so the user can restart it.
    let silence_limit = Duration::from_secs(config.node.startup_silence_secs);
    let mut silence = SilenceWatch::new(silence_limit, Instant::now(), SystemTime::now());
    let mut silence_ticks = interval(silence.tick);
    silence_ticks.set_missed_tick_behavior(MissedTickBehavior::Delay);

    'node_starting: loop {
        tokio::select! {
            _ = silence_ticks.tick(), if !silence_limit.is_zero() => {
                let output = node_output.load(Ordering::Relaxed);
                match silence.on_tick(Instant::now(), SystemTime::now(), output) {
                    Some(Silence::Began) => {
                        warn!("node: no output for {} s while starting", silence_limit.as_secs());
                        emit(&Event::NodeSilent { silent_secs: silence_limit.as_secs() });
                    }
                    Some(Silence::Ended(quiet)) => {
                        info!("node: output resumed after {} s without output", quiet.as_secs());
                        emit(&Event::NodeOutputResumed);
                    }
                    None => {}
                }
            }
            Some(phase) = startup_rx.recv() => {
                startup_state = match (&startup_state, phase) {
                    (NodeStartupState::Init,             "openingChainDb")    => NodeStartupState::OpeningChainDb,
                    (NodeStartupState::OpeningChainDb,   "openingImmutableDb") => NodeStartupState::OpeningImmutableDb,
                    (NodeStartupState::OpeningImmutableDb, "openedImmutableDb") => NodeStartupState::OpenedImmutableDb,
                    (NodeStartupState::OpenedImmutableDb, "openingVolatileDb") => NodeStartupState::OpeningVolatileDb,
                    (NodeStartupState::OpeningVolatileDb, "openedVolatileDb") => NodeStartupState::OpenedVolatileDb,
                    (NodeStartupState::OpenedVolatileDb, "openingLedgerDb")   => NodeStartupState::OpeningLedgerDb,
                    (NodeStartupState::OpeningLedgerDb,  "replayingLedger")   => NodeStartupState::ReplayingLedger,
                    // openedLedgerDb can follow either OpeningLedgerDb (no replay) or ReplayingLedger
                    (NodeStartupState::OpeningLedgerDb | NodeStartupState::ReplayingLedger, "openedLedgerDb") => NodeStartupState::OpenedLedgerDb,
                    (NodeStartupState::OpenedLedgerDb,   "chainDbReady") => {
                        emit(&Event::NodeSocketReady { waited_ms: unix_ms() - socket_wait_start });
                        break 'node_starting;
                    }
                    (state, phase) => {
                        warn!("unexpected startup phase '{phase}' in state {state:?}; ignoring");
                        startup_state
                    }
                };
            }
            _ = node_rx_socket.changed() => {
                let exit = node_rx_socket.borrow().clone().unwrap_or((None, None));
                warn!("cardano-node exited before socket was ready (code={:?}, signal={:?})", exit.0, exit.1);
                emit(&Event::NodeExited { code: exit.0, signal: exit.1 });
                return Ok(RunResult::NodeCrashed);
            }
            Some(cmd) = cmd_rx.recv() => {
                match cmd {
                    Cmd::Stop => {
                        stop_node(
                            "shutdown requested",
                            &mut shutdown_pipe,
                            &node_rx_socket,
                            &mut node_kill_tx,
                            cmd_rx,
                            &mut Settings {
                                config: &mut *config,
                                base_node_args,
                                wstate: &mut *wstate,
                            },
                        )
                        .await;
                        return Ok(RunResult::Stopped);
                    }
                    Cmd::StartMithril { force, wipe_chain } => {
                        let held = stop_node(
                            "mithril requested",
                            &mut shutdown_pipe,
                            &node_rx_socket,
                            &mut node_kill_tx,
                            cmd_rx,
                            &mut Settings {
                                config: &mut *config,
                                base_node_args,
                                wstate: &mut *wstate,
                            },
                        )
                        .await;
                        return Ok(held.after(RunResult::StartMithril { force, wipe_chain }));
                    }
                    Cmd::ProbeMithril => {
                        if let Some(mc) = config.mithril.clone() {
                            tokio::spawn(async move {
                                match mithril::probe(&mc).await {
                                    Ok((local, certified)) => {
                                        let local_count = local.unwrap_or(0);
                                        if certified.saturating_sub(local_count) >= mc.behind_threshold {
                                            emit(&Event::MithrilSignificantlyBehind {
                                                local_immutable_count: local_count,
                                                latest_certified_immutable: certified,
                                            });
                                        } else {
                                            emit(&Event::MithrilNotNeeded {
                                                local_immutable_count: local_count,
                                                latest_certified_immutable: certified,
                                            });
                                        }
                                    }
                                    Err(e) => warn!("Mithril behind-ness probe failed: {e}"),
                                }
                            });
                        }
                    }
                    Cmd::ValidateChainDir {
                        path,
                        default_chain_path,
                        required_space_bytes,
                    } => {
                        spawn_validate_chain_dir(
                            config.node.state_dir.clone(),
                            path,
                            default_chain_path,
                            required_space_bytes,
                        );
                    }
                    Cmd::RestartNode => {
                        let held = stop_node(
                            "node restart requested",
                            &mut shutdown_pipe,
                            &node_rx_socket,
                            &mut node_kill_tx,
                            cmd_rx,
                            &mut Settings {
                                config: &mut *config,
                                base_node_args,
                                wstate: &mut *wstate,
                            },
                        )
                        .await;
                        return Ok(held.after(RunResult::RestartRequested));
                    }
                    Cmd::SetChainPath { path } => {
                        wstate.chain_path = path;
                        info!("chain_path updated: {:?}", wstate.chain_path);
                        if let Err(e) = state::save(&config.node.state_dir, wstate).await {
                            warn!("Failed to save watchdog state: {e}");
                        }
                        state::apply_to_config(config, base_node_args, wstate);
                        let held = stop_node(
                            "chain path changed",
                            &mut shutdown_pipe,
                            &node_rx_socket,
                            &mut node_kill_tx,
                            cmd_rx,
                            &mut Settings {
                                config: &mut *config,
                                base_node_args,
                                wstate: &mut *wstate,
                            },
                        )
                        .await;
                        return Ok(held.after(RunResult::RestartRequested));
                    }
                    Cmd::SetNodeExtraArgs { args } => {
                        wstate.node_extra_args = args;
                        info!("node_extra_args updated (RTS flags): {:?}", wstate.node_extra_args);
                        if let Err(e) = state::save(&config.node.state_dir, wstate).await {
                            warn!("Failed to save watchdog state: {e}");
                        }
                        state::apply_to_config(config, base_node_args, wstate);
                        let held = stop_node(
                            "node arguments changed",
                            &mut shutdown_pipe,
                            &node_rx_socket,
                            &mut node_kill_tx,
                            cmd_rx,
                            &mut Settings {
                                config: &mut *config,
                                base_node_args,
                                wstate: &mut *wstate,
                            },
                        )
                        .await;
                        return Ok(held.after(RunResult::RestartRequested));
                    }
                    Cmd::SetElectronFlags { flags } => {
                        wstate.electron_flags = flags.clone();
                        info!("electron_flags updated (blank screen fix): {:?}", wstate.electron_flags);
                        if let Err(e) = state::save(&config.node.state_dir, wstate).await {
                            warn!("Failed to save watchdog state: {e}");
                        }
                        if let Some(ref tx) = el_restart_tx {
                            let _ = tx.send(ElectronRestartPayload {
                                flags,
                                wallet_port: None,
                                node_extra_args: wstate.node_extra_args.clone(),
                                startup_phase: startup_state.as_phase_str().map(String::from),
                            });
                        }
                        emit(&Event::ElectronRestarting);
                    }
                    _ => {} // stale command: ignore and keep waiting
                }
            }
        }
    }
    info!("node socket ready");
    // Node recovered — cap consecutive failures, not lifetime crashes.
    crashes.node_consecutive = 0;

    // Wallet supervisor loop
    let wallet_cfg = config.wallet.clone();
    let logs_dir = config
        .pub_logs_dir
        .as_deref()
        .unwrap_or(&config.wallet.state_dir);
    let wallet_log_path = format!("{logs_dir}/cardano-wallet.log");
    let mut attempt = 0u32;
    let mut node_rx = node_rx;

    // Pick the wallet API port once; all restarts reuse the same port so
    // Electron does not need to rediscover it after a wallet crash.
    // When api_port is absent from config the watchdog auto-picks and injects
    // --port <port> into the wallet command args. When api_port is present the
    // caller is responsible for having it in wallet.args already.
    let (wallet_port, inject_port_flag) = match wallet_cfg.api_port {
        Some(p) => (p, false),
        None => (pick_free_port()?, true),
    };

    // Set once the wallet is unrecoverable: it stays stopped, the node keeps
    // running, and the watchdog waits for the user.
    let mut wallet_failed = false;

    'supervisor: loop {
        if let Some(exit) = node_rx.borrow().clone() {
            warn!(
                "cardano-node exited (code={:?}, signal={:?})",
                exit.0, exit.1
            );
            emit(&Event::NodeExited {
                code: exit.0,
                signal: exit.1,
            });
            return Ok(RunResult::NodeCrashed);
        }

        while wallet_failed {
            tokio::select! {
                _ = node_rx.changed() => continue 'supervisor,
                cmd = cmd_rx.recv() => match cmd {
                    Some(Cmd::RestartWallet) => {
                        info!("retrying cardano-wallet after it was unrecoverable");
                        attempt = 0;
                        crashes.wallet_window.clear();
                        wallet_failed = false;
                    }
                    Some(Cmd::RestartNode | Cmd::StartNode) => {
                        let held = stop_node(
                            "node restart requested",
                            &mut shutdown_pipe,
                            &node_rx,
                            &mut node_kill_tx,
                            cmd_rx,
                            &mut Settings {
                                config: &mut *config,
                                base_node_args,
                                wstate: &mut *wstate,
                            },
                        )
                        .await;
                        return Ok(held.after(RunResult::RestartRequested));
                    }
                    Some(Cmd::Stop) | None => break 'supervisor,
                    Some(cmd) => {
                        let mut held = HeldRequests::default();
                        held.take(
                            cmd,
                            &mut Settings {
                                config: &mut *config,
                                base_node_args,
                                wstate: &mut *wstate,
                            },
                        )
                        .await;
                        if let Some((force, wipe_chain)) = held.mithril {
                            let held = stop_node(
                                "mithril requested",
                                &mut shutdown_pipe,
                                &node_rx,
                                &mut node_kill_tx,
                                cmd_rx,
                                &mut Settings {
                                    config: &mut *config,
                                    base_node_args,
                                    wstate: &mut *wstate,
                                },
                            )
                            .await;
                            return Ok(held.after(RunResult::StartMithril { force, wipe_chain }));
                        }
                    }
                },
            }
        }

        let wallet_log = open_log(&wallet_log_path);

        let mut wallet_cmd = Command::new(&wallet_cfg.exe);
        wallet_cmd.args(&wallet_cfg.args);
        if inject_port_flag {
            wallet_cmd.arg("--port").arg(wallet_port.to_string());
        }
        if !wallet_cfg
            .args
            .iter()
            .any(|a| a == WALLET_SHUTDOWN_HANDLER_FLAG)
        {
            wallet_cmd.arg(WALLET_SHUTDOWN_HANDLER_FLAG);
        }
        wallet_cmd
            .current_dir(&wallet_cfg.state_dir)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .kill_on_drop(true);

        tether_to_watchdog(&mut wallet_cmd);

        let wallet_started_at = unix_ms();
        let mut wallet = match wallet_cmd.spawn() {
            Ok(c) => c,
            Err(e) => {
                emit_error(&format!("failed to spawn wallet: {e}"));
                return Err(e.into());
            }
        };
        // Held apart from `wallet`: Child::wait() closes a stdin it still owns,
        // and that end-of-file would stop the wallet.
        let mut wallet_stdin = wallet.stdin.take();
        let wallet_pid = wallet.id().unwrap_or(0);
        emit(&Event::WalletStarted {
            pid: wallet_pid,
            started_at_unix_ms: wallet_started_at,
        });
        info!("cardano-wallet started (PID {wallet_pid}, attempt {attempt})");

        // One detector per wallet process, shared by both output streams.
        let wallet_damage: SharedDetector = Arc::new(Mutex::new(ChainDamageDetector::wallet()));
        tokio::spawn(pipe_to_log(
            wallet.stdout.take().unwrap(),
            Arc::clone(&wallet_log),
            false,
            None,
            Some(Arc::clone(&wallet_damage)),
            None,
        ));
        tokio::spawn(pipe_to_log(
            wallet.stderr.take().unwrap(),
            wallet_log,
            false,
            None,
            Some(wallet_damage),
            None,
        ));

        let wallet_wait_start = unix_ms();

        // Phase 1: wait for API ready OR early exit / node-death / stop.
        // Loop so that stale commands (e.g. a CancelMithril that arrived after
        // the download finished) are silently dropped rather than aborting the
        // normal startup sequence.
        'phase1: loop {
            tokio::select! {
                _ = wait_for_port(wallet_port) => break 'phase1,
                status = wallet.wait() => {
                    let exit = extract_exit(status.ok());
                    warn!("wallet exited before ready (code={:?}, signal={:?})", exit.0, exit.1);
                    emit(&Event::WalletExited { code: exit.0, signal: exit.1.clone(), phase: "pre_ready".to_string() });
                    attempt += 1;
                    if wallet_exhausted(attempt, crashes, &wallet_cfg) {
                        emit(&Event::WalletUnrecoverable { attempt });
                        wallet_failed = true;
                        continue 'supervisor;
                    }
                    info!("wallet restarting (attempt {attempt})");
                    emit(&Event::WalletRestarting { attempt, last_exit_code: exit.0, last_exit_signal: exit.1 });
                    sleep(Duration::from_millis(wallet_cfg.restart_delay_ms)).await;
                    continue 'supervisor;
                }
                _ = node_rx.changed() => {
                    let exit = node_rx.borrow().clone().unwrap_or((None, None));
                    warn!("cardano-node exited during wallet startup (code={:?}, signal={:?})", exit.0, exit.1);
                    emit(&Event::NodeExited { code: exit.0, signal: exit.1 });
                    info!("stopping wallet (node exited)");
                    stop_wallet(&mut wallet, &mut wallet_stdin, wallet_stop_bound).await;
                    return Ok(RunResult::NodeCrashed);
                }
                Some(cmd) = cmd_rx.recv() => {
                    match cmd {
                        Cmd::Stop => {
                            info!("stopping wallet (shutdown requested)");
                            stop_wallet(&mut wallet, &mut wallet_stdin, wallet_stop_bound).await;
                            break 'supervisor;
                        }
                        Cmd::StartMithril { force, wipe_chain } => {
                            info!("stopping wallet (mithril requested)");
                            stop_wallet(&mut wallet, &mut wallet_stdin, wallet_stop_bound).await;
                            let held = stop_node(
                                "mithril requested",
                                &mut shutdown_pipe,
                                &node_rx,
                                &mut node_kill_tx,
                                cmd_rx,
                                &mut Settings {
                                    config: &mut *config,
                                    base_node_args,
                                    wstate: &mut *wstate,
                                },
                            )
                            .await;
                            return Ok(held.after(RunResult::StartMithril { force, wipe_chain }));
                        }
                        Cmd::ProbeMithril => {
                            if let Some(mc) = config.mithril.clone() {
                                tokio::spawn(async move {
                                    match mithril::probe(&mc).await {
                                        Ok((local, certified)) => {
                                            let local_count = local.unwrap_or(0);
                                            if certified.saturating_sub(local_count) >= mc.behind_threshold {
                                                emit(&Event::MithrilSignificantlyBehind {
                                                    local_immutable_count: local_count,
                                                    latest_certified_immutable: certified,
                                                });
                                            } else {
                                                emit(&Event::MithrilNotNeeded {
                                                    local_immutable_count: local_count,
                                                    latest_certified_immutable: certified,
                                                });
                                            }
                                        }
                                        Err(e) => warn!("Mithril behind-ness probe failed: {e}"),
                                    }
                                });
                            }
                        }
                        Cmd::ValidateChainDir { path, default_chain_path, required_space_bytes } => {
                            spawn_validate_chain_dir(config.node.state_dir.clone(), path, default_chain_path, required_space_bytes);
                        }
                        Cmd::RestartNode => {
                            info!("stopping wallet (node restart requested)");
                            stop_wallet(&mut wallet, &mut wallet_stdin, wallet_stop_bound).await;
                            let held = stop_node(
                                "node restart requested",
                                &mut shutdown_pipe,
                                &node_rx,
                                &mut node_kill_tx,
                                cmd_rx,
                                &mut Settings {
                                    config: &mut *config,
                                    base_node_args,
                                    wstate: &mut *wstate,
                                },
                            )
                            .await;
                            return Ok(held.after(RunResult::RestartRequested));
                        }
                        Cmd::RestartWallet => {
                            info!("user-initiated wallet restart");
                            stop_wallet(&mut wallet, &mut wallet_stdin, wallet_stop_bound).await;
                            // A user restart is not a failure.
                            attempt = 0;
                            crashes.wallet_window.clear();
                            emit(&Event::WalletRestarting {
                                attempt,
                                last_exit_code: None,
                                last_exit_signal: None,
                            });
                            continue 'supervisor;
                        }
                        Cmd::SetChainPath { path } => {
                            wstate.chain_path = path;
                            info!("chain_path updated: {:?}", wstate.chain_path);
                            if let Err(e) = state::save(&config.node.state_dir, wstate).await {
                                warn!("Failed to save watchdog state: {e}");
                            }
                            state::apply_to_config(config, base_node_args, wstate);
                            stop_wallet(&mut wallet, &mut wallet_stdin, wallet_stop_bound).await;
                            let held = stop_node(
                                "chain path changed",
                                &mut shutdown_pipe,
                                &node_rx,
                                &mut node_kill_tx,
                                cmd_rx,
                                &mut Settings {
                                    config: &mut *config,
                                    base_node_args,
                                    wstate: &mut *wstate,
                                },
                            )
                            .await;
                            return Ok(held.after(RunResult::RestartRequested));
                        }
                        Cmd::SetNodeExtraArgs { args } => {
                            wstate.node_extra_args = args;
                            info!("node_extra_args updated (RTS flags): {:?}", wstate.node_extra_args);
                            if let Err(e) = state::save(&config.node.state_dir, wstate).await {
                                warn!("Failed to save watchdog state: {e}");
                            }
                            state::apply_to_config(config, base_node_args, wstate);
                            stop_wallet(&mut wallet, &mut wallet_stdin, wallet_stop_bound).await;
                            let held = stop_node(
                                "node arguments changed",
                                &mut shutdown_pipe,
                                &node_rx,
                                &mut node_kill_tx,
                                cmd_rx,
                                &mut Settings {
                                    config: &mut *config,
                                    base_node_args,
                                    wstate: &mut *wstate,
                                },
                            )
                            .await;
                            return Ok(held.after(RunResult::RestartRequested));
                        }
                        Cmd::SetElectronFlags { flags } => {
                            wstate.electron_flags = flags.clone();
                            info!("electron_flags updated (blank screen fix): {:?}", wstate.electron_flags);
                            if let Err(e) = state::save(&config.node.state_dir, wstate).await {
                                warn!("Failed to save watchdog state: {e}");
                            }
                            if let Some(ref tx) = el_restart_tx {
                                let _ = tx.send(ElectronRestartPayload {
                                    flags,
                                    wallet_port: None,
                                    node_extra_args: wstate.node_extra_args.clone(),
                                    startup_phase: None,
                                });
                            }
                            emit(&Event::ElectronRestarting);
                        }
                        _ => {} // stale command (e.g. CancelMithril): ignore and loop
                    }
                }
            }
        }

        emit(&Event::WalletReady {
            port: wallet_port,
            waited_ms: unix_ms() - wallet_wait_start,
        });
        info!("wallet API ready on port {wallet_port}");

        // The wallet recovered — max_restart_attempts caps *consecutive*
        // failed start cycles, so a rare-but-recurring crash (e.g. once a
        // day) must not accumulate into WalletUnrecoverable.
        attempt = 0;

        // Phase 2: wallet is ready — wait for exit, node death, or stop.
        // Loop so that stale commands are ignored instead of falling through
        // to the outer 'supervisor loop (which would restart the wallet).
        loop {
            tokio::select! {
                status = wallet.wait() => {
                    let exit = extract_exit(status.ok());
                    warn!("wallet exited (code={:?}, signal={:?})", exit.0, exit.1);
                    emit(&Event::WalletExited { code: exit.0, signal: exit.1.clone(), phase: "post_ready".to_string() });
                    attempt += 1;
                    if wallet_exhausted(attempt, crashes, &wallet_cfg) {
                        emit(&Event::WalletUnrecoverable { attempt });
                        wallet_failed = true;
                        continue 'supervisor;
                    }
                    info!("wallet restarting (attempt {attempt})");
                    emit(&Event::WalletRestarting { attempt, last_exit_code: exit.0, last_exit_signal: exit.1 });
                    sleep(Duration::from_millis(wallet_cfg.restart_delay_ms)).await;
                    continue 'supervisor;
                }
                _ = node_rx.changed() => {
                    let exit = node_rx.borrow().clone().unwrap_or((None, None));
                    warn!("cardano-node exited (code={:?}, signal={:?})", exit.0, exit.1);
                    emit(&Event::NodeExited { code: exit.0, signal: exit.1 });
                    info!("stopping wallet (node exited)");
                    stop_wallet(&mut wallet, &mut wallet_stdin, wallet_stop_bound).await;
                    return Ok(RunResult::NodeCrashed);
                }
                Some(cmd) = cmd_rx.recv() => {
                    match cmd {
                        Cmd::Stop => {
                            info!("stopping wallet (shutdown requested)");
                            stop_wallet(&mut wallet, &mut wallet_stdin, wallet_stop_bound).await;
                            break 'supervisor;
                        }
                        Cmd::StartMithril { force, wipe_chain } => {
                            info!("stopping wallet (mithril requested)");
                            stop_wallet(&mut wallet, &mut wallet_stdin, wallet_stop_bound).await;
                            let held = stop_node(
                                "mithril requested",
                                &mut shutdown_pipe,
                                &node_rx,
                                &mut node_kill_tx,
                                cmd_rx,
                                &mut Settings {
                                    config: &mut *config,
                                    base_node_args,
                                    wstate: &mut *wstate,
                                },
                            )
                            .await;
                            return Ok(held.after(RunResult::StartMithril { force, wipe_chain }));
                        }
                        Cmd::ProbeMithril => {
                            if let Some(mc) = config.mithril.clone() {
                                tokio::spawn(async move {
                                    match mithril::probe(&mc).await {
                                        Ok((local, certified)) => {
                                            let local_count = local.unwrap_or(0);
                                            if certified.saturating_sub(local_count) >= mc.behind_threshold {
                                                emit(&Event::MithrilSignificantlyBehind {
                                                    local_immutable_count: local_count,
                                                    latest_certified_immutable: certified,
                                                });
                                            } else {
                                                emit(&Event::MithrilNotNeeded {
                                                    local_immutable_count: local_count,
                                                    latest_certified_immutable: certified,
                                                });
                                            }
                                        }
                                        Err(e) => warn!("Mithril behind-ness probe failed: {e}"),
                                    }
                                });
                            }
                        }
                        Cmd::ValidateChainDir { path, default_chain_path, required_space_bytes } => {
                            spawn_validate_chain_dir(config.node.state_dir.clone(), path, default_chain_path, required_space_bytes);
                        }
                        Cmd::RestartNode => {
                            info!("stopping wallet (node restart requested)");
                            stop_wallet(&mut wallet, &mut wallet_stdin, wallet_stop_bound).await;
                            let held = stop_node(
                                "node restart requested",
                                &mut shutdown_pipe,
                                &node_rx,
                                &mut node_kill_tx,
                                cmd_rx,
                                &mut Settings {
                                    config: &mut *config,
                                    base_node_args,
                                    wstate: &mut *wstate,
                                },
                            )
                            .await;
                            return Ok(held.after(RunResult::RestartRequested));
                        }
                        Cmd::RestartWallet => {
                            info!("user-initiated wallet restart");
                            stop_wallet(&mut wallet, &mut wallet_stdin, wallet_stop_bound).await;
                            // A user restart is not a failure.
                            attempt = 0;
                            crashes.wallet_window.clear();
                            emit(&Event::WalletRestarting {
                                attempt,
                                last_exit_code: None,
                                last_exit_signal: None,
                            });
                            continue 'supervisor;
                        }
                        Cmd::SetChainPath { path } => {
                            wstate.chain_path = path;
                            info!("chain_path updated: {:?}", wstate.chain_path);
                            if let Err(e) = state::save(&config.node.state_dir, wstate).await {
                                warn!("Failed to save watchdog state: {e}");
                            }
                            state::apply_to_config(config, base_node_args, wstate);
                            stop_wallet(&mut wallet, &mut wallet_stdin, wallet_stop_bound).await;
                            let held = stop_node(
                                "chain path changed",
                                &mut shutdown_pipe,
                                &node_rx,
                                &mut node_kill_tx,
                                cmd_rx,
                                &mut Settings {
                                    config: &mut *config,
                                    base_node_args,
                                    wstate: &mut *wstate,
                                },
                            )
                            .await;
                            return Ok(held.after(RunResult::RestartRequested));
                        }
                        Cmd::SetNodeExtraArgs { args } => {
                            wstate.node_extra_args = args;
                            info!("node_extra_args updated (RTS flags): {:?}", wstate.node_extra_args);
                            if let Err(e) = state::save(&config.node.state_dir, wstate).await {
                                warn!("Failed to save watchdog state: {e}");
                            }
                            state::apply_to_config(config, base_node_args, wstate);
                            stop_wallet(&mut wallet, &mut wallet_stdin, wallet_stop_bound).await;
                            let held = stop_node(
                                "node arguments changed",
                                &mut shutdown_pipe,
                                &node_rx,
                                &mut node_kill_tx,
                                cmd_rx,
                                &mut Settings {
                                    config: &mut *config,
                                    base_node_args,
                                    wstate: &mut *wstate,
                                },
                            )
                            .await;
                            return Ok(held.after(RunResult::RestartRequested));
                        }
                        Cmd::SetElectronFlags { flags } => {
                            wstate.electron_flags = flags.clone();
                            info!("electron_flags updated (blank screen fix): {:?}", wstate.electron_flags);
                            if let Err(e) = state::save(&config.node.state_dir, wstate).await {
                                warn!("Failed to save watchdog state: {e}");
                            }
                            if let Some(ref tx) = el_restart_tx {
                                let _ = tx.send(ElectronRestartPayload {
                                    flags,
                                    wallet_port: Some(wallet_port),
                                    node_extra_args: wstate.node_extra_args.clone(),
                                    startup_phase: None,
                                });
                            }
                            emit(&Event::ElectronRestarting);
                        }
                        _ => {} // stale command: ignore and loop
                    }
                }
            }
        }
    }

    stop_node(
        "shutdown requested",
        &mut shutdown_pipe,
        &node_rx,
        &mut node_kill_tx,
        cmd_rx,
        &mut Settings {
            config: &mut *config,
            base_node_args,
            wstate: &mut *wstate,
        },
    )
    .await;

    Ok(RunResult::Stopped)
}

#[cfg(test)]
mod tests {
    use super::*;

    // ── pipe_to_log ───────────────────────────────────────────────────────────

    /// Run `input` through `pipe_to_log` and return what reached the log file.
    async fn piped_log(label: &str, input: Vec<u8>) -> Vec<u8> {
        let dir = std::env::temp_dir().join(format!("wdg-unit-{label}-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("out.log");
        let log = open_log(path.to_str().unwrap());
        pipe_to_log(std::io::Cursor::new(input), log, false, None, None, None).await;
        let out = std::fs::read(&path).unwrap();
        let _ = std::fs::remove_dir_all(&dir);
        out
    }

    #[tokio::test]
    async fn pipe_to_log_keeps_reading_after_invalid_utf8() {
        let out = piped_log("latin1", b"before\nJ\xFCrgen\nafter\n".to_vec()).await;
        assert_eq!(
            String::from_utf8(out).unwrap(),
            "before\nJ\u{FFFD}rgen\nafter\n"
        );
    }

    #[tokio::test]
    async fn pipe_to_log_logs_final_line_without_newline() {
        let out = piped_log("no-newline", b"one\ntwo".to_vec()).await;
        assert_eq!(out, b"one\ntwo");
    }

    #[tokio::test]
    async fn pipe_to_log_detects_an_unterminated_line_and_logs_it_once() {
        use tokio::io::AsyncWriteExt;
        const LINE: &str = "cardano-node.exe: Critical invariant violation: block Point (At (Block \
{blockPointSlot = SlotNo 200102375, blockPointHash = <redacted>})) that was in immutable db is gone \
before we could open ledgerdb";
        let dir = std::env::temp_dir().join(format!("wdg-unit-partial-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("out.log");
        let detector: SharedDetector = Arc::new(Mutex::new(ChainDamageDetector::node()));
        let (mut child_end, our_end) = tokio::io::duplex(64 * 1024);
        let reader = tokio::spawn(pipe_to_log(
            our_end,
            open_log(path.to_str().unwrap()),
            false,
            None,
            Some(Arc::clone(&detector)),
            None,
        ));

        child_end.write_all(LINE.as_bytes()).await.unwrap();
        sleep(PARTIAL_LINE_WAIT * 4).await;
        // The detector has reported the line, so it does not report it again.
        assert_eq!(detector.lock().unwrap().observe(LINE), None);
        assert_eq!(std::fs::read(&path).unwrap_or_default(), b"");

        child_end.write_all(b"Node configuration\n").await.unwrap();
        drop(child_end);
        reader.await.unwrap();
        let out = std::fs::read_to_string(&path).unwrap();
        let _ = std::fs::remove_dir_all(&dir);
        assert_eq!(out, format!("{LINE}Node configuration\n"));
    }

    #[tokio::test]
    async fn pipe_to_log_splits_overlong_line_without_losing_bytes() {
        let mut input = vec![b'a'; MAX_LOG_LINE_BYTES as usize * 2 + 10];
        input.extend_from_slice(b"\nnext\n");
        let expected = input.clone();
        let out = piped_log("overlong", input).await;
        assert_eq!(out, expected);
    }

    #[tokio::test]
    async fn pipe_to_log_counts_partial_lines_as_output() {
        use tokio::io::AsyncWriteExt;
        let dir = std::env::temp_dir().join(format!("wdg-unit-count-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("out.log");
        let output: OutputCount = Arc::new(AtomicU64::new(0));
        let (mut child_end, our_end) = tokio::io::duplex(1024);
        let reader = tokio::spawn(pipe_to_log(
            our_end,
            open_log(path.to_str().unwrap()),
            false,
            None,
            None,
            Some(Arc::clone(&output)),
        ));

        child_end.write_all(b"abc").await.unwrap();
        sleep(PARTIAL_LINE_WAIT * 3).await;
        assert_eq!(output.load(Ordering::Relaxed), 3);

        child_end.write_all(b"de\n").await.unwrap();
        drop(child_end);
        reader.await.unwrap();
        let _ = std::fs::remove_dir_all(&dir);
        assert_eq!(output.load(Ordering::Relaxed), 6);
    }

    // ── SilenceWatch ──────────────────────────────────────────────────────────

    /// A clock for a silence watch: monotonic and wall time, moved together
    /// unless a test moves one alone.
    struct Clock {
        now: Instant,
        wall: SystemTime,
    }

    impl Clock {
        fn new() -> Self {
            Clock {
                now: Instant::now(),
                wall: UNIX_EPOCH + Duration::from_secs(1_800_000_000),
            }
        }

        fn advance(&mut self, secs: u64) {
            self.now += Duration::from_secs(secs);
            self.wall += Duration::from_secs(secs);
        }
    }

    /// Ticks `watch` every 5 s for `secs` seconds with an unchanged
    /// `output`, and returns how many seconds in each report came.
    fn tick_quietly(
        watch: &mut SilenceWatch,
        clock: &mut Clock,
        secs: u64,
        output: u64,
    ) -> Vec<(u64, Silence)> {
        let mut reports = Vec::new();
        for t in (5..=secs).step_by(5) {
            clock.advance(5);
            if let Some(report) = watch.on_tick(clock.now, clock.wall, output) {
                reports.push((t, report));
            }
        }
        reports
    }

    #[test]
    fn silence_is_reported_once_at_the_limit() {
        let mut clock = Clock::new();
        let mut watch = SilenceWatch::new(Duration::from_secs(300), clock.now, clock.wall);
        assert_eq!(
            tick_quietly(&mut watch, &mut clock, 900, 0),
            vec![(300, Silence::Began)]
        );
    }

    #[test]
    fn output_keeps_a_silence_from_being_reported() {
        let mut clock = Clock::new();
        let mut watch = SilenceWatch::new(Duration::from_secs(300), clock.now, clock.wall);
        for t in 1..=180u64 {
            clock.advance(5);
            assert_eq!(watch.on_tick(clock.now, clock.wall, t), None, "tick {t}");
        }
    }

    #[test]
    fn output_ends_a_silence_and_a_new_one_is_reported() {
        let mut clock = Clock::new();
        let mut watch = SilenceWatch::new(Duration::from_secs(300), clock.now, clock.wall);
        assert_eq!(
            tick_quietly(&mut watch, &mut clock, 310, 0),
            vec![(300, Silence::Began)]
        );
        clock.advance(5);
        assert_eq!(
            watch.on_tick(clock.now, clock.wall, 1),
            Some(Silence::Ended(Duration::from_secs(315)))
        );
        assert_eq!(
            tick_quietly(&mut watch, &mut clock, 600, 1),
            vec![(300, Silence::Began)]
        );
    }

    #[test]
    fn output_without_a_reported_silence_reports_nothing() {
        let mut clock = Clock::new();
        let mut watch = SilenceWatch::new(Duration::from_secs(300), clock.now, clock.wall);
        assert!(tick_quietly(&mut watch, &mut clock, 200, 0).is_empty());
        clock.advance(5);
        assert_eq!(watch.on_tick(clock.now, clock.wall, 7), None);
    }

    #[test]
    fn a_late_tick_restarts_the_count_as_after_sleep() {
        let mut clock = Clock::new();
        let mut watch = SilenceWatch::new(Duration::from_secs(300), clock.now, clock.wall);
        assert!(tick_quietly(&mut watch, &mut clock, 200, 0).is_empty());
        // The machine slept for an hour, and the monotonic clock counted it.
        clock.advance(3600);
        assert_eq!(watch.on_tick(clock.now, clock.wall, 0), None);
        assert_eq!(
            tick_quietly(&mut watch, &mut clock, 600, 0),
            vec![(300, Silence::Began)]
        );
    }

    #[test]
    fn a_wall_clock_gap_restarts_the_count_as_after_sleep() {
        let mut clock = Clock::new();
        let mut watch = SilenceWatch::new(Duration::from_secs(300), clock.now, clock.wall);
        assert!(tick_quietly(&mut watch, &mut clock, 200, 0).is_empty());
        // The monotonic clock stopped while the machine slept; the wall
        // clock did not.
        clock.now += Duration::from_secs(5);
        clock.wall += Duration::from_secs(3600);
        assert_eq!(watch.on_tick(clock.now, clock.wall, 0), None);
        assert_eq!(
            tick_quietly(&mut watch, &mut clock, 600, 0),
            vec![(300, Silence::Began)]
        );
        // A wall clock set back counts as a gap too.
        clock.now += Duration::from_secs(5);
        clock.wall -= Duration::from_secs(60);
        assert_eq!(watch.on_tick(clock.now, clock.wall, 0), None);
    }

    #[test]
    fn the_tick_follows_the_limit() {
        let start = (Instant::now(), SystemTime::now());
        let watch = SilenceWatch::new(Duration::from_secs(300), start.0, start.1);
        assert_eq!(watch.tick, Duration::from_secs(5));
        assert_eq!(watch.max_gap(), Duration::from_secs(30));
        let watch = SilenceWatch::new(Duration::from_secs(1), start.0, start.1);
        assert_eq!(watch.tick, Duration::from_millis(50));
        assert_eq!(watch.max_gap(), Duration::from_secs(1));
    }

    // ── HeldRequests ──────────────────────────────────────────────────────────

    #[test]
    fn held_stop_replaces_a_planned_restart() {
        let held = HeldRequests {
            stop: true,
            mithril: Some((true, false)),
        };
        assert!(matches!(
            held.after(RunResult::RestartRequested),
            RunResult::Stopped
        ));
    }

    #[test]
    fn held_mithril_replaces_a_planned_restart() {
        let held = HeldRequests {
            stop: false,
            mithril: Some((true, false)),
        };
        assert!(matches!(
            held.after(RunResult::RestartRequested),
            RunResult::StartMithril {
                force: true,
                wipe_chain: false
            }
        ));
    }

    #[test]
    fn held_mithril_never_replaces_a_planned_stop() {
        let held = HeldRequests {
            stop: false,
            mithril: Some((true, true)),
        };
        assert!(matches!(held.after(RunResult::Stopped), RunResult::Stopped));
    }

    #[test]
    fn a_later_request_never_turns_off_a_rebuild_or_force() {
        assert_eq!(merge_mithril((false, true), (false, false)), (false, true));
        assert_eq!(merge_mithril((true, false), (false, false)), (true, false));
        assert_eq!(merge_mithril((false, false), (true, true)), (true, true));
        assert_eq!(merge_mithril((true, false), (false, true)), (true, true));
    }

    #[test]
    fn held_plain_sync_keeps_a_planned_rebuild() {
        let held = HeldRequests {
            stop: false,
            mithril: Some((false, false)),
        };
        assert!(matches!(
            held.after(RunResult::StartMithril {
                force: false,
                wipe_chain: true
            }),
            RunResult::StartMithril {
                force: false,
                wipe_chain: true
            }
        ));
    }

    #[test]
    fn held_rebuild_extends_a_planned_forced_sync() {
        let held = HeldRequests {
            stop: false,
            mithril: Some((false, true)),
        };
        assert!(matches!(
            held.after(RunResult::StartMithril {
                force: true,
                wipe_chain: false
            }),
            RunResult::StartMithril {
                force: true,
                wipe_chain: true
            }
        ));
    }

    #[test]
    fn held_stop_replaces_a_planned_mithril_sync() {
        let held = HeldRequests {
            stop: true,
            mithril: Some((false, true)),
        };
        assert!(matches!(
            held.after(RunResult::StartMithril {
                force: false,
                wipe_chain: true
            }),
            RunResult::Stopped
        ));
    }

    #[test]
    fn nothing_held_keeps_the_planned_step() {
        let held = HeldRequests::default();
        assert!(held.instead().is_none());
        assert!(matches!(
            held.after(RunResult::RestartRequested),
            RunResult::RestartRequested
        ));
    }

    // ── try_parse_startup_status ──────────────────────────────────────────────

    #[test]
    fn startup_status_opening_immutable_db() {
        assert_eq!(
            try_parse_startup_status("StartedOpeningImmutableDB blah"),
            Some("openingImmutableDb")
        );
    }

    #[test]
    fn startup_status_opened_immutable_db() {
        assert_eq!(
            try_parse_startup_status("OpenedImmutableDB blah"),
            Some("openedImmutableDb")
        );
    }

    #[test]
    fn startup_status_opening_volatile_db() {
        assert_eq!(
            try_parse_startup_status("StartedOpeningVolatileDB"),
            Some("openingVolatileDb")
        );
    }

    #[test]
    fn startup_status_opened_volatile_db() {
        assert_eq!(
            try_parse_startup_status("OpenedVolatileDB"),
            Some("openedVolatileDb")
        );
    }

    #[test]
    fn startup_status_opening_ledger_db() {
        assert_eq!(
            try_parse_startup_status("StartedOpeningLgrDB"),
            Some("openingLedgerDb")
        );
    }

    #[test]
    fn startup_status_opened_ledger_db() {
        assert_eq!(
            try_parse_startup_status("OpenedLgrDB"),
            Some("openedLedgerDb")
        );
    }

    #[test]
    fn startup_status_opening_chain_db() {
        assert_eq!(
            try_parse_startup_status("StartedOpeningDB extra"),
            Some("openingChainDb")
        );
    }

    #[test]
    fn startup_status_chain_db_ready() {
        assert_eq!(
            try_parse_startup_status("OpenedDB extra"),
            Some("chainDbReady")
        );
    }

    #[test]
    fn startup_status_replaying_ledger_from_genesis() {
        assert_eq!(
            try_parse_startup_status("Replaying ledger from genesis"),
            Some("replayingLedger")
        );
    }

    #[test]
    fn startup_status_replay_from_genesis_tag() {
        assert_eq!(
            try_parse_startup_status("ReplayFromGenesis context"),
            Some("replayingLedger")
        );
    }

    #[test]
    fn startup_status_no_match_returns_none() {
        assert_eq!(try_parse_startup_status("normal log line"), None);
        assert_eq!(try_parse_startup_status(""), None);
        assert_eq!(try_parse_startup_status("OpenedSomethingElse"), None);
    }

    // Verify that the more-specific "StartedOpeningImmutableDB" is matched
    // before the shorter "StartedOpeningDB" when both substrings appear.
    #[test]
    fn startup_status_specific_before_general() {
        // A line containing both substrings should yield the more specific result.
        let line = "StartedOpeningImmutableDB (also contains StartedOpeningDB)";
        assert_eq!(
            try_parse_startup_status(line),
            Some("openingImmutableDb"),
            "specific match should win over general"
        );
    }

    // ── try_parse_block_sync ──────────────────────────────────────────────────

    #[test]
    fn block_sync_replayed_block() {
        let (kind, pct) = try_parse_block_sync("Replayed block, Progress: 42.5%").unwrap();
        assert_eq!(kind, "replayedBlock");
        assert!((pct - 42.5).abs() < f64::EPSILON);
    }

    #[test]
    fn block_sync_validating_chunk() {
        let (kind, pct) = try_parse_block_sync("Validating chunk, Progress: 10.0%").unwrap();
        assert_eq!(kind, "validatingChunk");
        assert!((pct - 10.0).abs() < f64::EPSILON);
    }

    #[test]
    fn block_sync_validated_chunk() {
        let (kind, pct) = try_parse_block_sync("Validated chunk, Progress: 99.99%").unwrap();
        assert_eq!(kind, "validatingChunk");
        assert!((pct - 99.99).abs() < 0.001);
    }

    #[test]
    fn block_sync_pushing_ledger() {
        let (kind, pct) = try_parse_block_sync("Pushing ledger state, Progress: 0.01%").unwrap();
        assert_eq!(kind, "pushingLedger");
        assert!((pct - 0.01).abs() < 0.001);
    }

    #[test]
    fn block_sync_european_locale_comma_decimal() {
        // Cardano-node on European locales may emit "1,23%" instead of "1.23%".
        let (kind, pct) = try_parse_block_sync("Replayed block, Progress: 1,23%").unwrap();
        assert_eq!(kind, "replayedBlock");
        assert!((pct - 1.23).abs() < 0.001);
    }

    #[test]
    fn block_sync_no_progress_field_returns_none() {
        // Kind matches but no "Progress:" label → None.
        assert_eq!(
            try_parse_block_sync("Replayed block, no progress here"),
            None
        );
    }

    #[test]
    fn block_sync_no_kind_match_returns_none() {
        assert_eq!(
            try_parse_block_sync("some other log line, Progress: 50.0%"),
            None
        );
        assert_eq!(try_parse_block_sync(""), None);
    }

    #[test]
    fn block_sync_zero_percent() {
        let (_, pct) = try_parse_block_sync("Replayed block, Progress: 0.00%").unwrap();
        assert!((pct - 0.0).abs() < f64::EPSILON);
    }

    #[test]
    fn block_sync_hundred_percent() {
        let (_, pct) = try_parse_block_sync("Replayed block, Progress: 100.00%").unwrap();
        assert!((pct - 100.0).abs() < f64::EPSILON);
    }

    // ── await_migration ───────────────────────────────────────────────────────

    fn migrate_state(node_extra_args: &[&str]) -> Cmd {
        Cmd::MigrateState {
            chain_path: None,
            electron_flags: vec![],
            node_extra_args: node_extra_args.iter().map(|s| s.to_string()).collect(),
        }
    }

    #[tokio::test]
    async fn migration_reply_that_arrives_late_is_still_used() {
        let (tx, mut rx) = mpsc::channel(8);
        tokio::spawn(async move {
            sleep(Duration::from_millis(300)).await;
            let _ = tx.send(migrate_state(&["+RTS", "-c", "-RTS"])).await;
        });
        match await_migration(&mut rx, Duration::from_secs(5)).await {
            Migration::Received {
                node_extra_args, ..
            } => assert_eq!(node_extra_args, vec!["+RTS", "-c", "-RTS"]),
            _ => panic!("expected the reply"),
        }
    }

    #[tokio::test]
    async fn migration_wait_ignores_other_commands() {
        let (tx, mut rx) = mpsc::channel(8);
        tx.send(Cmd::ProbeMithril).await.unwrap();
        tx.send(migrate_state(&[])).await.unwrap();
        assert!(matches!(
            await_migration(&mut rx, Duration::from_secs(5)).await,
            Migration::Received { .. }
        ));
    }

    #[tokio::test]
    async fn migration_wait_ends_on_stop() {
        let (tx, mut rx) = mpsc::channel(8);
        tx.send(Cmd::Stop).await.unwrap();
        assert!(matches!(
            await_migration(&mut rx, Duration::from_secs(5)).await,
            Migration::Stopped
        ));
    }

    #[tokio::test]
    async fn migration_wait_ends_when_the_client_is_gone() {
        let (tx, mut rx) = mpsc::channel::<Cmd>(8);
        drop(tx);
        assert!(matches!(
            await_migration(&mut rx, Duration::from_secs(5)).await,
            Migration::Stopped
        ));
    }

    #[tokio::test]
    async fn migration_wait_times_out_without_a_reply() {
        let (_tx, mut rx) = mpsc::channel::<Cmd>(8);
        assert!(matches!(
            await_migration(&mut rx, Duration::from_millis(100)).await,
            Migration::TimedOut
        ));
    }
}
