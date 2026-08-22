use std::io::Write;
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::time::{SystemTime, UNIX_EPOCH};

use file_rotate::compression::Compression;
use file_rotate::suffix::AppendCount;
use file_rotate::{ContentLimit, FileRotate};

use anyhow::Result;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::net::TcpStream;
use tokio::process::{Child, Command};
use tokio::sync::{mpsc, oneshot, watch};
use tokio::time::{Duration, sleep, timeout};
use tracing::{info, warn};

#[cfg(unix)]
use nix::{
    sys::signal::{self, Signal},
    unistd::Pid,
};

use crate::chain_validation;
use crate::config::WatchdogConfig;
use crate::mithril;
use crate::protocol::{Command as Cmd, Event, emit};

type ExitInfo = (Option<i32>, Option<String>);

enum RunResult {
    Stopped,
    NodeCrashed,
    RestartRequested,
    StartMithril { force: bool, wipe_chain: bool },
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
    #[cfg(not(target_os = "linux"))]
    let _ = cmd;
}

// Put the watchdog into a kill-on-close job object so every child it spawns
// dies with it, even on TerminateProcess. Called once at startup; the job
// handle is intentionally leaked — the OS closes it when the watchdog dies,
// which is exactly the trigger we want.
#[cfg(windows)]
pub(crate) fn init_job_object() {
    use windows_sys::Win32::System::JobObjects::{
        AssignProcessToJobObject, CreateJobObjectW, JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE,
        JOBOBJECT_EXTENDED_LIMIT_INFORMATION, JobObjectExtendedLimitInformation,
        SetInformationJobObject,
    };
    use windows_sys::Win32::System::Threading::GetCurrentProcess;
    unsafe {
        let job = CreateJobObjectW(std::ptr::null(), std::ptr::null());
        if job.is_null() {
            warn!("CreateJobObjectW failed; children may outlive the watchdog");
            return;
        }
        let mut info: JOBOBJECT_EXTENDED_LIMIT_INFORMATION = std::mem::zeroed();
        info.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
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

// Sends a graceful-stop signal to a running child.
// Unix: SIGTERM. Windows: CTRL_BREAK_EVENT (requires CREATE_NEW_PROCESS_GROUP at spawn time).
fn graceful_stop(child: &Child) {
    let Some(pid) = child.id() else { return };
    #[cfg(unix)]
    {
        let _ = signal::kill(Pid::from_raw(pid as i32), Signal::SIGTERM);
    }
    #[cfg(windows)]
    unsafe {
        use windows_sys::Win32::System::Console::{CTRL_BREAK_EVENT, GenerateConsoleCtrlEvent};
        GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT, pid);
    }
}

// Graceful stop with a bounded wait; force-kills if the process doesn't exit in time.
async fn stop_child(child: &mut Child, secs: u64) {
    graceful_stop(child);
    if timeout(Duration::from_secs(secs), child.wait())
        .await
        .is_err()
    {
        let _ = child.start_kill();
        let _ = child.wait().await;
    }
}

// Wait for the node-watcher channel to signal exit.
// Returns false; node always exits gracefully after the shutdown pipe closes.
async fn wait_for_node_exit(
    node_rx: &watch::Receiver<Option<ExitInfo>>,
    _kill_tx: &mut Option<oneshot::Sender<()>>,
) -> bool {
    // Wait indefinitely for the node to exit gracefully. Force-killing leaves
    // the chain DB dirty and forces a full immutable-chunk validation on next
    // start (which can take many minutes). The shutdown-pipe EOF is sufficient
    // signal; the node will always exit eventually.
    let mut rx = node_rx.clone();
    loop {
        if rx.borrow().is_some() {
            break;
        }
        let _ = rx.changed().await;
    }
    false
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

async fn pipe_to_log(
    reader: impl tokio::io::AsyncRead + Unpin + Send + 'static,
    log: RotatingLog,
    parse_sync_progress: bool,
    startup_tx: Option<mpsc::UnboundedSender<&'static str>>,
) {
    let mut buf = BufReader::new(reader);
    let mut line = String::new();
    loop {
        line.clear();
        match buf.read_line(&mut line).await {
            Ok(0) | Err(_) => break,
            Ok(_) => {
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
                if let Ok(mut w) = log.lock() {
                    let _ = w.write_all(line.as_bytes());
                }
            }
        }
    }
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
        let mut sa = SECURITY_ATTRIBUTES {
            nLength: std::mem::size_of::<SECURITY_ATTRIBUTES>() as u32,
            lpSecurityDescriptor: std::ptr::null_mut(),
            bInheritHandle: 1, // read end inheritable by child
        };
        let ok = unsafe { CreatePipe(&mut read_handle, &mut write_handle, &mut sa, 0) };
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

/// Returns true if the chain directory exists and contains at least one entry.
async fn chain_has_data(state_dir: &str) -> bool {
    let chain = Path::new(state_dir).join("chain");
    let Ok(mut entries) = tokio::fs::read_dir(&chain).await else {
        return false;
    };
    entries.next_entry().await.ok().flatten().is_some()
}

pub async fn run(config: WatchdogConfig, mut cmd_rx: mpsc::Receiver<Cmd>) -> Result<()> {
    let watchdog_pid = std::process::id();
    info!("watchdog started (PID {watchdog_pid})");
    emit(&Event::WatchdogStarted { pid: watchdog_pid });

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
                false
            }
            Some("cutover-in-progress") => {
                let staging = std::path::PathBuf::from(&mc.state_dir).join("mithril-partial-sync");
                let _ = tokio::fs::remove_dir_all(&staging).await;
                warn!("Cleaned up stale Mithril staging from interrupted cutover");
                false
            }
            _ => false,
        }
    } else {
        false
    };

    // Emit chain status and, if empty, wait for the user to choose genesis vs Mithril.
    let has_chain = chain_has_data(&config.node.state_dir).await || after_mithril;
    info!("chain status: has_chain={has_chain}");
    emit(&Event::ChainStatus { has_chain });

    if !has_chain {
        // Hold here until the UI sends start_node or start_mithril.
        'wait: loop {
            match cmd_rx.recv().await {
                Some(Cmd::StartNode) => break 'wait,
                Some(Cmd::StartMithril { wipe_chain, .. }) => {
                    if let Some(ref mc) = config.mithril {
                        use mithril::PipelineResult;
                        match mithril::run_pipeline(mc, &mut cmd_rx, true, wipe_chain).await {
                            PipelineResult::Installed => {
                                after_mithril = true;
                                break 'wait;
                            }
                            PipelineResult::Stopped => {
                                emit(&Event::Stopped);
                                return Ok(());
                            }
                            PipelineResult::NotNeeded
                            | PipelineResult::Cancelled
                            | PipelineResult::UserCancelled
                            | PipelineResult::Failed => {
                                // Still no chain data (or install failed) — re-prompt.
                                emit(&Event::ChainStatus { has_chain: false });
                                continue 'wait;
                            }
                        }
                    }
                    // Mithril not configured; ignore and keep waiting.
                }
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
    }

    let mut node_crash_count = 0u32;
    loop {
        let result =
            run_node_wallet(&config, &mut cmd_rx, after_mithril, &mut node_crash_count).await?;
        after_mithril = false;

        match result {
            RunResult::Stopped => break,
            RunResult::NodeCrashed => {
                node_crash_count += 1;
                if node_crash_count >= config.node.max_crash_attempts {
                    emit_error(&format!(
                        "cardano-node unrecoverable after {node_crash_count} crashes"
                    ));
                    break;
                }
                info!("cardano-node crashed, restarting (attempt {node_crash_count})");
                sleep(Duration::from_millis(config.node.crash_restart_delay_ms)).await;
                // continue loop to restart node+wallet
            }
            RunResult::RestartRequested => {
                info!("user-initiated node restart");
                node_crash_count = 0;
                // continue loop immediately — no delay, no crash-count increment
            }
            RunResult::StartMithril { force, wipe_chain } => {
                if let Some(ref mc) = config.mithril {
                    use mithril::PipelineResult;
                    match mithril::run_pipeline(mc, &mut cmd_rx, force, wipe_chain).await {
                        PipelineResult::Installed => {
                            after_mithril = true;
                            // loop continues to restart node/wallet
                        }
                        PipelineResult::NotNeeded | PipelineResult::Cancelled => {
                            // loop continues to restart node/wallet
                        }
                        PipelineResult::Stopped => break,
                        PipelineResult::UserCancelled => {
                            // Wait for the user to choose a recovery action before
                            // restarting the node.
                            'recovery: loop {
                                match cmd_rx.recv().await {
                                    Some(Cmd::StartNode) => break 'recovery,
                                    Some(Cmd::StartMithril {
                                        force: f,
                                        wipe_chain: wc,
                                    }) => {
                                        match mithril::run_pipeline(mc, &mut cmd_rx, f, wc).await {
                                            PipelineResult::Installed => {
                                                after_mithril = true;
                                                break 'recovery;
                                            }
                                            PipelineResult::NotNeeded
                                            | PipelineResult::Cancelled => break 'recovery,
                                            PipelineResult::UserCancelled
                                            | PipelineResult::Failed => continue 'recovery,
                                            PipelineResult::Stopped => {
                                                emit(&Event::Stopped);
                                                return Ok(());
                                            }
                                        }
                                    }
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
                        PipelineResult::Failed => {
                            // Post-cutover install failure: chain may be partially installed.
                            // Must not restart node. Require wipe-and-full-sync to recover.
                            'failed_recovery: loop {
                                match cmd_rx.recv().await {
                                    Some(Cmd::StartMithril {
                                        force: f,
                                        wipe_chain: wc,
                                    }) => {
                                        match mithril::run_pipeline(mc, &mut cmd_rx, f, wc).await {
                                            PipelineResult::Installed => {
                                                after_mithril = true;
                                                break 'failed_recovery;
                                            }
                                            PipelineResult::NotNeeded
                                            | PipelineResult::Cancelled
                                            | PipelineResult::UserCancelled
                                            | PipelineResult::Failed => continue 'failed_recovery,
                                            PipelineResult::Stopped => {
                                                emit(&Event::Stopped);
                                                return Ok(());
                                            }
                                        }
                                    }
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
                                    _ => {} // StartNode is intentionally ignored here
                                }
                            }
                            // fall through to restart node/wallet after successful wipe+install
                        }
                    }
                } else {
                    break;
                }
            }
        }
    }

    emit(&Event::Stopped);
    info!("watchdog stopped");
    Ok(())
}

async fn run_node_wallet(
    config: &WatchdogConfig,
    cmd_rx: &mut mpsc::Receiver<Cmd>,
    after_mithril: bool,
    node_crash_count: &mut u32,
) -> Result<RunResult> {
    let node_log = open_log(&format!("{}/node.log", config.pub_logs_dir));

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

    // Windows: spawn in its own process group so CTRL_BREAK_EVENT can target it
    #[cfg(windows)]
    {
        const CREATE_NEW_PROCESS_GROUP: u32 = 0x0000_0200;
        node_cmd.creation_flags(CREATE_NEW_PROCESS_GROUP);
    }

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
    if after_mithril {
        if let Some(ref mc) = config.mithril {
            if let Err(e) = mithril::write_marker(&mc.state_dir, "node-start-verified").await {
                warn!("Failed to write node-start-verified marker: {e}");
            }
        }
        emit(&Event::MithrilStatus {
            phase: "completed".to_string(),
        });
    }

    // Startup phase channel: pipe_to_log feeds phases here so the node_starting
    // loop can gate wallet startup on chainDbReady.
    let (startup_tx, mut startup_rx) = mpsc::unbounded_channel::<&'static str>();

    // Forward node logs
    tokio::spawn(pipe_to_log(
        node_proc.stdout.take().unwrap(),
        Arc::clone(&node_log),
        true,
        Some(startup_tx.clone()),
    ));
    tokio::spawn(pipe_to_log(
        node_proc.stderr.take().unwrap(),
        node_log,
        true,
        Some(startup_tx),
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

    'node_starting: loop {
        tokio::select! {
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
                        info!("stopping node (shutdown requested)");
                        let shutdown_start = unix_ms();
                        shutdown_pipe.close_write();
                        let force_killed = wait_for_node_exit(&node_rx_socket, &mut node_kill_tx).await;
                        let shutdown_ms = unix_ms() - shutdown_start;
                        info!("node shut down in {shutdown_ms}ms (force_killed={force_killed})");
                        emit(&Event::NodeShutdownMs { ms: shutdown_ms, force_killed });
                        return Ok(RunResult::Stopped);
                    }
                    Cmd::StartMithril { force, wipe_chain } => {
                        info!("stopping node (mithril requested)");
                        let shutdown_start = unix_ms();
                        shutdown_pipe.close_write();
                        let force_killed = wait_for_node_exit(&node_rx_socket, &mut node_kill_tx).await;
                        let shutdown_ms = unix_ms() - shutdown_start;
                        info!("node shut down in {shutdown_ms}ms (force_killed={force_killed})");
                        emit(&Event::NodeShutdownMs { ms: shutdown_ms, force_killed });
                        return Ok(RunResult::StartMithril { force, wipe_chain });
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
                        let shutdown_start = unix_ms();
                        shutdown_pipe.close_write();
                        let force_killed =
                            wait_for_node_exit(&node_rx_socket, &mut node_kill_tx).await;
                        let shutdown_ms = unix_ms() - shutdown_start;
                        info!("user-initiated node restart, shut down in {shutdown_ms}ms (force_killed={force_killed})");
                        emit(&Event::NodeShutdownMs { ms: shutdown_ms, force_killed });
                        return Ok(RunResult::RestartRequested);
                    }
                    _ => {} // stale command: ignore and keep waiting
                }
            }
        }
    }
    info!("node socket ready");
    // Node recovered — cap consecutive failures, not lifetime crashes.
    *node_crash_count = 0;

    // Wallet supervisor loop
    let wallet_cfg = config.wallet.clone();
    let wallet_log_path = format!("{}/cardano-wallet.log", config.pub_logs_dir);
    let mut attempt = 0u32;
    let mut node_rx = node_rx;

    'supervisor: loop {
        if node_rx.borrow().is_some() {
            break;
        }

        let wallet_log = open_log(&wallet_log_path);

        let mut wallet_cmd = Command::new(&wallet_cfg.exe);
        wallet_cmd
            .args(&wallet_cfg.args)
            .current_dir(&wallet_cfg.state_dir)
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .kill_on_drop(true);

        // Windows: own process group for CTRL_BREAK targeting
        #[cfg(windows)]
        {
            const CREATE_NEW_PROCESS_GROUP: u32 = 0x0000_0200;
            wallet_cmd.creation_flags(CREATE_NEW_PROCESS_GROUP);
        }

        tether_to_watchdog(&mut wallet_cmd);

        let wallet_started_at = unix_ms();
        let mut wallet = match wallet_cmd.spawn() {
            Ok(c) => c,
            Err(e) => {
                emit_error(&format!("failed to spawn wallet: {e}"));
                return Err(e.into());
            }
        };
        let wallet_pid = wallet.id().unwrap_or(0);
        emit(&Event::WalletStarted {
            pid: wallet_pid,
            started_at_unix_ms: wallet_started_at,
        });
        info!("cardano-wallet started (PID {wallet_pid}, attempt {attempt})");

        tokio::spawn(pipe_to_log(
            wallet.stdout.take().unwrap(),
            Arc::clone(&wallet_log),
            false,
            None,
        ));
        tokio::spawn(pipe_to_log(
            wallet.stderr.take().unwrap(),
            wallet_log,
            false,
            None,
        ));

        let port = wallet_cfg.api_port;
        let wallet_wait_start = unix_ms();

        // Phase 1: wait for API ready OR early exit / node-death / stop.
        // Loop so that stale commands (e.g. a CancelMithril that arrived after
        // the download finished) are silently dropped rather than aborting the
        // normal startup sequence.
        'phase1: loop {
            tokio::select! {
                _ = wait_for_port(port) => break 'phase1,
                status = wallet.wait() => {
                    let exit = extract_exit(status.ok());
                    warn!("wallet exited before ready (code={:?}, signal={:?})", exit.0, exit.1);
                    emit(&Event::WalletExited { code: exit.0, signal: exit.1.clone(), phase: "pre_ready".to_string() });
                    attempt += 1;
                    if attempt >= wallet_cfg.max_restart_attempts {
                        warn!("wallet unrecoverable after {attempt} restart attempts");
                        emit(&Event::WalletUnrecoverable { attempt });
                        break 'supervisor;
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
                    stop_child(&mut wallet, 10).await;
                    return Ok(RunResult::NodeCrashed);
                }
                Some(cmd) = cmd_rx.recv() => {
                    match cmd {
                        Cmd::Stop => {
                            info!("stopping wallet (shutdown requested)");
                            stop_child(&mut wallet, 10).await;
                            break 'supervisor;
                        }
                        Cmd::StartMithril { force, wipe_chain } => {
                            info!("stopping wallet (mithril requested)");
                            stop_child(&mut wallet, 10).await;
                            info!("stopping node (mithril requested)");
                            let shutdown_start = unix_ms();
                            shutdown_pipe.close_write();
                            let node_rx_shutdown = node_rx.clone();
                            let force_killed =
                                wait_for_node_exit(&node_rx_shutdown, &mut node_kill_tx).await;
                            let shutdown_ms = unix_ms() - shutdown_start;
                            info!("node shut down in {shutdown_ms}ms (force_killed={force_killed})");
                            emit(&Event::NodeShutdownMs { ms: shutdown_ms, force_killed });
                            return Ok(RunResult::StartMithril { force, wipe_chain });
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
                            stop_child(&mut wallet, 10).await;
                            info!("stopping node (node restart requested)");
                            let shutdown_start = unix_ms();
                            shutdown_pipe.close_write();
                            let node_rx_shutdown = node_rx.clone();
                            let force_killed =
                                wait_for_node_exit(&node_rx_shutdown, &mut node_kill_tx).await;
                            let shutdown_ms = unix_ms() - shutdown_start;
                            info!("node shut down in {shutdown_ms}ms (force_killed={force_killed})");
                            emit(&Event::NodeShutdownMs { ms: shutdown_ms, force_killed });
                            return Ok(RunResult::RestartRequested);
                        }
                        Cmd::RestartWallet => {
                            info!("user-initiated wallet restart");
                            stop_child(&mut wallet, 10).await;
                            attempt += 1;
                            emit(&Event::WalletRestarting {
                                attempt,
                                last_exit_code: None,
                                last_exit_signal: None,
                            });
                            continue 'supervisor;
                        }
                        _ => {} // stale command (e.g. CancelMithril): ignore and loop
                    }
                }
            }
        }

        emit(&Event::WalletReady {
            port,
            waited_ms: unix_ms() - wallet_wait_start,
        });
        info!("wallet API ready on port {port}");

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
                    if attempt >= wallet_cfg.max_restart_attempts {
                        warn!("wallet unrecoverable after {attempt} restart attempts");
                        emit(&Event::WalletUnrecoverable { attempt });
                        break 'supervisor;
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
                    stop_child(&mut wallet, 10).await;
                    return Ok(RunResult::NodeCrashed);
                }
                Some(cmd) = cmd_rx.recv() => {
                    match cmd {
                        Cmd::Stop => {
                            info!("stopping wallet (shutdown requested)");
                            stop_child(&mut wallet, 10).await;
                            break 'supervisor;
                        }
                        Cmd::StartMithril { force, wipe_chain } => {
                            info!("stopping wallet (mithril requested)");
                            stop_child(&mut wallet, 10).await;
                            info!("stopping node (mithril requested)");
                            let shutdown_start = unix_ms();
                            shutdown_pipe.close_write();
                            let node_rx_shutdown = node_rx.clone();
                            let force_killed =
                                wait_for_node_exit(&node_rx_shutdown, &mut node_kill_tx).await;
                            let shutdown_ms = unix_ms() - shutdown_start;
                            info!("node shut down in {shutdown_ms}ms (force_killed={force_killed})");
                            emit(&Event::NodeShutdownMs { ms: shutdown_ms, force_killed });
                            return Ok(RunResult::StartMithril { force, wipe_chain });
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
                            stop_child(&mut wallet, 10).await;
                            info!("stopping node (node restart requested)");
                            let shutdown_start = unix_ms();
                            shutdown_pipe.close_write();
                            let node_rx_shutdown = node_rx.clone();
                            let force_killed =
                                wait_for_node_exit(&node_rx_shutdown, &mut node_kill_tx).await;
                            let shutdown_ms = unix_ms() - shutdown_start;
                            info!("node shut down in {shutdown_ms}ms (force_killed={force_killed})");
                            emit(&Event::NodeShutdownMs { ms: shutdown_ms, force_killed });
                            return Ok(RunResult::RestartRequested);
                        }
                        Cmd::RestartWallet => {
                            info!("user-initiated wallet restart");
                            stop_child(&mut wallet, 10).await;
                            attempt += 1;
                            emit(&Event::WalletRestarting {
                                attempt,
                                last_exit_code: None,
                                last_exit_signal: None,
                            });
                            continue 'supervisor;
                        }
                        _ => {} // stale command: ignore and loop
                    }
                }
            }
        }
    }

    // Signal cardano-node to shut down by closing the write end of the pipe (sends EOF)
    info!("stopping node (shutdown requested)");

    let shutdown_start = unix_ms();
    shutdown_pipe.close_write();

    // Wait up to 30s for node to exit; force-kill via kill channel if it doesn't.
    let node_rx_shutdown = node_rx.clone();
    let force_killed = wait_for_node_exit(&node_rx_shutdown, &mut node_kill_tx).await;

    let shutdown_ms = unix_ms() - shutdown_start;
    info!("node shut down in {shutdown_ms}ms (force_killed={force_killed})");
    emit(&Event::NodeShutdownMs {
        ms: shutdown_ms,
        force_killed,
    });

    Ok(RunResult::Stopped)
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
