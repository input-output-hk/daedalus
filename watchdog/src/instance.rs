// One watchdog per cluster.
//
// The first watchdog started for a state directory holds a lock there for its
// whole life. A later launch that finds the lock held never generates TLS
// certificates, removes sockets, opens the chain database or spawns a child:
// it asks the running instance, over a control channel, to bring its window
// to the front, and exits. When the running instance has no window (Electron
// has exited and the backend is stopping), the later launch waits for the lock
// to be released and then starts normally.
//
// Lock:            Unix: flock(2) on <state_dir>/watchdog.lock.
//                  Windows: a named mutex in the session namespace.
// Control channel: Unix: a socket at <state_dir>/watchdog.sock.
//                  Windows: a named pipe.
//
// Both locks are released by the OS when the process exits, however it exits,
// so a crashed watchdog never blocks the next launch.

use std::path::Path;
use std::sync::Arc;
use std::sync::atomic::{AtomicU8, AtomicU32, Ordering};
use std::time::{Duration, Instant};

use anyhow::Result;
use serde::{Deserialize, Serialize};
use tokio::io::{
    AsyncBufRead, AsyncBufReadExt, AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt, BufReader,
};
use tokio::time::{sleep, timeout};
use tracing::{info, warn};

use crate::protocol::{Event, emit};

pub use lock::InstanceLock;

/// How long a later launch keeps trying to reach a running instance that holds
/// the lock but has not answered yet. It may have taken the lock moments ago
/// and still be generating TLS certificates before it starts listening.
const CONTROL_ANSWER_LIMIT: Duration = Duration::from_secs(10);

/// Time allowed for each step of one control-channel exchange.
const CONTROL_STEP_TIMEOUT: Duration = Duration::from_secs(3);

/// Pause between attempts to reach the control channel or take the lock.
const RETRY_INTERVAL: Duration = Duration::from_millis(250);

/// Longest control-channel line accepted. Real messages are under 100 bytes.
const MAX_CONTROL_LINE: u64 = 4096;

// ── Window state ─────────────────────────────────────────────────────────────

/// Whether this instance has a window a second launch can bring forward.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Window {
    /// Electron is about to be spawned. An activation is queued and delivered
    /// once Electron connects.
    Starting,
    /// Electron (or, in standalone mode, the client on stdin/stdout) is
    /// attached.
    Present,
    /// Electron has exited and will not be respawned: the instance is stopping.
    Gone,
}

pub struct WindowState {
    kind: AtomicU8,
    pid: AtomicU32,
}

impl WindowState {
    pub fn starting() -> Arc<Self> {
        Arc::new(Self {
            kind: AtomicU8::new(Window::Starting as u8),
            pid: AtomicU32::new(0),
        })
    }

    /// `pid` is the Electron process ID, or 0 when it is not known.
    pub fn set_present(&self, pid: u32) {
        self.pid.store(pid, Ordering::SeqCst);
        self.kind.store(Window::Present as u8, Ordering::SeqCst);
    }

    pub fn set_gone(&self) {
        self.kind.store(Window::Gone as u8, Ordering::SeqCst);
    }

    fn get(&self) -> (Window, u32) {
        let kind = match self.kind.load(Ordering::SeqCst) {
            k if k == Window::Starting as u8 => Window::Starting,
            k if k == Window::Present as u8 => Window::Present,
            _ => Window::Gone,
        };
        (kind, self.pid.load(Ordering::SeqCst))
    }
}

// ── Control protocol ─────────────────────────────────────────────────────────
//
// server → client  {"watchdog_pid":N,"window":"present","electron_pid":N}
// client → server  {"cmd":"activate"}          (omitted when window is "gone")
// server → client  {"status":"activated"} | {"status":"no_window"}

#[derive(Debug, Serialize, Deserialize)]
struct Greeting {
    watchdog_pid: u32,
    window: Window,
    electron_pid: u32,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "cmd", rename_all = "snake_case")]
enum Request {
    Activate,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "status", rename_all = "snake_case")]
enum Reply {
    Activated,
    NoWindow,
}

#[derive(Debug, PartialEq, Eq)]
enum Answer {
    Activated {
        watchdog_pid: u32,
        electron_pid: u32,
    },
    NoWindow {
        watchdog_pid: u32,
    },
}

async fn write_line<W, T>(w: &mut W, msg: &T) -> std::io::Result<()>
where
    W: AsyncWrite + Unpin,
    T: Serialize,
{
    let mut buf = serde_json::to_vec(msg).map_err(std::io::Error::other)?;
    buf.push(b'\n');
    w.write_all(&buf).await?;
    w.flush().await
}

async fn read_line<R, T>(r: &mut R) -> std::io::Result<T>
where
    R: AsyncBufRead + Unpin,
    T: for<'de> Deserialize<'de>,
{
    let mut buf = Vec::new();
    let read = timeout(
        CONTROL_STEP_TIMEOUT,
        r.take(MAX_CONTROL_LINE).read_until(b'\n', &mut buf),
    )
    .await
    .map_err(|_| std::io::Error::new(std::io::ErrorKind::TimedOut, "no answer"))??;
    if read == 0 {
        return Err(std::io::ErrorKind::UnexpectedEof.into());
    }
    serde_json::from_slice(&buf)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))
}

/// Server side of one connection: greet, then act on the request.
async fn serve_one<S>(stream: S, window: Arc<WindowState>)
where
    S: AsyncRead + AsyncWrite,
{
    let (r, mut w) = tokio::io::split(stream);
    let mut r = BufReader::new(r);
    let (kind, electron_pid) = window.get();
    let greeting = Greeting {
        watchdog_pid: std::process::id(),
        window: kind,
        electron_pid,
    };
    if write_line(&mut w, &greeting).await.is_err() {
        return;
    }
    match read_line::<_, Request>(&mut r).await {
        Ok(Request::Activate) => {
            // Re-read: Electron may have exited since the greeting.
            let reply = if window.get().0 == Window::Gone {
                info!("second launch asked for the window; none is open (stopping)");
                Reply::NoWindow
            } else {
                info!("second launch asked for the window; bringing it to the front");
                emit(&Event::ActivateWindow);
                Reply::Activated
            };
            let _ = write_line(&mut w, &reply).await;
        }
        // A client that found no window disconnects without a request.
        Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {}
        Err(e) => warn!("control channel: ignored request: {e}"),
    }
}

/// Client side of one exchange with the running instance.
async fn exchange<S>(stream: S) -> std::io::Result<Answer>
where
    S: AsyncRead + AsyncWrite,
{
    let (r, mut w) = tokio::io::split(stream);
    let mut r = BufReader::new(r);
    let greeting: Greeting = read_line(&mut r).await?;
    let watchdog_pid = greeting.watchdog_pid;
    let electron_pid = greeting.electron_pid;
    if greeting.window == Window::Gone {
        return Ok(Answer::NoWindow { watchdog_pid });
    }
    // The user launched this process, so it may set the foreground window.
    // Pass that right on before asking, or Windows' focus-stealing prevention
    // turns the running instance's focus request into a taskbar flash.
    #[cfg(windows)]
    allow_set_foreground(electron_pid);
    write_line(&mut w, &Request::Activate).await?;
    Ok(match read_line::<_, Reply>(&mut r).await? {
        Reply::Activated => Answer::Activated {
            watchdog_pid,
            electron_pid,
        },
        Reply::NoWindow => Answer::NoWindow { watchdog_pid },
    })
}

#[cfg(windows)]
fn allow_set_foreground(electron_pid: u32) {
    use windows_sys::Win32::UI::WindowsAndMessaging::{ASFW_ANY, AllowSetForegroundWindow};
    let target = if electron_pid == 0 {
        ASFW_ANY
    } else {
        electron_pid
    };
    if unsafe { AllowSetForegroundWindow(target) } == 0 {
        warn!(
            "AllowSetForegroundWindow({target}) failed: {}",
            std::io::Error::last_os_error()
        );
    }
}

// ── Transport ────────────────────────────────────────────────────────────────

#[cfg(unix)]
mod transport {
    use std::path::{Path, PathBuf};

    pub const SOCKET_FILE: &str = "watchdog.sock";

    pub fn socket_path(state_dir: &Path) -> PathBuf {
        state_dir.join(SOCKET_FILE)
    }

    pub fn describe(state_dir: &Path) -> String {
        socket_path(state_dir).display().to_string()
    }

    pub async fn connect(state_dir: &Path) -> std::io::Result<tokio::net::UnixStream> {
        tokio::net::UnixStream::connect(socket_path(state_dir)).await
    }

    /// Bind the control socket. The caller holds the instance lock, so any
    /// existing socket file was left by an instance that has exited.
    pub fn bind(state_dir: &Path) -> std::io::Result<tokio::net::UnixListener> {
        use std::os::unix::fs::PermissionsExt;
        let path = socket_path(state_dir);
        match std::fs::remove_file(&path) {
            Ok(()) => {}
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => return Err(e),
        }
        let listener = tokio::net::UnixListener::bind(&path)?;
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o600))?;
        Ok(listener)
    }
}

#[cfg(windows)]
mod transport {
    use std::path::Path;
    use tokio::net::windows::named_pipe::{ClientOptions, NamedPipeClient};
    use tokio::time::{Duration, Instant, sleep};

    pub fn pipe_name(state_dir: &Path) -> String {
        format!(
            r"\\.\pipe\daedalus-watchdog-{}",
            super::instance_key(state_dir)
        )
    }

    pub fn describe(state_dir: &Path) -> String {
        pipe_name(state_dir)
    }

    pub async fn connect(state_dir: &Path) -> std::io::Result<NamedPipeClient> {
        use windows_sys::Win32::Foundation::ERROR_PIPE_BUSY;
        let name = pipe_name(state_dir);
        let deadline = Instant::now() + super::CONTROL_STEP_TIMEOUT;
        loop {
            match ClientOptions::new().open(&name) {
                Ok(client) => return Ok(client),
                Err(e)
                    if e.raw_os_error() == Some(ERROR_PIPE_BUSY as i32)
                        && Instant::now() < deadline =>
                {
                    sleep(Duration::from_millis(50)).await;
                }
                Err(e) => return Err(e),
            }
        }
    }
}

/// Start answering second launches. Call once, after the event sink is set up
/// so that `emit` reaches Electron.
pub fn spawn_control_server(state_dir: &str, window: Arc<WindowState>) {
    let state_dir = std::path::PathBuf::from(state_dir);

    #[cfg(unix)]
    {
        let listener = match transport::bind(&state_dir) {
            Ok(l) => l,
            Err(e) => {
                warn!(
                    "control socket {} unavailable, a second launch will not be able to bring this window forward: {e}",
                    transport::describe(&state_dir)
                );
                return;
            }
        };
        tokio::spawn(async move {
            loop {
                match listener.accept().await {
                    Ok((stream, _)) => {
                        tokio::spawn(serve_one(stream, Arc::clone(&window)));
                    }
                    Err(e) => {
                        warn!("control socket accept failed: {e}");
                        sleep(RETRY_INTERVAL).await;
                    }
                }
            }
        });
    }

    #[cfg(windows)]
    {
        use tokio::net::windows::named_pipe::ServerOptions;
        let name = transport::pipe_name(&state_dir);
        // first_pipe_instance: refuse to share the name with a pipe some other
        // process created first.
        let mut server = match ServerOptions::new().first_pipe_instance(true).create(&name) {
            Ok(s) => s,
            Err(e) => {
                warn!(
                    "control pipe {name} unavailable, a second launch will not be able to bring this window forward: {e}"
                );
                return;
            }
        };
        tokio::spawn(async move {
            loop {
                if let Err(e) = server.connect().await {
                    warn!("control pipe connect failed: {e}");
                    sleep(RETRY_INTERVAL).await;
                    continue;
                }
                let connected = server;
                server = match ServerOptions::new().create(&name) {
                    Ok(s) => s,
                    Err(e) => {
                        warn!("control pipe {name} could not be re-created: {e}");
                        tokio::spawn(serve_one(connected, Arc::clone(&window)));
                        return;
                    }
                };
                tokio::spawn(serve_one(connected, Arc::clone(&window)));
            }
        });
    }
}

async fn request_activation(state_dir: &Path) -> std::io::Result<Answer> {
    let stream = timeout(CONTROL_STEP_TIMEOUT, transport::connect(state_dir))
        .await
        .map_err(|_| std::io::Error::new(std::io::ErrorKind::TimedOut, "connect timed out"))??;
    exchange(stream).await
}

// ── Claim ────────────────────────────────────────────────────────────────────

/// Take the instance lock for `state_dir`, or hand over to the instance that
/// holds it. A running instance without a window is stopping its backend;
/// `stop_limit` is how long to wait for it to exit, the longest such a stop
/// takes (`WatchdogConfig::backend_stop_limit`).
///
/// `Ok(Some(lock))`: this process is the instance for the cluster; keep the
/// lock alive until exit. `Ok(None)`: the running instance brought its window
/// to the front; exit. `Err`: no instance can be started now; exit.
pub async fn claim(state_dir: &str, stop_limit: Duration) -> Result<Option<InstanceLock>> {
    let dir = Path::new(state_dir);
    if let Some(lock) = lock::try_acquire(dir)? {
        info!("instance lock acquired: {}", lock::describe(dir));
        return Ok(Some(lock));
    }

    let holder = lock::holder_pid(dir)
        .map(|pid| format!(" (watchdog PID {pid})"))
        .unwrap_or_default();
    info!(
        "another watchdog{holder} is running for {state_dir}; asking it over {} to bring its window to the front",
        transport::describe(dir)
    );

    let deadline = Instant::now() + CONTROL_ANSWER_LIMIT;
    loop {
        match request_activation(dir).await {
            Ok(Answer::Activated {
                watchdog_pid,
                electron_pid,
            }) => {
                info!(
                    "running instance (watchdog PID {watchdog_pid}, Electron PID {electron_pid}) brought its window to the front; exiting"
                );
                return Ok(None);
            }
            Ok(Answer::NoWindow { watchdog_pid }) => {
                info!(
                    "running instance (watchdog PID {watchdog_pid}) has no window and is stopping; waiting up to {} s for it to exit",
                    stop_limit.as_secs()
                );
                let started = Instant::now();
                return match lock::acquire_within(dir, stop_limit).await? {
                    Some(lock) => {
                        info!(
                            "previous instance exited after {} s; instance lock acquired: {}",
                            started.elapsed().as_secs(),
                            lock::describe(dir)
                        );
                        Ok(Some(lock))
                    }
                    None => Err(anyhow::anyhow!(
                        "previous instance (watchdog PID {watchdog_pid}) still running after {} s; exiting without starting a second backend",
                        stop_limit.as_secs()
                    )),
                };
            }
            Err(e) => {
                // The holder may have exited between our lock attempt and now.
                if let Some(lock) = lock::try_acquire(dir)? {
                    info!(
                        "previous instance exited; instance lock acquired: {}",
                        lock::describe(dir)
                    );
                    return Ok(Some(lock));
                }
                if Instant::now() >= deadline {
                    return Err(anyhow::anyhow!(
                        "running instance{holder} did not answer on {} within {} s ({e}); exiting without starting a second backend",
                        transport::describe(dir),
                        CONTROL_ANSWER_LIMIT.as_secs()
                    ));
                }
                sleep(RETRY_INTERVAL).await;
            }
        }
    }
}

// ── Lock ─────────────────────────────────────────────────────────────────────

/// Stable identifier for a state directory, for names that live in a global
/// namespace (Windows mutexes and pipes). Case and separator differences that
/// Windows treats as the same path map to the same key.
#[cfg(any(windows, test))]
fn instance_key(state_dir: &Path) -> String {
    let normalized = state_dir
        .to_string_lossy()
        .replace('/', "\\")
        .trim_end_matches('\\')
        .to_lowercase();
    // FNV-1a, 64-bit.
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for byte in normalized.bytes() {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    format!("{hash:016x}")
}

#[cfg(unix)]
mod lock {
    use std::fs::{File, OpenOptions};
    use std::io::Write;
    use std::os::unix::io::AsRawFd;
    use std::path::{Path, PathBuf};
    use std::time::Instant;

    use tokio::time::{Duration, sleep};

    pub const LOCK_FILE: &str = "watchdog.lock";

    /// Holds the lock for as long as it is alive. The file descriptor is
    /// close-on-exec, so children never inherit the lock.
    pub struct InstanceLock {
        _file: File,
    }

    fn lock_path(state_dir: &Path) -> PathBuf {
        state_dir.join(LOCK_FILE)
    }

    pub fn describe(state_dir: &Path) -> String {
        lock_path(state_dir).display().to_string()
    }

    pub fn try_acquire(state_dir: &Path) -> std::io::Result<Option<InstanceLock>> {
        std::fs::create_dir_all(state_dir)?;
        let mut file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(lock_path(state_dir))?;
        if unsafe { libc::flock(file.as_raw_fd(), libc::LOCK_EX | libc::LOCK_NB) } != 0 {
            let err = std::io::Error::last_os_error();
            return if err.raw_os_error() == Some(libc::EWOULDBLOCK) {
                Ok(None)
            } else {
                Err(err)
            };
        }
        // The PID is for log lines only; the lock is the flock, not the file.
        let _ = file.set_len(0);
        let _ = writeln!(file, "{}", std::process::id());
        Ok(Some(InstanceLock { _file: file }))
    }

    pub async fn acquire_within(
        state_dir: &Path,
        limit: Duration,
    ) -> std::io::Result<Option<InstanceLock>> {
        let deadline = Instant::now() + limit;
        loop {
            if let Some(lock) = try_acquire(state_dir)? {
                return Ok(Some(lock));
            }
            if Instant::now() >= deadline {
                return Ok(None);
            }
            sleep(super::RETRY_INTERVAL).await;
        }
    }

    pub fn holder_pid(state_dir: &Path) -> Option<u32> {
        std::fs::read_to_string(lock_path(state_dir))
            .ok()?
            .trim()
            .parse()
            .ok()
    }
}

#[cfg(windows)]
mod lock {
    use std::path::Path;

    use tokio::time::Duration;
    use windows_sys::Win32::Foundation::{
        CloseHandle, HANDLE, WAIT_ABANDONED, WAIT_OBJECT_0, WAIT_TIMEOUT,
    };
    use windows_sys::Win32::System::Threading::{CreateMutexW, WaitForSingleObject};

    /// Owns the named mutex. The handle is never closed: the OS releases the
    /// mutex when the process exits. Mutex ownership belongs to the thread
    /// that waited, so acquisition happens on the main thread, which lives
    /// until the process exits.
    pub struct InstanceLock {
        _handle: isize,
    }

    fn mutex_name(state_dir: &Path) -> String {
        format!(
            r"Local\Daedalus-watchdog-{}",
            super::instance_key(state_dir)
        )
    }

    pub fn describe(state_dir: &Path) -> String {
        mutex_name(state_dir)
    }

    fn wait(state_dir: &Path, millis: u32) -> std::io::Result<Option<InstanceLock>> {
        let name: Vec<u16> = mutex_name(state_dir)
            .encode_utf16()
            .chain(std::iter::once(0))
            .collect();
        let handle: HANDLE = unsafe { CreateMutexW(std::ptr::null(), 0, name.as_ptr()) };
        if handle.is_null() {
            return Err(std::io::Error::last_os_error());
        }
        match unsafe { WaitForSingleObject(handle, millis) } {
            // WAIT_ABANDONED: the previous owner exited without releasing it.
            // The mutex guards no data, so that is an ordinary acquisition.
            WAIT_OBJECT_0 | WAIT_ABANDONED => Ok(Some(InstanceLock {
                _handle: handle as isize,
            })),
            WAIT_TIMEOUT => {
                unsafe { CloseHandle(handle) };
                Ok(None)
            }
            _ => {
                let err = std::io::Error::last_os_error();
                unsafe { CloseHandle(handle) };
                Err(err)
            }
        }
    }

    pub fn try_acquire(state_dir: &Path) -> std::io::Result<Option<InstanceLock>> {
        wait(state_dir, 0)
    }

    /// Blocks the calling thread, which must be the main thread (see
    /// `InstanceLock`). Nothing else runs on it at this point of startup.
    pub async fn acquire_within(
        state_dir: &Path,
        limit: Duration,
    ) -> std::io::Result<Option<InstanceLock>> {
        wait(
            state_dir,
            u32::try_from(limit.as_millis()).unwrap_or(u32::MAX - 1),
        )
    }

    pub fn holder_pid(_state_dir: &Path) -> Option<u32> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn instance_key_ignores_case_and_separators() {
        let a = instance_key(Path::new(
            r"C:\Users\alice\AppData\Roaming\Daedalus Pre-Prod",
        ));
        let b = instance_key(Path::new(
            "c:/users/ALICE/appdata/roaming/daedalus pre-prod/",
        ));
        assert_eq!(a, b);
        assert_eq!(a.len(), 16);
    }

    #[test]
    fn instance_key_differs_per_cluster() {
        let preprod = instance_key(Path::new(r"C:\Users\a\AppData\Roaming\Daedalus Pre-Prod"));
        let mainnet = instance_key(Path::new(r"C:\Users\a\AppData\Roaming\Daedalus Mainnet"));
        assert_ne!(preprod, mainnet);
    }

    #[test]
    fn window_state_moves_from_starting_to_present_to_gone() {
        let w = WindowState::starting();
        assert_eq!(w.get(), (Window::Starting, 0));
        w.set_present(4242);
        assert_eq!(w.get(), (Window::Present, 4242));
        w.set_gone();
        assert_eq!(w.get().0, Window::Gone);
    }

    #[test]
    fn control_messages_have_a_stable_wire_format() {
        let g = Greeting {
            watchdog_pid: 1,
            window: Window::Present,
            electron_pid: 2,
        };
        assert_eq!(
            serde_json::to_string(&g).unwrap(),
            r#"{"watchdog_pid":1,"window":"present","electron_pid":2}"#
        );
        assert_eq!(
            serde_json::to_string(&Request::Activate).unwrap(),
            r#"{"cmd":"activate"}"#
        );
        assert_eq!(
            serde_json::to_string(&Reply::NoWindow).unwrap(),
            r#"{"status":"no_window"}"#
        );
    }

    /// Run one server/client exchange over an in-memory stream.
    async fn exchange_with(window: Arc<WindowState>) -> Answer {
        let (client, server) = tokio::io::duplex(1024);
        let served = tokio::spawn(serve_one(server, window));
        let answer = exchange(client).await.unwrap();
        served.await.unwrap();
        answer
    }

    #[tokio::test]
    async fn present_window_is_activated() {
        let w = WindowState::starting();
        w.set_present(7);
        assert_eq!(
            exchange_with(w).await,
            Answer::Activated {
                watchdog_pid: std::process::id(),
                electron_pid: 7,
            }
        );
    }

    #[tokio::test]
    async fn starting_window_is_activated_once_it_connects() {
        // The event is queued for Electron, which reads it after connecting.
        assert!(matches!(
            exchange_with(WindowState::starting()).await,
            Answer::Activated { .. }
        ));
    }

    #[tokio::test]
    async fn gone_window_reports_no_window() {
        let w = WindowState::starting();
        w.set_gone();
        assert!(matches!(exchange_with(w).await, Answer::NoWindow { .. }));
    }

    #[tokio::test]
    async fn silent_server_times_out() {
        let (client, _server) = tokio::io::duplex(64);
        let err = exchange(client).await.unwrap_err();
        assert_eq!(err.kind(), std::io::ErrorKind::TimedOut);
    }

    #[cfg(unix)]
    fn temp_state_dir(label: &str) -> std::path::PathBuf {
        let dir = std::env::temp_dir().join(format!(
            "wdg-instance-{label}-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .subsec_nanos()
        ));
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[cfg(unix)]
    #[test]
    fn lock_is_exclusive_and_released_on_drop() {
        let dir = temp_state_dir("exclusive");
        let first = lock::try_acquire(&dir).unwrap().expect("first acquires");
        assert!(lock::try_acquire(&dir).unwrap().is_none());
        assert_eq!(lock::holder_pid(&dir), Some(std::process::id()));
        drop(first);
        assert!(lock::try_acquire(&dir).unwrap().is_some());
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn acquire_within_waits_for_release() {
        let dir = temp_state_dir("wait");
        let first = lock::try_acquire(&dir).unwrap().unwrap();
        let release = tokio::spawn(async move {
            sleep(Duration::from_millis(400)).await;
            drop(first);
        });
        let started = Instant::now();
        let second = lock::acquire_within(&dir, Duration::from_secs(5))
            .await
            .unwrap();
        assert!(second.is_some());
        assert!(started.elapsed() >= Duration::from_millis(300));
        release.await.unwrap();
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn acquire_within_gives_up_at_the_limit() {
        let dir = temp_state_dir("limit");
        let _first = lock::try_acquire(&dir).unwrap().unwrap();
        let second = lock::acquire_within(&dir, Duration::from_millis(300))
            .await
            .unwrap();
        assert!(second.is_none());
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn control_socket_round_trip() {
        let dir = temp_state_dir("socket");
        let w = WindowState::starting();
        w.set_present(99);
        spawn_control_server(dir.to_str().unwrap(), w);
        let answer = request_activation(&dir).await.unwrap();
        assert!(matches!(answer, Answer::Activated { .. }));
        use std::os::unix::fs::PermissionsExt;
        let mode = std::fs::metadata(transport::socket_path(&dir))
            .unwrap()
            .permissions()
            .mode();
        assert_eq!(mode & 0o777, 0o600);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn control_socket_absent_is_an_error() {
        let dir = temp_state_dir("nosocket");
        assert!(request_activation(&dir).await.is_err());
        let _ = std::fs::remove_dir_all(&dir);
    }
}
