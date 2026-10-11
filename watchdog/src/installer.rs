//! Starting the Daedalus update installer once the backend has stopped.
//!
//! The installer replaces cardano-node and cardano-wallet, so it may only run
//! after both have exited. Electron verifies the installer and sends
//! `install_update`; the watchdog turns that into a stop, waits for the
//! supervisor to return, and only then starts the installer, detached so that
//! it outlives the watchdog.

use std::sync::{Arc, Mutex};

use tracing::{error, info};

use crate::protocol::{Event, emit};

/// An installer Electron asked the watchdog to start after the backend stops.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UpdateInstaller {
    pub path: String,
    pub args: Vec<String>,
}

/// The installer request, set by the IPC reader and read once the supervisor
/// has returned.
pub type InstallerRequest = Arc<Mutex<Option<UpdateInstaller>>>;

/// Records `installer` as the one to start after the backend stops.
pub fn request(slot: &InstallerRequest, installer: UpdateInstaller) {
    info!(
        "update installer requested: {} {:?}; stopping the backend first",
        installer.path, installer.args
    );
    if let Ok(mut current) = slot.lock() {
        *current = Some(installer);
    }
}

/// Starts the requested installer, if any, and reports the outcome to
/// Electron. `backend_stopped` is false when the supervisor returned an error,
/// in which case a child may still be running and nothing is started.
///
/// Returns `Some(true)` when an installer was started, `Some(false)` when one
/// was requested but not started, and `None` when none was requested.
pub async fn start_requested(slot: &InstallerRequest, backend_stopped: bool) -> Option<bool> {
    let installer = slot.lock().ok().and_then(|mut s| s.take())?;
    if !backend_stopped {
        let message = "the backend did not stop cleanly, so the installer was not started";
        error!("update installer {}: {message}", installer.path);
        emit(&Event::UpdateInstallerFailed {
            message: message.to_string(),
        });
        return Some(false);
    }
    info!(
        "starting update installer: {} {:?}",
        installer.path, installer.args
    );
    match launch(&installer).await {
        Ok(pid) => {
            info!("update installer started (PID {pid})");
            emit(&Event::UpdateInstallerLaunched { pid });
            Some(true)
        }
        Err(e) => {
            error!(
                "update installer {} could not be started: {e}",
                installer.path
            );
            emit(&Event::UpdateInstallerFailed {
                message: e.to_string(),
            });
            Some(false)
        }
    }
}

// Windows: CreateProcess with CREATE_BREAKAWAY_FROM_JOB, so the installer
// leaves the watchdog's kill-on-close job and survives the watchdog's exit
// (init_job_object allows breakaway; children that do not ask for it stay in
// the job). The installer asks for elevation (RequestExecutionLevel highest),
// and CreateProcess cannot elevate: for an administrator it fails with
// ERROR_ELEVATION_REQUIRED, and then ShellExecuteEx starts it, which shows the
// UAC prompt. An elevated process is created outside the job.
#[cfg(windows)]
async fn launch(installer: &UpdateInstaller) -> std::io::Result<u32> {
    use std::os::windows::process::CommandExt;
    const CREATE_BREAKAWAY_FROM_JOB: u32 = 0x0100_0000;
    const DETACHED_PROCESS: u32 = 0x0000_0008;
    const ERROR_ACCESS_DENIED: i32 = 5;
    const ERROR_ELEVATION_REQUIRED: i32 = 740;

    let spawned = std::process::Command::new(&installer.path)
        .args(&installer.args)
        .creation_flags(CREATE_BREAKAWAY_FROM_JOB | DETACHED_PROCESS)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .spawn();
    match spawned {
        Ok(child) => Ok(child.id()),
        Err(e)
            if matches!(
                e.raw_os_error(),
                Some(ERROR_ELEVATION_REQUIRED | ERROR_ACCESS_DENIED)
            ) =>
        {
            info!("update installer needs ShellExecute ({e}); starting it that way");
            let installer = installer.clone();
            tokio::task::spawn_blocking(move || shell_execute(&installer))
                .await
                .map_err(std::io::Error::other)?
        }
        Err(e) => Err(e),
    }
}

#[cfg(windows)]
fn shell_execute(installer: &UpdateInstaller) -> std::io::Result<u32> {
    use windows_sys::Win32::Foundation::CloseHandle;
    use windows_sys::Win32::System::Threading::GetProcessId;
    use windows_sys::Win32::UI::Shell::{
        SEE_MASK_NOASYNC, SEE_MASK_NOCLOSEPROCESS, SHELLEXECUTEINFOW, ShellExecuteExW,
    };
    const SW_SHOWNORMAL: i32 = 1;

    fn wide(s: &str) -> Vec<u16> {
        s.encode_utf16().chain(std::iter::once(0)).collect()
    }
    let verb = wide("open");
    let file = wide(&installer.path);
    let params = wide(&windows_command_line(&installer.args));

    let mut info: SHELLEXECUTEINFOW = unsafe { std::mem::zeroed() };
    info.cbSize = std::mem::size_of::<SHELLEXECUTEINFOW>() as u32;
    info.fMask = SEE_MASK_NOCLOSEPROCESS | SEE_MASK_NOASYNC;
    info.lpVerb = verb.as_ptr();
    info.lpFile = file.as_ptr();
    info.lpParameters = params.as_ptr();
    info.nShow = SW_SHOWNORMAL;
    if unsafe { ShellExecuteExW(&mut info) } == 0 {
        return Err(std::io::Error::last_os_error());
    }
    let mut pid = 0;
    if !info.hProcess.is_null() {
        pid = unsafe { GetProcessId(info.hProcess) };
        unsafe { CloseHandle(info.hProcess) };
    }
    Ok(pid)
}

// Joins arguments into one Windows command line, quoting each that is empty
// or contains whitespace or quotes, with the backslash rules CommandLineToArgvW
// applies.
#[cfg(any(windows, test))]
fn windows_command_line(args: &[String]) -> String {
    let mut line = String::new();
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            line.push(' ');
        }
        if !arg.is_empty() && !arg.contains([' ', '\t', '"']) {
            line.push_str(arg);
            continue;
        }
        line.push('"');
        let mut backslashes = 0;
        for c in arg.chars() {
            match c {
                '\\' => backslashes += 1,
                '"' => {
                    line.extend(std::iter::repeat_n('\\', backslashes * 2 + 1));
                    line.push('"');
                    backslashes = 0;
                }
                c => {
                    line.extend(std::iter::repeat_n('\\', backslashes));
                    line.push(c);
                    backslashes = 0;
                }
            }
        }
        line.extend(std::iter::repeat_n('\\', backslashes * 2));
        line.push('"');
    }
    line
}

// macOS: the installer is a .pkg, which `open` hands to Installer.app. `open`
// returns once the hand-off is done, and fails for a missing file.
#[cfg(target_os = "macos")]
async fn launch(installer: &UpdateInstaller) -> std::io::Result<u32> {
    let mut cmd = tokio::process::Command::new("/usr/bin/open");
    cmd.arg(&installer.path);
    if !installer.args.is_empty() {
        cmd.arg("--args").args(&installer.args);
    }
    cmd.stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null());
    let mut child = cmd.spawn()?;
    let pid = child.id().unwrap_or(0);
    let status = tokio::time::timeout(std::time::Duration::from_secs(30), child.wait())
        .await
        .map_err(|_| std::io::Error::other("open did not return within 30 s"))??;
    if status.success() {
        Ok(pid)
    } else {
        Err(std::io::Error::other(format!("open exited with {status}")))
    }
}

// Other Unix systems, where Daedalus updates through its own installUpdate
// path and the watchdog is not asked to start an installer. Used by the
// integration tests: the program runs in its own process group and is not
// tethered to the watchdog, so it outlives it.
#[cfg(all(unix, not(target_os = "macos")))]
async fn launch(installer: &UpdateInstaller) -> std::io::Result<u32> {
    use std::os::unix::process::CommandExt;
    let child = std::process::Command::new(&installer.path)
        .args(&installer.args)
        .process_group(0)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .spawn()?;
    Ok(child.id())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn args(list: &[&str]) -> Vec<String> {
        list.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn command_line_leaves_plain_arguments_alone() {
        assert_eq!(
            windows_command_line(&args(&["/S", "/D=C:\\x"])),
            "/S /D=C:\\x"
        );
    }

    #[test]
    fn command_line_quotes_spaces_and_empty_arguments() {
        assert_eq!(
            windows_command_line(&args(&["C:\\Program Files\\x", ""])),
            "\"C:\\Program Files\\x\" \"\""
        );
    }

    #[test]
    fn command_line_escapes_quotes_and_trailing_backslashes() {
        assert_eq!(windows_command_line(&args(&["a\"b"])), "\"a\\\"b\"");
        assert_eq!(windows_command_line(&args(&["a b\\"])), "\"a b\\\\\"");
    }

    #[tokio::test]
    async fn nothing_requested_starts_nothing() {
        let slot = InstallerRequest::default();
        assert_eq!(start_requested(&slot, true).await, None);
    }

    #[tokio::test]
    async fn nothing_starts_when_the_backend_did_not_stop() {
        let slot = InstallerRequest::default();
        request(
            &slot,
            UpdateInstaller {
                path: "/bin/true".to_string(),
                args: vec![],
            },
        );
        assert_eq!(start_requested(&slot, false).await, Some(false));
    }
}
