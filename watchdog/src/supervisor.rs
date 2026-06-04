use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};

use anyhow::Result;
use nix::sys::signal::{self, Signal};
use nix::unistd::Pid;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::TcpStream;
use tokio::process::{Child, Command};
use tokio::sync::{mpsc, watch};
use tokio::time::{sleep, timeout, Duration};
use tracing::{info, warn};

use crate::config::WatchdogConfig;
use crate::protocol::{Command as Cmd, Event, emit};

type ExitInfo = (Option<i32>, Option<String>);

fn unix_ms() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as u64
}

fn extract_exit(status: Option<std::process::ExitStatus>) -> ExitInfo {
    let Some(s) = status else { return (None, None) };
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if let Some(sig) = s.signal() {
            let name = Signal::try_from(sig)
                .ok()
                .map(|s| s.as_str().to_string());
            return (None, name);
        }
    }
    (s.code(), None)
}

fn sigterm(child: &Child) {
    if let Some(pid) = child.id() {
        let _ = signal::kill(Pid::from_raw(pid as i32), Signal::SIGTERM);
    }
}

async fn pipe_to_file(
    reader: impl tokio::io::AsyncRead + Unpin + Send + 'static,
    mut file: tokio::fs::File,
) {
    let mut buf = BufReader::new(reader);
    let mut line = String::new();
    loop {
        line.clear();
        match buf.read_line(&mut line).await {
            Ok(0) | Err(_) => break,
            Ok(_) => {
                let _ = file.write_all(line.as_bytes()).await;
            }
        }
    }
}

async fn wait_for_socket(path: &Path) {
    loop {
        if path.exists() {
            return;
        }
        sleep(Duration::from_millis(500)).await;
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

pub async fn run(config: WatchdogConfig, mut cmd_rx: mpsc::Receiver<Cmd>) -> Result<()> {
    let node_log = tokio::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&config.node_log_file)
        .await?;

    // Shutdown pipe for cardano-node: EOF on read end (fd 3) triggers graceful shutdown.
    // We hold the write end; dropping it sends EOF.
    let mut pipe_fds: [libc::c_int; 2] = [-1, -1];
    unsafe {
        libc::pipe(pipe_fds.as_mut_ptr());
        // Write end should not be inherited by wallet or other children
        let flags = libc::fcntl(pipe_fds[1], libc::F_GETFD);
        libc::fcntl(pipe_fds[1], libc::F_SETFD, flags | libc::FD_CLOEXEC);
    }
    let pipe_read_fd = pipe_fds[0];
    let pipe_write_fd = pipe_fds[1];

    // Spawn cardano-node
    let mut node_cmd = Command::new(&config.node.exe);
    node_cmd
        .args(&config.node.args)
        .args(["--shutdown-ipc", "3"])
        .current_dir(&config.node.state_dir)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .kill_on_drop(false);

    unsafe {
        node_cmd.pre_exec(move || {
            if pipe_read_fd != 3 {
                libc::dup2(pipe_read_fd, 3);
                libc::close(pipe_read_fd);
            }
            Ok(())
        });
    }

    let node_started_at = unix_ms();
    let mut node_proc = node_cmd.spawn()?;
    let node_pid = node_proc.id().unwrap_or(0);

    // Close parent's copy of the read end
    unsafe { libc::close(pipe_read_fd); }

    emit(&Event::NodeStarted { pid: node_pid, started_at_unix_ms: node_started_at });
    info!("cardano-node started (PID {node_pid})");

    // Forward node logs
    let node_log2 = node_log.try_clone().await?;
    tokio::spawn(pipe_to_file(node_proc.stdout.take().unwrap(), node_log2));
    tokio::spawn(pipe_to_file(node_proc.stderr.take().unwrap(), node_log));

    // Watch node for exit
    let (node_tx, node_rx) = watch::channel::<Option<ExitInfo>>(None);
    tokio::spawn(async move {
        let status = node_proc.wait().await;
        let _ = node_tx.send(Some(extract_exit(status.ok())));
    });

    // Wait for node socket (120s timeout)
    let socket_path = Path::new(&config.node.socket_path);
    info!("waiting for node socket: {}", socket_path.display());
    timeout(Duration::from_secs(120), wait_for_socket(socket_path))
        .await
        .map_err(|_| anyhow::anyhow!("Timed out waiting for cardano-node socket"))?;
    info!("node socket ready");

    // Wallet supervisor loop
    let wallet_cfg = config.wallet.clone();
    let wallet_log_path = config.wallet_log_file.clone();
    let mut attempt = 0u32;
    let mut node_rx = node_rx;

    'supervisor: loop {
        // If node already gone, stop immediately
        if node_rx.borrow().is_some() {
            break;
        }

        let wallet_log = tokio::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&wallet_log_path)
            .await?;

        let mut wallet_cmd = Command::new(&wallet_cfg.exe);
        wallet_cmd
            .args(&wallet_cfg.args)
            .current_dir(&wallet_cfg.state_dir)
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .kill_on_drop(false);

        let wallet_started_at = unix_ms();
        let mut wallet = wallet_cmd.spawn()?;
        let wallet_pid = wallet.id().unwrap_or(0);
        emit(&Event::WalletStarted { pid: wallet_pid, started_at_unix_ms: wallet_started_at });
        info!("cardano-wallet started (PID {wallet_pid}, attempt {attempt})");

        tokio::spawn(pipe_to_file(wallet.stdout.take().unwrap(), wallet_log.try_clone().await?));
        tokio::spawn(pipe_to_file(wallet.stderr.take().unwrap(), wallet_log));

        let port = wallet_cfg.api_port;

        // Phase 1: wait for API ready OR early exit/node-death/stop
        let ready = tokio::select! {
            _ = wait_for_port(port) => true,
            status = wallet.wait() => {
                let info = extract_exit(status.ok());
                warn!("wallet exited before ready ({info:?})");
                emit(&Event::WalletExited { code: info.0, signal: info.1 });
                attempt += 1;
                emit(&Event::WalletRestarting { attempt });
                sleep(Duration::from_millis(wallet_cfg.restart_delay_ms)).await;
                continue 'supervisor;
            }
            _ = node_rx.changed() => {
                let info = node_rx.borrow().clone().unwrap_or((None, None));
                emit(&Event::NodeExited { code: info.0, signal: info.1 });
                sigterm(&wallet);
                let _ = wallet.wait().await;
                break 'supervisor;
            }
            Some(Cmd::Stop) = cmd_rx.recv() => {
                sigterm(&wallet);
                let _ = wallet.wait().await;
                break 'supervisor;
            }
        };

        if ready {
            emit(&Event::WalletReady { port });
            info!("wallet API ready on port {port}");
        }

        // Phase 2: wallet is ready — wait for exit, node death, or stop
        tokio::select! {
            status = wallet.wait() => {
                let info = extract_exit(status.ok());
                warn!("wallet exited ({info:?})");
                emit(&Event::WalletExited { code: info.0, signal: info.1 });
                attempt += 1;
                emit(&Event::WalletRestarting { attempt });
                sleep(Duration::from_millis(wallet_cfg.restart_delay_ms)).await;
                continue 'supervisor;
            }
            _ = node_rx.changed() => {
                let info = node_rx.borrow().clone().unwrap_or((None, None));
                emit(&Event::NodeExited { code: info.0, signal: info.1 });
                sigterm(&wallet);
                let _ = wallet.wait().await;
                break 'supervisor;
            }
            Some(Cmd::Stop) = cmd_rx.recv() => {
                sigterm(&wallet);
                let _ = wallet.wait().await;
                break 'supervisor;
            }
        }
    }

    // Shut down node by closing the write end of the shutdown pipe (sends EOF → fd 3)
    info!("closing node shutdown pipe");
    unsafe { libc::close(pipe_write_fd); }

    // Wait up to 30s for node to exit
    timeout(Duration::from_secs(30), async {
        loop {
            if node_rx.borrow().is_some() {
                break;
            }
            let _ = node_rx.changed().await;
        }
    })
    .await
    .unwrap_or_else(|_| warn!("cardano-node did not exit within 30s"));

    emit(&Event::Stopped);
    info!("watchdog stopped");
    Ok(())
}
