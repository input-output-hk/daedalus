import { spawn } from 'child_process';
import * as net from 'net';
import * as path from 'path';
import type { NodeConfig } from '../config';
import { logger } from '../utils/logging';

export type WatchdogOptions = {
  watchdogBin: string;
  nodeBin: string;
  walletBin: string;
  stateDir: string;
  nodeConfig: NodeConfig;
  tlsPath: string;
  configPath: string;
  syncTolerance: string;
  isStaging: boolean;
  cluster: string;
  metadataUrl?: string;
  rtsFlags: Array<string>;
  nodeLogFile: string;
  walletLogFile: string;
};

export type WatchdogHandle = {
  pid: number;
  wpid: number;
  watchdogPid: number;
  connected: boolean;
  walletPort: number;
  nodeStartedAt: number | null;
  walletStartedAt: number | null;
  walletRestartCount: number;
  nodeForceKilled: boolean;
  lastWalletExitCode: number | null;
  lastWalletExitSignal: string | null;
  nodeSocketWaitMs: number | null;
  walletReadyWaitMs: number | null;
  stop(timeoutSeconds?: number): Promise<void>;
  kill(): void;
  send(msg: object): void;
};

type WatchdogEvent =
  | { event: 'watchdog_started'; pid: number }
  | { event: 'node_started'; pid: number; started_at_unix_ms: number }
  | { event: 'wallet_started'; pid: number; started_at_unix_ms: number }
  | { event: 'wallet_ready'; port: number; waited_ms: number }
  | {
      event: 'wallet_exited';
      code: number | null;
      signal: string | null;
      phase: string;
    }
  | {
      event: 'wallet_restarting';
      attempt: number;
      last_exit_code: number | null;
      last_exit_signal: string | null;
    }
  | { event: 'node_socket_ready'; waited_ms: number }
  | { event: 'node_force_killed' }
  | { event: 'wallet_unrecoverable'; attempt: number }
  | { event: 'node_shutdown_ms'; ms: number; force_killed: boolean }
  | { event: 'node_exited'; code: number | null; signal: string | null }
  | { event: 'stopped' }
  | { event: 'error'; message: string };

function getFreePort(): Promise<number> {
  return new Promise((resolve, reject) => {
    const srv = net.createServer();
    srv.listen(0, '127.0.0.1', () => {
      const port = (srv.address() as net.AddressInfo).port;
      srv.close(() => resolve(port));
    });
    srv.on('error', reject);
  });
}

function buildNodeArgs(opts: WatchdogOptions, nodePort: number): string[] {
  const { stateDir, nodeConfig, rtsFlags } = opts;
  const { configFile, topologyFile } = nodeConfig.network;
  const rtsOpts = [...(rtsFlags ?? []), '-N'];

  const args = [
    'run',
    '--socket-path',
    process.platform === 'win32'
      ? '\\\\.\\pipe\\cardano-node.socket'
      : 'cardano-node.socket',
    '--topology',
    topologyFile,
    '--database-path',
    'chain',
    '--port',
    String(nodePort),
    '--config',
    configFile,
  ];

  if (nodeConfig.signingKey) {
    args.push('--signing-key', nodeConfig.signingKey);
  }
  if (nodeConfig.delegationCertificate) {
    args.push('--delegation-certificate', nodeConfig.delegationCertificate);
  }

  args.push('+RTS', ...rtsOpts, '-RTS');
  return args;
}

function buildWalletArgs(opts: WatchdogOptions, walletPort: number): string[] {
  const {
    stateDir,
    tlsPath,
    syncTolerance,
    metadataUrl,
    isStaging,
    nodeConfig,
  } = opts;
  const socketPath =
    process.platform === 'win32'
      ? '\\\\.\\pipe\\cardano-node.socket'
      : path.join(stateDir, 'cardano-node.socket');
  const walletDb = path.join(stateDir, 'wallets');
  const syncToleranceSecs = parseInt(syncTolerance.replace('s', ''), 10);
  const configDir = path.dirname(nodeConfig.network.configFile);

  const args = [
    'serve',
    '+RTS',
    '-N',
    '-RTS',
    '--port',
    String(walletPort),
    '--database',
    walletDb,
    '--tls-ca-cert',
    path.join(tlsPath, 'server/ca.crt'),
    '--tls-sv-cert',
    path.join(tlsPath, 'server/server.crt'),
    '--tls-sv-key',
    path.join(tlsPath, 'server/server.key'),
    '--node-socket',
    socketPath,
  ];

  if (isStaging) {
    args.push('--mainnet');
  } else {
    args.push('--testnet', path.join(configDir, 'genesis-byron.json'));
  }

  if (!Number.isNaN(syncToleranceSecs)) {
    args.push('--sync-tolerance', `${syncToleranceSecs}s`);
  }

  const tokenMetadataServer = metadataUrl ?? 'https://tokens.cardano.org';
  args.push('--token-metadata-server', tokenMetadataServer);

  return args;
}

export async function startWatchdog(
  opts: WatchdogOptions,
  onNodeExited: (code: number | null, signal: string | null) => void,
  onWalletExited: (code: number | null, signal: string | null) => void,
  onWalletRestarted?: () => void,
  onWatchdogCrashed?: (code: number | null, signal: string | null) => void
): Promise<WatchdogHandle> {
  const [nodePort, walletPort] = await Promise.all([
    getFreePort(),
    getFreePort(),
  ]);

  const watchdogConfig = {
    node: {
      exe: opts.nodeBin,
      args: buildNodeArgs(opts, nodePort),
      state_dir: opts.stateDir,
      socket_path:
        process.platform === 'win32'
          ? '\\\\.\\pipe\\cardano-node.socket'
          : path.join(opts.stateDir, 'cardano-node.socket'),
    },
    wallet: {
      exe: opts.walletBin,
      args: buildWalletArgs(opts, walletPort),
      state_dir: opts.stateDir,
      api_port: walletPort,
      restart_delay_ms: 1000,
    },
    node_log_file: opts.nodeLogFile,
    wallet_log_file: opts.walletLogFile,
  };

  const proc = spawn(opts.watchdogBin, [], {
    stdio: ['pipe', 'pipe', 'pipe'],
    detached: false,
  });

  // Log watchdog stderr
  proc.stderr?.on('data', (chunk: Buffer) => {
    logger.info('[watchdog]', { msg: chunk.toString().trim() });
  });

  // Write config as first stdin line
  proc.stdin?.write(JSON.stringify(watchdogConfig) + '\n');

  const handle: WatchdogHandle = {
    pid: 0,
    wpid: 0,
    watchdogPid: 0,
    connected: false,
    walletPort,
    nodeStartedAt: null,
    walletStartedAt: null,
    walletRestartCount: 0,
    nodeForceKilled: false,
    lastWalletExitCode: null,
    lastWalletExitSignal: null,
    nodeSocketWaitMs: null,
    walletReadyWaitMs: null,
    stop(timeoutSeconds = 30) {
      return new Promise((resolve) => {
        proc.stdin?.write(JSON.stringify({ cmd: 'stop' }) + '\n');
        // Close stdin so the watchdog's blocking stdin-reader thread sees EOF
        // and the Tokio runtime can finish its shutdown instead of hanging.
        proc.stdin?.end();
        const t = setTimeout(() => {
          proc.kill('SIGKILL');
          resolve();
        }, timeoutSeconds * 1000);
        proc.once('exit', () => {
          clearTimeout(t);
          resolve();
        });
      });
    },
    kill() {
      proc.kill('SIGKILL');
    },
    send(_msg: object) {
      // Fault injection not yet supported via watchdog IPC
    },
  };

  return new Promise((resolve, reject) => {
    let resolved = false;

    const startupTimeoutHandle = setTimeout(() => {
      if (!resolved) {
        resolved = true;
        proc.kill('SIGKILL');
        reject(
          new Error(
            'cardano-watchdog: timed out waiting for wallet to become ready'
          )
        );
      }
    }, 120_000);

    const rl = require('readline').createInterface({ input: proc.stdout });
    rl.on('line', (line: string) => {
      let ev: WatchdogEvent;
      try {
        ev = JSON.parse(line);
      } catch {
        return;
      }

      logger.info('watchdog event', { ev });

      switch (ev.event) {
        case 'watchdog_started':
          handle.watchdogPid = ev.pid;
          break;
        case 'node_started':
          handle.pid = ev.pid;
          handle.nodeStartedAt = ev.started_at_unix_ms;
          break;
        case 'wallet_started':
          handle.wpid = ev.pid;
          handle.walletStartedAt = ev.started_at_unix_ms;
          break;
        case 'wallet_ready':
          handle.walletReadyWaitMs = ev.waited_ms;
          handle.connected = true;
          if (!resolved) {
            resolved = true;
            clearTimeout(startupTimeoutHandle);
            resolve(handle);
          } else {
            onWalletRestarted?.();
          }
          break;
        case 'wallet_exited':
          handle.connected = false;
          logger.info(`watchdog wallet_exited (phase=${ev.phase})`, {
            code: ev.code,
            signal: ev.signal,
            phase: ev.phase,
          });
          onWalletExited(ev.code, ev.signal);
          break;
        case 'wallet_restarting':
          handle.walletRestartCount = ev.attempt;
          handle.lastWalletExitCode = ev.last_exit_code;
          handle.lastWalletExitSignal = ev.last_exit_signal;
          logger.info(`watchdog restarting wallet (attempt ${ev.attempt})`, {
            last_exit_code: ev.last_exit_code,
            last_exit_signal: ev.last_exit_signal,
          });
          break;
        case 'node_socket_ready':
          handle.nodeSocketWaitMs = ev.waited_ms;
          logger.info(`watchdog node_socket_ready after ${ev.waited_ms}ms`, {
            waited_ms: ev.waited_ms,
          });
          break;
        case 'node_force_killed':
          handle.nodeForceKilled = true;
          logger.warn('watchdog: cardano-node was force-killed');
          break;
        case 'wallet_unrecoverable':
          logger.error(
            `watchdog: wallet is unrecoverable after ${ev.attempt} attempt(s)`,
            { attempt: ev.attempt }
          );
          if (!resolved) {
            resolved = true;
            clearTimeout(startupTimeoutHandle);
            reject(
              new Error(
                `cardano-watchdog: wallet is unrecoverable after ${ev.attempt} attempt(s)`
              )
            );
          }
          break;
        case 'node_shutdown_ms':
          logger.info(
            `watchdog node_shutdown_ms: ${ev.ms}ms (force_killed=${ev.force_killed})`,
            { ms: ev.ms, force_killed: ev.force_killed }
          );
          break;
        case 'node_exited':
          onNodeExited(ev.code, ev.signal);
          break;
        case 'error':
          logger.error('watchdog error', { message: ev.message });
          if (!resolved) {
            resolved = true;
            clearTimeout(startupTimeoutHandle);
            reject(new Error(ev.message));
          }
          break;
        default:
          break;
      }
    });

    proc.on('exit', (code, signal) => {
      handle.connected = false;
      if (!resolved) {
        resolved = true;
        clearTimeout(startupTimeoutHandle);
        reject(
          new Error(
            `cardano-watchdog exited unexpectedly (code=${code}, signal=${signal})`
          )
        );
      } else {
        onWatchdogCrashed?.(code, signal);
      }
    });

    proc.on('error', (err) => {
      if (!resolved) {
        resolved = true;
        clearTimeout(startupTimeoutHandle);
        reject(err);
      }
    });
  });
}
