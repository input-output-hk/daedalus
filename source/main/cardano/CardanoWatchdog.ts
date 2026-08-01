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
  mithrilBin?: string;
  snapshotConverterBin?: string;
  converterConfig?: string;
  aggregatorUrl?: string;
  genesisVkey?: string;
  ancillaryVkey?: string;
  mithrilBehindThreshold?: number;
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
  nodeStartupPhase: string | null;
  blockSyncProgress: {
    replayedBlock: number;
    validatingChunk: number;
    pushingLedger: number;
  };
  mithrilPhase: string | null;
  hasChain: boolean | null;
  stop(timeoutSeconds?: number): Promise<void>;
  kill(): void;
  send(msg: object): void;
  startMithril(opts?: { force?: boolean; wipeChain?: boolean }): void;
  cancelMithril(): void;
  probeMithril(): void;
  startNode(): void;
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
  | {
      event: 'node_block_sync_progress';
      kind: 'replayedBlock' | 'validatingChunk' | 'pushingLedger';
      progress: number;
    }
  | { event: 'node_startup_status'; phase: string }
  | { event: 'stopped' }
  | { event: 'error'; message: string }
  | { event: 'mithril_status'; phase: string }
  | {
      event: 'mithril_progress';
      files_downloaded: number;
      files_total: number;
      bytes_downloaded: number;
      bytes_total: number;
      seconds_elapsed: number;
      step_num: number;
      total_steps: number;
      phase: 'snapshot' | 'ledger';
    }
  | {
      event: 'mithril_not_needed';
      local_immutable_count: number;
      latest_certified_immutable: number;
    }
  | {
      event: 'mithril_significantly_behind';
      local_immutable_count: number;
      latest_certified_immutable: number;
    }
  | { event: 'mithril_error'; code: string; message: string }
  | { event: 'chain_status'; has_chain: boolean };

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
  onWatchdogCrashed?: (code: number | null, signal: string | null) => void,
  onBlockSyncProgress?: (
    kind: 'replayedBlock' | 'validatingChunk' | 'pushingLedger',
    progress: number
  ) => void,
  onNodeStartupPhase?: (phase: string) => void,
  onHandleCreated?: (handle: WatchdogHandle) => void,
  onMithrilStatus?: (phase: string) => void,
  onMithrilProgress?: (progress: {
    filesDownloaded: number;
    filesTotal: number;
    bytesDownloaded: number;
    bytesTotal: number;
    secondsElapsed: number;
    stepNum: number;
    totalSteps: number;
    phase: 'snapshot' | 'ledger';
  }) => void,
  onMithrilNotNeeded?: (
    localImmutableCount: number,
    latestCertifiedImmutable: number
  ) => void,
  onMithrilError?: (code: string, message: string) => void,
  onChainStatus?: (hasChain: boolean) => void,
  onMithrilSignificantlyBehind?: (
    localImmutableCount: number,
    latestCertifiedImmutable: number
  ) => void
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
    ...(opts.mithrilBin
      ? {
          mithril: {
            mithril_bin: opts.mithrilBin,
            snapshot_converter_bin: opts.snapshotConverterBin,
            converter_config: opts.converterConfig,
            aggregator_url: opts.aggregatorUrl,
            genesis_vkey: opts.genesisVkey,
            ancillary_vkey: opts.ancillaryVkey,
            state_dir: opts.stateDir,
            chain_path: require('path').join(opts.stateDir, 'chain'),
            behind_threshold: opts.mithrilBehindThreshold,
          },
        }
      : {}),
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
    nodeStartupPhase: null,
    blockSyncProgress: {
      replayedBlock: 0,
      validatingChunk: 0,
      pushingLedger: 0,
    },
    mithrilPhase: null,
    hasChain: null,
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
    startMithril({ force = false, wipeChain = false } = {}) {
      proc.stdin?.write(
        JSON.stringify({
          cmd: 'start_mithril',
          ...(force ? { force: true } : {}),
          ...(wipeChain ? { wipe_chain: true } : {}),
        }) + '\n'
      );
    },
    cancelMithril() {
      proc.stdin?.write(JSON.stringify({ cmd: 'cancel_mithril' }) + '\n');
    },
    probeMithril() {
      proc.stdin?.write(JSON.stringify({ cmd: 'probe_mithril' }) + '\n');
    },
    startNode() {
      proc.stdin?.write(JSON.stringify({ cmd: 'start_node' }) + '\n');
    },
  };

  onHandleCreated?.(handle);

  return new Promise((resolve, reject) => {
    let resolved = false;
    // Set when wallet_unrecoverable fires before the promise has settled.
    // We defer the rejection to proc.on('exit') so the Rust supervisor can
    // finish its node-shutdown sequence before we restart, avoiding a race
    // on the chain database lock file.
    let pendingRejectionMessage: string | null = null;
    // Set when node_exited arrives so proc.on('exit') does not fire a second
    // _handleCardanoNodeExit via onWatchdogCrashed — the node exit was already
    // handled by onNodeExited and the watchdog exiting cleanly is expected.
    let nodeExitedFired = false;

    // No TypeScript-side startup timeout: ledger replay from genesis can take
    // many minutes. The Rust watchdog emits `error` for fatal failures and
    // the proc.on('exit') handler below catches unexpected watchdog exits.

    const rl = require('readline').createInterface({ input: proc.stdout });
    rl.on('line', (line: string) => {
      let ev: WatchdogEvent;
      try {
        ev = JSON.parse(line);
      } catch {
        return;
      }

      if (
        ev.event !== 'node_block_sync_progress' &&
        ev.event !== 'mithril_progress'
      ) {
        logger.info('watchdog event', { ev });
      }

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
            // Defer rejection to proc.on('exit'): by then the Rust supervisor
            // has completed its node shutdown and released the chain DB lock.
            pendingRejectionMessage = `cardano-watchdog: wallet is unrecoverable after ${ev.attempt} attempt(s)`;
          }
          break;
        case 'node_shutdown_ms':
          logger.info(
            `watchdog node_shutdown_ms: ${ev.ms}ms (force_killed=${ev.force_killed})`,
            { ms: ev.ms, force_killed: ev.force_killed }
          );
          break;
        case 'node_exited':
          nodeExitedFired = true;
          onNodeExited(ev.code, ev.signal);
          break;
        case 'node_block_sync_progress':
          handle.blockSyncProgress[ev.kind] = ev.progress;
          onBlockSyncProgress?.(ev.kind, ev.progress);
          break;
        case 'node_startup_status':
          handle.nodeStartupPhase = ev.phase;
          onNodeStartupPhase?.(ev.phase);
          break;
        case 'error':
          logger.error('watchdog error', { message: ev.message });
          if (!resolved) {
            resolved = true;
            reject(new Error(ev.message));
          }
          break;
        case 'mithril_status':
          handle.mithrilPhase = ev.phase;
          onMithrilStatus?.(ev.phase);
          break;
        case 'mithril_progress':
          onMithrilProgress?.({
            filesDownloaded: ev.files_downloaded,
            filesTotal: ev.files_total,
            bytesDownloaded: ev.bytes_downloaded,
            bytesTotal: ev.bytes_total,
            secondsElapsed: ev.seconds_elapsed,
            stepNum: ev.step_num,
            totalSteps: ev.total_steps,
            phase: ev.phase,
          });
          break;
        case 'mithril_not_needed':
          onMithrilNotNeeded?.(
            ev.local_immutable_count,
            ev.latest_certified_immutable
          );
          break;
        case 'mithril_significantly_behind':
          onMithrilSignificantlyBehind?.(
            ev.local_immutable_count,
            ev.latest_certified_immutable
          );
          break;
        case 'mithril_error':
          onMithrilError?.(ev.code, ev.message);
          break;
        case 'chain_status':
          handle.hasChain = ev.has_chain;
          onChainStatus?.(ev.has_chain);
          break;
        default:
          break;
      }
    });

    proc.on('exit', (code, signal) => {
      handle.connected = false;
      if (!resolved) {
        resolved = true;
        reject(
          new Error(
            pendingRejectionMessage ??
              `cardano-watchdog exited unexpectedly (code=${code}, signal=${signal})`
          )
        );
      } else if (!nodeExitedFired) {
        // Watchdog exited without a preceding node_exited event — unexpected crash.
        onWatchdogCrashed?.(code, signal);
      }
      // else: node_exited already called onNodeExited; watchdog exiting is expected.
    });

    proc.on('error', (err) => {
      if (!resolved) {
        resolved = true;
        reject(err);
      }
    });
  });
}
