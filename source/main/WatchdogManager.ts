import { spawn, ChildProcess } from 'child_process';
import { createInterface } from 'readline';
import { logger } from './utils/logging';

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

export interface WatchdogConfig {
  node: { exe: string; args: string[]; state_dir: string; socket_path: string };
  wallet: {
    exe: string;
    args: string[];
    state_dir: string;
    api_port: number;
    restart_delay_ms?: number;
    max_restart_attempts?: number;
  };
  node_log_file: string;
  wallet_log_file: string;
  mithril?: {
    mithril_bin: string;
    snapshot_converter_bin: string;
    converter_config: string;
    aggregator_url: string;
    genesis_vkey: string;
    ancillary_vkey?: string;
    state_dir: string;
    chain_path: string;
    behind_threshold?: number;
  };
}

export interface MithrilProgress {
  filesDownloaded: number;
  filesTotal: number;
  bytesDownloaded: number;
  bytesTotal: number;
  secondsElapsed: number;
  stepNum: number;
  totalSteps: number;
  phase: 'snapshot' | 'ledger';
}

export interface WatchdogState {
  // Identity
  watchdogPid: number;
  nodePid: number;
  walletPid: number;
  nodeStartedAt: number | null;
  walletStartedAt: number | null;
  walletRestartCount: number;
  walletPort: number | null;

  // Chain/sync
  hasChain: boolean | null;
  nodeStartupPhase: string | null;
  blockSyncProgress: {
    replayedBlock: number;
    validatingChunk: number;
    pushingLedger: number;
  };

  // Mithril
  mithrilPhase: string | null;
  mithrilProgress: MithrilProgress | null;

  // Mithril probe result: set when node is significantly behind the certified tip
  mithrilSignificantlyBehind: {
    localImmutableCount: number;
    latestCertifiedImmutable: number;
  } | null;

  // Error
  lastError: string | null;
  walletUnrecoverable: boolean;

  // Diagnostics
  nodeSocketWaitMs: number | null;
  walletReadyWaitMs: number | null;
  nodeForceKilled: boolean;
  lastWalletExitCode: number | null;
  lastWalletExitSignal: string | null;
}

type EventHandler = (event: Record<string, unknown>) => void;

// ---------------------------------------------------------------------------
// WatchdogManager
// ---------------------------------------------------------------------------

const STOP_TIMEOUT_MS = 45_000;

class WatchdogManager {
  private proc: ChildProcess | null = null;
  private handlers: EventHandler[] = [];
  private state: WatchdogState = WatchdogManager.makeInitialState();

  // wallet-ready promise plumbing
  private _walletReadyResolve: ((port: number) => void) | null = null;
  private _walletReadyReject: ((reason: string) => void) | null = null;
  walletReadyPromise: Promise<number> = new Promise(() => {});
  private _pendingRejection: string | null = null;

  private static makeInitialState(): WatchdogState {
    return {
      watchdogPid: 0,
      nodePid: 0,
      walletPid: 0,
      nodeStartedAt: null,
      walletStartedAt: null,
      walletRestartCount: 0,
      walletPort: null,
      hasChain: null,
      nodeStartupPhase: null,
      blockSyncProgress: { replayedBlock: 0, validatingChunk: 0, pushingLedger: 0 },
      mithrilPhase: null,
      mithrilProgress: null,
      mithrilSignificantlyBehind: null,
      lastError: null,
      walletUnrecoverable: false,
      nodeSocketWaitMs: null,
      walletReadyWaitMs: null,
      nodeForceKilled: false,
      lastWalletExitCode: null,
      lastWalletExitSignal: null,
    };
  }

  // ---------------------------------------------------------------------------
  // Lifecycle
  // ---------------------------------------------------------------------------

  start(exePath: string, config: WatchdogConfig): void {
    this.state = WatchdogManager.makeInitialState();
    this._pendingRejection = null;

    this.walletReadyPromise = new Promise<number>((resolve, reject) => {
      this._walletReadyResolve = resolve;
      this._walletReadyReject = reject;
    });

    const proc = spawn(exePath, [], {
      stdio: ['pipe', 'pipe', 'pipe'],
    });
    this.proc = proc;

    logger.info('WatchdogManager: spawned watchdog', { pid: proc.pid, exe: exePath });

    // Write config as first stdin line
    const configLine = JSON.stringify(config) + '\n';
    proc.stdin!.write(configLine);

    // Read stdout line-by-line
    const rl = createInterface({ input: proc.stdout!, crlfDelay: Infinity });
    rl.on('line', (line) => {
      if (!line.trim()) return;
      let event: Record<string, unknown>;
      try {
        event = JSON.parse(line);
      } catch (e) {
        logger.warn('WatchdogManager: failed to parse stdout line', { line });
        return;
      }
      this.handleEvent(event);
    });

    proc.stderr?.on('data', (chunk: Buffer) => {
      logger.warn('WatchdogManager stderr', { text: chunk.toString() });
    });

    proc.on('exit', (code, signal) => {
      logger.info('WatchdogManager: process exited', { code, signal });
      if (this._pendingRejection != null) {
        this._walletReadyReject?.(this._pendingRejection);
      } else {
        // Unexpected exit — reject so BackendLifecycle can schedule a restart
        this._walletReadyReject?.('watchdog_exited_unexpectedly');
      }
      this._walletReadyResolve = null;
      this._walletReadyReject = null;
    });
  }

  // ---------------------------------------------------------------------------
  // Commands
  // ---------------------------------------------------------------------------

  sendCommand(cmd: object): void {
    if (!this.proc?.stdin?.writable) {
      logger.warn('WatchdogManager: sendCommand called but stdin not writable', { cmd });
      return;
    }
    this.proc.stdin.write(JSON.stringify(cmd) + '\n');
  }

  stop(): Promise<void> {
    return new Promise<void>((resolve) => {
      const proc = this.proc;
      if (!proc) {
        resolve();
        return;
      }

      const timer = setTimeout(() => {
        logger.warn('WatchdogManager: stop timeout after 45s; proceeding');
        resolve();
      }, STOP_TIMEOUT_MS);

      proc.once('exit', () => {
        clearTimeout(timer);
        resolve();
      });

      this.sendCommand({ cmd: 'stop' });
      proc.stdin?.end();
    });
  }

  // ---------------------------------------------------------------------------
  // Event handlers
  // ---------------------------------------------------------------------------

  onEvent(handler: EventHandler): void {
    this.handlers.push(handler);
  }

  getState(): WatchdogState {
    return this.state;
  }

  // ---------------------------------------------------------------------------
  // Private: event dispatch & state updates
  // ---------------------------------------------------------------------------

  private handleEvent(event: Record<string, unknown>): void {
    const eventType = event.event as string | undefined;
    if (eventType !== 'node_block_sync_progress') {
      logger.info('WatchdogManager event:', { ...event });
    }

    const s = this.state;

    switch (eventType) {
      case 'watchdog_started':
        s.watchdogPid = event.pid as number;
        break;

      case 'chain_status':
        s.hasChain = event.has_chain as boolean;
        break;

      case 'node_started':
        s.nodePid = event.pid as number;
        s.nodeStartedAt = event.started_at_unix_ms as number;
        break;

      case 'node_socket_ready':
        s.nodeSocketWaitMs = event.waited_ms as number;
        break;

      case 'node_startup_status':
        s.nodeStartupPhase = event.phase as string;
        break;

      case 'node_block_sync_progress': {
        const kind = event.kind as string;
        const progress = event.progress as number;
        if (kind === 'replayedBlock') {
          s.blockSyncProgress.replayedBlock = progress;
        } else if (kind === 'validatingChunk') {
          s.blockSyncProgress.validatingChunk = progress;
        } else if (kind === 'pushingLedger') {
          s.blockSyncProgress.pushingLedger = progress;
        }
        break;
      }

      case 'node_force_killed':
        s.nodeForceKilled = true;
        break;

      case 'wallet_started':
        s.walletPid = event.pid as number;
        s.walletStartedAt = event.started_at_unix_ms as number;
        break;

      case 'wallet_ready':
        s.walletPort = event.port as number;
        s.walletReadyWaitMs = event.waited_ms as number;
        this._walletReadyResolve?.(event.port as number);
        this._walletReadyResolve = null;
        this._walletReadyReject = null;
        break;

      case 'wallet_exited':
        s.lastWalletExitCode = event.code as number | null;
        s.lastWalletExitSignal = event.signal as string | null;
        break;

      case 'wallet_restarting':
        s.walletRestartCount = event.attempt as number;
        break;

      case 'wallet_unrecoverable':
        s.walletUnrecoverable = true;
        this._pendingRejection = 'wallet_unrecoverable';
        break;

      case 'mithril_significantly_behind':
        s.mithrilSignificantlyBehind = {
          localImmutableCount: event.local_immutable_count as number,
          latestCertifiedImmutable: event.latest_certified_immutable as number,
        };
        break;

      case 'mithril_status':
        s.mithrilPhase = event.phase as string;
        break;

      case 'mithril_progress':
        s.mithrilProgress = {
          filesDownloaded: event.files_downloaded as number,
          filesTotal: event.files_total as number,
          bytesDownloaded: event.bytes_downloaded as number,
          bytesTotal: event.bytes_total as number,
          secondsElapsed: event.seconds_elapsed as number,
          stepNum: event.step_num as number,
          totalSteps: event.total_steps as number,
          phase: event.phase as 'snapshot' | 'ledger',
        };
        break;

      case 'mithril_error':
        s.mithrilPhase = 'error';
        s.lastError = event.message as string;
        break;

      case 'error':
        s.lastError = event.message as string;
        break;

      case 'stopped':
        // Terminal — nothing to update; process will exit shortly
        break;

      default:
        logger.debug('WatchdogManager: unhandled event type', { eventType });
        break;
    }

    // Dispatch to all registered handlers
    for (const handler of this.handlers) {
      try {
        handler(event);
      } catch (e) {
        logger.error('WatchdogManager: event handler threw', { error: e });
      }
    }
  }
}

export default WatchdogManager;
