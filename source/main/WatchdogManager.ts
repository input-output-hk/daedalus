import { createInterface } from 'readline';
import type { Interface as ReadlineInterface } from 'readline';
import net from 'net';
import { logger } from './utils/logging';

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

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
  private rl: ReadlineInterface | null = null;
  private socket: net.Socket | null = null;
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
      blockSyncProgress: {
        replayedBlock: 0,
        validatingChunk: 0,
        pushingLedger: 0,
      },
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

  // In the new architecture watchdog is PID 1 and spawned Electron as a child.
  // On Windows, events and commands travel over a named pipe (DAEDALUS_IPC_PIPE)
  // because Chromium may reassign stdin/stdout handles during browser-process
  // startup. On other platforms stdin/stdout are used as before.
  start(): void {
    this.state = WatchdogManager.makeInitialState();
    this._pendingRejection = null;

    this.walletReadyPromise = new Promise<number>((resolve, reject) => {
      this._walletReadyResolve = resolve;
      this._walletReadyReject = reject;
    });

    const pipeName = process.env.DAEDALUS_IPC_PIPE;

    let rl: ReadlineInterface;

    if (pipeName) {
      logger.info('WatchdogManager: connecting to IPC named pipe', {
        pipeName,
      });
      const socket = net.createConnection({ path: pipeName });
      this.socket = socket;

      socket.on('error', (err) => {
        logger.error('WatchdogManager: IPC pipe error', { error: String(err) });
      });

      rl = createInterface({ input: socket, crlfDelay: Infinity });
    } else {
      logger.info('WatchdogManager: listening on stdin (watchdog is parent)');
      rl = createInterface({ input: process.stdin, crlfDelay: Infinity });
    }

    this.rl = rl;

    rl.on('line', (line) => {
      if (!line.trim()) return;
      let event: Record<string, unknown>;
      try {
        event = JSON.parse(line);
      } catch (e) {
        logger.warn('WatchdogManager: failed to parse IPC line', { line });
        return;
      }
      this.handleEvent(event);
    });

    // Pipe/socket closes when watchdog exits or disconnects.
    rl.on('close', () => {
      logger.info('WatchdogManager: IPC channel closed (watchdog exited)');
      this.socket = null;
      if (this._pendingRejection != null) {
        this._walletReadyReject?.(this._pendingRejection);
      } else {
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
    const line = JSON.stringify(cmd) + '\n';
    if (this.socket) {
      if (!this.socket.writable) {
        logger.warn(
          'WatchdogManager: sendCommand called but IPC pipe not writable',
          { cmd }
        );
        return;
      }
      this.socket.write(line);
      return;
    }
    if (!process.stdout.writable) {
      logger.warn(
        'WatchdogManager: sendCommand called but stdout not writable',
        { cmd }
      );
      return;
    }
    process.stdout.write(line);
  }

  stop(): Promise<void> {
    return new Promise<void>((resolve) => {
      const rl = this.rl;
      if (!rl) {
        resolve();
        return;
      }

      const timer = setTimeout(() => {
        logger.warn('WatchdogManager: stop timeout after 45s; proceeding');
        resolve();
      }, STOP_TIMEOUT_MS);

      rl.once('close', () => {
        clearTimeout(timer);
        resolve();
      });

      this.sendCommand({ cmd: 'stop' });
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
    if (
      eventType !== 'node_block_sync_progress' &&
      eventType !== 'mithril_progress'
    ) {
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
        // Reset wallet and startup state so the renderer goes back to
        // 'node-starting' while the restarted node works through its
        // startup sequence before the wallet comes up again.
        s.walletPort = null;
        s.walletPid = 0;
        s.walletStartedAt = null;
        s.nodeStartupPhase = null;
        s.blockSyncProgress = {
          replayedBlock: 0,
          validatingChunk: 0,
          pushingLedger: 0,
        };
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
