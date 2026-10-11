import { createInterface } from 'readline';
import type { Interface as ReadlineInterface } from 'readline';
import net from 'net';
import { logger } from './utils/logging';
import type {
  BackendStopProgress,
  RequestedRestart,
} from '../common/types/watchdog.types';

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
  // Runtime overrides (from watchdog-state.json)
  nodeExtraArgs: string[];

  // Shutdown
  shutdownRequested: boolean;
  backendStopProgress: BackendStopProgress | null;

  // cardano-node crashed too often; the watchdog waits for a retry
  nodeUnrecoverable: boolean;
  // A restart the user asked for, until it has stopped the process
  requestedRestart: RequestedRestart | null;
}

type EventHandler = (event: Record<string, unknown>) => void;

// ---------------------------------------------------------------------------
// WatchdogManager
// ---------------------------------------------------------------------------

// How long stop() waits for the watchdog's first answer to the stop command.
export const STOP_ACK_TIMEOUT_MS = 30_000;

// How long stop() keeps waiting past the bound the watchdog reports for the
// stage it is in. The watchdog kills a process that reaches that bound, so
// this only expires when the watchdog itself has stopped responding.
export const STOP_MARGIN_MS = 30_000;

// How long stop() waits, after the watchdog has reported the backend stopped,
// for its report on starting a requested update installer.
export const INSTALLER_RESULT_TIMEOUT_MS = 30_000;

// What came of an install_update request. message says why it failed.
export type InstallerResult = { launched: boolean; message: string | null };

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

  // stop() plumbing
  private _channelClosed = false;
  private _watchdogStopped = false;
  private _stopPromise: Promise<void> | null = null;
  private _finishStop: (() => void) | null = null;
  private _stopTimer: ReturnType<typeof setTimeout> | null = null;
  private _stopDeadline = 0;

  // True from node_started until the node exits or is stopped
  private _nodeRunning = false;

  // install_update plumbing: requested, then the watchdog's answer
  private _installRequested = false;
  private _installResult: InstallerResult | null = null;

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
      nodeExtraArgs: [],
      shutdownRequested: false,
      backendStopProgress: null,
      nodeUnrecoverable: false,
      requestedRestart: null,
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

    this._attachChannel(rl);
  }

  // Reads watchdog events from `rl`, one JSON object per line.
  private _attachChannel(rl: ReadlineInterface): void {
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
      this._channelClosed = true;
      this._finishStop?.();
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
      this._noteRequestedRestart(cmd);
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
    this._noteRequestedRestart(cmd);
  }

  // Commands that make the watchdog stop and start a running process again.
  // Sent while the node is not running they restart nothing.
  private _noteRequestedRestart(cmd: object): void {
    if (!this._nodeRunning) return;
    const name = (cmd as { cmd?: string }).cmd;
    if (
      name === 'restart_node' ||
      name === 'set_node_extra_args' ||
      name === 'set_chain_path'
    ) {
      this.state.requestedRestart = 'node';
    } else if (
      name === 'restart_wallet' &&
      this.state.requestedRestart !== 'node'
    ) {
      this.state.requestedRestart = 'wallet';
    }
  }

  // Asks the watchdog to stop the backend and resolves once it reports that it
  // has stopped, or its IPC channel closes. Every call returns the same promise,
  // so a second quit request neither sends a second stop nor resolves early.
  //
  // The watchdog bounds every stage of a stop and reports each stage with its
  // bound in backend_stop_progress events. The safety timer below is extended
  // past each reported bound, so it only fires when the watchdog has stopped
  // answering.
  stop(): Promise<void> {
    if (this._stopPromise) return this._stopPromise;
    this.state.shutdownRequested = true;
    this._stopPromise = new Promise<void>((resolve) => {
      if (!this.rl || this._channelClosed || this._watchdogStopped) {
        resolve();
        return;
      }
      this._finishStop = () => {
        if (this._stopTimer) clearTimeout(this._stopTimer);
        this._stopTimer = null;
        this._finishStop = null;
        resolve();
      };
      this._extendStopDeadline(STOP_ACK_TIMEOUT_MS);
      this.sendCommand({ cmd: 'stop' });
    });
    return this._stopPromise;
  }

  // Asks the watchdog to start the update installer at `path` once it has
  // stopped cardano-wallet and cardano-node, which the installer replaces. The
  // watchdog treats it as a stop; stop() then also waits for its answer.
  requestInstall(path: string, args: Array<string> = []): void {
    this._installRequested = true;
    this.sendCommand({ cmd: 'install_update', path, args });
  }

  // null when no installer was requested. Otherwise the watchdog's answer, or
  // a failure when none came before stop() resolved.
  getInstallResult(): InstallerResult | null {
    if (!this._installRequested) return null;
    return (
      this._installResult ?? {
        launched: false,
        message: 'The watchdog did not report starting the installer.',
      }
    );
  }

  private _extendStopDeadline(ms: number): void {
    const deadline = Date.now() + ms;
    if (this._stopTimer && deadline <= this._stopDeadline) return;
    if (this._stopTimer) clearTimeout(this._stopTimer);
    this._stopDeadline = deadline;
    this._stopTimer = setTimeout(() => {
      logger.warn(
        'WatchdogManager: watchdog gave no stop progress in time; proceeding',
        { waitedMs: ms, backendStopProgress: this.state.backendStopProgress }
      );
      this._finishStop?.();
    }, ms);
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
    const isRepeatedStopProgress =
      eventType === 'backend_stop_progress' &&
      this.state.backendStopProgress?.stage === event.stage;
    if (
      eventType !== 'node_block_sync_progress' &&
      eventType !== 'mithril_progress' &&
      !isRepeatedStopProgress
    ) {
      logger.info('WatchdogManager event:', { ...event });
    }

    const s = this.state;

    switch (eventType) {
      case 'watchdog_started':
        s.watchdogPid = event.pid as number;
        s.nodeExtraArgs = (event.node_extra_args as string[]) ?? [];
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
        s.backendStopProgress = null;
        // A node start is a retry or restart: whatever was unrecoverable
        // before is being tried again, and a requested restart has stopped
        // the old node.
        s.nodeUnrecoverable = false;
        s.walletUnrecoverable = false;
        s.requestedRestart = null;
        this._nodeRunning = true;
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
        s.backendStopProgress = null;
        s.walletUnrecoverable = false;
        break;

      case 'wallet_ready':
        s.walletPort = event.port as number;
        s.walletReadyWaitMs = event.waited_ms as number;
        // Wallet ready implies chain data exists — set hasChain so loadingPhase
        // can progress past 'starting' when Electron restarts mid-session and
        // the watchdog skips re-emitting chain_status.
        s.hasChain = true;
        s.requestedRestart = null;
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
        s.requestedRestart = null;
        this._pendingRejection = 'wallet_unrecoverable';
        break;

      case 'node_unrecoverable':
        s.nodeUnrecoverable = true;
        s.requestedRestart = null;
        this._nodeRunning = false;
        break;

      case 'node_exited':
      case 'node_shutdown_ms':
        this._nodeRunning = false;
        break;

      case 'mithril_significantly_behind':
        s.mithrilSignificantlyBehind = {
          localImmutableCount: event.local_immutable_count as number,
          latestCertifiedImmutable: event.latest_certified_immutable as number,
        };
        break;

      case 'mithril_status':
        s.mithrilPhase = event.phase as string;
        // A Mithril sync replaced the requested node restart.
        if (s.requestedRestart === 'node') s.requestedRestart = null;
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
        // Terminal: the backend is stopped and the watchdog exits next.
        this._watchdogStopped = true;
        this._nodeRunning = false;
        s.requestedRestart = null;
        if (this._installRequested && this._installResult === null) {
          // The installer starts next; wait for the watchdog's report on it.
          if (this._finishStop) {
            this._extendStopDeadline(INSTALLER_RESULT_TIMEOUT_MS);
          }
        } else {
          this._finishStop?.();
        }
        break;

      case 'update_installer_launched':
        this._installResult = { launched: true, message: null };
        this._finishStop?.();
        break;

      case 'update_installer_failed':
        this._installResult = {
          launched: false,
          message: event.message as string,
        };
        this._finishStop?.();
        break;

      case 'activate_window':
        // No state; index.ts brings the main window forward.
        break;

      case 'migrate_state_saved':
        // No state; BackendLifecycle deletes the migrated settings.
        break;

      case 'backend_stop_progress': {
        const progress: BackendStopProgress = {
          stage: event.stage as BackendStopProgress['stage'],
          elapsedMs: event.elapsed_ms as number,
          timeoutMs: event.timeout_ms as number,
        };
        s.backendStopProgress = progress;
        if (this._finishStop) {
          const remainingMs = Math.max(
            0,
            progress.timeoutMs - progress.elapsedMs
          );
          this._extendStopDeadline(remainingMs + STOP_MARGIN_MS);
        }
        break;
      }

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
