import {
  isMithrilPartialSyncSuppressingDiskSpaceCheck,
  makeIdlePartialSyncStatus,
  type MithrilPartialSyncStatusSnapshot,
  type MithrilPartialSyncFailureAction,
} from '../../common/types/mithril-partial-sync.types';
import type { MithrilBootstrapStatusUpdate } from '../../common/types/mithril-bootstrap.types';
import { logger } from '../utils/logging';
import type { WatchdogHandle } from '../cardano/CardanoWatchdog';

type StatusSender<T> = (status: T) => Promise<void>;

const PROBE_INTERVAL_MS = 30_000;

// Watchdog mithril phases during which chain data is actively being
// downloaded or installed ('completed'/'cancelled' are terminal).
const ACTIVE_MITHRIL_PHASES = new Set([
  'preparing',
  'downloading',
  'verifying',
  'converting',
  'installing',
  'finalizing',
]);

export class MithrilController {
  _partialSyncStatus: MithrilPartialSyncStatusSnapshot =
    makeIdlePartialSyncStatus();
  _partialSyncStatusSender: StatusSender<MithrilPartialSyncStatusSnapshot> | null =
    null;
  _bootstrapStatusSender: StatusSender<MithrilBootstrapStatusUpdate> | null =
    null;
  _availabilitySender: StatusSender<{
    isEnabled: boolean;
    isSignificantlyBehind: boolean;
  }> | null = null;
  _bootstrapMode = false;
  _bootstrapStatus: MithrilBootstrapStatusUpdate['status'] = 'idle';
  _watchdogHandle: WatchdogHandle | null = null;
  _probeIntervalId: ReturnType<typeof setInterval> | null = null;

  setWatchdogHandle(handle: WatchdogHandle | null): void {
    this._watchdogHandle = handle;
    if (this._probeIntervalId !== null) {
      clearInterval(this._probeIntervalId);
      this._probeIntervalId = null;
    }
    if (handle !== null) {
      this._probeIntervalId = setInterval(() => {
        this._watchdogHandle?.probeMithril();
      }, PROBE_INTERVAL_MS);
    }
  }

  setPartialSyncStatusSender(
    sender: StatusSender<MithrilPartialSyncStatusSnapshot>
  ): void {
    this._partialSyncStatusSender = sender;
  }

  setBootstrapStatusSender(
    sender: StatusSender<MithrilBootstrapStatusUpdate> | null
  ): void {
    this._bootstrapStatusSender = sender;
  }

  setAvailabilitySender(
    sender: StatusSender<{
      isEnabled: boolean;
      isSignificantlyBehind: boolean;
    }> | null
  ): void {
    this._availabilitySender = sender;
  }

  _sendAvailability(isSignificantlyBehind: boolean): void {
    if (!this._availabilitySender) return;
    Promise.resolve()
      .then(() =>
        this._availabilitySender?.({ isEnabled: true, isSignificantlyBehind })
      )
      .catch((error) => {
        logger.warn('MithrilController: failed to send availability status', {
          error,
        });
      });
  }

  onWatchdogMithrilSignificantlyBehind(
    _localImmutableCount: number,
    _latestCertifiedImmutable: number
  ): void {
    this._sendAvailability(true);
  }

  broadcastBootstrapStatus(status: MithrilBootstrapStatusUpdate): void {
    this._bootstrapStatus = status.status;
    if (!this._bootstrapStatusSender) return;
    Promise.resolve()
      .then(() => this._bootstrapStatusSender?.(status))
      .catch((error) => {
        logger.warn('MithrilController: failed to send bootstrap status', {
          error,
        });
      });
  }

  startNode(): void {
    this._bootstrapMode = false;
    this._watchdogHandle?.startNode();
    // Reset partial sync status to idle so overlay closes
    this.broadcastPartialSyncStatus(makeIdlePartialSyncStatus()).catch(
      () => {}
    );
  }

  // Keep backward compat alias used by decision channel handler
  startBootstrapNode(): void {
    this.startNode();
  }

  startMithril({ wipeChain }: { wipeChain: boolean }): void {
    if (wipeChain) {
      this._bootstrapMode = true;
      this._bootstrapStatus = 'idle';
      // Reset partial sync status to idle so that the syncStatusChannel onRequest
      // handler returns the bootstrap _currentStatus rather than stale 'cancelled'.
      this._partialSyncStatus = makeIdlePartialSyncStatus();
    } else {
      const stoppingStatus: MithrilPartialSyncStatusSnapshot = {
        ...this._partialSyncStatus,
        status: 'stopping-node',
      };
      this.broadcastPartialSyncStatus(stoppingStatus).catch(() => {});
    }
    this._watchdogHandle?.startMithril({ force: true, wipeChain });
  }

  startBootstrapMithril(): void {
    this.startMithril({ wipeChain: true });
  }

  cancelMithril(): Promise<void> {
    if (this._bootstrapMode) {
      // In bootstrap mode the 'cancelled' watchdog event goes to the bootstrap
      // sender; don't touch partial-sync status or it will be stuck at 'cancelling'.
      this._watchdogHandle?.cancelMithril();
      return Promise.resolve();
    }
    const cancellingStatus: MithrilPartialSyncStatusSnapshot = {
      ...this._partialSyncStatus,
      status: 'cancelling',
    };
    return this.broadcastPartialSyncStatus(cancellingStatus).then(() => {
      this._watchdogHandle?.cancelMithril();
    });
  }

  getPartialSyncStatus(): MithrilPartialSyncStatusSnapshot {
    return this._partialSyncStatus;
  }

  // Whether the disk-space checker must not stop cardano-node right now.
  // The partial-sync status list alone is not enough: a bootstrap
  // (startMithril with wipeChain) deliberately resets partial-sync status
  // to idle while tens of GB download through the watchdog, and while the
  // watchdog holds an empty chain awaiting the user's genesis-vs-Mithril
  // decision, cardanoNode.stop() would tear the watchdog down and strand
  // both flows.
  isDiskSpaceCheckSuppressed(): boolean {
    if (this._bootstrapMode) return true;
    const handle = this._watchdogHandle;
    if (handle) {
      if (handle.hasChain === false) return true;
      if (
        handle.mithrilPhase !== null &&
        ACTIVE_MITHRIL_PHASES.has(handle.mithrilPhase)
      ) {
        return true;
      }
    }
    return isMithrilPartialSyncSuppressingDiskSpaceCheck(
      this._partialSyncStatus.status
    );
  }

  async broadcastPartialSyncStatus(
    status: MithrilPartialSyncStatusSnapshot
  ): Promise<void> {
    this._partialSyncStatus = status;

    if (!this._partialSyncStatusSender) return;

    Promise.resolve()
      .then(() => this._partialSyncStatusSender?.(status))
      .catch((error) => {
        logger.warn('MithrilController: failed to send partial sync status', {
          error,
        });
      });
  }

  onWatchdogMithrilStatus(phase: string): void {
    if (this._bootstrapMode) {
      this.broadcastBootstrapStatus({
        status: phase as MithrilBootstrapStatusUpdate['status'],
      });
      if (phase === 'finalizing') {
        this._bootstrapMode = false;
      }
      return;
    }
    // Reset stale transfer progress when a new sync cycle begins so the bar
    // doesn't inherit completed-run counts (filesDownloaded === filesTotal)
    // from a previous session and immediately show 100%.
    const isNewSyncStart = phase === 'stopping-node' || phase === 'preparing';
    const status: MithrilPartialSyncStatusSnapshot = {
      ...this._partialSyncStatus,
      ...(isNewSyncStart ? { transferProgress: {} } : {}),
      status: phase as MithrilPartialSyncStatusSnapshot['status'],
    };
    this.broadcastPartialSyncStatus(status).catch((error) => {
      logger.warn(
        'MithrilController: failed to broadcast watchdog mithril status',
        { error }
      );
    });
  }

  onWatchdogMithrilProgress(progress: {
    filesDownloaded: number;
    filesTotal: number;
    bytesDownloaded: number;
    bytesTotal: number;
    secondsElapsed: number;
    stepNum: number;
    totalSteps: number;
    phase: 'snapshot' | 'ledger';
  }): void {
    if (this._bootstrapMode) {
      const bootstrapUpdate: MithrilBootstrapStatusUpdate =
        progress.phase === 'snapshot'
          ? {
              status: this._bootstrapStatus,
              filesDownloaded: progress.filesDownloaded,
              filesTotal: progress.filesTotal,
              ...(progress.bytesTotal > 0
                ? {
                    snapshotBytesDownloaded: progress.bytesDownloaded,
                    snapshotBytesTotal: progress.bytesTotal,
                  }
                : {}),
              elapsedSeconds: progress.secondsElapsed,
            }
          : {
              status: this._bootstrapStatus,
              elapsedSeconds: progress.secondsElapsed,
              ...(progress.bytesTotal > 0
                ? {
                    ancillaryBytesDownloaded: progress.bytesDownloaded,
                    ancillaryBytesTotal: progress.bytesTotal,
                  }
                : {}),
            };
      this.broadcastBootstrapStatus(bootstrapUpdate);
      return;
    }
    const prev = this._partialSyncStatus.transferProgress;
    const transferProgress =
      progress.phase === 'snapshot'
        ? {
            filesDownloaded: progress.filesDownloaded,
            filesTotal: progress.filesTotal,
            ...(progress.bytesTotal > 0
              ? {
                  snapshotBytesDownloaded: progress.bytesDownloaded,
                  snapshotBytesTotal: progress.bytesTotal,
                }
              : {}),
            elapsedSeconds: progress.secondsElapsed,
            // Preserve ledger bytes if the ledger step somehow arrived first.
            ...(typeof prev.ancillaryBytesTotal === 'number'
              ? {
                  ancillaryBytesDownloaded: prev.ancillaryBytesDownloaded,
                  ancillaryBytesTotal: prev.ancillaryBytesTotal,
                }
              : {}),
          }
        : {
            // Snapshot files are all downloaded by the time we hit the ledger step.
            filesDownloaded: prev.filesDownloaded,
            filesTotal: prev.filesTotal,
            elapsedSeconds: progress.secondsElapsed,
            ...(progress.bytesTotal > 0
              ? {
                  ancillaryBytesDownloaded: progress.bytesDownloaded,
                  ancillaryBytesTotal: progress.bytesTotal,
                }
              : {}),
          };
    const status: MithrilPartialSyncStatusSnapshot = {
      ...this._partialSyncStatus,
      transferProgress,
    };
    this.broadcastPartialSyncStatus(status).catch((error) => {
      logger.warn(
        'MithrilController: failed to broadcast watchdog mithril progress',
        { error }
      );
    });
  }

  onWatchdogMithrilError(code: string, message: string): void {
    if (this._bootstrapMode) {
      this._bootstrapMode = false;
      this.broadcastBootstrapStatus({
        status: 'failed',
        error: { message, code: code as any },
      });
      return;
    }
    // Populate recovery actions based on the error boundary:
    // - Pre-cutover failures leave the chain intact → retry and normal restart are safe.
    // - Post-cutover (INSTALL_FAILED) may leave the chain partially installed → only wipe.
    const allowedRecoveryActions: MithrilPartialSyncFailureAction[] =
      code === 'INSTALL_FAILED'
        ? ['wipe-and-full-sync']
        : ['retry', 'restart-normal', 'wipe-and-full-sync'];
    const status: MithrilPartialSyncStatusSnapshot = {
      ...this._partialSyncStatus,
      status: 'failed',
      error: { message, code: code as any },
      allowedRecoveryActions,
    };
    this.broadcastPartialSyncStatus(status).catch((error) => {
      logger.warn(
        'MithrilController: failed to broadcast watchdog mithril error',
        { error }
      );
    });
  }

  onWatchdogMithrilNotNeeded(): void {
    if (this._bootstrapMode) {
      // Shouldn't happen during bootstrap (force=true), but reset mode if it does.
      this._bootstrapMode = false;
    }
    this._sendAvailability(false);
    const status: MithrilPartialSyncStatusSnapshot =
      makeIdlePartialSyncStatus();
    this.broadcastPartialSyncStatus(status).catch((error) => {
      logger.warn(
        'MithrilController: failed to broadcast watchdog mithril not-needed',
        { error }
      );
    });
  }

  async startPartialSync(): Promise<void> {
    this.startMithril({ wipeChain: false });
  }

  async cancelPartialSync(): Promise<void> {
    await this.cancelMithril();
  }

  // Best-effort shutdown reap, called once from safeExit(). Fully try/caught so it can't block shutdown.
  reapPartialSyncOnShutdown(): void {
    try {
      if (this._watchdogHandle) {
        logger.info(
          'MithrilController: cancelling mithril via watchdog handle on shutdown'
        );
        this._watchdogHandle.cancelMithril();
      }
    } catch (error) {
      logger.warn(
        'MithrilController: failed to reap partial sync process on shutdown',
        { error }
      );
    }
  }

  // initialize() is called from IPC channel setup; kept as a no-op for backward compatibility.
  initialize(): void {}
}

const mithrilController = new MithrilController();

export const getMithrilController = (): MithrilController => mithrilController;
