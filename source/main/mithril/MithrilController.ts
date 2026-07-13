import type { CardanoNodeState } from '../../common/types/cardano-node.types';
import type {
  MithrilBootstrapDecision,
  MithrilBootstrapStatusUpdate,
  MithrilSnapshotItem,
} from '../../common/types/mithril-bootstrap.types';
import { isMithrilBootstrapBlockingNodeStart } from '../../common/types/mithril-bootstrap.types';
import type {
  MithrilPartialSyncAvailability,
  MithrilPartialSyncStatusSnapshot,
} from '../../common/types/mithril-partial-sync.types';
import {
  isMithrilPartialSyncBlockingNodeStart,
  isMithrilPartialSyncWorkingStatus,
  makeIdlePartialSyncStatus,
} from '../../common/types/mithril-partial-sync.types';
import type { PartialSyncPreflightContext } from '../utils/chainStorageCoordinator';
import {
  chainStorageCoordinator,
  getMithrilBootstrapService,
} from '../utils/chainStorageCoordinator';
import { logger } from '../utils/logging';
import { MithrilPartialSyncService } from './MithrilPartialSyncService';
import {
  MithrilStartupGate,
  MithrilStartupGateDependencies,
  MithrilStartupGateResult,
} from './MithrilStartupGate';
import { MithrilDecisionCancelledError } from './mithrilDecision';

type StatusSender<T> = (status: T) => Promise<void>;

type PartialSyncHandlers = {
  start(context: PartialSyncPreflightContext): Promise<void>;
  cancel(): Promise<void>;
  finalizeCancel(): Promise<void>;
  forceKill(): void | Promise<void>;
  abandonCancel(): Promise<void>;
  assertStartAllowed(): void;
  restartNormal(): Promise<void>;
  wipeAndFullSync(): Promise<void>;
  finalizeWipeAndFullSync(): Promise<void>;
};

const DEFAULT_BOOTSTRAP_STATUS: MithrilBootstrapStatusUpdate = {
  status: 'idle',
  snapshot: null,
  error: null,
};

export class MithrilController {
  _isInitialized = false;
  _bootstrapStatus: MithrilBootstrapStatusUpdate = {
    ...DEFAULT_BOOTSTRAP_STATUS,
  };
  _partialSyncStatus: MithrilPartialSyncStatusSnapshot =
    makeIdlePartialSyncStatus();
  _decisionListeners: Array<(decision: MithrilBootstrapDecision) => void> = [];
  _decisionWaiters: Array<{
    resolve: (decision: MithrilBootstrapDecision) => void;
    reject: (error: Error) => void;
  }> = [];
  _bootstrapStatusSender: StatusSender<MithrilBootstrapStatusUpdate> | null =
    null;
  _partialSyncStatusSender: StatusSender<MithrilPartialSyncStatusSnapshot> | null =
    null;
  _pendingDecision: MithrilBootstrapDecision | null = null;
  _nodeStateProvider: () => CardanoNodeState | null | undefined = () =>
    undefined;
  _partialSyncService = new MithrilPartialSyncService();
  _stopNodeForPartialSync: (() => Promise<void>) | undefined;
  _restartStartupFlowAfterPartialSync: (() => Promise<void>) | undefined;
  _startupGate = new MithrilStartupGate(this);

  initialize(): void {
    if (this._isInitialized) return;
    this._isInitialized = true;

    const bootstrapService = getMithrilBootstrapService();
    this._bootstrapStatus = bootstrapService.status;
    this._partialSyncStatus = this._partialSyncService.status;

    chainStorageCoordinator.syncMithrilWorkDir().catch((error) => {
      logger.warn('MithrilController: failed to sync Mithril work directory', {
        error,
      });
    });

    bootstrapService.onStatus((status) => {
      this.broadcastBootstrapStatus(status).catch((error) => {
        logger.warn('MithrilController: failed to broadcast bootstrap status', {
          error,
        });
      });
    });

    this._partialSyncService.onStatus((status) => {
      this.broadcastPartialSyncStatus(status).catch((error) => {
        logger.warn(
          'MithrilController: failed to broadcast partial sync status',
          { error }
        );
      });
    });

    this.onBootstrapDecision((decision) => {
      this._startupGate.onBootstrapDecision(decision);
    });
  }

  setBootstrapStatusSender(
    sender: StatusSender<MithrilBootstrapStatusUpdate>
  ): void {
    this._bootstrapStatusSender = sender;
  }

  setPartialSyncStatusSender(
    sender: StatusSender<MithrilPartialSyncStatusSnapshot>
  ): void {
    this._partialSyncStatusSender = sender;
  }

  setNodeStateProvider(
    provider: () => CardanoNodeState | null | undefined
  ): void {
    this._nodeStateProvider = provider;
  }

  configurePartialSyncRuntime(dependencies: {
    stopNode?: () => Promise<void>;
    restartStartupFlow?: () => Promise<void>;
  }): void {
    this._stopNodeForPartialSync = dependencies.stopNode;
    this._restartStartupFlowAfterPartialSync = dependencies.restartStartupFlow;
  }

  configureStartupGate(dependencies: MithrilStartupGateDependencies): void {
    this._startupGate.configure(dependencies);
  }

  getBootstrapStatus(): MithrilBootstrapStatusUpdate {
    return this._bootstrapStatus;
  }

  getPartialSyncStatus(): MithrilPartialSyncStatusSnapshot {
    return this._partialSyncStatus;
  }

  async getPartialSyncAvailability(): Promise<MithrilPartialSyncAvailability> {
    const isEnabled = chainStorageCoordinator.isPartialSyncEnabled();
    if (!isEnabled) {
      return { isEnabled: false, isSignificantlyBehind: false };
    }
    // Skip the behind-ness probe while a sync is working or terminal-'cancelled': it would spawn a
    //  concurrent mithril-client metadata child mid-run. Degrading to not-behind is safe — the prompt is
    //  session-suppressed once a sync starts, and the probe degrades to not-behind on any failure anyway.
    const { status } = this._partialSyncStatus;
    if (isMithrilPartialSyncWorkingStatus(status) || status === 'cancelled') {
      return { isEnabled, isSignificantlyBehind: false };
    }
    const behindness =
      await this._partialSyncService.getPartialSyncBehindness();
    return { isEnabled, ...behindness };
  }

  getPendingBootstrapDecision(): MithrilBootstrapDecision | null {
    return this._pendingDecision;
  }

  getNodeState(): CardanoNodeState | null | undefined {
    return this._nodeStateProvider();
  }

  isBootstrapNodeStartBlocked(): boolean {
    return isMithrilBootstrapBlockingNodeStart(this._bootstrapStatus.status);
  }

  isPartialSyncNodeStartBlocked(): boolean {
    return isMithrilPartialSyncBlockingNodeStart(
      this._partialSyncStatus.status
    );
  }

  isPartialSyncActive(): boolean {
    return (
      chainStorageCoordinator.isPartialSyncInProgress() ||
      this._partialSyncStatus.status !== 'idle'
    );
  }

  // Best-effort shutdown reap, called once from safeExit(): cancel()/forceKill() don't run on a plain
  //  quit, so this is all that stops a quit-mid-download orphaning a detached mithril-client. The
  //  isPartialSyncActive() guard is broader than needed but harmless. Fully try/caught so it can't block shutdown.
  reapPartialSyncOnShutdown(): void {
    try {
      if (!this.isPartialSyncActive()) return;
      logger.info(
        'MithrilController: reaping active partial sync process on shutdown',
        {
          status: this._partialSyncStatus.status,
        }
      );
      this._partialSyncService.forceKillForShutdown();
    } catch (error) {
      logger.warn(
        'MithrilController: failed to reap partial sync process on shutdown',
        {
          error,
        }
      );
    }
  }

  onBootstrapDecision(
    handler: (decision: MithrilBootstrapDecision) => void
  ): () => void {
    this._decisionListeners.push(handler);
    return () => {
      this._decisionListeners = this._decisionListeners.filter(
        (listener) => listener !== handler
      );
    };
  }

  async broadcastBootstrapStatus(
    status: MithrilBootstrapStatusUpdate
  ): Promise<void> {
    this._bootstrapStatus = status;
    this._startupGate.onBootstrapStatus(status);

    if (!this._bootstrapStatusSender) return;

    Promise.resolve()
      .then(() => this._bootstrapStatusSender?.(status))
      .catch((error) => {
        logger.warn('MithrilController: failed to send bootstrap status', {
          error,
        });
      });
  }

  async broadcastPartialSyncStatus(
    status: MithrilPartialSyncStatusSnapshot
  ): Promise<void> {
    const previousStatus = this._partialSyncStatus.status;
    this._partialSyncStatus = status;
    this._startupGate.onPartialSyncStatus(status);

    if (status.status === 'finalizing' && previousStatus !== 'finalizing') {
      this._restartStartupFlowAfterPartialSync?.().catch((error) => {
        logger.warn(
          'MithrilController: failed to restart startup flow after partial sync install',
          { error }
        );
      });
    }

    if (!this._partialSyncStatusSender) return;

    Promise.resolve()
      .then(() => this._partialSyncStatusSender?.(status))
      .catch((error) => {
        logger.warn('MithrilController: failed to send partial sync status', {
          error,
        });
      });
  }

  setBootstrapStatus(
    update: Partial<MithrilBootstrapStatusUpdate>
  ): MithrilBootstrapStatusUpdate {
    this._bootstrapStatus = {
      ...this._bootstrapStatus,
      ...update,
    };
    return this._bootstrapStatus;
  }

  setPartialSyncStatus(
    status: MithrilPartialSyncStatusSnapshot
  ): MithrilPartialSyncStatusSnapshot {
    this._partialSyncStatus = status;
    return this._partialSyncStatus;
  }

  waitForBootstrapDecision(): Promise<MithrilBootstrapDecision> {
    if (this._pendingDecision) return Promise.resolve(this._pendingDecision);
    return new Promise((resolve, reject) => {
      this._decisionWaiters.push({ resolve, reject });
    });
  }

  async submitBootstrapDecision(
    decision: MithrilBootstrapDecision
  ): Promise<void> {
    logger.info('[MITHRIL] Received bootstrap decision', {
      decision,
      previousDecision: this._pendingDecision,
      status: this._bootstrapStatus.status,
    });

    this._pendingDecision = decision;
    this._decisionWaiters.forEach(({ resolve }) => resolve(decision));
    this._decisionWaiters = [];
    this._decisionListeners.forEach((listener) => listener(decision));

    if (decision !== 'decline') return;

    if (this._bootstrapStatus.status === 'failed') {
      logger.info(
        '[MITHRIL] Keeping failed status active while decline recovery starts'
      );
      return;
    }

    const update = this.setBootstrapStatus({
      status: 'idle',
      snapshot: null,
      error: null,
      elapsedSeconds: undefined,
    });
    await this.broadcastBootstrapStatus(update);
  }

  resetBootstrapDecisionState(
    options: { suppressStatusBroadcast?: boolean } = {}
  ): void {
    this._pendingDecision = null;
    const cancellationError = new MithrilDecisionCancelledError();
    this._decisionWaiters.forEach(({ reject }) => reject(cancellationError));
    this._decisionWaiters = [];

    const update = this.setBootstrapStatus({
      status: 'idle',
      snapshot: null,
      error: null,
      elapsedSeconds: undefined,
      filesDownloaded: undefined,
      filesTotal: undefined,
      ancillaryBytesDownloaded: undefined,
      ancillaryBytesTotal: undefined,
      progressItems: undefined,
    });

    if (options.suppressStatusBroadcast) return;

    this.broadcastBootstrapStatus(update).catch((error) => {
      logger.warn(
        'MithrilController: failed to broadcast idle status during decision reset',
        { error }
      );
    });
  }

  async listSnapshots(): Promise<Array<MithrilSnapshotItem>> {
    return chainStorageCoordinator.listSnapshots();
  }

  async startBootstrap(options: {
    digest?: string;
    wipeChain?: boolean;
  }): Promise<void> {
    await chainStorageCoordinator.startBootstrap(options.digest, {
      wipeChain: options.wipeChain,
      nodeState: this.getNodeState(),
    });
  }

  async ensureMithrilStartupGate(currentGeneration: number) {
    return this._startupGate.ensureMithrilStartupGate(currentGeneration);
  }

  resetStartupGateOnDirectoryChange(): void {
    // The behind-ness caches describe the previous chain directory; drop them
    // together with the startup-gate reset so the next probe re-reads.
    this._partialSyncService.onChainDirectoryChanged();
    this._startupGate.resetOnDirectoryChange();
  }

  syncPendingDecision(): void {
    this._startupGate.syncPendingDecision();
  }

  async handleStoppedNodeStartup<TResponse>(options: {
    currentGeneration: number;
    getStaleResponse: () => TResponse;
    response: TResponse;
  }): Promise<MithrilStartupGateResult<TResponse>> {
    return this._startupGate.handleStoppedNodeStartup(options);
  }

  async cancelBootstrap(): Promise<void> {
    await chainStorageCoordinator.cancelBootstrap();
  }

  async startPartialSync(): Promise<void> {
    await chainStorageCoordinator.startPartialSync(
      this._getPartialSyncDependencies(),
      {
        nodeState: this.getNodeState(),
      }
    );
  }

  async cancelPartialSync(): Promise<void> {
    await chainStorageCoordinator.cancelPartialSync(
      this._getPartialSyncDependencies()
    );
  }

  async restartNormalFromPartialSync(): Promise<void> {
    await chainStorageCoordinator.restartNormalFromPartialSync(
      this._getPartialSyncDependencies(),
      {
        nodeState: this.getNodeState(),
      }
    );
  }

  async wipeAndFullSyncFromPartialSync(): Promise<void> {
    await chainStorageCoordinator.wipeAndFullSyncFromPartialSync(
      this._getPartialSyncDependencies(),
      {
        nodeState: this.getNodeState(),
      }
    );
  }

  async finalizePartialSync(): Promise<void> {
    // Dismiss-driven success finalize. No node orchestration → direct to the service,
    // bypassing the coordinator. Idempotent / terminal-state only.
    await this._partialSyncService.finalizeCompletedPartialSync();
  }

  _getPartialSyncDependencies(): {
    handlers: PartialSyncHandlers;
    nodeStopHandler?: () => Promise<void>;
    startupHandler?: () => Promise<void>;
    getNodeState?: () => CardanoNodeState | null | undefined;
  } {
    return {
      handlers: {
        assertStartAllowed: () => this._partialSyncService.assertStartAllowed(),
        start: async (context) => this._partialSyncService.start(context),
        cancel: async () => this._partialSyncService.cancel(),
        finalizeCancel: async () => this._partialSyncService.finalizeCancel(),
        forceKill: () => this._partialSyncService.forceKill(),
        abandonCancel: async () => this._partialSyncService.abandonCancel(),
        // Recovery actions may target a boundary reached in a previous session
        // (startup-owned emission); seed the service with the broadcast snapshot
        // before delegating so its allowed-action assertion sees that boundary.
        restartNormal: async () => {
          this._partialSyncService.adoptRecoverySnapshot(
            this.getPartialSyncStatus()
          );
          await this._partialSyncService.restartNormal();
        },
        wipeAndFullSync: async () => {
          this._partialSyncService.adoptRecoverySnapshot(
            this.getPartialSyncStatus()
          );
          await this._partialSyncService.wipeAndFullSync();
        },
        finalizeWipeAndFullSync: async () =>
          this._partialSyncService.finalizeWipeAndFullSync(),
      },
      nodeStopHandler: this._stopNodeForPartialSync,
      startupHandler: this._restartStartupFlowAfterPartialSync,
      getNodeState: () => this.getNodeState(),
    };
  }
}

const mithrilController = new MithrilController();

export const getMithrilController = (): MithrilController => mithrilController;
