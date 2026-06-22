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
import { isMithrilPartialSyncBlockingNodeStart } from '../../common/types/mithril-partial-sync.types';
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
  MithrilStartupGateState,
} from './MithrilStartupGate';
import {
  isMithrilDecisionCancelledError,
  MithrilDecisionCancelledError,
} from './mithrilDecision';

type StatusSender<T> = (status: T) => Promise<void>;

type PartialSyncHandlers = {
  start(context: PartialSyncPreflightContext): Promise<void>;
  cancel(): Promise<void>;
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

const DEFAULT_PARTIAL_SYNC_STATUS: MithrilPartialSyncStatusSnapshot = {
  status: 'idle',
  allowedRecoveryActions: [],
  transferProgress: {},
  progressItems: [],
  error: null,
};

export { isMithrilDecisionCancelledError, MithrilDecisionCancelledError };

export class MithrilController {
  _isInitialized = false;
  _bootstrapStatus: MithrilBootstrapStatusUpdate = {
    ...DEFAULT_BOOTSTRAP_STATUS,
  };
  _partialSyncStatus: MithrilPartialSyncStatusSnapshot = {
    ...DEFAULT_PARTIAL_SYNC_STATUS,
  };
  _bootstrapStatusListeners: Array<
    (status: MithrilBootstrapStatusUpdate) => void
  > = [];
  _partialSyncStatusListeners: Array<
    (status: MithrilPartialSyncStatusSnapshot) => void
  > = [];
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

  getStartupGateState(): MithrilStartupGateState {
    return this._startupGate.state;
  }

  getBootstrapStatus(): MithrilBootstrapStatusUpdate {
    return this._bootstrapStatus;
  }

  getPartialSyncStatus(): MithrilPartialSyncStatusSnapshot {
    return this._partialSyncStatus;
  }

  async getPartialSyncAvailability(): Promise<MithrilPartialSyncAvailability> {
    const { isEnabled } = chainStorageCoordinator.getPartialSyncAvailability();
    if (!isEnabled) {
      return { isEnabled: false, isSignificantlyBehind: false };
    }
    const behindness = await this._partialSyncService.getPartialSyncBehindness();
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

  onBootstrapStatus(
    handler: (status: MithrilBootstrapStatusUpdate) => void
  ): () => void {
    this._bootstrapStatusListeners.push(handler);
    return () => {
      this._bootstrapStatusListeners = this._bootstrapStatusListeners.filter(
        (listener) => listener !== handler
      );
    };
  }

  onPartialSyncStatus(
    handler: (status: MithrilPartialSyncStatusSnapshot) => void
  ): () => void {
    this._partialSyncStatusListeners.push(handler);
    return () => {
      this._partialSyncStatusListeners =
        this._partialSyncStatusListeners.filter(
          (listener) => listener !== handler
        );
    };
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
    this._bootstrapStatusListeners.forEach((listener) => listener(status));

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
    this._partialSyncStatus = status;
    this._startupGate.onPartialSyncStatus(status);
    this._partialSyncStatusListeners.forEach((listener) => listener(status));

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
    // Dismiss-driven success finalize (PRD D9). No node orchestration → direct to the service,
    // bypassing the coordinator (Decision (b)). Idempotent / terminal-state only.
    await this._partialSyncService.finalizeCompletedPartialSync();
  }

  _getPartialSyncDependencies(): {
    handlers: PartialSyncHandlers;
    nodeStopHandler?: () => Promise<void>;
    startupHandler?: () => Promise<void>;
  } {
    return {
      handlers: {
        assertStartAllowed: () => this._partialSyncService.assertStartAllowed(),
        start: async (context) => this._partialSyncService.start(context),
        cancel: async () => this._partialSyncService.cancel(),
        restartNormal: async () => this._partialSyncService.restartNormal(),
        wipeAndFullSync: async () => this._partialSyncService.wipeAndFullSync(),
        finalizeWipeAndFullSync: async () =>
          this._partialSyncService.finalizeWipeAndFullSync(),
      },
      nodeStopHandler: this._stopNodeForPartialSync,
      startupHandler: this._restartStartupFlowAfterPartialSync,
    };
  }
}

const mithrilController = new MithrilController();

export const getMithrilController = (): MithrilController => mithrilController;
