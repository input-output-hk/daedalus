import type { CardanoNodeState } from '../../common/types/cardano-node.types';
import { CardanoNodeStates } from '../../common/types/cardano-node.types';
import type {
  ChainStorageConfig,
  ChainStorageValidation,
  MithrilSnapshotItem,
} from '../../common/types/mithril-bootstrap.types';
import { MithrilBootstrapService } from '../mithril/MithrilBootstrapService';
import { launcherConfig } from '../config';
import { logger } from './logging';
import { ChainStorageManager } from './chainStorageManager';
import { runSerializedMutation } from './chainStorageManagerShared';
import type { ManagedChainLayoutResult } from './chainStorageManagerShared';

export type PartialSyncPreflightContext = {
  layoutResult: ManagedChainLayoutResult;
  mithrilWorkDir: string;
};

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

type PartialSyncStartupHandler = () => Promise<unknown>;
type PartialSyncNodeStopHandler = () => Promise<void>;
type PartialSyncNodeStateGetter = () => CardanoNodeState | null | undefined;

type PartialSyncDependencies = {
  handlers: PartialSyncHandlers;
  nodeStopHandler?: PartialSyncNodeStopHandler;
  startupHandler?: PartialSyncStartupHandler;
  getNodeState?: PartialSyncNodeStateGetter;
};

const PARTIAL_SYNC_DISABLED_CODE = 'PARTIAL_SYNC_DISABLED';
const PARTIAL_SYNC_ALREADY_RUNNING_CODE = 'PARTIAL_SYNC_ALREADY_RUNNING';
const PARTIAL_SYNC_LAYOUT_UNSUPPORTED_CODE = 'PARTIAL_SYNC_LAYOUT_UNSUPPORTED';
const PARTIAL_SYNC_CANCEL_JOIN_TIMEOUT_MS = 15_000;
const PARTIAL_SYNC_CANCEL_FORCE_KILL_TIMEOUT_MS = 1_000;

class ChainStorageCoordinator {
  _chainStorageManager: ChainStorageManager;
  _mithrilBootstrapService: MithrilBootstrapService;
  _mutationQueue: Promise<void> = Promise.resolve();
  _bootstrapInProgress = false;
  _partialSyncInProgress = false;
  _partialSyncRunPromise: Promise<void> | null = null;
  _directoryChangedCallbacks: Array<() => void> = [];

  constructor() {
    this._chainStorageManager = new ChainStorageManager();
    this._mithrilBootstrapService = new MithrilBootstrapService(
      undefined,
      this._chainStorageManager
    );
  }

  getChainStorageManager(): ChainStorageManager {
    return this._chainStorageManager;
  }

  getMithrilBootstrapService(): MithrilBootstrapService {
    return this._mithrilBootstrapService;
  }

  isPartialSyncInProgress(): boolean {
    return this._partialSyncInProgress;
  }

  isPartialSyncEnabled(): boolean {
    return launcherConfig.mithrilPartialSyncEnabled === true;
  }

  async getConfig(): Promise<ChainStorageConfig> {
    await this._awaitPendingMutations();
    return this._chainStorageManager.getConfig();
  }

  async validate(path: string | null): Promise<ChainStorageValidation> {
    return this._chainStorageManager.validate(path);
  }

  async prepareForLocationChange(
    nodeState?: CardanoNodeState | null
  ): Promise<ChainStorageValidation | null> {
    return this._withMutationLock('prepareForLocationChange', async () => {
      this._assertBootstrapMutationAllowed(
        'prepare chain storage location change'
      );
      this._assertNodeStopped(
        nodeState,
        'prepare chain storage location change'
      );

      const validation =
        await this._chainStorageManager.prepareForLocationChange();

      if (!validation) {
        return null;
      }

      await this._syncMithrilWorkDir();
      this._notifyDirectoryChanged();
      return validation;
    });
  }

  onDirectoryChanged(callback: () => void): void {
    this._directoryChangedCallbacks.push(callback);
  }

  async setDirectory(
    path: string | null,
    nodeState?: CardanoNodeState | null
  ): Promise<ChainStorageValidation> {
    return this._withMutationLock('setDirectory', async () => {
      this._assertBootstrapMutationAllowed('change chain storage directory');
      this._assertNodeStopped(nodeState, 'change chain storage directory');

      const validation =
        path == null
          ? await this._chainStorageManager.resetToDefault()
          : await this._chainStorageManager.setDirectory(path);

      if (!validation.isValid) {
        return validation;
      }

      await this._syncMithrilWorkDir();
      this._notifyDirectoryChanged();
      return validation;
    });
  }

  async ensureManagedChainLayout(
    nodeState?: CardanoNodeState | null
  ): Promise<ManagedChainLayoutResult> {
    return this._withMutationLock('ensureManagedChainLayout', async () => {
      return this._ensureManagedChainLayoutAndSyncWorkDir(nodeState);
    });
  }

  async resolveDiskSpaceCheckPath(): Promise<string> {
    return this._chainStorageManager.resolveDiskSpaceCheckPath();
  }

  async isManagedChainEmpty(): Promise<boolean> {
    return this._chainStorageManager.isManagedChainEmpty();
  }

  async syncMithrilWorkDir(): Promise<string> {
    return this._syncMithrilWorkDir();
  }

  async _syncMithrilWorkDir(): Promise<string> {
    const workDir = await this._chainStorageManager.resolveMithrilWorkDir();
    this._mithrilBootstrapService.setWorkDir(workDir);
    return workDir;
  }

  async listSnapshots(): Promise<Array<MithrilSnapshotItem>> {
    await this.syncMithrilWorkDir();
    return this._mithrilBootstrapService.listSnapshots();
  }

  async startBootstrap(
    digest?: string,
    options?: {
      wipeChain?: boolean;
      nodeState?: CardanoNodeState | null;
    }
  ): Promise<void> {
    await this._withMutationLock('startBootstrap', async () => {
      this._assertBootstrapMutationAllowed('start Mithril bootstrap');

      const layoutResult = await this._ensureManagedChainLayoutAndSyncWorkDir(
        options?.nodeState
      );
      if (!options?.wipeChain) {
        const isManagedChainEmpty =
          await this._chainStorageManager.isManagedChainEmpty();

        if (!isManagedChainEmpty) {
          logger.warn(
            '[MITHRIL] Rejecting bootstrap start on non-empty managed chain without wipeChain',
            {
              managedChainPath: layoutResult.managedChainPath,
            }
          );
          throw new Error(
            'Cannot start Mithril bootstrap on a non-empty managed chain without wipeChain.'
          );
        }
      }
      this._bootstrapInProgress = true;
    });

    try {
      await this._mithrilBootstrapService.startBootstrap(digest, {
        wipeChain: options?.wipeChain,
      });
    } finally {
      this._bootstrapInProgress = false;
    }
  }

  async cancelBootstrap(): Promise<void> {
    await this._mithrilBootstrapService.cancel();
  }

  async startPartialSync(
    dependencies: PartialSyncDependencies,
    options?: {
      nodeState?: CardanoNodeState | null;
    }
  ): Promise<void> {
    const preflightContext = await this._withMutationLock(
      'startPartialSync',
      async () => {
        this._assertPartialSyncFeatureEnabled();
        this._assertPartialSyncStartAllowed();
        dependencies.handlers.assertStartAllowed();

        // The click-time snapshot can go stale while this call waits on the
        // mutation lock, so prefer the live node state. This only narrows the
        // race window: process exit does not guarantee file handles are released.
        const nodeState = await this._ensureNodeStoppedForPartialSync(
          dependencies.nodeStopHandler,
          dependencies.getNodeState
            ? dependencies.getNodeState()
            : options?.nodeState
        );

        const layoutResult =
          await this._chainStorageManager.ensureManagedChainLayout({
            nodeState,
          });

        if (layoutResult.isRecoveryFallback) {
          logger.warn(
            '[MITHRIL] Rejecting partial sync start on recovery fallback layout',
            {
              managedChainPath: layoutResult.managedChainPath,
            }
          );
          throw new Error(PARTIAL_SYNC_LAYOUT_UNSUPPORTED_CODE);
        }

        const mithrilWorkDir =
          await this._chainStorageManager.resolveMithrilWorkDir();

        this._partialSyncInProgress = true;

        return {
          layoutResult,
          mithrilWorkDir,
        };
      }
    );

    const runPromise = (async () => {
      try {
        await dependencies.handlers.start(preflightContext);
      } finally {
        this._partialSyncInProgress = false;
        this._partialSyncRunPromise = null;
      }
    })();

    this._partialSyncRunPromise = runPromise;
    await runPromise;
  }

  async cancelPartialSync(
    dependencies: PartialSyncDependencies
  ): Promise<void> {
    await dependencies.handlers.cancel();

    const run = this._partialSyncRunPromise;
    const settled = run
      ? await this._awaitRunSettledBounded(run, dependencies.handlers.forceKill)
      : true;

    if (settled) {
      logger.info(
        '[MITHRIL] Partial sync run settled after cancel; finalizing cancel',
        {
          hadRun: !!run,
          settled,
        }
      );
      await dependencies.handlers.finalizeCancel();
      return;
    }

    logger.warn(
      '[MITHRIL] Partial sync run unsettled after cancel; abandoning cancel',
      {
        hadRun: !!run,
        settled,
      }
    );
    await dependencies.handlers.abandonCancel();
  }

  async restartNormalFromPartialSync(
    dependencies: PartialSyncDependencies,
    options?: {
      nodeState?: CardanoNodeState | null;
    }
  ): Promise<void> {
    if (!dependencies.startupHandler) {
      throw new Error(
        'Mithril partial sync startup handler is not configured.'
      );
    }

    await this._withMutationLock('restartNormalFromPartialSync', async () => {
      this._assertPartialSyncFeatureEnabled();
      this._assertBootstrapMutationAllowed(
        'restart normally after Mithril partial sync'
      );
      await this._ensureNodeStoppedForPartialSyncAction(
        dependencies.nodeStopHandler,
        options?.nodeState,
        'restart normally after Mithril partial sync'
      );

      await dependencies.handlers.restartNormal();
    });

    await dependencies.startupHandler();
  }

  async wipeAndFullSyncFromPartialSync(
    dependencies: PartialSyncDependencies,
    options?: {
      nodeState?: CardanoNodeState | null;
    }
  ): Promise<void> {
    if (!dependencies.startupHandler) {
      throw new Error(
        'Mithril partial sync startup handler is not configured.'
      );
    }

    await this._withMutationLock('wipeAndFullSyncFromPartialSync', async () => {
      this._assertBootstrapMutationAllowed(
        'wipe chain storage and full sync after Mithril partial sync'
      );
      const nodeState = await this._ensureNodeStoppedForPartialSyncAction(
        dependencies.nodeStopHandler,
        options?.nodeState,
        'wipe chain storage and full sync after Mithril partial sync'
      );

      await dependencies.handlers.wipeAndFullSync();
      await this._ensureManagedChainLayoutAndSyncWorkDir(nodeState);
      await this._mithrilBootstrapService.wipeChainAndSnapshots(
        'Mithril partial sync requested wipe-and-full-sync recovery.'
      );
      await dependencies.handlers.finalizeWipeAndFullSync();
    });

    await dependencies.startupHandler();
  }

  async wipeChainAndSnapshots(
    reason: string,
    nodeState?: CardanoNodeState | null
  ): Promise<void> {
    await this._withMutationLock('wipeChainAndSnapshots', async () => {
      this._assertBootstrapMutationAllowed(
        'wipe chain storage and snapshot data'
      );
      this._assertNodeStopped(
        nodeState,
        'wipe chain storage and snapshot data'
      );

      await this._ensureManagedChainLayoutAndSyncWorkDir(nodeState);
      await this._mithrilBootstrapService.wipeChainAndSnapshots(reason);
    });
  }

  _assertBootstrapMutationAllowed(action: string): void {
    if (this._bootstrapInProgress) {
      throw new Error(
        `Cannot ${action} while Mithril bootstrap is in progress.`
      );
    }

    if (this._partialSyncInProgress) {
      throw new Error(
        `Cannot ${action} while Mithril partial sync is in progress.`
      );
    }
  }

  _assertPartialSyncStartAllowed(): void {
    if (this._bootstrapInProgress) {
      logger.warn(
        '[MITHRIL] Rejecting partial sync start: Mithril bootstrap is in progress',
        null
      );
      throw new Error(PARTIAL_SYNC_ALREADY_RUNNING_CODE);
    }

    if (this._partialSyncInProgress) {
      logger.warn(
        '[MITHRIL] Rejecting partial sync start: a partial sync run is already in progress',
        null
      );
      throw new Error(PARTIAL_SYNC_ALREADY_RUNNING_CODE);
    }
  }

  _assertPartialSyncFeatureEnabled(): void {
    if (!this.isPartialSyncEnabled()) {
      logger.warn(
        '[MITHRIL] Rejecting partial sync action: partial sync is disabled by launcher configuration',
        null
      );
      throw new Error(PARTIAL_SYNC_DISABLED_CODE);
    }
  }

  _assertNodeStopped(
    nodeState: CardanoNodeState | null | undefined,
    action: string
  ): void {
    if (nodeState != null && nodeState !== CardanoNodeStates.STOPPED) {
      throw new Error(
        `Daedalus can only ${action} while cardano-node is stopped.`
      );
    }
  }

  async _ensureNodeStoppedForPartialSync(
    nodeStopHandler: PartialSyncNodeStopHandler | undefined,
    nodeState: CardanoNodeState | null | undefined
  ): Promise<CardanoNodeState | null | undefined> {
    return this._ensureNodeStoppedForPartialSyncAction(
      nodeStopHandler,
      nodeState,
      'start Mithril partial sync'
    );
  }

  async _ensureNodeStoppedForPartialSyncAction(
    nodeStopHandler: PartialSyncNodeStopHandler | undefined,
    nodeState: CardanoNodeState | null | undefined,
    action: string
  ): Promise<CardanoNodeState | null | undefined> {
    if (nodeState == null || nodeState === CardanoNodeStates.STOPPED) {
      return nodeState;
    }

    if (!nodeStopHandler) {
      this._assertNodeStopped(nodeState, action);
      return nodeState;
    }

    logger.info('[MITHRIL] Stopping cardano-node before partial sync action', {
      action,
      nodeState,
    });
    await nodeStopHandler();
    return CardanoNodeStates.STOPPED;
  }

  async _ensureManagedChainLayoutAndSyncWorkDir(
    nodeState?: CardanoNodeState | null
  ): Promise<ManagedChainLayoutResult> {
    const layoutResult =
      await this._chainStorageManager.ensureManagedChainLayout({ nodeState });

    await this._syncMithrilWorkDir();
    return layoutResult;
  }

  async _awaitPendingMutations(): Promise<void> {
    await this._mutationQueue.catch(() => undefined);
  }

  async _awaitRunSettledBounded(
    run: Promise<void>,
    forceKill: () => void | Promise<void>
  ): Promise<boolean> {
    const settled = await this._awaitSettledWithin(
      run,
      PARTIAL_SYNC_CANCEL_JOIN_TIMEOUT_MS
    );

    if (settled) {
      return true;
    }

    logger.warn(
      '[MITHRIL] Partial sync run did not settle within cancel join timeout; escalating to forceKill',
      {
        joinTimeoutMs: PARTIAL_SYNC_CANCEL_JOIN_TIMEOUT_MS,
        forceKillTimeoutMs: PARTIAL_SYNC_CANCEL_FORCE_KILL_TIMEOUT_MS,
      }
    );

    await forceKill();

    return this._awaitSettledWithin(
      run,
      PARTIAL_SYNC_CANCEL_FORCE_KILL_TIMEOUT_MS
    );
  }

  async _awaitSettledWithin(
    run: Promise<void>,
    timeoutMs: number
  ): Promise<boolean> {
    let timeoutId: ReturnType<typeof setTimeout> | undefined;

    try {
      return await Promise.race([
        run.then(
          () => true,
          () => true
        ),
        new Promise<boolean>((resolve) => {
          timeoutId = setTimeout(() => resolve(false), timeoutMs);
        }),
      ]);
    } finally {
      if (timeoutId) {
        clearTimeout(timeoutId);
      }
    }
  }

  _notifyDirectoryChanged(): void {
    for (const callback of this._directoryChangedCallbacks) {
      try {
        callback();
      } catch (error) {
        logger.warn(
          'ChainStorageCoordinator: directory-change callback failed',
          {
            error,
          }
        );
      }
    }
  }

  async _withMutationLock<T>(
    label: string,
    operation: () => Promise<T>
  ): Promise<T> {
    return runSerializedMutation(
      this,
      'ChainStorageCoordinator',
      label,
      operation
    );
  }
}

export const chainStorageCoordinator = new ChainStorageCoordinator();

export const getChainStorageManager = (): ChainStorageManager =>
  chainStorageCoordinator.getChainStorageManager();

export const getMithrilBootstrapService = (): MithrilBootstrapService =>
  chainStorageCoordinator.getMithrilBootstrapService();
