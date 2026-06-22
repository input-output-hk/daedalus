import type { CardanoNodeState } from '../../common/types/cardano-node.types';
import { CardanoNodeStates } from '../../common/types/cardano-node.types';
import type {
  ChainStorageConfig,
  ChainStorageValidation,
  MithrilSnapshotItem,
} from '../../common/types/mithril-bootstrap.types';
import type { MithrilPartialSyncAvailability } from '../../common/types/mithril-partial-sync.types';
import { MithrilBootstrapService } from '../mithril/MithrilBootstrapService';
import { launcherConfig } from '../config';
import { logger } from './logging';
import { ChainStorageManager } from './chainStorageManager';
import type { ManagedChainLayoutResult } from './chainStorageManagerShared';

export type PartialSyncPreflightContext = {
  layoutResult: ManagedChainLayoutResult;
  mithrilWorkDir: string;
};

type PartialSyncHandlers = {
  start(context: PartialSyncPreflightContext): Promise<void>;
  cancel(): Promise<void>;
  assertStartAllowed(): void;
  restartNormal(): Promise<void>;
  wipeAndFullSync(): Promise<void>;
  finalizeWipeAndFullSync(): Promise<void>;
};

type PartialSyncStartupHandler = () => Promise<unknown>;
type PartialSyncNodeStopHandler = () => Promise<void>;

type PartialSyncDependencies = {
  handlers: PartialSyncHandlers;
  nodeStopHandler?: PartialSyncNodeStopHandler;
  startupHandler?: PartialSyncStartupHandler;
};

const PARTIAL_SYNC_DISABLED_ERROR =
  'Mithril partial sync is disabled by launcher configuration.';

class ChainStorageCoordinator {
  _chainStorageManager: ChainStorageManager;
  _mithrilBootstrapService: MithrilBootstrapService;
  _mutationQueue: Promise<void> = Promise.resolve();
  _bootstrapInProgress = false;
  _partialSyncInProgress = false;
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

  getPartialSyncAvailability(): MithrilPartialSyncAvailability {
    return {
      isEnabled: launcherConfig.mithrilPartialSyncEnabled === true,
      isSignificantlyBehind: false, // task-ux-102 replaces with certified-immutable-gap signal
      behindByImmutables: undefined, // task-ux-102 populates the raw gap
    };
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

      if (this._bootstrapInProgress) {
        throw new Error('Mithril bootstrap is already in progress.');
      }

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

        const nodeState = await this._ensureNodeStoppedForPartialSync(
          dependencies.nodeStopHandler,
          options?.nodeState
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
          throw new Error(
            'Cannot start Mithril partial sync while chain storage is using recovery fallback state.'
          );
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

    try {
      await dependencies.handlers.start(preflightContext);
    } finally {
      this._partialSyncInProgress = false;
    }
  }

  async cancelPartialSync(
    dependencies: PartialSyncDependencies
  ): Promise<void> {
    await dependencies.handlers.cancel();
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
      throw new Error(
        'Cannot start Mithril partial sync while Mithril bootstrap is in progress.'
      );
    }

    if (this._partialSyncInProgress) {
      throw new Error('Mithril partial sync is already in progress.');
    }
  }

  _assertPartialSyncFeatureEnabled(): void {
    if (launcherConfig.mithrilPartialSyncEnabled !== true) {
      throw new Error(PARTIAL_SYNC_DISABLED_ERROR);
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
    const previousMutation = this._mutationQueue;
    let releaseLock: (() => void) | undefined;

    this._mutationQueue = new Promise<void>((resolve) => {
      releaseLock = resolve;
    });

    await previousMutation.catch(() => undefined);

    try {
      return await operation();
    } catch (error) {
      logger.warn('ChainStorageCoordinator: serialized mutation failed', {
        error,
        label,
      });
      throw error;
    } finally {
      releaseLock?.();
    }
  }
}

export const chainStorageCoordinator = new ChainStorageCoordinator();

export const getChainStorageManager = (): ChainStorageManager =>
  chainStorageCoordinator.getChainStorageManager();

export const getMithrilBootstrapService = (): MithrilBootstrapService =>
  chainStorageCoordinator.getMithrilBootstrapService();

export const getMithrilPartialSyncDisabledError = (): string =>
  PARTIAL_SYNC_DISABLED_ERROR;
