import type { CardanoNodeState } from '../../common/types/cardano-node.types';
import { CardanoNodeStates } from '../../common/types/cardano-node.types';
import type {
  ChainStorageConfig,
  ChainStorageValidation,
} from '../../common/types/mithril-bootstrap.types';
import { launcherConfig } from '../config';
import { logger } from './logging';
import { ChainStorageManager } from './chainStorageManager';
import { runSerializedMutation } from './chainStorageManagerShared';
import type { ManagedChainLayoutResult } from './chainStorageManagerShared';

export type PartialSyncPreflightContext = {
  layoutResult: ManagedChainLayoutResult;
  mithrilWorkDir: string;
};

class ChainStorageCoordinator {
  _chainStorageManager: ChainStorageManager;
  _mutationQueue: Promise<void> = Promise.resolve();
  _directoryChangedCallbacks: Array<() => void> = [];

  constructor() {
    this._chainStorageManager = new ChainStorageManager();
  }

  getChainStorageManager(): ChainStorageManager {
    return this._chainStorageManager;
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
    return this._chainStorageManager.resolveMithrilWorkDir();
  }

  async wipeChainAndSnapshots(
    reason: string,
    nodeState?: CardanoNodeState | null
  ): Promise<void> {
    await this._withMutationLock('wipeChainAndSnapshots', async () => {
      this._assertNodeStopped(
        nodeState,
        'wipe chain storage and snapshot data'
      );

      logger.info('ChainStorageCoordinator: wiping chain storage', { reason });
      await this._ensureManagedChainLayoutAndSyncWorkDir(nodeState);
      await this._chainStorageManager.emptyManagedContents();
    });
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
