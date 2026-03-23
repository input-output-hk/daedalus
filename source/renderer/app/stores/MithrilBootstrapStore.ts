import { action, computed, observable, runInAction } from 'mobx';
import Store from './lib/Store';
import type {
  MithrilBootstrapStatus,
  MithrilBootstrapStatusUpdate,
  MithrilSnapshotItem,
  MithrilBootstrapError,
  MithrilBootstrapDecision,
  ChainStorageValidation,
  MithrilProgressItem,
} from '../../../common/types/mithril-bootstrap.types';
import {
  mithrilBootstrapDecisionChannel,
  mithrilBootstrapStartChannel,
  mithrilBootstrapStatusChannel,
  mithrilBootstrapCancelChannel,
  mithrilBootstrapSnapshotsChannel,
} from '../ipc/mithrilBootstrapChannel';
import {
  setChainStorageDirectoryChannel,
  getChainStorageDirectoryChannel,
  validateChainStorageDirectoryChannel,
} from '../ipc/chainStorageChannel';
import { logger } from '../utils/logging';

const DEFAULT_STATUS: MithrilBootstrapStatusUpdate = {
  status: 'idle',
  snapshot: null,
  error: null,
};

const isDecisionCycleStatus = (status: MithrilBootstrapStatus) =>
  status === 'decision' || status === 'idle' || status === 'cancelled';

const isWorkingStatus = (status: MithrilBootstrapStatus) =>
  status === 'preparing' ||
  status === 'downloading' ||
  status === 'verifying' ||
  status === 'unpacking' ||
  status === 'converting' ||
  status === 'finalizing' ||
  status === 'starting-node';

export default class MithrilBootstrapStore extends Store {
  @observable status: MithrilBootstrapStatus = DEFAULT_STATUS.status;
  @observable snapshot: MithrilSnapshotItem | null =
    DEFAULT_STATUS.snapshot ?? null;
  @observable filesDownloaded: number | undefined =
    DEFAULT_STATUS.filesDownloaded;
  @observable filesTotal: number | undefined = DEFAULT_STATUS.filesTotal;
  @observable elapsedSeconds: number | undefined =
    DEFAULT_STATUS.elapsedSeconds;
  @observable error: MithrilBootstrapError | null =
    DEFAULT_STATUS.error ?? null;
  @observable snapshots: Array<MithrilSnapshotItem> = [];
  @observable isFetchingSnapshots = false;
  @observable customChainPath: string | null = null;
  @observable defaultChainPath: string | null = null;
  @observable defaultChainStorageValidation: ChainStorageValidation = {
    isValid: true,
    path: null,
  };
  @observable chainStorageValidation: ChainStorageValidation = {
    isValid: true,
    path: null,
  };
  @observable isChainStorageLoading = false;
  @observable storageLocationConfirmed = false;
  @observable ancillaryBytesDownloaded: number | undefined = undefined;
  @observable ancillaryBytesTotal: number | undefined = undefined;
  @observable progressItems: MithrilProgressItem[] = [];
  @observable bootstrapStartedAt: number | null = null;

  @computed
  get bytesDownloaded(): number | undefined {
    if (
      this.snapshot == null ||
      typeof this.snapshot.size !== 'number' ||
      this.snapshot.size <= 0 ||
      this.filesDownloaded == null ||
      this.filesTotal == null ||
      this.filesTotal <= 0
    ) {
      return undefined;
    }

    const normalizedFilesDownloaded = Math.min(
      Math.max(this.filesDownloaded, 0),
      this.filesTotal
    );

    return Math.round(
      (normalizedFilesDownloaded / this.filesTotal) * this.snapshot.size
    );
  }

  @computed
  get ancillaryProgress(): number | undefined {
    if (
      this.ancillaryBytesDownloaded == null ||
      this.ancillaryBytesTotal == null ||
      this.ancillaryBytesTotal <= 0
    ) {
      return undefined;
    }
    return (this.ancillaryBytesDownloaded / this.ancillaryBytesTotal) * 100;
  }

  setup() {
    mithrilBootstrapStatusChannel.onReceive(this._updateStatus);
    this.syncStatus().catch((error) => {
      logger.warn('MithrilBootstrapStore: failed to sync status', { error });
    });
    this.loadChainStorageConfig().catch((error) => {
      logger.warn(
        'MithrilBootstrapStore: failed to load chain storage config',
        {
          error,
        }
      );
    });
  }

  @action
  syncStatus = async () => {
    const status = await mithrilBootstrapStatusChannel.request();
    this._updateStatus(status);
  };

  @action
  _updateStatus = (update: MithrilBootstrapStatusUpdate): Promise<void> => {
    const previousStatus = this.status;
    this.status = update.status;
    // Defense-in-depth: clear stale ancillary/progress state when starting a
    // fresh bootstrap run after a terminal status (failed/completed).
    // This handles the case where the decision-cycle guard does not fire
    // (e.g. failed → preparing without going through idle/decision).
    if (
      this.status === 'preparing' &&
      (previousStatus === 'failed' || previousStatus === 'completed')
    ) {
      this.ancillaryBytesDownloaded = undefined;
      this.ancillaryBytesTotal = undefined;
      this.progressItems = [];
      this.bootstrapStartedAt = null;
    }
    // Record start timestamp when entering a working status
    if (isWorkingStatus(this.status) && this.bootstrapStartedAt == null) {
      const backendElapsed = update.elapsedSeconds;
      this.bootstrapStartedAt =
        typeof backendElapsed === 'number' &&
        Number.isFinite(backendElapsed) &&
        backendElapsed > 0
          ? Date.now() - backendElapsed * 1000
          : Date.now();
    }
    if (
      isDecisionCycleStatus(this.status) &&
      !isDecisionCycleStatus(previousStatus)
    ) {
      this.storageLocationConfirmed = false;
      this.ancillaryBytesDownloaded = undefined;
      this.ancillaryBytesTotal = undefined;
      this.progressItems = [];
      this.bootstrapStartedAt = null;
    }
    if ('snapshot' in update) {
      this.snapshot = update.snapshot ?? null;
    }
    if ('filesDownloaded' in update) {
      this.filesDownloaded = update.filesDownloaded;
    }
    if ('filesTotal' in update) {
      this.filesTotal = update.filesTotal;
    }
    if ('elapsedSeconds' in update) {
      this.elapsedSeconds = update.elapsedSeconds;
    }
    if ('ancillaryBytesDownloaded' in update) {
      this.ancillaryBytesDownloaded = update.ancillaryBytesDownloaded;
    }
    if ('ancillaryBytesTotal' in update) {
      this.ancillaryBytesTotal = update.ancillaryBytesTotal;
    }
    if ('progressItems' in update && update.progressItems != null) {
      const next = update.progressItems;
      const prev = this.progressItems;
      const changed =
        next.length !== prev.length ||
        next.some(
          (item, i) => item.id !== prev[i]?.id || item.state !== prev[i]?.state
        );
      if (changed) {
        this.progressItems = next;
      }
    }
    if ('error' in update) {
      this.error = update.error ?? null;
    }
    return Promise.resolve();
  };

  @action
  loadSnapshots = async () => {
    this.isFetchingSnapshots = true;
    try {
      const snapshots = await mithrilBootstrapSnapshotsChannel.request();
      runInAction('load Mithril snapshots', () => {
        this.snapshots = snapshots || [];
      });
    } catch (error) {
      logger.warn('MithrilBootstrapStore: failed to load snapshots', { error });
    } finally {
      runInAction('finish loading Mithril snapshots', () => {
        this.isFetchingSnapshots = false;
      });
    }
  };

  @action
  setDecision = async (decision: MithrilBootstrapDecision) => {
    await mithrilBootstrapDecisionChannel.request({ decision });
  };

  @action
  startBootstrap = async (
    digest?: string,
    options?: {
      wipeChain?: boolean;
    }
  ) => {
    await mithrilBootstrapStartChannel.request({
      digest,
      wipeChain: options?.wipeChain,
    });
  };

  @action
  cancelBootstrap = async () => {
    await mithrilBootstrapCancelChannel.request();
  };

  @action
  loadChainStorageConfig = async () => {
    this.isChainStorageLoading = true;
    try {
      const config = await getChainStorageDirectoryChannel.request();

      const defaultValidation: ChainStorageValidation = {
        isValid: true,
        path: null,
        resolvedPath: config?.defaultPath,
        availableSpaceBytes: config?.availableSpaceBytes,
        requiredSpaceBytes: config?.requiredSpaceBytes,
      };

      const validation =
        config?.customPath != null
          ? await validateChainStorageDirectoryChannel.request({
              path: config.customPath,
            })
          : defaultValidation;

      runInAction('load chain storage config', () => {
        this.customChainPath = config?.customPath ?? null;
        this.defaultChainPath = config?.defaultPath ?? null;
        this.defaultChainStorageValidation = defaultValidation;
        this.chainStorageValidation = validation;
      });
    } catch (error) {
      logger.warn(
        'MithrilBootstrapStore: failed to load chain storage config',
        {
          error,
        }
      );
    } finally {
      runInAction('finish chain storage config load', () => {
        this.isChainStorageLoading = false;
      });
    }
  };

  @action
  setChainStorageDirectory = async (path: string | null) => {
    this.isChainStorageLoading = true;

    try {
      const validation = await setChainStorageDirectoryChannel.request({
        path,
      });

      runInAction('set chain storage directory', () => {
        this.chainStorageValidation = validation;
        if (validation.isValid) {
          this.customChainPath = validation.path ?? null;
          if (validation.path == null && validation.resolvedPath) {
            this.defaultChainPath = validation.resolvedPath;
            this.defaultChainStorageValidation = validation;
          }
        }
      });

      return validation;
    } finally {
      runInAction('finish setting chain storage directory', () => {
        this.isChainStorageLoading = false;
      });
    }
  };

  @action
  resetChainStorageDirectory = async () => {
    return this.setChainStorageDirectory(null);
  };

  @action
  validateChainStorageDirectory = async (path: string) => {
    try {
      return await validateChainStorageDirectoryChannel.request({ path });
    } catch (error) {
      logger.warn(
        'MithrilBootstrapStore: failed to validate chain storage directory',
        {
          error,
          path,
        }
      );

      return {
        isValid: false,
        path,
        reason: 'unknown' as const,
        message: 'Unable to validate selected directory.',
      };
    }
  };

  @action
  confirmStorageLocation = () => {
    this.storageLocationConfirmed = true;
  };

  @action
  clearStorageLocationConfirmation = () => {
    this.storageLocationConfirmed = false;
  };
}
