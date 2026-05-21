import { action, computed, observable } from 'mobx';
import type {
  MithrilPartialSyncFailureAction,
  MithrilPartialSyncStatus,
  MithrilPartialSyncStatusUpdate,
} from '../../../common/types/mithril-partial-sync.types';
import Store from './lib/Store';
import {
  mithrilPartialSyncCancelChannel,
  mithrilPartialSyncRestartNormalChannel,
  mithrilPartialSyncStartChannel,
  mithrilPartialSyncStatusChannel,
  mithrilPartialSyncWipeAndFullSyncChannel,
} from '../ipc/mithrilPartialSyncChannel';
import { logger } from '../utils/logging';

const DEFAULT_STATUS: MithrilPartialSyncStatusUpdate = {
  status: 'idle',
  allowedRecoveryActions: [],
  error: null,
};

const WORKING_STATUSES: MithrilPartialSyncStatus[] = [
  'stopping-node',
  'preparing',
  'downloading',
  'verifying',
  'converting',
  'installing',
  'finalizing',
  'starting-node',
];

const TERMINAL_STATUSES: MithrilPartialSyncStatus[] = [
  'completed',
  'failed',
  'cancelled',
];

const DISPLAY_STATUSES: MithrilPartialSyncStatus[] = [
  'preparing',
  'downloading',
  'verifying',
  'converting',
  'installing',
  'finalizing',
  'starting-node',
  'completed',
  'failed',
  'cancelled',
];

const STATUS_POLL_INTERVAL = 1000;
const START_PENDING_STATUS: MithrilPartialSyncStatus = 'stopping-node';

export default class MithrilPartialSyncStore extends Store {
  @observable status: MithrilPartialSyncStatus = DEFAULT_STATUS.status;
  @observable allowedRecoveryActions: MithrilPartialSyncFailureAction[] = [];
  @observable filesDownloaded: number | undefined = undefined;
  @observable filesTotal: number | undefined = undefined;
  @observable elapsedSeconds: number | undefined = undefined;
  @observable ancillaryBytesDownloaded: number | undefined = undefined;
  @observable ancillaryBytesTotal: number | undefined = undefined;
  @observable progressItems = [];
  @observable error = DEFAULT_STATUS.error;
  @observable logPath: string | undefined = undefined;
  @observable isCompletedOverlayDismissed = false;
  _statusPollingInterval: ReturnType<typeof setInterval> | null = null;
  _syncStatusPromise: Promise<void> | null = null;
  _statusSyncGeneration = 0;
  _teardownGeneration = 0;
  _pendingStatusSyncAfterCurrent = false;
  _isTornDown = false;

  setup() {
    this.syncStatus().catch((error) => {
      logger.warn('MithrilPartialSyncStore: failed to sync status', { error });
    });
    this._setStatusPollingInterval();
  }

  teardown() {
    this._isTornDown = true;
    this._statusSyncGeneration += 1;
    this._teardownGeneration += 1;
    this._pendingStatusSyncAfterCurrent = false;
    super.teardown();
    this._clearStatusPollingInterval();
  }

  @computed
  get isActive(): boolean {
    return this.status !== 'idle';
  }

  @computed
  get isWorking(): boolean {
    return WORKING_STATUSES.includes(this.status);
  }

  @computed
  get isTerminal(): boolean {
    return TERMINAL_STATUSES.includes(this.status);
  }

  @computed
  get hasDisplayStatus(): boolean {
    return DISPLAY_STATUSES.includes(this.status);
  }

  @computed
  get shouldShowOverlay(): boolean {
    return this.hasDisplayStatus && !this.isCompletedOverlayDismissed;
  }

  @computed
  get canRetry(): boolean {
    return this.allowedRecoveryActions.includes('retry');
  }

  @computed
  get canRestartNormally(): boolean {
    return this.allowedRecoveryActions.includes('restart-normal');
  }

  @computed
  get canWipeAndFullSync(): boolean {
    return this.allowedRecoveryActions.includes('wipe-and-full-sync');
  }

  @action
  _setStatusPollingInterval = () => {
    if (!this.isWorking) return;
    this._clearStatusPollingInterval();
    this._statusPollingInterval = setInterval(() => {
      this.syncStatus().catch((error) => {
        logger.warn('MithrilPartialSyncStore: failed to poll status', {
          error,
        });
      });
    }, STATUS_POLL_INTERVAL);
  };

  @action
  _clearStatusPollingInterval = () => {
    if (this._statusPollingInterval) {
      clearInterval(this._statusPollingInterval);
      this._statusPollingInterval = null;
    }
  };

  @action
  syncStatus = async () => {
    if (this._isTornDown) {
      return;
    }

    if (this._syncStatusPromise) {
      this._pendingStatusSyncAfterCurrent = true;
      return this._syncStatusPromise;
    }

    const syncGeneration = this._statusSyncGeneration;
    const teardownGeneration = this._teardownGeneration;

    const syncPromise = (async () => {
      const status = await mithrilPartialSyncStatusChannel.request();
      if (
        syncGeneration !== this._statusSyncGeneration ||
        teardownGeneration !== this._teardownGeneration
      ) {
        return;
      }
      this._updateStatus(status);
    })();

    this._syncStatusPromise = syncPromise;

    try {
      await syncPromise;
    } finally {
      if (this._syncStatusPromise === syncPromise) {
        this._syncStatusPromise = null;
      }

      if (
        this._pendingStatusSyncAfterCurrent &&
        !this._isTornDown &&
        teardownGeneration === this._teardownGeneration
      ) {
        this._pendingStatusSyncAfterCurrent = false;
        await this.syncStatus();
      }
    }
  };

  @action
  _updateStatus = (update: MithrilPartialSyncStatusUpdate) => {
    if (this._isTornDown) {
      return;
    }

    this.status = update.status;
    if (update.status !== 'completed') {
      this.isCompletedOverlayDismissed = false;
    }
    this.allowedRecoveryActions = update.allowedRecoveryActions;

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

    if ('progressItems' in update) {
      this.progressItems = update.progressItems ?? [];
    }

    if ('error' in update) {
      this.error = update.error ?? null;
    }

    if ('logPath' in update) {
      this.logPath = update.logPath;
    }

    if (this.isWorking) {
      this._setStatusPollingInterval();
    } else {
      this._clearStatusPollingInterval();
    }
  };

  @action
  dismissCompletedOverlay = () => {
    if (this.status === 'completed') {
      this.isCompletedOverlayDismissed = true;
    }
  };

  @action
  startPartialSync = async () => {
    this._statusSyncGeneration += 1;
    if (this._syncStatusPromise) {
      this._pendingStatusSyncAfterCurrent = true;
    }
    this._updateStatus({
      status: START_PENDING_STATUS,
      allowedRecoveryActions: [],
      filesDownloaded: undefined,
      filesTotal: undefined,
      elapsedSeconds: undefined,
      ancillaryBytesDownloaded: undefined,
      ancillaryBytesTotal: undefined,
      progressItems: [],
      error: null,
      logPath: undefined,
    });

    try {
      await mithrilPartialSyncStartChannel.request();
    } finally {
      await this.syncStatus();
    }
  };

  @action
  cancelPartialSync = async () => {
    await mithrilPartialSyncCancelChannel.request();
    await this.syncStatus();
  };

  @action
  restartNormally = async () => {
    await mithrilPartialSyncRestartNormalChannel.request();
    await this.syncStatus();
  };

  @action
  wipeAndFullSync = async () => {
    await mithrilPartialSyncWipeAndFullSyncChannel.request();
    await this.syncStatus();
  };
}
