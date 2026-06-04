import { action, computed, observable } from 'mobx';
import type {
  MithrilPartialSyncFailureAction,
  MithrilPartialSyncStatus,
  MithrilPartialSyncStatusSnapshot,
} from '../../../common/types/mithril-partial-sync.types';
import {
  isMithrilPartialSyncActiveStatus,
  isMithrilPartialSyncOverlayStatus,
  isMithrilPartialSyncTerminalStatus,
  isMithrilPartialSyncWorkingStatus,
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

const DEFAULT_STATUS: MithrilPartialSyncStatusSnapshot = {
  status: 'idle',
  allowedRecoveryActions: [],
  transferProgress: {},
  progressItems: [],
  error: null,
};

const START_PENDING_STATUS: MithrilPartialSyncStatus = 'stopping-node';

const toStartError = (error: unknown): Error => {
  if (error instanceof Error) {
    return error;
  }

  if (
    error &&
    typeof error === 'object' &&
    typeof (error as { message?: unknown }).message === 'string'
  ) {
    return new Error((error as { message: string }).message);
  }

  return new Error('Unable to start Mithril partial sync.');
};

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
  _isTornDown = false;

  setup() {
    mithrilPartialSyncStatusChannel.onReceive(async (update) => {
      this._updateStatus(update);
    });
    this.syncStatus().catch((error) => {
      logger.warn('MithrilPartialSyncStore: failed to sync status', { error });
    });
  }

  teardown() {
    this._isTornDown = true;
    super.teardown();
  }

  @computed
  get isActive(): boolean {
    return isMithrilPartialSyncActiveStatus(this.status);
  }

  @computed
  get isWorking(): boolean {
    return isMithrilPartialSyncWorkingStatus(this.status);
  }

  @computed
  get isTerminal(): boolean {
    return isMithrilPartialSyncTerminalStatus(this.status);
  }

  @computed
  get hasDisplayStatus(): boolean {
    return isMithrilPartialSyncOverlayStatus(this.status);
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
  syncStatus = async () => {
    if (this._isTornDown) {
      return;
    }

    const status = await mithrilPartialSyncStatusChannel.request();
    this._updateStatus(status);
  };

  @action
  _updateStatus = (update: MithrilPartialSyncStatusSnapshot) => {
    if (this._isTornDown) {
      return;
    }

    this.status = update.status;
    if (update.status !== 'completed') {
      this.isCompletedOverlayDismissed = false;
    }
    this.allowedRecoveryActions = update.allowedRecoveryActions;
    this.filesDownloaded = update.transferProgress.filesDownloaded;
    this.filesTotal = update.transferProgress.filesTotal;
    this.elapsedSeconds = update.transferProgress.elapsedSeconds;
    this.ancillaryBytesDownloaded =
      update.transferProgress.ancillaryBytesDownloaded;
    this.ancillaryBytesTotal = update.transferProgress.ancillaryBytesTotal;
    this.progressItems = update.progressItems;
    this.error = update.error;
    this.logPath = update.logPath;
  };

  @action
  dismissCompletedOverlay = () => {
    if (this.status === 'completed') {
      this.isCompletedOverlayDismissed = true;
    }
  };

  @action
  startPartialSync = async () => {
    let startError: unknown;
    this._updateStatus({
      status: START_PENDING_STATUS,
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
      logPath: undefined,
    });

    try {
      await mithrilPartialSyncStartChannel.request();
    } catch (error) {
      startError = error;
    } finally {
      await this.syncStatus();
    }

    if (!startError) {
      return;
    }

    if (this.status !== START_PENDING_STATUS) {
      return;
    }

    throw toStartError(startError);
  };

  @action
  cancelPartialSync = async () => {
    await mithrilPartialSyncCancelChannel.request();
  };

  @action
  restartNormally = async () => {
    await mithrilPartialSyncRestartNormalChannel.request();
  };

  @action
  wipeAndFullSync = async () => {
    await mithrilPartialSyncWipeAndFullSyncChannel.request();
  };
}
