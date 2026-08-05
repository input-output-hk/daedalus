import { action, computed, observable, runInAction } from 'mobx';
import Store from './lib/Store';
import {
  isMithrilSyncOverlayStatus,
  isMithrilSyncTerminalStatus,
  isMithrilSyncWorkingStatus,
  type MithrilSyncStatus,
  type MithrilSyncStatusUpdate,
  type MithrilSyncFlowType,
  type MithrilPartialSyncFailureAction,
  type MithrilSyncError,
} from '../../../common/types/mithril-sync.types';
import type {
  MithrilSnapshotItem,
  MithrilProgressItem,
  ChainStorageValidation,
  MithrilBootstrapDecision,
} from '../../../common/types/mithril-bootstrap.types';
import {
  mithrilBootstrapDecisionChannel,
  mithrilBootstrapSnapshotsChannel,
} from '../ipc/mithrilBootstrapChannel';
import {
  mithrilSyncStartChannel,
  mithrilSyncCancelChannel,
  mithrilSyncRestartNodeChannel,
} from '../ipc/mithrilSyncActionChannels';
import {
  setChainStorageDirectoryChannel,
  getChainStorageDirectoryChannel,
  validateChainStorageDirectoryChannel,
  prepareChainStorageLocationChangeChannel,
} from '../ipc/chainStorageChannel';
import {
  mithrilSyncStatusChannel,
  mithrilAvailabilityChannel,
} from '../ipc/mithrilSyncChannel';
import { logger } from '../utils/logging';
import { toMithrilStartError } from '../utils/mithrilErrorMessage';

const DEFAULT_STATUS: MithrilSyncStatus = 'idle';

const isDecisionCycleStatus = (status: MithrilSyncStatus) =>
  status === 'decision' || status === 'idle' || status === 'cancelled';

const isBootstrapWorkingStatus = (status: MithrilSyncStatus) =>
  status === 'preparing' ||
  status === 'downloading' ||
  status === 'verifying' ||
  status === 'unpacking' ||
  status === 'converting' ||
  status === 'finalizing' ||
  status === 'starting-node';

const START_PENDING_STATUS: MithrilSyncStatus = 'stopping-node';

export default class MithrilSyncStore extends Store {
  // Common
  @observable status: MithrilSyncStatus = DEFAULT_STATUS;
  @observable flowType: MithrilSyncFlowType | null = null;
  @observable filesDownloaded: number | undefined = undefined;
  @observable filesTotal: number | undefined = undefined;
  @observable snapshotBytesDownloaded: number | undefined = undefined;
  @observable snapshotBytesTotal: number | undefined = undefined;
  @observable ancillaryBytesDownloaded: number | undefined = undefined;
  @observable ancillaryBytesTotal: number | undefined = undefined;
  @observable progressItems: MithrilProgressItem[] = [];
  @observable error: MithrilSyncError | null = null;
  @observable logPath: string | undefined = undefined;
  @observable startedAt: number | null = null;

  // Bootstrap-specific
  @observable snapshot: MithrilSnapshotItem | null = null;
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
  @observable isApplyingStorageLocation = false;
  @observable pendingChainPath: string | null | undefined = undefined;
  @observable isRecoveryFallback = false;
  @observable storageLocationConfirmed = false;
  @observable bootstrapStartedAt: number | null = null;

  // Partial-sync-specific
  @observable allowedRecoveryActions: MithrilPartialSyncFailureAction[] = [];
  @observable isPartialSyncEnabled = false;
  @observable isSignificantlyBehind = false;
  @observable isProbeFailed = false;
  @observable isAtOrPastSnapshot = false;
  @observable certifiedEpoch: number | undefined = undefined;
  @observable isCompletedOverlayDismissed = false;
  @observable proactivePromptDismissedThisSession = false;
  @observable mithrilAttemptStartedThisSession = false;

  _isTornDown = false;
  @observable _returnToStorageInFlight = false;

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

  @computed
  get isWorking(): boolean {
    return isMithrilSyncWorkingStatus(this.status);
  }

  @computed
  get isTerminal(): boolean {
    return isMithrilSyncTerminalStatus(this.status);
  }

  @computed
  get hasDisplayStatus(): boolean {
    return isMithrilSyncOverlayStatus(this.status);
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
    return (
      this.status === 'cancelled' ||
      (this.status === 'failed' &&
        this.allowedRecoveryActions.includes('restart-normal'))
    );
  }

  @computed
  get canWipeAndFullSync(): boolean {
    return (
      this.status === 'cancelled' ||
      (this.status === 'failed' &&
        this.allowedRecoveryActions.includes('wipe-and-full-sync'))
    );
  }

  setup() {
    mithrilSyncStatusChannel.onReceive(async (update) => {
      this._updateStatus(update);
    });
    mithrilAvailabilityChannel.onReceive(
      async ({ isEnabled, isSignificantlyBehind }) => {
        runInAction('MithrilSyncStore: availability update', () => {
          this.isPartialSyncEnabled = isEnabled;
          this.isSignificantlyBehind = isSignificantlyBehind;
        });
      }
    );
    this.syncStatus().catch((error) => {
      logger.warn('MithrilSyncStore: failed to sync status', { error });
    });
    this.loadChainStorageConfig().catch((error) => {
      logger.warn('MithrilSyncStore: failed to load chain storage config', {
        error,
      });
    });
  }

  teardown() {
    this._isTornDown = true;
    super.teardown();
  }

  @action
  syncStatus = async () => {
    if (this._isTornDown) return;
    try {
      const status = await mithrilSyncStatusChannel.request();
      this._updateStatus(status);
    } catch (error) {
      logger.warn('MithrilSyncStore: syncStatus failed', { error });
    }
  };

  @action
  _updateStatus = (update: MithrilSyncStatusUpdate): void => {
    if (this._isTornDown) return;

    const previousStatus = this.status;
    this.status = update.status;
    this.flowType = update.flowType;

    const isWorkingNow = isMithrilSyncWorkingStatus(this.status);

    // Bootstrap-specific elapsed anchor behaviour
    if (update.flowType === 'bootstrap') {
      // Defense-in-depth: clear stale state when starting a fresh bootstrap run
      if (
        this.status === 'preparing' &&
        (previousStatus === 'failed' || previousStatus === 'completed')
      ) {
        this.snapshotBytesDownloaded = undefined;
        this.snapshotBytesTotal = undefined;
        this.ancillaryBytesDownloaded = undefined;
        this.ancillaryBytesTotal = undefined;
        this.progressItems = [];
        this.bootstrapStartedAt = null;
      }
      if (
        isBootstrapWorkingStatus(this.status) &&
        this.bootstrapStartedAt == null
      ) {
        this.bootstrapStartedAt = Date.now();
      }
      if (
        isDecisionCycleStatus(this.status) &&
        !isDecisionCycleStatus(previousStatus)
      ) {
        this.storageLocationConfirmed = false;
        this.isApplyingStorageLocation = false;
        this.pendingChainPath = undefined;
        this.snapshotBytesDownloaded = undefined;
        this.snapshotBytesTotal = undefined;
        this.ancillaryBytesDownloaded = undefined;
        this.ancillaryBytesTotal = undefined;
        this.progressItems = [];
        this.bootstrapStartedAt = null;
      }
    }

    // Partial-sync elapsed anchor behaviour
    if (update.flowType === 'partial-sync') {
      if (isWorkingNow && !isMithrilSyncWorkingStatus(previousStatus)) {
        this.startedAt = null;
      }
      if (isWorkingNow && this.startedAt == null) {
        this.startedAt = Date.now();
      }
      if (this.status === 'idle') {
        this.startedAt = null;
        // Re-arm the proactive prompt when returning to idle (e.g. after cancel
        // + restart-normally). The user is back to chain sync; if a subsequent
        // probe still shows significantly-behind, the prompt should re-appear.
        this.mithrilAttemptStartedThisSession = false;
      }
      if (update.status !== 'completed') {
        this.isCompletedOverlayDismissed = false;
      }
    }

    // Common progress fields
    if ('filesDownloaded' in update) {
      this.filesDownloaded = update.filesDownloaded;
    }
    if ('filesTotal' in update) {
      this.filesTotal = update.filesTotal;
    }
    if ('snapshotBytesDownloaded' in update) {
      this.snapshotBytesDownloaded = update.snapshotBytesDownloaded;
    }
    if ('snapshotBytesTotal' in update) {
      this.snapshotBytesTotal = update.snapshotBytesTotal;
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
    if ('logPath' in update) {
      this.logPath = update.logPath;
    }

    // Bootstrap-specific
    if ('snapshot' in update) {
      this.snapshot = update.snapshot ?? null;
    }

    // Partial-sync-specific
    if (update.allowedRecoveryActions != null) {
      this.allowedRecoveryActions = update.allowedRecoveryActions;
    }
  };

  // ---- Bootstrap methods ----

  @action
  loadSnapshots = async () => {
    this.isFetchingSnapshots = true;
    try {
      const snapshots = await mithrilBootstrapSnapshotsChannel.request();
      runInAction('load Mithril snapshots', () => {
        this.snapshots = snapshots || [];
      });
    } catch (error) {
      logger.warn('MithrilSyncStore: failed to load snapshots', { error });
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
    _digest?: string,
    _options?: { wipeChain?: boolean }
  ) => {
    await mithrilSyncStartChannel.request({ wipeChain: true });
  };

  @action
  cancelBootstrap = async () => {
    try {
      await mithrilSyncCancelChannel.request();
    } catch (error) {
      logger.warn('MithrilSyncStore: cancel bootstrap rejected', { error });
    }
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
        this.isRecoveryFallback = Boolean(config?.isRecoveryFallback);
      });
    } catch (error) {
      logger.warn('MithrilSyncStore: failed to load chain storage config', {
        error,
      });
    } finally {
      runInAction('finish chain storage config load', () => {
        this.isChainStorageLoading = false;
      });
    }
  };

  @action
  setChainStorageDirectory = async (path: string | null) => {
    this.isChainStorageLoading = true;
    this.isApplyingStorageLocation = true;
    this.pendingChainPath = path;

    try {
      const validation = await setChainStorageDirectoryChannel.request({
        path,
      });

      runInAction('set chain storage directory', () => {
        this.isRecoveryFallback = false;
        this.chainStorageValidation = validation;
        if (validation.isValid) {
          this.storageLocationConfirmed = true;
          this.pendingChainPath = undefined;
          this.customChainPath = validation.path ?? null;
          if (validation.path == null && validation.resolvedPath) {
            this.defaultChainPath = validation.resolvedPath;
            this.defaultChainStorageValidation = validation;
          }
        } else {
          this.storageLocationConfirmed = false;
          this.pendingChainPath = validation.path ?? path ?? null;
        }
      });

      return validation;
    } catch (error) {
      runInAction('revert storage location apply state', () => {
        this.storageLocationConfirmed = false;
        this.pendingChainPath = undefined;
      });
      throw error;
    } finally {
      runInAction('finish setting chain storage directory', () => {
        this.isChainStorageLoading = false;
        this.isApplyingStorageLocation = false;
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
        'MithrilSyncStore: failed to validate chain storage directory',
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
    this.isRecoveryFallback = false;
    this.storageLocationConfirmed = true;
    this.isApplyingStorageLocation = false;
    this.pendingChainPath = undefined;
  };

  @action
  clearStorageLocationConfirmation = () => {
    this.storageLocationConfirmed = false;
    this.isApplyingStorageLocation = false;
    this.pendingChainPath = undefined;
  };

  @action
  returnToStorageLocation = async () => {
    if (this._returnToStorageInFlight) return;
    this._returnToStorageInFlight = true;

    try {
      const previousCustomPath = this.customChainPath;
      const cleanupValidation =
        await prepareChainStorageLocationChangeChannel.request();

      const previousPathValidation =
        cleanupValidation && previousCustomPath != null
          ? await this.validateChainStorageDirectory(previousCustomPath)
          : null;

      runInAction('return to chain storage location picker', () => {
        this.storageLocationConfirmed = false;
        this.isApplyingStorageLocation = false;

        if (cleanupValidation && previousCustomPath != null) {
          this.customChainPath = null;
          this.defaultChainPath =
            cleanupValidation.resolvedPath ?? this.defaultChainPath;
          this.defaultChainStorageValidation = cleanupValidation;
          this.chainStorageValidation =
            previousPathValidation && previousPathValidation.isValid
              ? previousPathValidation
              : {
                  isValid: true,
                  path: previousCustomPath,
                  resolvedPath: previousCustomPath,
                  availableSpaceBytes: cleanupValidation.availableSpaceBytes,
                  requiredSpaceBytes: cleanupValidation.requiredSpaceBytes,
                  chainSubdirectoryStatus: 'will-create',
                };
          this.pendingChainPath = previousCustomPath;
        } else {
          this.pendingChainPath = undefined;
        }
      });
    } catch (error) {
      logger.error('MithrilSyncStore: failed to prepare for location change', {
        error,
      });
      runInAction('return to chain storage location picker — error', () => {
        this.storageLocationConfirmed = false;
        this.isApplyingStorageLocation = false;
      });
    } finally {
      runInAction('return to storage — clear in-flight', () => {
        this._returnToStorageInFlight = false;
      });
    }
  };

  // ---- Partial-sync methods ----

  @action
  dismissCompletedOverlay = async () => {
    if (this.status !== 'completed') return;
    runInAction('MithrilSyncStore: dismiss completed overlay', () => {
      this.isCompletedOverlayDismissed = true;
    });
    await this.syncStatus();
  };

  @action
  dismissProactivePrompt = () => {
    this.proactivePromptDismissedThisSession = true;
  };

  @action
  startPartialSync = async () => {
    let startError: unknown;
    this.mithrilAttemptStartedThisSession = true;
    this._updateStatus({
      status: START_PENDING_STATUS,
      flowType: 'partial-sync',
      allowedRecoveryActions: [],
      progressItems: [],
      error: null,
      logPath: undefined,
    });

    try {
      await mithrilSyncStartChannel.request({ wipeChain: false });
    } catch (error) {
      startError = error;
    } finally {
      await this.syncStatus();
    }

    if (!startError) return;

    if (this.status === 'failed') {
      logger.warn(
        'MithrilSyncStore: swallowed partial sync start rejection after backend status resync',
        { error: startError, status: this.status }
      );
      return;
    }

    if (this.status === 'idle') {
      runInAction(
        'MithrilSyncStore: re-arm prompt after rejected start',
        () => {
          this.mithrilAttemptStartedThisSession = false;
        }
      );
    }

    throw toMithrilStartError(startError);
  };

  @action
  cancelPartialSync = async () => {
    try {
      await mithrilSyncCancelChannel.request();
    } catch (error) {
      logger.warn('MithrilSyncStore: cancel partial sync rejected', { error });
    } finally {
      await this.syncStatus();
    }
  };

  @action
  restartNormally = async () => {
    try {
      await mithrilSyncRestartNodeChannel.request();
    } catch (error) {
      logger.warn('MithrilSyncStore: restart node rejected', { error });
    }
  };

  @action
  wipeAndFullSync = async () => {
    try {
      await mithrilSyncStartChannel.request({ wipeChain: true });
    } catch (error) {
      logger.warn('MithrilSyncStore: wipe and full sync rejected', { error });
    }
  };
}
