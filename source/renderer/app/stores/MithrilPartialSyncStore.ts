import { action, computed, observable } from 'mobx';
import type {
  MithrilPartialSyncAvailability,
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
  mithrilPartialSyncAvailabilityChannel,
  mithrilPartialSyncCancelChannel,
  mithrilPartialSyncFinalizeChannel,
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

const AVAILABILITY_REFRESH_INTERVAL = 30_000;

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
  @observable startedAt: number | null = null;
  @observable ancillaryBytesDownloaded: number | undefined = undefined;
  @observable ancillaryBytesTotal: number | undefined = undefined;
  @observable progressItems = [];
  @observable error = DEFAULT_STATUS.error;
  @observable logPath: string | undefined = undefined;
  @observable isCompletedOverlayDismissed = false;
  @observable proactivePromptDismissedThisSession = false;
  @observable isPartialSyncEnabled = false;
  @observable isSignificantlyBehind = false;
  @observable behindByImmutables: number | undefined = undefined;
  _availabilityRefreshInterval: ReturnType<typeof setInterval> | null = null;
  _isTornDown = false;
  _isRefreshingAvailability = false;

  setup() {
    mithrilPartialSyncStatusChannel.onReceive(async (update) => {
      this._updateStatus(update);
    });
    this.syncStatus().catch((error) => {
      logger.warn('MithrilPartialSyncStore: failed to sync status', { error });
    });
    this._refreshAvailability();
    // Re-fetch availability on EVERY tick — not only while work is in flight.
    // The one-shot setup read can land before the backend behind-ness probe has
    // settled, so the Diagnostics Mithril section must self-correct on first
    // load (ISSUE-1) without a full reload. The re-entrancy guard in
    // `_refreshAvailability` keeps overlapping probes from piling up, and the
    // kill switch short-circuits cheaply when partial sync is disabled.
    this._availabilityRefreshInterval = setInterval(() => {
      this._refreshAvailability();
    }, AVAILABILITY_REFRESH_INTERVAL);
  }

  teardown() {
    this._isTornDown = true;
    if (this._availabilityRefreshInterval) {
      clearInterval(this._availabilityRefreshInterval);
      this._availabilityRefreshInterval = null;
    }
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

    const previousStatus = this.status;
    this.status = update.status;

    // Renderer-side elapsed anchor (mirrors MithrilBootstrapStore). A fresh
    // working run re-anchors to THIS run; we then stamp the anchor on the first
    // working frame (honoring any backend elapsedSeconds so a re-attach to an
    // in-flight op shows the true elapsed); the anchor is released only when
    // fully idle so terminal overlays keep their frozen elapsed value.
    const isWorkingNow = isMithrilPartialSyncWorkingStatus(this.status);
    if (isWorkingNow && !isMithrilPartialSyncWorkingStatus(previousStatus)) {
      this.startedAt = null;
    }
    if (isWorkingNow && this.startedAt == null) {
      const backendElapsed = update.transferProgress.elapsedSeconds;
      this.startedAt =
        typeof backendElapsed === 'number' &&
        Number.isFinite(backendElapsed) &&
        backendElapsed > 0
          ? Date.now() - backendElapsed * 1000
          : Date.now();
    }
    if (this.status === 'idle') {
      this.startedAt = null;
    }

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
  _refreshAvailability = async () => {
    if (this._isTornDown || this._isRefreshingAvailability) {
      return;
    }

    this._isRefreshingAvailability = true;
    try {
      const availability: MithrilPartialSyncAvailability = await mithrilPartialSyncAvailabilityChannel.request();
      this._applyAvailability(availability);
    } catch (error) {
      logger.warn('MithrilPartialSyncStore: failed to refresh availability', {
        error,
      });
    } finally {
      this._isRefreshingAvailability = false;
    }
  };

  @action
  _applyAvailability = (availability: MithrilPartialSyncAvailability) => {
    if (this._isTornDown) {
      return;
    }

    this.isPartialSyncEnabled = availability.isEnabled;
    this.isSignificantlyBehind = availability.isSignificantlyBehind;
    this.behindByImmutables = availability.behindByImmutables;
  };

  @action
  dismissCompletedOverlay = async () => {
    if (this.status !== 'completed') {
      return;
    }
    // Flip the renderer dismiss flag first so the success screen hides, then
    // tell the backend to finalize: reset-to-idle + remove staging + clear the
    // marker. ADR D-702a-1 amends lock #16/D9 — this finalize is now invoked by
    // the completed-overlay auto-timeout (MithrilPartialSyncOverlay) instead of
    // an explicit "Continue to Daedalus" click; the finalize call is unchanged.
    this.isCompletedOverlayDismissed = true;
    await mithrilPartialSyncFinalizeChannel.request();
  };

  @action
  dismissProactivePrompt = () => {
    this.proactivePromptDismissedThisSession = true;
  };

  // `retry` reuses this start path — there is no dedicated retry IPC channel
  // (PRD D8 / gap #24). onRetry in the overlay wires straight to startPartialSync.
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
    try {
      await mithrilPartialSyncCancelChannel.request();
    } finally {
      // Always resync so the UI never sticks on the optimistic frame (D5f) —
      // including the stopping-node no-op and the post-cutover rejection.
      await this.syncStatus();
    }
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
