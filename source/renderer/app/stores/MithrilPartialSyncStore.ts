import { action, computed, observable, runInAction } from 'mobx';
import type {
  MithrilPartialSyncAvailability,
  MithrilPartialSyncFailureAction,
  MithrilPartialSyncStatus,
  MithrilPartialSyncStatusSnapshot,
} from '../../../common/types/mithril-partial-sync.types';
import type { MithrilProgressItem } from '../../../common/types/mithril-bootstrap.types';
import {
  isMithrilPartialSyncOverlayStatus,
  isMithrilPartialSyncTerminalStatus,
  isMithrilPartialSyncWorkingStatus,
  makeIdlePartialSyncStatus,
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
import { toMithrilStartError } from '../utils/mithrilErrorMessage';

const DEFAULT_STATUS: MithrilPartialSyncStatusSnapshot =
  makeIdlePartialSyncStatus();

const START_PENDING_STATUS: MithrilPartialSyncStatus = 'stopping-node';

const AVAILABILITY_REFRESH_INTERVAL = 30_000;
// Bounds each availability request so a wedged main process cannot pin _isRefreshingAvailability true forever and strand future refreshes.
const AVAILABILITY_REQUEST_TIMEOUT_MS = 10_000;
// Slower poll cadence once availability is settled-stable; the poll slows to this but never stops.
const AVAILABILITY_REFRESH_BACKOFF_INTERVAL = 300_000;
// Consecutive stable reads required before backing off, so a premature not-behind read cannot slow the poll before it self-corrects.
const STABLE_READS_BEFORE_BACKOFF = 2;

export default class MithrilPartialSyncStore extends Store {
  @observable status: MithrilPartialSyncStatus = DEFAULT_STATUS.status;
  @observable allowedRecoveryActions: MithrilPartialSyncFailureAction[] = [];
  @observable filesDownloaded: number | undefined = undefined;
  @observable filesTotal: number | undefined = undefined;
  @observable startedAt: number | null = null;
  @observable ancillaryBytesDownloaded: number | undefined = undefined;
  @observable ancillaryBytesTotal: number | undefined = undefined;
  @observable progressItems: MithrilProgressItem[] = [];
  @observable error = DEFAULT_STATUS.error;
  @observable logPath: string | undefined = undefined;
  @observable isCompletedOverlayDismissed = false;
  @observable proactivePromptDismissedThisSession = false;
  // Session-scoped re-pop guard: set once an attempt begins so the proactive prompt never re-offers regardless of outcome;
  //  reset only when a start rejection resyncs to idle (the attempt never took hold).
  @observable mithrilAttemptStartedThisSession = false;
  @observable isPartialSyncEnabled = false;
  @observable isSignificantlyBehind = false;
  // True when the backend behind-ness probe failed, so "not behind" cannot be
  // trusted; the Diagnostics section shows an availability-unknown hint instead.
  @observable isProbeFailed = false;
  // True when the backend probe found the local tip at or past the latest
  // certified snapshot. Defaults to false: the disabled/working availability
  // short-circuits return no flag at all.
  @observable isAtOrPastSnapshot = false;
  // Mithril certified-beacon epoch: early-resolving fallback anchor for the late networkTip.epoch.
  //  Undefined until the backend supplies it; then computeBehindByEpochs degrades to networkTip-only.
  @observable certifiedEpoch: number | undefined = undefined;
  _availabilityRefreshInterval: ReturnType<typeof setInterval> | null = null;
  _isTornDown = false;
  _isRefreshingAvailability = false;
  // Plain poll-state; deliberately not @observable (nothing reactive reads it).
  _consecutiveStableReads = 0;

  setup() {
    mithrilPartialSyncStatusChannel.onReceive(async (update) => {
      this._updateStatus(update);
    });
    this.syncStatus().catch((error) => {
      logger.warn('MithrilPartialSyncStore: failed to sync status', { error });
    });
    this._refreshAvailability();
    // Poll starts fast and self-rearms between fast and back-off inside _refreshAvailability; the interval never stops.
    //  Ticks while active/cancelled return early there, but idle/failed/completed keep probing so Diagnostics self-corrects on first load.
    this._armAvailabilityInterval(AVAILABILITY_REFRESH_INTERVAL);
  }

  // (Re)arms the availability poll, clearing any existing interval first so a fast/back-off cadence switch never leaves two intervals running.
  _armAvailabilityInterval(intervalMs: number) {
    if (this._availabilityRefreshInterval) {
      clearInterval(this._availabilityRefreshInterval);
    }
    this._availabilityRefreshInterval = setInterval(() => {
      this._refreshAvailability();
    }, intervalMs);
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

    // Renderer-side elapsed anchor: re-anchored on the first frame of a fresh working run (honoring backend
    //  elapsedSeconds when re-attaching to an in-flight op), and released only at idle so terminal overlays keep a frozen elapsed value.
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
    this.ancillaryBytesDownloaded =
      update.transferProgress.ancillaryBytesDownloaded;
    this.ancillaryBytesTotal = update.transferProgress.ancillaryBytesTotal;
    this.progressItems = update.progressItems;
    this.error = update.error;
    this.logPath = update.logPath;
  };

  // Availability is stable when partial sync is disabled or the node is caught up.
  _isAvailabilityStable(): boolean {
    return !this.isPartialSyncEnabled || !this.isSignificantlyBehind;
  }

  @action
  _refreshAvailability = async () => {
    if (this._isTornDown || this._isRefreshingAvailability) {
      return;
    }

    // Skip the probe while work is active or terminal-cancelled: it would spawn a concurrent mithril-client
    //  metadata child (fork/readdir cost) for a figure only Diagnostics could show mid-run.
    if (this.isWorking || this.status === 'cancelled') {
      return;
    }

    this._isRefreshingAvailability = true;
    // Timeout is always cleared in finally so a settled request leaves no dangling timer.
    let timeoutId: ReturnType<typeof setTimeout> | undefined;
    const withTimeout = new Promise<never>((_, reject) => {
      timeoutId = setTimeout(
        () => reject(new Error('availability request timed out')),
        AVAILABILITY_REQUEST_TIMEOUT_MS
      );
    });
    try {
      const availability: MithrilPartialSyncAvailability = await Promise.race([
        mithrilPartialSyncAvailabilityChannel.request(),
        withTimeout,
      ]);
      this._applyAvailability(availability);
      // Re-arm the fast cadence on the first unstable read after a back-off so a node that falls behind again is re-detected promptly.
      if (this._isAvailabilityStable()) {
        this._consecutiveStableReads += 1;
        if (this._consecutiveStableReads === STABLE_READS_BEFORE_BACKOFF) {
          this._armAvailabilityInterval(AVAILABILITY_REFRESH_BACKOFF_INTERVAL);
        }
      } else {
        const wasBackedOff =
          this._consecutiveStableReads >= STABLE_READS_BEFORE_BACKOFF;
        this._consecutiveStableReads = 0;
        if (wasBackedOff) {
          this._armAvailabilityInterval(AVAILABILITY_REFRESH_INTERVAL);
        }
      }
    } catch (error) {
      logger.warn('MithrilPartialSyncStore: failed to refresh availability', {
        error,
      });
    } finally {
      if (timeoutId) {
        clearTimeout(timeoutId);
      }
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
    this.isProbeFailed = Boolean(availability.isProbeFailed);
    this.isAtOrPastSnapshot = Boolean(availability.isAtOrPastSnapshot);
    this.certifiedEpoch = availability.certifiedEpoch;
  };

  @action
  dismissCompletedOverlay = async () => {
    if (this.status !== 'completed') {
      return;
    }
    // Await finalize before flipping the dismiss flag; resync on any outcome.
    try {
      await mithrilPartialSyncFinalizeChannel.request();
      // Flip only on finalize success so the success frame never hides before the backend finalizes. Post-await
      //  under strict mode this must run in its own action context or the mutation would throw.
      runInAction('MithrilPartialSyncStore: dismiss completed overlay', () => {
        this.isCompletedOverlayDismissed = true;
      });
    } catch (error) {
      logger.warn(
        'MithrilPartialSyncStore: failed to finalize completed overlay',
        { error }
      );
      // On failure do not flip; the resync below reflects the true backend state, so the overlay persists if the backend is still 'completed'.
    } finally {
      await this.syncStatus();
    }
  };

  @action
  dismissProactivePrompt = () => {
    this.proactivePromptDismissedThisSession = true;
  };

  // `retry` reuses this start path — there is no dedicated retry IPC channel.
  // onRetry in the overlay wires straight to startPartialSync.
  @action
  startPartialSync = async () => {
    let startError: unknown;
    this.mithrilAttemptStartedThisSession = true;
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

    if (this.status === 'failed') {
      // The backend already reports the failure and the error view renders
      // from that status; rethrowing would only duplicate the surface.
      logger.warn(
        'MithrilPartialSyncStore: swallowed partial sync start rejection after backend status resync',
        {
          error: startError,
          status: this.status,
        }
      );
      return;
    }

    if (this.status === 'idle') {
      // Start rejected back to idle: the attempt never took hold, so re-arm the prompt guard. Post-await,
      //  this needs its own action context under strict mode.
      runInAction(
        'MithrilPartialSyncStore: re-arm prompt after rejected start',
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
      await mithrilPartialSyncCancelChannel.request();
    } catch (error) {
      // The resync below reflects the true backend outcome (a failed status
      // drives the error view); the rejection adds no renderer state.
      logger.warn('MithrilPartialSyncStore: cancel partial sync rejected', {
        error,
      });
    } finally {
      // Always resync so the UI never sticks on the optimistic frame —
      // including the stopping-node no-op and the post-cutover rejection.
      await this.syncStatus();
    }
  };

  @action
  restartNormally = async () => {
    try {
      await mithrilPartialSyncRestartNormalChannel.request();
    } catch (error) {
      // Same contract as cancel: resync reflects the backend outcome; no
      // renderer-side error surface is added.
      logger.warn('MithrilPartialSyncStore: restart-normal request rejected', {
        error,
      });
    } finally {
      await this.syncStatus();
    }
  };

  @action
  wipeAndFullSync = async () => {
    try {
      await mithrilPartialSyncWipeAndFullSyncChannel.request();
    } catch (error) {
      logger.warn(
        'MithrilPartialSyncStore: wipe-and-full-sync request rejected',
        { error }
      );
    } finally {
      await this.syncStatus();
    }
  };
}
