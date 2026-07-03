import { action, computed, observable, runInAction } from 'mobx';
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

const DEFAULT_STATUS: MithrilPartialSyncStatusSnapshot = makeIdlePartialSyncStatus();

const START_PENDING_STATUS: MithrilPartialSyncStatus = 'stopping-node';

const AVAILABILITY_REFRESH_INTERVAL = 30_000;
// Bound each availability `request()` so a wedged main process can
// never keep `_isRefreshingAvailability` pinned `true` forever (which would kill
// every future refresh and strand the idle consumers — the proactive prompt and
// the Diagnostics dialog).
const AVAILABILITY_REQUEST_TIMEOUT_MS = 10_000;
// Known-stable back-off cadence (5 min). The poll SLOWS to this once
// availability is settled-stable; it never STOPS, so the idle consumers keep updating.
const AVAILABILITY_REFRESH_BACKOFF_INTERVAL = 300_000;
// Guard: require this many CONSECUTIVE stable reads before slowing, so a
// premature first not-behind read (a probe that lands before the backend behind-ness
// settles) can never slow the poll before it self-corrects.
const STABLE_READS_BEFORE_BACKOFF = 2;

export default class MithrilPartialSyncStore extends Store {
  @observable status: MithrilPartialSyncStatus = DEFAULT_STATUS.status;
  @observable allowedRecoveryActions: MithrilPartialSyncFailureAction[] = [];
  @observable filesDownloaded: number | undefined = undefined;
  @observable filesTotal: number | undefined = undefined;
  @observable startedAt: number | null = null;
  @observable ancillaryBytesDownloaded: number | undefined = undefined;
  @observable ancillaryBytesTotal: number | undefined = undefined;
  @observable progressItems = [];
  @observable error = DEFAULT_STATUS.error;
  @observable logPath: string | undefined = undefined;
  @observable isCompletedOverlayDismissed = false;
  @observable proactivePromptDismissedThisSession = false;
  // Separate session-scoped re-pop guard, set true when a Mithril
  // attempt begins (`startPartialSync`). AND-ed into the proactive-prompt gate so
  // the prompt never re-offers after an attempt regardless of the terminal
  // outcome (completed/cancelled/failed/restart-normal). In-memory and
  // session-scoped; reset ONLY when a start rejection resyncs to idle (the
  // attempt never took hold, so the prompt may re-offer). Distinct from
  // `proactivePromptDismissedThisSession`, which stays single-purpose ("user
  // clicked Standard Sync on the prompt").
  @observable mithrilAttemptStartedThisSession = false;
  @observable isPartialSyncEnabled = false;
  @observable isSignificantlyBehind = false;
  // True when the backend behind-ness probe failed, so "not behind" cannot be
  // trusted; the Diagnostics section shows an availability-unknown hint instead.
  @observable isProbeFailed = false;
  // The Mithril certified-beacon epoch, the early-sync fallback
  // anchor for the late-resolving `networkTip.epoch`. DECLARATION ONLY here —
  // `_applyAvailability` populates it; the backend produces it in the
  // probe + IPC payload. Until then it stays `undefined` ⇒ `computeBehindByEpochs`
  // degrades to networkTip-only (no regression).
  @observable certifiedEpoch: number | null | undefined = undefined;
  _availabilityRefreshInterval: ReturnType<typeof setInterval> | null = null;
  _isTornDown = false;
  _isRefreshingAvailability = false;
  // Count of consecutive settled-stable availability reads. Plain
  // internal poll-state (not observable — no computed/component reads it). Reset to
  // 0 on every unstable read; the back-off engages only once it reaches
  // STABLE_READS_BEFORE_BACKOFF.
  _consecutiveStableReads = 0;

  setup() {
    mithrilPartialSyncStatusChannel.onReceive(async (update) => {
      this._updateStatus(update);
    });
    this.syncStatus().catch((error) => {
      logger.warn('MithrilPartialSyncStore: failed to sync status', { error });
    });
    this._refreshAvailability();
    // Re-fetch availability on every tick EXCEPT while partial-sync work is
    // active or terminal-`cancelled` — the skip-when-active guard inside
    // `_refreshAvailability` returns early on those ticks so no concurrent
    // mithril-client metadata child is spawned mid-run.
    // In `idle`/`failed`/`completed` the probe still fires on EVERY tick: the
    // one-shot setup read can land before the backend behind-ness probe has
    // settled, so the Diagnostics Mithril section must self-correct on first
    // load without a full reload. The re-entrancy guard in
    // `_refreshAvailability` keeps overlapping probes from piling up, and the
    // kill switch short-circuits cheaply when partial sync is disabled. The poll
    // starts at the fast 30s cadence and self-rearms to the known-stable back-off
    // cadence (and back) inside `_refreshAvailability`; the interval itself
    // never stops — a gated tick simply returns early until status leaves the
    // active set, after which the next tick resumes against freshly
    // invalidated caches.
    this._armAvailabilityInterval(AVAILABILITY_REFRESH_INTERVAL);
  }

  // (Re)arm the availability poll at the given cadence. Clears any
  // existing interval first so a cadence switch (fast ⇄ back-off) never leaves two
  // intervals running. Used by `setup()` and the back-off in `_refreshAvailability`.
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
    this.ancillaryBytesDownloaded =
      update.transferProgress.ancillaryBytesDownloaded;
    this.ancillaryBytesTotal = update.transferProgress.ancillaryBytesTotal;
    this.progressItems = update.progressItems;
    this.error = update.error;
    this.logPath = update.logPath;
  };

  // Availability is settled-stable when partial sync is disabled
  // (never available) or enabled-and-not-behind (caught up). A SINGLE stable read
  // must not slow the poll — that gating lives in the back-off below.
  _isAvailabilityStable(): boolean {
    return !this.isPartialSyncEnabled || !this.isSignificantlyBehind;
  }

  @action
  _refreshAvailability = async () => {
    if (this._isTornDown || this._isRefreshingAvailability) {
      return;
    }

    // Skip the probe while Mithril work is active or a cancel outcome is
    // terminal-cancelled: a probe here would spawn a concurrent
    // mithril-client metadata child (and pay the fork/readdir cost) for a
    // figure only the Diagnostics dialog could display mid-run (frozen
    // pre-run values; accepted trade-off). Behavior change from "re-fetch on
    // EVERY tick".
    if (this.isWorking || this.status === 'cancelled') {
      return;
    }

    this._isRefreshingAvailability = true;
    // Race the request against a timeout that is ALWAYS cleared in
    // `finally`, so a wedged main process cannot pin `_isRefreshingAvailability`
    // forever (the timeout rejects, the catch logs, and the guard clears so the
    // next tick can refresh). A settled request leaves no dangling timer.
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
      // Known-stable back-off. Slow the poll only after
      // STABLE_READS_BEFORE_BACKOFF CONSECUTIVE settled-stable reads — never on the
      // premature FIRST not-behind read (a probe that lands before the
      // backend behind-ness settles reads not-behind while the node IS behind; a
      // counter of 1 keeps the fast poll so it self-corrects). On the first unstable
      // read after a back-off, reset the counter and re-arm the fast 30s cadence
      // immediately so a node that falls behind again is re-detected. SLOW, never STOP.
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
    // The early-sync beacon anchor for the container figure/gate.
    // Undefined until the backend supplies it ⇒ degrades to networkTip-only
    // (no regression).
    this.certifiedEpoch = availability.certifiedEpoch;
  };

  @action
  dismissCompletedOverlay = async () => {
    if (this.status !== 'completed') {
      return;
    }
    // Await the finalize BEFORE flipping the dismiss flag, and
    // resync on any outcome. This finalize is
    // invoked by the completed-overlay auto-timeout (MithrilPartialSyncOverlay)
    // instead of an explicit "Continue to Daedalus" click; the finalize call is
    // unchanged. Reuses the store's `try { … } finally { await this.syncStatus() }`
    // resync pattern (as in cancelPartialSync/startPartialSync).
    try {
      await mithrilPartialSyncFinalizeChannel.request();
      // Flip ONLY on finalize success so the success screen never hides
      // optimistically before the backend has actually finalized. This mutation
      // resumes AFTER the await, outside the enclosing `@action`'s synchronous
      // span, so it MUST run in its own action context — under strict mode
      // (`enforceActions: 'observed'`, source/renderer/app/index.tsx) the
      // overlay observes `isCompletedOverlayDismissed` via `shouldShowOverlay`,
      // and mutating it outside an action would throw (then get swallowed by the
      // catch below, leaving the flag stuck false). Mirrors the store's existing
      // post-await action pattern (`_updateStatus`/`_applyAvailability`).
      runInAction('MithrilPartialSyncStore: dismiss completed overlay', () => {
        this.isCompletedOverlayDismissed = true;
      });
    } catch (error) {
      logger.warn(
        'MithrilPartialSyncStore: failed to finalize completed overlay',
        { error }
      );
      // No blind flip — keep the overlay up; the resync below reflects the true
      // backend state. With today's backend `_resetToIdleStatus()` runs BEFORE
      // the (possibly failing) `fs.remove(stagingRoot)`, so on failure the
      // backend is already idle and `syncStatus()` pulls `idle`; `idle` is not an
      // overlay status, so the overlay hides via status anyway (the staging/.lock
      // leak is renderer-unreachable and deferred to the out-of-scope backend
      // reorder). If a future backend reorder leaves status at `completed` on
      // failure, the no-blind-flip + resync would correctly persist the overlay.
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
    // Mark that a Mithril attempt has begun this session so the
    // proactive prompt never re-offers once status returns to idle (any outcome).
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
      // The attempt never took hold, so re-arm the session-scoped proactive
      // prompt guard. This mutation resumes after the awaits above, outside
      // the enclosing action's synchronous span, so it needs its own action
      // context under strict mode.
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
