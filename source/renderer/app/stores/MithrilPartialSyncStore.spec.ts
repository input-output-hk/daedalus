import { autorun, configure } from 'mobx';
import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import MithrilPartialSyncStore from './MithrilPartialSyncStore';
import { noopAnalyticsTracker } from '../analytics';
import { logger } from '../utils/logging';

// The real logger writes to the `electronLog` global, which jest does not
// provide (jsdom). Stub it so the store's catch-branch logger.warn calls (e.g.
// dismissCompletedOverlay's finalize-failure path) do not crash and stay
// assertable. Mirrors the pattern in MithrilPartialSyncSection.spec.tsx.
jest.mock('../utils/logging', () => ({
  logger: {
    warn: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
    debug: jest.fn(),
  },
}));

jest.useFakeTimers();

const mockStatusRequest = jest.fn();
const mockStartRequest = jest.fn();
const mockCancelRequest = jest.fn();
const mockRestartNormalRequest = jest.fn();
const mockWipeAndFullSyncRequest = jest.fn();
const mockAvailabilityRequest = jest.fn();
const mockFinalizeRequest = jest.fn();
let registeredStatusHandler;

jest.mock('../ipc/mithrilPartialSyncChannel', () => ({
  mithrilPartialSyncStatusChannel: {
    request: (...args) => mockStatusRequest(...args),
    onReceive: (handler) => {
      registeredStatusHandler = handler;
    },
  },
  mithrilPartialSyncStartChannel: {
    request: (...args) => mockStartRequest(...args),
  },
  mithrilPartialSyncCancelChannel: {
    request: (...args) => mockCancelRequest(...args),
  },
  mithrilPartialSyncRestartNormalChannel: {
    request: (...args) => mockRestartNormalRequest(...args),
  },
  mithrilPartialSyncWipeAndFullSyncChannel: {
    request: (...args) => mockWipeAndFullSyncRequest(...args),
  },
  mithrilPartialSyncAvailabilityChannel: {
    request: (...args) => mockAvailabilityRequest(...args),
  },
  mithrilPartialSyncFinalizeChannel: {
    request: (...args) => mockFinalizeRequest(...args),
  },
}));

describe('MithrilPartialSyncStore', () => {
  const api: Api = ({
    ada: jest.fn(),
  } as unknown) as Api;
  const actions: ActionsMap = (jest.fn() as unknown) as ActionsMap;

  const setupStore = () =>
    new MithrilPartialSyncStore(api, actions, noopAnalyticsTracker);

  beforeEach(() => {
    jest.resetAllMocks();
    registeredStatusHandler = undefined;
    mockAvailabilityRequest.mockResolvedValue({
      isEnabled: true,
      isSignificantlyBehind: false,
    });
  });

  afterEach(() => {
    jest.clearAllTimers();
  });

  it('syncs cached backend status during setup and subscribes for pushed updates', async () => {
    const store = setupStore();
    mockStatusRequest.mockResolvedValue({
      status: 'failed',
      allowedRecoveryActions: ['retry', 'restart-normal'],
      transferProgress: {
        filesDownloaded: 10,
        filesTotal: 20,
        elapsedSeconds: 30,
      },
      progressItems: [],
      logPath: '/tmp/mithril-partial-sync.log',
      error: {
        message: 'Restore failed',
        stage: 'installing',
      },
    });

    store.setup();
    await mockStatusRequest.mock.results[0].value;
    await Promise.resolve();

    expect(mockStatusRequest).toHaveBeenCalledTimes(1);
    expect(typeof registeredStatusHandler).toBe('function');
    expect(store.status).toBe('failed');
    expect(store.allowedRecoveryActions).toEqual(['retry', 'restart-normal']);
    expect(store.canRetry).toBe(true);
    expect(store.canRestartNormally).toBe(true);
    expect(store.canWipeAndFullSync).toBe(false);
    expect(store.filesDownloaded).toBe(10);
    expect(store.filesTotal).toBe(20);
    expect(store.elapsedSeconds).toBe(30);
    expect(store.logPath).toBe('/tmp/mithril-partial-sync.log');
    expect(store.error).toEqual({
      message: 'Restore failed',
      stage: 'installing',
    });
  });

  it('replaces recovery actions and clears explicit fields on status update', () => {
    const store = setupStore();

    store._updateStatus({
      status: 'downloading',
      allowedRecoveryActions: ['retry'],
      transferProgress: {
        filesDownloaded: 5,
        filesTotal: 10,
        elapsedSeconds: 12,
        ancillaryBytesDownloaded: 100,
        ancillaryBytesTotal: 200,
      },
      progressItems: [
        {
          id: 'download',
          label: 'Downloading',
          state: 'active',
        },
      ],
      error: {
        message: 'temporary',
      },
      logPath: '/tmp/a.log',
    });

    store._updateStatus({
      status: 'cancelled',
      allowedRecoveryActions: ['wipe-and-full-sync'],
      transferProgress: {},
      progressItems: [],
      error: null,
      logPath: undefined,
    });

    expect(store.status).toBe('cancelled');
    expect(store.allowedRecoveryActions).toEqual(['wipe-and-full-sync']);
    expect(store.canRetry).toBe(false);
    expect(store.canRestartNormally).toBe(false);
    expect(store.canWipeAndFullSync).toBe(true);
    expect(store.filesDownloaded).toBeUndefined();
    expect(store.filesTotal).toBeUndefined();
    expect(store.elapsedSeconds).toBeUndefined();
    expect(store.ancillaryBytesDownloaded).toBeUndefined();
    expect(store.ancillaryBytesTotal).toBeUndefined();
    expect(store.progressItems).toEqual([]);
    expect(store.error).toBeNull();
    expect(store.logPath).toBeUndefined();
    expect(store.isActive).toBe(true);
    expect(store.isWorking).toBe(false);
    expect(store.isTerminal).toBe(true);
  });

  it('shows the overlay only for backend-confirmed display states and can dismiss completed', async () => {
    const store = setupStore();
    mockFinalizeRequest.mockResolvedValue(undefined);
    // dismissCompletedOverlay now resyncs via syncStatus()
    // in its finally; back the status channel with a realistic post-finalize
    // idle snapshot so the added resync resolves.
    mockStatusRequest.mockResolvedValue({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    store._updateStatus({
      status: 'stopping-node',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    expect(store.shouldShowOverlay).toBe(true);

    store._updateStatus({
      status: 'preparing',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    expect(store.shouldShowOverlay).toBe(true);

    store._updateStatus({
      status: 'completed',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    expect(store.shouldShowOverlay).toBe(true);

    await store.dismissCompletedOverlay();

    expect(store.shouldShowOverlay).toBe(false);
    expect(mockFinalizeRequest).toHaveBeenCalledTimes(1);

    store._updateStatus({
      status: 'failed',
      allowedRecoveryActions: ['retry'],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    expect(store.shouldShowOverlay).toBe(true);
  });

  it('dismissCompletedOverlay flips the dismiss flag only AFTER finalize resolves (no premature optimistic hide) and resyncs', async () => {
    const store = setupStore();
    let resolveFinalize: () => void = () => {};
    mockFinalizeRequest.mockReturnValue(
      new Promise<void>((resolve) => {
        resolveFinalize = resolve;
      })
    );
    // Keep the post-finalize resync on 'completed' so the success flag flip is
    // the only thing under test (an idle resync would also clear the flag via
    // _updateStatus, masking the flip).
    mockStatusRequest.mockResolvedValue({
      status: 'completed',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    store._updateStatus({
      status: 'completed',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    // Exercise the production strict-mode path: the post-await flag flip resumes
    // OUTSIDE the enclosing `@action`, so it must run in its own action context
    // (the store wraps it in `runInAction`). Under `enforceActions: 'observed'`
    // (the renderer's config in source/renderer/app/index.tsx) with an active
    // observer reading `shouldShowOverlay` (→ `isCompletedOverlayDismissed`),
    // mutating that observable outside an action throws — and that throw would be
    // swallowed by `dismissCompletedOverlay`'s catch, leaving the flag stuck
    // false and logging a false finalize-failure. Both are asserted against
    // below. The MobX 5 default is no enforcement, so without this configure the
    // regression is invisible (the defect this test guards). Restored to the
    // default in the finally so it does not leak to other tests in this file.
    configure({ enforceActions: 'observed' });
    const observedOverlay: boolean[] = [];
    const dispose = autorun(() => {
      // Read the observed chain so the strict-mode guard is armed at flip time.
      observedOverlay.push(store.shouldShowOverlay);
    });

    try {
      const dismissal = store.dismissCompletedOverlay();
      // The finalize IPC is still pending → the flag must NOT have flipped yet.
      await Promise.resolve();
      expect(store.isCompletedOverlayDismissed).toBe(false);
      expect(mockFinalizeRequest).toHaveBeenCalledTimes(1);

      resolveFinalize();
      await dismissal;

      // Flag flips only after the awaited finalize; the finally resync ran. The
      // flip would be false here if the runInAction wrap regressed (strict-mode
      // throw swallowed by the catch).
      expect(store.isCompletedOverlayDismissed).toBe(true);
      expect(mockStatusRequest).toHaveBeenCalled();
      // Success path must NOT take the catch branch (no false finalize-failure).
      expect(logger.warn).not.toHaveBeenCalled();
    } finally {
      dispose();
      configure({ enforceActions: 'never' });
    }
  });

  it('dismissCompletedOverlay swallows a finalize rejection: keeps the flag false, resyncs, and never throws (overlay hides via idle resync)', async () => {
    const store = setupStore();
    mockFinalizeRequest.mockRejectedValue(new Error('finalize failed'));
    // Today's backend runs _resetToIdleStatus() BEFORE the (failing) fs.remove,
    // so on failure the resync pulls idle and the overlay hides via status —
    // NOT via the dismiss flag (which must stay false). The staging/.lock leak
    // is renderer-unreachable and deferred to the out-of-scope backend reorder.
    mockStatusRequest.mockResolvedValue({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    store._updateStatus({
      status: 'completed',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    expect(store.shouldShowOverlay).toBe(true);

    await expect(store.dismissCompletedOverlay()).resolves.toBeUndefined();

    expect(store.isCompletedOverlayDismissed).toBe(false);
    expect(mockStatusRequest).toHaveBeenCalled();
    expect(store.status).toBe('idle');
    expect(store.shouldShowOverlay).toBe(false);
    expect(logger.warn).toHaveBeenCalledWith(
      'MithrilPartialSyncStore: failed to finalize completed overlay',
      expect.objectContaining({ error: expect.any(Error) })
    );
  });

  it('delegates recovery and lifecycle actions through payload-free IPC requests', async () => {
    const store = setupStore();
    mockStartRequest.mockResolvedValue(undefined);
    mockStatusRequest.mockResolvedValue({
      status: 'stopping-node',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    await store.startPartialSync();
    await store.cancelPartialSync();
    await store.restartNormally();
    await store.wipeAndFullSync();

    expect(mockStartRequest).toHaveBeenCalledTimes(1);
    expect(mockStartRequest).toHaveBeenCalledWith();
    expect(mockCancelRequest).toHaveBeenCalledTimes(1);
    expect(mockCancelRequest).toHaveBeenCalledWith();
    expect(mockRestartNormalRequest).toHaveBeenCalledTimes(1);
    expect(mockRestartNormalRequest).toHaveBeenCalledWith();
    expect(mockWipeAndFullSyncRequest).toHaveBeenCalledTimes(1);
    expect(mockWipeAndFullSyncRequest).toHaveBeenCalledWith();
    // startPartialSync resyncs once (its finally) and cancelPartialSync now
    // always resyncs too (its finally), so syncStatus fires twice.
    expect(mockStatusRequest).toHaveBeenCalledTimes(2);
  });

  it('always resyncs after a cancel even when the cancel request rejects', async () => {
    const store = setupStore();
    mockCancelRequest.mockRejectedValue(new Error('post-cutover'));
    mockStatusRequest.mockResolvedValue({
      status: 'failed',
      allowedRecoveryActions: ['retry'],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    await expect(store.cancelPartialSync()).rejects.toThrow('post-cutover');

    expect(mockCancelRequest).toHaveBeenCalledTimes(1);
    expect(mockStatusRequest).toHaveBeenCalledTimes(1);
    expect(store.status).toBe('failed');
  });

  it('absorbs pushed status updates while the long-running start request is still pending', async () => {
    const store = setupStore();
    let resolveStart: () => void;
    mockStatusRequest
      .mockResolvedValueOnce({
        status: 'idle',
        allowedRecoveryActions: [],
        transferProgress: {},
        progressItems: [],
        error: null,
      })
      .mockResolvedValue({
        status: 'failed',
        allowedRecoveryActions: ['retry'],
        transferProgress: {},
        progressItems: [],
        error: null,
      });
    store.setup();
    mockStartRequest.mockReturnValue(
      new Promise<void>((resolve) => {
        resolveStart = resolve;
      })
    );
    const startPromise = store.startPartialSync();

    expect(store.status).toBe('stopping-node');
    expect(mockStartRequest).toHaveBeenCalledTimes(1);

    registeredStatusHandler({
      status: 'preparing',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    expect(store.status).toBe('preparing');

    registeredStatusHandler({
      status: 'downloading',
      allowedRecoveryActions: [],
      transferProgress: {
        filesDownloaded: 2,
        filesTotal: 10,
      },
      progressItems: [],
      error: null,
    });

    expect(store.status).toBe('downloading');
    expect(store.filesDownloaded).toBe(2);
    expect(store.filesTotal).toBe(10);

    resolveStart!();
    registeredStatusHandler({
      status: 'failed',
      allowedRecoveryActions: ['retry'],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    await startPromise;

    expect(store.status).toBe('failed');
    expect(store.canRetry).toBe(true);
  });

  it('absorbs raw start request rejections once backend status reports the failure', async () => {
    const store = setupStore();
    mockStartRequest.mockRejectedValue({
      stage: 'verifying',
      code: 'PARTIAL_SYNC_STAGED_DB_INVALID',
    });
    mockStatusRequest.mockResolvedValue({
      status: 'failed',
      allowedRecoveryActions: ['retry'],
      transferProgress: {},
      progressItems: [],
      error: {
        stage: 'verifying',
        code: 'PARTIAL_SYNC_STAGED_DB_INVALID',
        message: 'Mithril partial sync failed.',
      },
    });

    await expect(store.startPartialSync()).resolves.toBeUndefined();

    expect(store.status).toBe('failed');
    expect(store.canRetry).toBe(true);
    expect(store.error).toEqual(
      expect.objectContaining({
        stage: 'verifying',
        code: 'PARTIAL_SYNC_STAGED_DB_INVALID',
      })
    );
    expect(logger.warn).toHaveBeenCalledWith(
      'MithrilPartialSyncStore: swallowed partial sync start rejection after backend status resync',
      expect.objectContaining({
        status: 'failed',
      })
    );
  });

  it('rethrows the original start failure while status is still pending', async () => {
    const store = setupStore();
    mockStartRequest.mockRejectedValue(
      new Error('Mithril partial sync is disabled by launcher configuration.')
    );
    mockStatusRequest.mockResolvedValue({
      status: 'stopping-node',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    await expect(store.startPartialSync()).rejects.toThrow(
      'Mithril partial sync is disabled by launcher configuration.'
    );

    expect(store.status).toBe('stopping-node');
  });

  it('accepts pushed backend updates and ignores them after teardown', async () => {
    const store = setupStore();
    store.setup();

    registeredStatusHandler({
      status: 'verifying',
      allowedRecoveryActions: ['retry'],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    expect(store.status).toBe('verifying');
    expect(store.canRetry).toBe(true);

    store.teardown();
    registeredStatusHandler({
      status: 'downloading',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    expect(store.status).toBe('verifying');
  });

  it('syncStatus requests current backend state directly', async () => {
    const store = setupStore();
    mockStatusRequest.mockResolvedValue({
      status: 'preparing',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    await store.syncStatus();

    expect(mockStatusRequest).toHaveBeenCalledTimes(1);
    expect(store.status).toBe('preparing');
  });

  it('does not issue a new status request when start settles after teardown', async () => {
    const store = setupStore();
    let resolveStart: () => void;

    mockStartRequest.mockReturnValue(
      new Promise<void>((resolve) => {
        resolveStart = resolve;
      })
    );

    const startPromise = store.startPartialSync();
    store.teardown();
    resolveStart!();

    await startPromise;

    expect(mockStatusRequest).not.toHaveBeenCalled();
    expect(store.status).toBe('stopping-node');
  });

  it('consumes the availability read model with a one-shot query during setup', async () => {
    const store = setupStore();
    mockStatusRequest.mockResolvedValue({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    mockAvailabilityRequest.mockResolvedValue({
      isEnabled: true,
      isSignificantlyBehind: true,
      behindByImmutables: 42,
    });

    store.setup();
    await mockAvailabilityRequest.mock.results[0].value;
    await Promise.resolve();

    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(1);
    expect(store.isPartialSyncEnabled).toBe(true);
    expect(store.isSignificantlyBehind).toBe(true);
    expect(store.behindByImmutables).toBe(42);
  });

  it('keeps partial sync hidden by default until the first availability response lands', () => {
    const store = setupStore();

    expect(store.isPartialSyncEnabled).toBe(false);
    expect(store.isSignificantlyBehind).toBe(false);
    expect(store.behindByImmutables).toBeUndefined();
  });

  it('populates certifiedEpoch from the availability payload and leaves it undefined when absent', () => {
    const store = setupStore();

    expect(store.certifiedEpoch).toBeUndefined();

    store._applyAvailability({
      isEnabled: true,
      isSignificantlyBehind: true,
      behindByImmutables: 42,
      certifiedEpoch: 320,
    });
    expect(store.certifiedEpoch).toBe(320);

    // A payload without the field (e.g. a degraded probe)
    // leaves certifiedEpoch undefined so the figure degrades to networkTip-only.
    store._applyAvailability({
      isEnabled: true,
      isSignificantlyBehind: false,
    });
    expect(store.certifiedEpoch).toBeUndefined();
  });

  it('refreshes availability on every interval tick while idle so first load self-corrects', async () => {
    const store = setupStore();
    mockStatusRequest.mockResolvedValue({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    // A BEHIND node (unstable read) keeps the fast 30s
    // cadence — the known-stable back-off only engages once availability is
    // settled-stable. Using an unstable read here keeps this test exercising the
    // idle-poll-still-fires contract (the `isWorking` guard stays removed) without
    // the cadence backing off mid-test.
    mockAvailabilityRequest.mockResolvedValue({
      isEnabled: true,
      isSignificantlyBehind: true,
    });

    store.setup();
    await mockAvailabilityRequest.mock.results[0].value;
    await Promise.resolve();

    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(1);

    // Idle: the interval still refreshes so the Diagnostics Mithril section
    // self-corrects on first load without a full reload. Awaiting the
    // previous probe plus a microtask flush lets `_refreshAvailability`'s finally
    // clear the re-entrancy guard before the next tick (the bounded request races
    // the call against a timeout, so the continuation resolves one microtask after
    // the request promise itself).
    jest.advanceTimersByTime(30_000);
    await mockAvailabilityRequest.mock.results[1].value;
    await Promise.resolve();
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(2);

    jest.advanceTimersByTime(30_000);
    await mockAvailabilityRequest.mock.results[2].value;
    await Promise.resolve();
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(3);
  });

  it('clears the re-entrancy guard via the request timeout when a refresh never settles', async () => {
    const store = setupStore();
    mockStatusRequest.mockResolvedValue({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    // The first availability request never settles — without the bounded request
    // it would pin `_isRefreshingAvailability` true forever and kill all future
    // refreshes. Subsequent requests resolve normally.
    mockAvailabilityRequest
      .mockReturnValueOnce(new Promise(() => {}))
      .mockResolvedValue({
        isEnabled: true,
        isSignificantlyBehind: true,
      });

    store.setup();

    // The setup refresh issued the never-settling request and pinned the guard.
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(1);
    expect(store._isRefreshingAvailability).toBe(true);

    // Advance past AVAILABILITY_REQUEST_TIMEOUT_MS (10_000) → the race rejects →
    // the catch logs → the finally clears the guard. Flush the rejection
    // microtasks (the never-settling request itself stays pending, so we cannot
    // await its result).
    jest.advanceTimersByTime(10_000);
    await Promise.resolve();
    await Promise.resolve();
    await Promise.resolve();

    expect(store._isRefreshingAvailability).toBe(false);
    expect(logger.warn).toHaveBeenCalledWith(
      'MithrilPartialSyncStore: failed to refresh availability',
      expect.objectContaining({ error: expect.any(Error) })
    );

    // The next interval tick can now issue a fresh request (guard no longer pinned).
    jest.advanceTimersByTime(30_000);
    await mockAvailabilityRequest.mock.results[1].value;
    await Promise.resolve();
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(2);
  });

  it('a single premature not-behind read does NOT slow the poll; only the SECOND consecutive stable read backs off', async () => {
    const store = setupStore();
    mockStatusRequest.mockResolvedValue({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    // Settled-stable reads (enabled && !behind). The FIRST such read may be a
    // premature probe before behind-ness settles, so it must NOT slow the poll.
    mockAvailabilityRequest.mockResolvedValue({
      isEnabled: true,
      isSignificantlyBehind: false,
    });

    store.setup();
    await mockAvailabilityRequest.mock.results[0].value;
    await Promise.resolve();
    // First stable read → counter 1, still the fast cadence.
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(1);
    expect(store._consecutiveStableReads).toBe(1);

    // The 30s tick still fires (proving the first stable read did NOT back off),
    // producing the SECOND consecutive stable read → counter 2 → back off to 5 min.
    jest.advanceTimersByTime(30_000);
    await mockAvailabilityRequest.mock.results[1].value;
    await Promise.resolve();
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(2);
    expect(store._consecutiveStableReads).toBe(2);

    // Now backed off: a further 30s does NOT tick.
    jest.advanceTimersByTime(30_000);
    await Promise.resolve();
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(2);

    // The slow interval fires only after the full back-off window (5 min total
    // from when it was armed at the 30s mark).
    jest.advanceTimersByTime(300_000 - 30_000);
    await mockAvailabilityRequest.mock.results[2].value;
    await Promise.resolve();
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(3);
  });

  it('a later fall-behind re-arms the fast 30s cadence immediately', async () => {
    const store = setupStore();
    mockStatusRequest.mockResolvedValue({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    mockAvailabilityRequest.mockResolvedValue({
      isEnabled: true,
      isSignificantlyBehind: false,
    });

    // Two consecutive stable reads → back off to the 5-min cadence.
    store.setup();
    await mockAvailabilityRequest.mock.results[0].value;
    await Promise.resolve();
    jest.advanceTimersByTime(30_000);
    await mockAvailabilityRequest.mock.results[1].value;
    await Promise.resolve();
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(2);
    expect(store._consecutiveStableReads).toBe(2);

    // The node falls behind: the next (slow) tick reads unstable.
    mockAvailabilityRequest.mockResolvedValue({
      isEnabled: true,
      isSignificantlyBehind: true,
    });
    jest.advanceTimersByTime(300_000);
    await mockAvailabilityRequest.mock.results[2].value;
    await Promise.resolve();
    // Unstable read → counter resets and the fast 30s cadence is re-armed.
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(3);
    expect(store._consecutiveStableReads).toBe(0);

    // Fast cadence restored: a 30s advance ticks again (no need to wait 5 min).
    jest.advanceTimersByTime(30_000);
    await mockAvailabilityRequest.mock.results[3].value;
    await Promise.resolve();
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(4);
  });

  it('skips the availability probe while partial-sync work is active or terminal-cancelled without touching the guard or stable-read counter', async () => {
    const store = setupStore();

    // The mid-run availability gate: every working status plus terminal
    // `cancelled` returns early — no channel request, no
    // `_isRefreshingAvailability` flip, no `_consecutiveStableReads` advance
    // (a probe here would spawn a concurrent mithril-client metadata child
    // mid-run).
    const gatedStatuses = [
      'downloading',
      'converting',
      'cancelling',
      'cancelled',
      'verifying',
    ] as const;

    for (const status of gatedStatuses) {
      store._updateStatus({
        status,
        allowedRecoveryActions: [],
        transferProgress: {},
        progressItems: [],
        error: null,
      });
      store._consecutiveStableReads = 1;

      const refresh = store._refreshAvailability();
      // The gate sits BEFORE `_isRefreshingAvailability = true`, so a skipped
      // tick never consumes the in-flight guard — checked synchronously so
      // even a transient flip would be visible.
      expect(store._isRefreshingAvailability).toBe(false);
      await refresh;

      expect(mockAvailabilityRequest).not.toHaveBeenCalled();
      expect(store._isRefreshingAvailability).toBe(false);
      expect(store._consecutiveStableReads).toBe(1);
    }
  });

  it('still probes availability on idle, failed, and completed ticks (the gate skips only active/cancelled)', async () => {
    const store = setupStore();
    // Unstable reads keep the back-off out of play so only the gate is under test.
    mockAvailabilityRequest.mockResolvedValue({
      isEnabled: true,
      isSignificantlyBehind: true,
    });

    let expectedRequests = 0;
    for (const status of ['idle', 'failed', 'completed'] as const) {
      store._updateStatus({
        status,
        allowedRecoveryActions: [],
        transferProgress: {},
        progressItems: [],
        error: null,
      });

      await store._refreshAvailability();

      expectedRequests += 1;
      expect(mockAvailabilityRequest).toHaveBeenCalledTimes(expectedRequests);
    }
  });

  it('a gated tick neither re-arms nor clears the availability interval — the poll resumes once status leaves the active set', async () => {
    const store = setupStore();
    mockStatusRequest.mockResolvedValue({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    // Unstable reads pin the fast 30s cadence so a back-off re-arm can never
    // mask the assertion that gated ticks leave the interval untouched.
    mockAvailabilityRequest.mockResolvedValue({
      isEnabled: true,
      isSignificantlyBehind: true,
    });

    store.setup();
    await mockAvailabilityRequest.mock.results[0].value;
    await Promise.resolve();
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(1);
    const armedInterval = store._availabilityRefreshInterval;
    expect(armedInterval).not.toBeNull();

    // Work starts: the interval keeps ticking but every tick is gated.
    store._updateStatus({
      status: 'downloading',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    jest.advanceTimersByTime(30_000 * 3);
    await Promise.resolve();

    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(1);
    // Same interval handle: the skipped ticks neither cleared nor re-armed it.
    expect(store._availabilityRefreshInterval).toBe(armedInterval);

    // Status leaves the active set (`failed` is not gated): the very next tick
    // probes again — the interval never stopped ticking through the gated window.
    store._updateStatus({
      status: 'failed',
      allowedRecoveryActions: ['retry'],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    jest.advanceTimersByTime(30_000);
    await mockAvailabilityRequest.mock.results[1].value;
    await Promise.resolve();
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(2);
    expect(store._availabilityRefreshInterval).toBe(armedInterval);
  });

  it('clears the availability refresh interval on teardown', async () => {
    const store = setupStore();
    mockStatusRequest.mockResolvedValue({
      status: 'downloading',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    store.setup();
    await mockAvailabilityRequest.mock.results[0].value;
    await Promise.resolve();

    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(1);

    store.teardown();
    jest.advanceTimersByTime(30_000 * 3);

    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(1);
  });

  it('defaults the proactive prompt dismissal to false and sets it on dismiss', () => {
    const store = setupStore();

    expect(store.proactivePromptDismissedThisSession).toBe(false);

    store.dismissProactivePrompt();

    expect(store.proactivePromptDismissedThisSession).toBe(true);
  });

  it('sets the re-pop guard when a Mithril attempt begins (startPartialSync)', async () => {
    const store = setupStore();
    mockStartRequest.mockResolvedValue(undefined);
    mockStatusRequest.mockResolvedValue({
      status: 'stopping-node',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    expect(store.mithrilAttemptStartedThisSession).toBe(false);

    await store.startPartialSync();

    expect(store.mithrilAttemptStartedThisSession).toBe(true);
  });

  it('keeps the proactive prompt dismissal across status updates (session scope)', () => {
    const store = setupStore();

    store.dismissProactivePrompt();
    expect(store.proactivePromptDismissedThisSession).toBe(true);

    store._updateStatus({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    // Distinct from isCompletedOverlayDismissed, which IS reset on a
    // non-completed status update; the proactive dismissal is session-scoped.
    expect(store.proactivePromptDismissedThisSession).toBe(true);
  });

  it('re-arms isWorking on terminal completed, cancelled, and failed states', () => {
    const store = setupStore();

    (['completed', 'cancelled', 'failed'] as const).forEach((status) => {
      store._updateStatus({
        status,
        allowedRecoveryActions: [],
        transferProgress: {},
        progressItems: [],
        error: null,
      });

      expect(store.isWorking).toBe(false);
      expect(store.isTerminal).toBe(true);
    });
  });

  describe('diagnostics → overlay handoff (live store contract)', () => {
    // Each test in this describe calls store.setup(), which internally calls
    // syncStatus(). Mock the status request to return idle so that the async
    // syncStatus() path does not reach logger.warn (electronLog is unavailable
    // in jsdom). The outer beforeEach already calls jest.resetAllMocks(), so
    // this nested beforeEach runs after the reset and restores the safe mock.
    beforeEach(() => {
      mockStatusRequest.mockResolvedValue({
        status: 'idle',
        allowedRecoveryActions: [],
        transferProgress: {},
        progressItems: [],
        error: null,
      });
    });

    it('fresh live store hides the overlay before any backend push', () => {
      const store = setupStore();
      store.setup();

      expect(store.shouldShowOverlay).toBe(false);
    });

    it('live downloading push flips shouldShowOverlay true and populates overlay-driving getters', () => {
      const store = setupStore();
      store.setup();

      jest.setSystemTime(100_000);
      registeredStatusHandler({
        status: 'downloading',
        allowedRecoveryActions: [],
        transferProgress: {
          filesDownloaded: 4,
          filesTotal: 9,
          elapsedSeconds: 12,
        },
        progressItems: [],
        error: null,
      });

      expect(store.shouldShowOverlay).toBe(true);
      expect(store.status).toBe('downloading');
      expect(store.filesDownloaded).toBe(4);
      expect(store.filesTotal).toBe(9);
      expect(store.startedAt).toBe(100_000 - 12_000);
    });

    it('live failed push exposes all three recovery flags and error for the overlay', () => {
      const store = setupStore();
      store.setup();

      registeredStatusHandler({
        status: 'failed',
        allowedRecoveryActions: [
          'retry',
          'restart-normal',
          'wipe-and-full-sync',
        ],
        transferProgress: {},
        progressItems: [],
        error: { message: 'Restore failed', stage: 'installing' },
      });

      expect(store.shouldShowOverlay).toBe(true);
      expect(store.status).toBe('failed');
      expect(store.canRetry).toBe(true);
      expect(store.canRestartNormally).toBe(true);
      expect(store.canWipeAndFullSync).toBe(true);
      expect(store.error).toEqual({
        message: 'Restore failed',
        stage: 'installing',
      });
    });

    it('live completed push keeps overlay shown; dismissCompletedOverlay flips it off and calls finalize once', async () => {
      const store = setupStore();
      mockFinalizeRequest.mockResolvedValue(undefined);
      store.setup();

      registeredStatusHandler({
        status: 'completed',
        allowedRecoveryActions: [],
        transferProgress: {},
        progressItems: [],
        error: null,
      });

      expect(store.shouldShowOverlay).toBe(true);

      await store.dismissCompletedOverlay();

      expect(store.shouldShowOverlay).toBe(false);
      expect(mockFinalizeRequest).toHaveBeenCalledTimes(1);
    });
  });

  it('anchors a renderer-side startedAt when entering a working status', () => {
    const store = setupStore();
    jest.setSystemTime(10_000);
    store._updateStatus({
      status: 'stopping-node',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    expect(store.startedAt).toBe(10_000);
  });

  it('anchors startedAt to the backend elapsed when present', () => {
    const store = setupStore();
    jest.setSystemTime(100_000);
    store._updateStatus({
      status: 'downloading',
      allowedRecoveryActions: [],
      transferProgress: { elapsedSeconds: 12 },
      progressItems: [],
      error: null,
    });
    expect(store.startedAt).toBe(100_000 - 12_000);
  });

  it('keeps startedAt stable across a run, frozen on terminal, released on idle', () => {
    const store = setupStore();
    jest.setSystemTime(5_000);
    store._updateStatus({
      status: 'stopping-node',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    jest.setSystemTime(9_000);
    store._updateStatus({
      status: 'downloading',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    expect(store.startedAt).toBe(5_000); // stable through the run

    store._updateStatus({
      status: 'completed',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    expect(store.startedAt).toBe(5_000); // frozen on terminal

    store._updateStatus({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    expect(store.startedAt).toBeNull(); // released on idle

    jest.setSystemTime(20_000);
    store._updateStatus({
      status: 'stopping-node',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    expect(store.startedAt).toBe(20_000); // re-anchors next run
  });
});
