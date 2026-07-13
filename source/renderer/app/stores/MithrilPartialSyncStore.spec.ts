import { autorun, configure } from 'mobx';
import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import MithrilPartialSyncStore from './MithrilPartialSyncStore';
import { noopAnalyticsTracker } from '../analytics';
import { logger } from '../utils/logging';

// The real logger writes to the electronLog global, absent under jsdom; stub it so the store's catch-branch logger.warn calls stay assertable and don't crash.
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
  const api: Api = {
    ada: jest.fn(),
  } as unknown as Api;
  const actions: ActionsMap = jest.fn() as unknown as ActionsMap;

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
    expect(store.ancillaryBytesDownloaded).toBeUndefined();
    expect(store.ancillaryBytesTotal).toBeUndefined();
    expect(store.progressItems).toEqual([]);
    expect(store.error).toBeNull();
    expect(store.logPath).toBeUndefined();
    expect(store.isWorking).toBe(false);
    expect(store.isTerminal).toBe(true);
  });

  it('shows the overlay only for backend-confirmed display states and can dismiss completed', async () => {
    const store = setupStore();
    mockFinalizeRequest.mockResolvedValue(undefined);
    // dismissCompletedOverlay resyncs via syncStatus() in its finally, so back the status channel with a post-finalize idle snapshot.
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
    // Keep the resync status on 'completed'; an idle resync would clear the flag via _updateStatus and mask the flip under test.
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

    // Enable MobX strict mode (the renderer's enforceActions: 'observed'): the post-await flag
    // flip runs outside the enclosing @action, so it must be wrapped in runInAction. The MobX 5
    // default enforces nothing and hides this regression; restored in finally so it doesn't leak.
    configure({ enforceActions: 'observed' });
    const observedOverlay: boolean[] = [];
    const dispose = autorun(() => {
      // Read the observed chain so the strict-mode guard is armed at flip time.
      observedOverlay.push(store.shouldShowOverlay);
    });

    try {
      const dismissal = store.dismissCompletedOverlay();
      await Promise.resolve();
      expect(store.isCompletedOverlayDismissed).toBe(false);
      expect(mockFinalizeRequest).toHaveBeenCalledTimes(1);

      resolveFinalize();
      await dismissal;

      expect(store.isCompletedOverlayDismissed).toBe(true);
      expect(mockStatusRequest).toHaveBeenCalled();
      expect(logger.warn).not.toHaveBeenCalled();
    } finally {
      dispose();
      configure({ enforceActions: 'never' });
    }
  });

  it('dismissCompletedOverlay swallows a finalize rejection: keeps the flag false, resyncs, and never throws (overlay hides via idle resync)', async () => {
    const store = setupStore();
    mockFinalizeRequest.mockRejectedValue(new Error('finalize failed'));
    // The backend runs _resetToIdleStatus() before the failing fs.remove, so a finalize failure still resyncs to idle and hides the overlay via status, not the dismiss flag (which stays false).
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
    expect(mockStatusRequest).toHaveBeenCalledTimes(4);
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

    await expect(store.cancelPartialSync()).resolves.toBeUndefined();

    expect(mockCancelRequest).toHaveBeenCalledTimes(1);
    expect(mockStatusRequest).toHaveBeenCalledTimes(1);
    expect(store.status).toBe('failed');
    expect(logger.warn).toHaveBeenCalledWith(
      'MithrilPartialSyncStore: cancel partial sync rejected',
      expect.objectContaining({ error: expect.any(Error) })
    );
  });

  it('resyncs and never throws when the restart-normal request rejects', async () => {
    const store = setupStore();
    mockRestartNormalRequest.mockRejectedValue(new Error('backend refused'));
    mockStatusRequest.mockResolvedValue({
      status: 'failed',
      allowedRecoveryActions: ['retry'],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    await expect(store.restartNormally()).resolves.toBeUndefined();

    expect(mockStatusRequest).toHaveBeenCalledTimes(1);
    expect(store.status).toBe('failed');
    expect(logger.warn).toHaveBeenCalledWith(
      'MithrilPartialSyncStore: restart-normal request rejected',
      expect.objectContaining({ error: expect.any(Error) })
    );
  });

  it('resyncs and never throws when the wipe-and-full-sync request rejects', async () => {
    const store = setupStore();
    mockWipeAndFullSyncRequest.mockRejectedValue(new Error('backend refused'));
    mockStatusRequest.mockResolvedValue({
      status: 'failed',
      allowedRecoveryActions: ['retry'],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    await expect(store.wipeAndFullSync()).resolves.toBeUndefined();

    expect(mockStatusRequest).toHaveBeenCalledTimes(1);
    expect(store.status).toBe('failed');
    expect(logger.warn).toHaveBeenCalledWith(
      'MithrilPartialSyncStore: wipe-and-full-sync request rejected',
      expect.objectContaining({ error: expect.any(Error) })
    );
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

  it('wraps a non-Error start rejection that carries a message string', async () => {
    const store = setupStore();
    mockStartRequest.mockRejectedValue({ message: 'start rejected over IPC' });
    mockStatusRequest.mockResolvedValue({
      status: 'stopping-node',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    await expect(store.startPartialSync()).rejects.toThrow(
      'start rejected over IPC'
    );
  });

  it('throws an empty-message error for unusable rejections so the UI applies its fallback', async () => {
    const store = setupStore();
    mockStartRequest.mockRejectedValue('not-an-error');
    mockStatusRequest.mockResolvedValue({
      status: 'stopping-node',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    await expect(store.startPartialSync()).rejects.toMatchObject({
      message: '',
    });
  });

  it('rethrows a start rejection that resyncs to idle and re-arms the proactive prompt guard', async () => {
    const store = setupStore();
    mockStartRequest.mockRejectedValue(new Error('preflight rejected'));
    mockStatusRequest.mockResolvedValue({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    await expect(store.startPartialSync()).rejects.toThrow(
      'preflight rejected'
    );

    expect(store.status).toBe('idle');
    expect(store.mithrilAttemptStartedThisSession).toBe(false);
  });

  it('rethrows a start rejection that resyncs to a working status and keeps the attempt guard set', async () => {
    const store = setupStore();
    mockStartRequest.mockRejectedValue(new Error('channel dropped'));
    mockStatusRequest.mockResolvedValue({
      status: 'preparing',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    await expect(store.startPartialSync()).rejects.toThrow('channel dropped');

    expect(store.mithrilAttemptStartedThisSession).toBe(true);
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
  });

  it('keeps partial sync hidden by default until the first availability response lands', () => {
    const store = setupStore();

    expect(store.isPartialSyncEnabled).toBe(false);
    expect(store.isSignificantlyBehind).toBe(false);
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

    store._applyAvailability({
      isEnabled: true,
      isSignificantlyBehind: false,
    });
    expect(store.certifiedEpoch).toBeUndefined();
  });

  it('mirrors the at-or-past-snapshot flag from the availability payload and clears it when absent', () => {
    const store = setupStore();

    expect(store.isAtOrPastSnapshot).toBe(false);

    store._applyAvailability({
      isEnabled: true,
      isSignificantlyBehind: false,
      isAtOrPastSnapshot: true,
    });
    expect(store.isAtOrPastSnapshot).toBe(true);

    store._applyAvailability({
      isEnabled: true,
      isSignificantlyBehind: false,
    });
    expect(store.isAtOrPastSnapshot).toBe(false);
  });

  it('flags a failed behind-ness probe from the availability payload and clears it when absent', () => {
    const store = setupStore();

    expect(store.isProbeFailed).toBe(false);

    store._applyAvailability({
      isEnabled: true,
      isSignificantlyBehind: false,
      isProbeFailed: true,
    });
    expect(store.isProbeFailed).toBe(true);

    store._applyAvailability({
      isEnabled: true,
      isSignificantlyBehind: false,
    });
    expect(store.isProbeFailed).toBe(false);
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
    // Use an unstable (behind) read so the known-stable back-off never engages and the fast 30s cadence stays fixed for the idle-poll assertion.
    mockAvailabilityRequest.mockResolvedValue({
      isEnabled: true,
      isSignificantlyBehind: true,
    });

    store.setup();
    await mockAvailabilityRequest.mock.results[0].value;
    await Promise.resolve();

    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(1);

    // Await the prior probe plus a microtask flush so _refreshAvailability's finally clears the re-entrancy guard before the next tick (the bounded request resolves one microtask after its own promise).
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
    mockAvailabilityRequest
      .mockReturnValueOnce(new Promise(() => {}))
      .mockResolvedValue({
        isEnabled: true,
        isSignificantlyBehind: true,
      });

    store.setup();

    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(1);
    expect(store._isRefreshingAvailability).toBe(true);

    // Advance past AVAILABILITY_REQUEST_TIMEOUT_MS so the bounded race rejects and the finally clears the guard; flush the rejection microtasks since the never-settling request itself can't be awaited.
    jest.advanceTimersByTime(10_000);
    await Promise.resolve();
    await Promise.resolve();
    await Promise.resolve();

    expect(store._isRefreshingAvailability).toBe(false);
    expect(logger.warn).toHaveBeenCalledWith(
      'MithrilPartialSyncStore: failed to refresh availability',
      expect.objectContaining({ error: expect.any(Error) })
    );

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
    mockAvailabilityRequest.mockResolvedValue({
      isEnabled: true,
      isSignificantlyBehind: false,
    });

    store.setup();
    await mockAvailabilityRequest.mock.results[0].value;
    await Promise.resolve();
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(1);
    expect(store._consecutiveStableReads).toBe(1);

    jest.advanceTimersByTime(30_000);
    await mockAvailabilityRequest.mock.results[1].value;
    await Promise.resolve();
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(2);
    expect(store._consecutiveStableReads).toBe(2);

    jest.advanceTimersByTime(30_000);
    await Promise.resolve();
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(2);

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

    store.setup();
    await mockAvailabilityRequest.mock.results[0].value;
    await Promise.resolve();
    jest.advanceTimersByTime(30_000);
    await mockAvailabilityRequest.mock.results[1].value;
    await Promise.resolve();
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(2);
    expect(store._consecutiveStableReads).toBe(2);

    mockAvailabilityRequest.mockResolvedValue({
      isEnabled: true,
      isSignificantlyBehind: true,
    });
    jest.advanceTimersByTime(300_000);
    await mockAvailabilityRequest.mock.results[2].value;
    await Promise.resolve();
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(3);
    expect(store._consecutiveStableReads).toBe(0);

    jest.advanceTimersByTime(30_000);
    await mockAvailabilityRequest.mock.results[3].value;
    await Promise.resolve();
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(4);
  });

  it('skips the availability probe while partial-sync work is active or terminal-cancelled without touching the guard or stable-read counter', async () => {
    const store = setupStore();

    // The gate exists because a probe mid-run would spawn a concurrent mithril-client metadata child.
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
    expect(store._availabilityRefreshInterval).toBe(armedInterval);

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
    // setup() calls syncStatus(); the outer beforeEach's resetAllMocks() wipes the status mock, so re-mock it here (after the reset) to return idle and keep syncStatus() off the jsdom-unavailable logger.warn path.
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
