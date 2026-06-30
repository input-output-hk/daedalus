import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import MithrilPartialSyncStore from './MithrilPartialSyncStore';
import { noopAnalyticsTracker } from '../analytics';

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

  it('refreshes availability on every interval tick while idle so first load self-corrects', async () => {
    const store = setupStore();
    mockStatusRequest.mockResolvedValue({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    store.setup();
    await mockAvailabilityRequest.mock.results[0].value;
    await Promise.resolve();

    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(1);

    // Idle: the interval still refreshes so the Diagnostics Mithril section
    // self-corrects on first load without a full reload (ISSUE-1). Awaiting the
    // previous probe lets the re-entrancy guard clear before the next tick.
    jest.advanceTimersByTime(30_000);
    await mockAvailabilityRequest.mock.results[1].value;
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(2);

    jest.advanceTimersByTime(30_000);
    await mockAvailabilityRequest.mock.results[2].value;
    expect(mockAvailabilityRequest).toHaveBeenCalledTimes(3);
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
