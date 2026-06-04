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

  it('shows the overlay only for backend-confirmed display states and can dismiss completed', () => {
    const store = setupStore();

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

    store.dismissCompletedOverlay();

    expect(store.shouldShowOverlay).toBe(false);

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
    expect(mockStatusRequest).toHaveBeenCalledTimes(1);
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
});
