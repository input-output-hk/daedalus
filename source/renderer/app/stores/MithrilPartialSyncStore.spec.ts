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
const mockStatusOnReceive = jest.fn();

jest.mock('../ipc/mithrilPartialSyncChannel', () => ({
  mithrilPartialSyncStatusChannel: {
    request: (...args) => mockStatusRequest(...args),
    onReceive: (...args) => mockStatusOnReceive(...args),
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
  });

  afterEach(() => {
    jest.clearAllTimers();
  });

  it('syncs cached backend status during setup without registering a long-lived listener', async () => {
    const store = setupStore();
    mockStatusRequest.mockResolvedValue({
      status: 'failed',
      allowedRecoveryActions: ['retry', 'restart-normal'],
      filesDownloaded: 10,
      filesTotal: 20,
      elapsedSeconds: 30,
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
    expect(mockStatusOnReceive).not.toHaveBeenCalled();
    expect(store.status).toBe('failed');
    expect(store.allowedRecoveryActions).toEqual([
      'retry',
      'restart-normal',
    ]);
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
      filesDownloaded: 5,
      filesTotal: 10,
      elapsedSeconds: 12,
      ancillaryBytesDownloaded: 100,
      ancillaryBytesTotal: 200,
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
      filesDownloaded: undefined,
      filesTotal: undefined,
      elapsedSeconds: undefined,
      ancillaryBytesDownloaded: undefined,
      ancillaryBytesTotal: undefined,
      progressItems: undefined,
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

  it('delegates recovery and lifecycle actions through payload-free IPC requests and refreshes status afterwards', async () => {
    const store = setupStore();
    mockStartRequest.mockResolvedValue(undefined);
    mockStatusRequest.mockResolvedValue({
      status: 'preparing',
      allowedRecoveryActions: [],
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
    expect(store.status).toBe('preparing');
  });

  it('starts polling immediately while the long-running start request is still pending', async () => {
    const store = setupStore();
    let resolveStart: () => void;
    mockStartRequest.mockReturnValue(
      new Promise<void>((resolve) => {
        resolveStart = resolve;
      })
    );
    mockStatusRequest
      .mockResolvedValueOnce({
        status: 'preparing',
        allowedRecoveryActions: [],
      })
      .mockResolvedValueOnce({
        status: 'downloading',
        allowedRecoveryActions: [],
        filesDownloaded: 2,
        filesTotal: 10,
      })
      .mockResolvedValueOnce({
        status: 'failed',
        allowedRecoveryActions: ['retry'],
      });

    const startPromise = store.startPartialSync();

    expect(store.status).toBe('stopping-node');
    expect(mockStartRequest).toHaveBeenCalledTimes(1);

    jest.advanceTimersByTime(1000);
    await Promise.resolve();
    await mockStatusRequest.mock.results[0].value;
    await Promise.resolve();

    expect(store.status).toBe('preparing');

    jest.advanceTimersByTime(1000);
    await Promise.resolve();
    await mockStatusRequest.mock.results[1].value;
    await Promise.resolve();

    expect(store.status).toBe('downloading');
    expect(store.filesDownloaded).toBe(2);
    expect(store.filesTotal).toBe(10);

    resolveStart!();
    await Promise.resolve();
    await mockStatusRequest.mock.results[2].value;
    await startPromise;

    expect(store.status).toBe('failed');
    expect(store.canRetry).toBe(true);
  });

  it('ignores an older setup status response once start seeds a working state', async () => {
    const store = setupStore();
    let resolveSetupStatus: (value: {
      status: 'idle';
      allowedRecoveryActions: [];
    }) => void;
    let resolveStart: () => void;

    mockStatusRequest
      .mockReturnValueOnce(
        new Promise((resolve) => {
          resolveSetupStatus = resolve;
        })
      )
      .mockResolvedValueOnce({
        status: 'preparing',
        allowedRecoveryActions: [],
      })
      .mockResolvedValueOnce({
        status: 'downloading',
        allowedRecoveryActions: [],
        filesDownloaded: 3,
        filesTotal: 9,
      })
      .mockResolvedValueOnce({
        status: 'downloading',
        allowedRecoveryActions: [],
        filesDownloaded: 3,
        filesTotal: 9,
      });
    mockStartRequest.mockReturnValue(
      new Promise<void>((resolve) => {
        resolveStart = resolve;
      })
    );

    store.setup();
    const startPromise = store.startPartialSync();

    expect(store.status).toBe('stopping-node');

    resolveSetupStatus!({
      status: 'idle',
      allowedRecoveryActions: [],
    });
    await Promise.resolve();
    await mockStatusRequest.mock.results[0].value;
    await Promise.resolve();
    await Promise.resolve();

    expect(store.status).toBe('stopping-node');

    expect(mockStatusRequest).toHaveBeenCalledTimes(2);
    await mockStatusRequest.mock.results[1].value;
    await Promise.resolve();

    expect(store.status).toBe('preparing');

    jest.advanceTimersByTime(1000);
    await Promise.resolve();
    await mockStatusRequest.mock.results[2].value;
    await Promise.resolve();

    expect(store.status).toBe('downloading');
    expect(store.filesDownloaded).toBe(3);
    expect(store.filesTotal).toBe(9);

    resolveStart!();
    await startPromise;
  });

  it('queues a follow-up sync after start without overlapping an older in-flight request', async () => {
    const store = setupStore();
    let resolveSetupStatus: (value: {
      status: 'idle';
      allowedRecoveryActions: [];
    }) => void;
    let resolveStart: () => void;

    mockStatusRequest
      .mockReturnValueOnce(
        new Promise((resolve) => {
          resolveSetupStatus = resolve;
        })
      )
      .mockResolvedValueOnce({
        status: 'preparing',
        allowedRecoveryActions: [],
      })
      .mockResolvedValueOnce({
        status: 'downloading',
        allowedRecoveryActions: [],
      })
      .mockResolvedValueOnce({
        status: 'downloading',
        allowedRecoveryActions: [],
      });
    mockStartRequest.mockReturnValue(
      new Promise<void>((resolve) => {
        resolveStart = resolve;
      })
    );

    store.setup();
    const startPromise = store.startPartialSync();

    expect(mockStatusRequest).toHaveBeenCalledTimes(1);

    resolveSetupStatus!({
      status: 'idle',
      allowedRecoveryActions: [],
    });
    await mockStatusRequest.mock.results[0].value;
    await Promise.resolve();
    await Promise.resolve();

    expect(mockStatusRequest).toHaveBeenCalledTimes(2);
    await mockStatusRequest.mock.results[1].value;
    await Promise.resolve();
    expect(store.status).toBe('preparing');

    jest.advanceTimersByTime(1000);
    await Promise.resolve();
    await mockStatusRequest.mock.results[2].value;
    await Promise.resolve();
    expect(mockStatusRequest).toHaveBeenCalledTimes(3);
    expect(store.status).toBe('downloading');

    resolveStart!();
    await startPromise;
  });

  it('polls for updated backend status and stops polling on teardown', async () => {
    const store = setupStore();
    mockStatusRequest
      .mockResolvedValueOnce({
        status: 'downloading',
        allowedRecoveryActions: ['retry'],
      })
      .mockResolvedValueOnce({
        status: 'verifying',
        allowedRecoveryActions: ['retry'],
      });

    store.setup();
    await mockStatusRequest.mock.results[0].value;
    await Promise.resolve();

    jest.advanceTimersByTime(1000);
    await mockStatusRequest.mock.results[1].value;
    await Promise.resolve();

    expect(mockStatusRequest).toHaveBeenCalledTimes(2);
    expect(store.status).toBe('verifying');
    expect(store.canRetry).toBe(true);

    store.teardown();
    jest.advanceTimersByTime(2000);

    expect(mockStatusRequest).toHaveBeenCalledTimes(2);
    expect(mockStatusOnReceive).not.toHaveBeenCalled();
  });

  it('resets polling cleanly when stores are recreated', async () => {
    mockStatusRequest
      .mockResolvedValueOnce({
        status: 'downloading',
        allowedRecoveryActions: [],
      })
      .mockResolvedValueOnce({
        status: 'downloading',
        allowedRecoveryActions: [],
      })
      .mockResolvedValueOnce({
        status: 'verifying',
        allowedRecoveryActions: [],
      });

    const firstStore = setupStore();
    firstStore.setup();
    await mockStatusRequest.mock.results[0].value;
    await Promise.resolve();
    firstStore.teardown();

    const secondStore = setupStore();
    secondStore.setup();
    await mockStatusRequest.mock.results[1].value;
    await Promise.resolve();

    jest.advanceTimersByTime(1000);
    await Promise.resolve();
    await mockStatusRequest.mock.results[2].value;

    expect(mockStatusOnReceive).not.toHaveBeenCalled();
    expect(mockStatusRequest).toHaveBeenCalledTimes(3);

    secondStore.teardown();
  });

  it('reuses an in-flight status request instead of overlapping on the shared IPC response channel', async () => {
    const store = setupStore();
    let resolveStatus: (value: {
      status: 'preparing';
      allowedRecoveryActions: [];
    }) => void;
    mockStatusRequest.mockReturnValue(
      new Promise((resolve) => {
        resolveStatus = resolve;
      })
    );

    const firstSync = store.syncStatus();
    const secondSync = store.syncStatus();

    expect(mockStatusRequest).toHaveBeenCalledTimes(1);

    resolveStatus!({
      status: 'preparing',
      allowedRecoveryActions: [],
    });

    await Promise.all([firstSync, secondSync]);

    expect(store.status).toBe('preparing');
  });

  it('ignores late in-flight status completions after teardown', async () => {
    const store = setupStore();
    let resolveStatus: (value: {
      status: 'downloading';
      allowedRecoveryActions: ['retry'];
    }) => void;

    mockStatusRequest.mockReturnValue(
      new Promise((resolve) => {
        resolveStatus = resolve;
      })
    );

    store.setup();
    store.teardown();

    resolveStatus!({
      status: 'downloading',
      allowedRecoveryActions: ['retry'],
    });

    await mockStatusRequest.mock.results[0].value;
    await Promise.resolve();
    jest.advanceTimersByTime(2000);

    expect(store.status).toBe('idle');
    expect(store.canRetry).toBe(false);
    expect(mockStatusRequest).toHaveBeenCalledTimes(1);
  });

  it('does not run a queued follow-up sync after teardown', async () => {
    const store = setupStore();
    let resolveSetupStatus: (value: {
      status: 'idle';
      allowedRecoveryActions: [];
    }) => void;

    mockStatusRequest.mockReturnValue(
      new Promise((resolve) => {
        resolveSetupStatus = resolve;
      })
    );
    mockStartRequest.mockImplementation(async () => {
      store.teardown();
    });

    const setupPromise = store.syncStatus();
    const startPromise = store.startPartialSync();

    expect(mockStatusRequest).toHaveBeenCalledTimes(1);

    resolveSetupStatus!({
      status: 'idle',
      allowedRecoveryActions: [],
    });

    await Promise.all([setupPromise, startPromise]);
    await Promise.resolve();

    expect(mockStatusRequest).toHaveBeenCalledTimes(1);
    expect(store.status).toBe('stopping-node');
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
