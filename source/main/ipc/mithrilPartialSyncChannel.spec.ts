import type {} from './mithrilPartialSyncChannel';

const chainStorageCoordinatorMock = {
  startPartialSync: jest.fn(),
  cancelPartialSync: jest.fn(),
  restartNormalFromPartialSync: jest.fn(),
  wipeAndFullSyncFromPartialSync: jest.fn(),
  setPartialSyncHandlers: jest.fn(),
  setPartialSyncNodeStopHandler: jest.fn(),
};

const mithrilPartialSyncServiceMock = {
  status: {
    status: 'idle',
    allowedRecoveryActions: [],
    error: null,
  },
  onStatus: jest.fn(),
  assertStartAllowed: jest.fn(),
  start: jest.fn(),
  cancel: jest.fn(),
  restartNormal: jest.fn(),
  wipeAndFullSync: jest.fn(),
  finalizeWipeAndFullSync: jest.fn(),
};

const getMithrilBootstrapNodeStateMock = jest.fn().mockReturnValue('stopped');

const mockChannels: Array<{
  onRequest: jest.Mock;
  send: jest.Mock;
}> = [];

jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn().mockImplementation(() => {
    const channel = {
      onRequest: jest.fn(),
      send: jest.fn().mockResolvedValue(undefined),
    };
    mockChannels.push(channel);
    return channel;
  }),
}));

jest.mock('../utils/chainStorageCoordinator', () => ({
  chainStorageCoordinator: chainStorageCoordinatorMock,
}));

jest.mock('../mithril/MithrilPartialSyncService', () => ({
  MithrilPartialSyncService: jest
    .fn()
    .mockImplementation(() => mithrilPartialSyncServiceMock),
}));

jest.mock('./mithrilBootstrapChannel', () => ({
  getMithrilBootstrapNodeState: getMithrilBootstrapNodeStateMock,
}));

const loadModule = () => {
  let moduleExports;

  jest.isolateModules(() => {
    moduleExports = require('./mithrilPartialSyncChannel');
  });

  return moduleExports as typeof import('./mithrilPartialSyncChannel');
};

const flushPromises = () => new Promise((resolve) => setTimeout(resolve, 0));

describe('mithrilPartialSyncChannel', () => {
  beforeEach(() => {
    jest.resetModules();
    jest.clearAllMocks();
    mockChannels.length = 0;
    mithrilPartialSyncServiceMock.status = {
      status: 'idle',
      allowedRecoveryActions: [],
      error: null,
    };
  });

  it('returns the cached idle status with allowed recovery actions for status requests', async () => {
    const moduleExports = loadModule();

    moduleExports.handleMithrilPartialSyncRequests({ webContents: {} } as never);

    const statusHandler = mockChannels[1].onRequest.mock.calls[0][0];

    await expect(statusHandler()).resolves.toEqual({
      status: 'idle',
      allowedRecoveryActions: [],
      error: null,
    });
    expect(moduleExports.getMithrilPartialSyncStatus()).toEqual({
      status: 'idle',
      allowedRecoveryActions: [],
      error: null,
    });
  });

  it('fires status listeners even when renderer delivery rejects', async () => {
    const moduleExports = loadModule();
    const window = { webContents: {} };

    moduleExports.handleMithrilPartialSyncRequests(window as never);
    mockChannels[1].send.mockRejectedValueOnce(new Error('webContents destroyed'));

    const handler = jest.fn();
    moduleExports.onMithrilPartialSyncStatus(handler);

    await moduleExports.emitMithrilPartialSyncStatus({
      status: 'downloading',
      allowedRecoveryActions: [],
      error: null,
    });
    await flushPromises();

    expect(handler).toHaveBeenCalledWith({
      status: 'downloading',
      allowedRecoveryActions: [],
      error: null,
    });
  });

  it('does not block status emission when renderer delivery never resolves', async () => {
    const moduleExports = loadModule();
    const window = { webContents: {} };

    moduleExports.handleMithrilPartialSyncRequests(window as never);
    mockChannels[1].send.mockReturnValueOnce(new Promise(() => {}));

    const handler = jest.fn();
    moduleExports.onMithrilPartialSyncStatus(handler);

    await expect(
      moduleExports.emitMithrilPartialSyncStatus({
        status: 'starting-node',
        allowedRecoveryActions: ['wipe-and-full-sync'],
        error: null,
      })
    ).resolves.toBeUndefined();

    expect(handler).toHaveBeenCalledWith({
      status: 'starting-node',
      allowedRecoveryActions: ['wipe-and-full-sync'],
      error: null,
    });
    expect(moduleExports.getMithrilPartialSyncStatus()).toEqual({
      status: 'starting-node',
      allowedRecoveryActions: ['wipe-and-full-sync'],
      error: null,
    });
  });

  it('rebinds status delivery to a replacement window without duplicating request handlers', async () => {
    const moduleExports = loadModule();
    const firstWindow = { webContents: { id: 1 } };
    const secondWindow = { webContents: { id: 2 } };

    moduleExports.handleMithrilPartialSyncRequests(firstWindow as never);

    const requestHandlerCountAfterFirst = mockChannels.filter(
      (channel) => channel.onRequest.mock.calls.length > 0
    ).length;

    moduleExports.handleMithrilPartialSyncRequests(secondWindow as never);

    const requestHandlerCountAfterSecond = mockChannels.filter(
      (channel) => channel.onRequest.mock.calls.length > 0
    ).length;

    expect(requestHandlerCountAfterSecond).toBe(requestHandlerCountAfterFirst);

    await moduleExports.emitMithrilPartialSyncStatus({
      status: 'preparing',
      allowedRecoveryActions: [],
      error: null,
    });

    const lastSendCall =
      mockChannels[1].send.mock.calls[mockChannels[1].send.mock.calls.length - 1];
    expect(lastSendCall[1]).toBe(secondWindow.webContents);
  });

  it('delegates all partial-sync actions through the coordinator', async () => {
    const moduleExports = loadModule();

    moduleExports.handleMithrilPartialSyncRequests({ webContents: {} } as never);

    await expect(mockChannels[0].onRequest.mock.calls[0][0]()).resolves.toBe(
      undefined
    );
    await expect(mockChannels[2].onRequest.mock.calls[0][0]()).resolves.toBe(
      undefined
    );
    await expect(mockChannels[3].onRequest.mock.calls[0][0]()).resolves.toBe(
      undefined
    );
    await expect(mockChannels[4].onRequest.mock.calls[0][0]()).resolves.toBe(
      undefined
    );

    expect(chainStorageCoordinatorMock.startPartialSync).toHaveBeenCalledWith({
      nodeState: 'stopped',
    });
    expect(chainStorageCoordinatorMock.cancelPartialSync).toHaveBeenCalledTimes(
      1
    );
    expect(
      chainStorageCoordinatorMock.restartNormalFromPartialSync
    ).toHaveBeenCalledWith({
      nodeState: 'stopped',
    });
    expect(
      chainStorageCoordinatorMock.wipeAndFullSyncFromPartialSync
    ).toHaveBeenCalledWith({
      nodeState: 'stopped',
    });
  });

  it('registers the partial sync service with the coordinator only once', () => {
    const moduleExports = loadModule();

    moduleExports.handleMithrilPartialSyncRequests({ webContents: {} } as never);
    moduleExports.handleMithrilPartialSyncRequests({ webContents: {} } as never);

    expect(chainStorageCoordinatorMock.setPartialSyncHandlers).toHaveBeenCalledTimes(
      1
    );
    expect(mithrilPartialSyncServiceMock.onStatus).toHaveBeenCalledTimes(1);
  });

  it('replaces cached status objects instead of merging stale recovery actions', () => {
    const moduleExports = loadModule();

    moduleExports.setMithrilPartialSyncStatus({
      status: 'failed',
      allowedRecoveryActions: ['retry', 'restart-normal'],
      error: {
        message: 'failed',
      },
    });

    const nextStatus = moduleExports.setMithrilPartialSyncStatus({
      status: 'completed',
      allowedRecoveryActions: [],
      error: null,
    });

    expect(nextStatus).toEqual({
      status: 'completed',
      allowedRecoveryActions: [],
      error: null,
    });
  });

  it('uses the registered active-state provider instead of cached status', () => {
    const moduleExports = loadModule();
    const provider = jest.fn(() => true);

    moduleExports.setMithrilPartialSyncStatus({
      status: 'idle',
      allowedRecoveryActions: [],
      error: null,
    });
    moduleExports.setMithrilPartialSyncActiveProvider(provider);

    expect(moduleExports.isMithrilPartialSyncActive()).toBe(true);
    expect(provider).toHaveBeenCalledTimes(1);
  });
});
