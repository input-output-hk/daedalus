import type {} from './mithrilBootstrapChannel';

const mockChannels: Array<{
  onRequest: jest.Mock;
  send: jest.Mock;
}> = [];

const mithrilBootstrapServiceMock = {
  status: {
    status: 'idle',
    snapshot: null,
    error: null,
  },
  onStatus: jest.fn(),
};

const chainStorageCoordinatorMock = {
  syncMithrilWorkDir: jest.fn(),
  listSnapshots: jest.fn(),
  startBootstrap: jest.fn(),
  cancelBootstrap: jest.fn(),
};

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
  getMithrilBootstrapService: jest.fn(() => mithrilBootstrapServiceMock),
}));

const loadModule = () => {
  let moduleExports;

  jest.isolateModules(() => {
    moduleExports = require('./mithrilBootstrapChannel');
  });

  return moduleExports as typeof import('./mithrilBootstrapChannel');
};

const flushPromises = () => new Promise((resolve) => setTimeout(resolve, 0));

describe('mithrilBootstrapChannel', () => {
  beforeEach(() => {
    jest.resetModules();
    jest.clearAllMocks();
    mockChannels.length = 0;
    mithrilBootstrapServiceMock.status = {
      status: 'idle',
      snapshot: null,
      error: null,
    };
    chainStorageCoordinatorMock.syncMithrilWorkDir.mockResolvedValue(
      '/tmp/state/chain'
    );
    chainStorageCoordinatorMock.listSnapshots.mockResolvedValue([]);
    chainStorageCoordinatorMock.startBootstrap.mockResolvedValue(undefined);
    chainStorageCoordinatorMock.cancelBootstrap.mockResolvedValue(undefined);
  });

  it('rejects outstanding decision waiters, clears pending state, and emits idle on reset', async () => {
    const moduleExports = loadModule();
    const window = { webContents: {} };

    moduleExports.handleMithrilBootstrapRequests(window as never);

    const waiter = moduleExports.waitForMithrilBootstrapDecision();
    const waiterResult = waiter
      .then(() => ({ error: null }))
      .catch((error) => ({ error }));

    moduleExports.resetMithrilDecisionState();
    await flushPromises();

    const { error } = await waiterResult;
    expect(error).toBeInstanceOf(moduleExports.MithrilDecisionCancelledError);
    expect(moduleExports.getPendingMithrilBootstrapDecision()).toBeNull();
    expect(moduleExports.getMithrilBootstrapStatus()).toEqual(
      expect.objectContaining({ status: 'idle' })
    );
    expect(mockChannels[2].send).toHaveBeenCalledWith(
      expect.objectContaining({ status: 'idle' }),
      window.webContents
    );
  });

  it('can reset outstanding decision waiters without broadcasting idle', async () => {
    const moduleExports = loadModule();
    const window = { webContents: {} };

    moduleExports.handleMithrilBootstrapRequests(window as never);

    const waiter = moduleExports.waitForMithrilBootstrapDecision();
    const waiterResult = waiter
      .then(() => ({ error: null }))
      .catch((error) => ({ error }));

    moduleExports.resetMithrilDecisionState({
      suppressStatusBroadcast: true,
    });
    await flushPromises();

    const { error } = await waiterResult;
    expect(error).toBeInstanceOf(moduleExports.MithrilDecisionCancelledError);
    expect(moduleExports.getPendingMithrilBootstrapDecision()).toBeNull();
    expect(moduleExports.getMithrilBootstrapStatus()).toEqual(
      expect.objectContaining({ status: 'idle' })
    );
    expect(mockChannels[2].send).not.toHaveBeenCalled();
  });

  it('resolves decision waiters and stores the pending decision until reset', async () => {
    const moduleExports = loadModule();

    moduleExports.handleMithrilBootstrapRequests({ webContents: {} } as never);

    const waiter = moduleExports.waitForMithrilBootstrapDecision();
    const decisionHandler = mockChannels[0].onRequest.mock.calls[0][0];

    await decisionHandler({ decision: 'accept' });

    await expect(waiter).resolves.toBe('accept');
    expect(moduleExports.getPendingMithrilBootstrapDecision()).toBe('accept');
  });

  it('fires status listeners even when sendStatusUpdate rejects', async () => {
    const moduleExports = loadModule();
    const window = { webContents: {} };
    moduleExports.handleMithrilBootstrapRequests(window as never);

    // Make sendStatusUpdate reject
    mockChannels[2].send.mockRejectedValueOnce(
      new Error('webContents destroyed')
    );

    const handler = jest.fn();
    moduleExports.onMithrilBootstrapStatus(handler);

    // Get the onStatus callback and invoke it with a status
    const onStatusCallback =
      mithrilBootstrapServiceMock.onStatus.mock.calls[0][0];
    await onStatusCallback({
      status: 'downloading',
      snapshot: null,
      error: null,
    });
    await flushPromises();

    expect(handler).toHaveBeenCalledWith(
      expect.objectContaining({ status: 'downloading' })
    );
  });

  it('rebinds status delivery to a replacement window without duplicating request handlers', async () => {
    const moduleExports = loadModule();
    const firstWindow = { webContents: { id: 1 } };
    const secondWindow = { webContents: { id: 2 } };

    moduleExports.handleMithrilBootstrapRequests(firstWindow as never);

    const requestHandlerCountAfterFirst = mockChannels.filter(
      (ch) => ch.onRequest.mock.calls.length > 0
    ).length;

    moduleExports.handleMithrilBootstrapRequests(secondWindow as never);

    // IPC request handlers should NOT be registered again
    const requestHandlerCountAfterSecond = mockChannels.filter(
      (ch) => ch.onRequest.mock.calls.length > 0
    ).length;
    expect(requestHandlerCountAfterSecond).toBe(requestHandlerCountAfterFirst);

    // Status updates should now target the second window
    const onStatusCallback =
      mithrilBootstrapServiceMock.onStatus.mock.calls[0][0];
    await onStatusCallback({
      status: 'downloading',
      snapshot: null,
      error: null,
    });
    await flushPromises();

    // The status channel send should have targeted the second window's webContents
    const statusChannel = mockChannels[2];
    const lastSendCall =
      statusChannel.send.mock.calls[statusChannel.send.mock.calls.length - 1];
    expect(lastSendCall[1]).toBe(secondWindow.webContents);
  });
});
