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

  it('resolves decision waiters and stores the pending decision until reset', async () => {
    const moduleExports = loadModule();

    moduleExports.handleMithrilBootstrapRequests({ webContents: {} } as never);

    const waiter = moduleExports.waitForMithrilBootstrapDecision();
    const decisionHandler = mockChannels[0].onRequest.mock.calls[0][0];

    await decisionHandler({ decision: 'accept' });

    await expect(waiter).resolves.toBe('accept');
    expect(moduleExports.getPendingMithrilBootstrapDecision()).toBe('accept');
  });
});
