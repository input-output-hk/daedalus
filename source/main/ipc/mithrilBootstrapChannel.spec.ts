import type {} from './mithrilBootstrapChannel';

const mockChannels: Array<{
  onRequest: jest.Mock;
  send: jest.Mock;
}> = [];

const mithrilControllerMock = {
  setBootstrapStatusSender: jest.fn(),
  initialize: jest.fn(),
  getBootstrapStatus: jest.fn(() => ({
    status: 'idle',
    snapshot: null,
    error: null,
  })),
  getNodeState: jest.fn(() => 'stopped'),
  setNodeStateProvider: jest.fn(),
  listSnapshots: jest.fn(),
  submitBootstrapDecision: jest.fn(),
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

jest.mock('../mithril/MithrilController', () => ({
  getMithrilController: () => mithrilControllerMock,
}));

const loadModule = () => {
  let moduleExports;

  jest.isolateModules(() => {
    moduleExports = require('./mithrilBootstrapChannel');
  });

  return moduleExports as typeof import('./mithrilBootstrapChannel');
};

describe('mithrilBootstrapChannel', () => {
  beforeEach(() => {
    jest.resetModules();
    jest.clearAllMocks();
    mockChannels.length = 0;
    mithrilControllerMock.getBootstrapStatus.mockReturnValue({
      status: 'idle',
      snapshot: null,
      error: null,
    });
    mithrilControllerMock.listSnapshots.mockResolvedValue([]);
    mithrilControllerMock.submitBootstrapDecision.mockResolvedValue(undefined);
    mithrilControllerMock.startBootstrap.mockResolvedValue(undefined);
    mithrilControllerMock.cancelBootstrap.mockResolvedValue(undefined);
  });

  it('binds status delivery to the latest window without duplicating request handlers', async () => {
    const moduleExports = loadModule();
    const firstWindow = { webContents: { id: 1 } };
    const secondWindow = { webContents: { id: 2 } };

    moduleExports.handleMithrilBootstrapRequests(firstWindow as never);
    const requestHandlerCountAfterFirst = mockChannels.filter(
      (channel) => channel.onRequest.mock.calls.length > 0
    ).length;

    moduleExports.handleMithrilBootstrapRequests(secondWindow as never);
    const requestHandlerCountAfterSecond = mockChannels.filter(
      (channel) => channel.onRequest.mock.calls.length > 0
    ).length;

    expect(requestHandlerCountAfterSecond).toBe(requestHandlerCountAfterFirst);
    expect(mithrilControllerMock.initialize).toHaveBeenCalledTimes(2);

    const sender =
      mithrilControllerMock.setBootstrapStatusSender.mock.calls[1][0];
    await sender({ status: 'downloading', snapshot: null, error: null });

    expect(mockChannels[2].send).toHaveBeenCalledWith(
      { status: 'downloading', snapshot: null, error: null },
      secondWindow.webContents
    );
  });

  it('registers thin request handlers that delegate to the controller', async () => {
    const moduleExports = loadModule();

    moduleExports.handleMithrilBootstrapRequests({ webContents: {} } as never);

    await expect(mockChannels[2].onRequest.mock.calls[0][0]()).resolves.toEqual(
      {
        status: 'idle',
        snapshot: null,
        error: null,
      }
    );
    await expect(mockChannels[4].onRequest.mock.calls[0][0]()).resolves.toEqual(
      []
    );
    await expect(
      mockChannels[0].onRequest.mock.calls[0][0]({ decision: 'accept' })
    ).resolves.toBeUndefined();
    await expect(
      mockChannels[1].onRequest.mock.calls[0][0]({
        digest: 'digest',
        wipeChain: true,
      })
    ).resolves.toBeUndefined();
    await expect(mockChannels[3].onRequest.mock.calls[0][0]()).resolves.toBe(
      undefined
    );

    expect(mithrilControllerMock.submitBootstrapDecision).toHaveBeenCalledWith(
      'accept'
    );
    expect(mithrilControllerMock.startBootstrap).toHaveBeenCalledWith({
      digest: 'digest',
      wipeChain: true,
    });
    expect(mithrilControllerMock.cancelBootstrap).toHaveBeenCalledTimes(1);
  });

  it('keeps compatibility exports as controller proxies', () => {
    const moduleExports = loadModule();

    moduleExports.setMithrilBootstrapNodeStateProvider(
      () => 'running' as never
    );

    expect(mithrilControllerMock.setNodeStateProvider).toHaveBeenCalledTimes(1);
  });
});
