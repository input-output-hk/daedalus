import type {} from './chainStorageChannel';

const mockChannels: Array<{ onRequest: jest.Mock }> = [];

jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn().mockImplementation(() => {
    const channel = {
      onRequest: jest.fn(),
    };
    mockChannels.push(channel);
    return channel;
  }),
}));

jest.mock('../utils/chainStorageCoordinator', () => {
  const validate = jest.fn();
  const setDirectory = jest.fn();
  const getConfig = jest.fn();
  const prepareForLocationChange = jest.fn();

  return {
    chainStorageCoordinator: {
      validate,
      setDirectory,
      getConfig,
      prepareForLocationChange,
    },
    __mocks: {
      validate,
      setDirectory,
      getConfig,
      prepareForLocationChange,
    },
  };
});

const loadModule = () => {
  let moduleExports;
  let chainStorageCoordinatorMock;

  jest.isolateModules(() => {
    moduleExports = require('./chainStorageChannel');
    chainStorageCoordinatorMock = jest.requireMock(
      '../utils/chainStorageCoordinator'
    ).__mocks;
  });

  return {
    moduleExports: moduleExports as typeof import('./chainStorageChannel'),
    chainStorageCoordinatorMock,
  };
};

describe('chainStorageChannel', () => {
  beforeEach(() => {
    jest.resetModules();
    jest.clearAllMocks();
    mockChannels.length = 0;
  });

  it('registers request handlers for get, set, validate, and prepare', () => {
    const { moduleExports } = loadModule();
    moduleExports.handleChainStorageRequests();

    expect(mockChannels).toHaveLength(4);
    for (const channel of mockChannels) {
      expect(channel.onRequest).toHaveBeenCalledTimes(1);
    }
  });

  it('delegates get requests to the coordinator config loader', async () => {
    const { moduleExports, chainStorageCoordinatorMock } = loadModule();
    moduleExports.handleChainStorageRequests();
    const getHandler = mockChannels[1].onRequest.mock.calls[0][0];
    chainStorageCoordinatorMock.getConfig.mockResolvedValue({
      customPath: null,
      defaultPath: '/tmp/state/chain',
      availableSpaceBytes: 4096,
      requiredSpaceBytes: 1024,
    });

    const result = await getHandler();

    expect(chainStorageCoordinatorMock.getConfig).toHaveBeenCalledTimes(1);
    expect(result).toEqual(
      expect.objectContaining({
        defaultPath: '/tmp/state/chain',
      })
    );
  });

  it('returns session recovery metadata from get requests when startup fallback happened', async () => {
    const { moduleExports, chainStorageCoordinatorMock } = loadModule();
    moduleExports.handleChainStorageRequests();
    const getHandler = mockChannels[1].onRequest.mock.calls[0][0];
    chainStorageCoordinatorMock.getConfig.mockResolvedValue({
      customPath: null,
      defaultPath: '/tmp/state/chain',
      availableSpaceBytes: 4096,
      requiredSpaceBytes: 1024,
      isRecoveryFallback: true,
    });

    const result = await getHandler();

    expect(result).toEqual(
      expect.objectContaining({
        customPath: null,
        isRecoveryFallback: true,
      })
    );
  });

  it('delegates prepare requests to the coordinator cleanup hook', async () => {
    const { moduleExports, chainStorageCoordinatorMock } = loadModule();
    moduleExports.handleChainStorageRequests();
    const prepareHandler = mockChannels[3].onRequest.mock.calls[0][0];
    chainStorageCoordinatorMock.prepareForLocationChange.mockResolvedValue({
      isValid: true,
      path: null,
      resolvedPath: '/tmp/state/chain',
      availableSpaceBytes: 4096,
      requiredSpaceBytes: 1024,
    });

    const result = await prepareHandler();

    expect(
      chainStorageCoordinatorMock.prepareForLocationChange
    ).toHaveBeenCalledTimes(1);
    expect(result).toEqual(
      expect.objectContaining({
        path: null,
        resolvedPath: '/tmp/state/chain',
      })
    );
  });
});
