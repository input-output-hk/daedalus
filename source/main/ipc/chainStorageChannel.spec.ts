import { handleChainStorageRequests } from './chainStorageChannel';
import { logger } from '../utils/logging';

jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn().mockImplementation(() => ({
    onRequest: jest.fn(),
  })),
}));

jest.mock('../utils/chainStorageCoordinator', () => {
  const verifySymlink = jest.fn();
  const validate = jest.fn();
  const setDirectory = jest.fn();
  const getConfig = jest.fn();

  return {
    chainStorageCoordinator: {
      verifySymlink,
      validate,
      setDirectory,
      getConfig,
    },
    __mocks: {
      verifySymlink,
      validate,
      setDirectory,
      getConfig,
    },
  };
});

jest.mock('../utils/logging', () => ({
  logger: {
    warn: jest.fn(),
  },
}));

const chainStorageCoordinatorMock = jest.requireMock(
  '../utils/chainStorageCoordinator'
).__mocks;

describe('chainStorageChannel startup verification', () => {
  const flushPromises = () => new Promise((resolve) => setTimeout(resolve, 0));

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('logs warning when startup symlink verification is invalid', async () => {
    chainStorageCoordinatorMock.verifySymlink.mockResolvedValue({
      isValid: false,
      path: '/mnt/missing-chain',
      reason: 'path-not-found',
    });

    handleChainStorageRequests();
    await flushPromises();

    expect(chainStorageCoordinatorMock.verifySymlink).toHaveBeenCalledTimes(1);
    expect(logger.warn).toHaveBeenCalledWith(
      'Chain storage symlink verification failed on startup',
      expect.objectContaining({
        verification: expect.objectContaining({
          isValid: false,
          path: '/mnt/missing-chain',
        }),
      })
    );
  });

  it('logs warning when startup verification throws', async () => {
    chainStorageCoordinatorMock.verifySymlink.mockRejectedValue(
      new Error('verification failed')
    );

    handleChainStorageRequests();
    await flushPromises();

    expect(logger.warn).toHaveBeenCalledWith(
      'Chain storage symlink startup verification failed',
      expect.objectContaining({
        error: expect.any(Error),
      })
    );
  });
});
