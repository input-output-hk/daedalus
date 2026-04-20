import type {} from './handleDiskSpace';

const chainStorageManagerMock = {
  resolveDiskSpaceCheckPath: jest.fn(),
};

const chainStorageCoordinatorMock = {
  ensureManagedChainLayout: jest.fn(),
  isManagedChainEmpty: jest.fn(),
  wipeChainAndSnapshots: jest.fn(),
  onDirectoryChanged: jest.fn(),
};

const getDiskSpaceStatusChannelMock = {
  onReceive: jest.fn(),
  send: jest.fn(),
};

const mithrilBootstrapStatusChannelMock = {
  send: jest.fn(),
};

const fsExtraMock = {
  pathExists: jest.fn(),
};

const resetMithrilDecisionStateMock = jest.fn();

let directoryChangedHandler: (() => void) | null = null;

let mithrilBootstrapStatus = {
  status: 'idle',
  snapshot: null,
  error: null,
};

jest.mock('check-disk-space', () => jest.fn());

jest.mock('fs-extra', () => fsExtraMock);

jest.mock('../config', () => ({
  DISK_SPACE_CHECK_DONT_BOTHER_ME_INTERVAL: 1000,
  DISK_SPACE_CHECK_LONG_INTERVAL: 5000,
  DISK_SPACE_CHECK_MEDIUM_INTERVAL: 3000,
  DISK_SPACE_CHECK_SHORT_INTERVAL: 1000,
  DISK_SPACE_RECOMMENDED_PERCENTAGE: 25,
  DISK_SPACE_REQUIRED: 1024,
  DISK_SPACE_CHECK_TIMEOUT: 1,
  DISK_SPACE_REQUIRED_MARGIN_PERCENTAGE: 10,
  stateDirectoryPath: '/tmp/state',
  launcherConfig: {},
}));

jest.mock('./logging', () => ({
  logger: {
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

jest.mock('../ipc/get-disk-space-status', () => ({
  getDiskSpaceStatusChannel: getDiskSpaceStatusChannelMock,
}));

jest.mock('./chainStorageCoordinator', () => ({
  chainStorageCoordinator: chainStorageCoordinatorMock,
  getChainStorageManager: jest.fn(() => chainStorageManagerMock),
}));

jest.mock('../ipc/mithrilBootstrapChannel', () => ({
  getPendingMithrilBootstrapDecision: jest.fn(() => null),
  getMithrilBootstrapStatus: jest.fn(() => mithrilBootstrapStatus),
  isMithrilDecisionCancelledError: jest.fn(
    (error) => error?.name === 'MithrilDecisionCancelledError'
  ),
  onMithrilBootstrapDecision: jest.fn(),
  onMithrilBootstrapStatus: jest.fn(),
  resetMithrilDecisionState: resetMithrilDecisionStateMock,
  waitForMithrilBootstrapDecision: jest.fn(() => new Promise(() => {})),
  mithrilBootstrapStatusChannel: mithrilBootstrapStatusChannelMock,
  setMithrilBootstrapStatus: jest.fn((update) => {
    mithrilBootstrapStatus = {
      ...mithrilBootstrapStatus,
      ...update,
    };
  }),
}));

const { logger } = require('./logging') as typeof import('./logging');

const flushPromises = async (count = 6) => {
  for (let index = 0; index < count; index += 1) {
    await Promise.resolve();
  }
};

const waitForAssertion = async (assertion: () => void, attempts = 20) => {
  let lastError;

  for (let attempt = 0; attempt < attempts; attempt += 1) {
    try {
      assertion();
      return;
    } catch (error) {
      lastError = error;
      await flushPromises();
    }
  }

  throw lastError;
};

const createDeferred = <T>() => {
  let resolve: (value: T | PromiseLike<T>) => void;
  let reject: (reason?: unknown) => void;
  const promise = new Promise<T>((promiseResolve, promiseReject) => {
    resolve = promiseResolve;
    reject = promiseReject;
  });

  return {
    promise,
    resolve: resolve!,
    reject: reject!,
  };
};

const createCardanoNode = () => {
  const cardanoNode = {
    state: 'stopped',
    start: jest.fn().mockImplementation(async () => {
      cardanoNode.state = 'running';
    }),
    stop: jest.fn().mockResolvedValue(undefined),
    restart: jest.fn().mockResolvedValue(undefined),
    _startupTries: 0,
  };

  return cardanoNode;
};

describe('handleDiskSpace', () => {
  const checkDiskSpace = require('check-disk-space') as jest.Mock;
  const realSetInterval = global.setInterval;
  const realClearInterval = global.clearInterval;

  beforeEach(() => {
    jest.clearAllMocks();
    jest.useFakeTimers();
    global.setInterval = jest.fn(() => 1) as unknown as typeof setInterval;
    global.clearInterval = jest.fn() as unknown as typeof clearInterval;
    mithrilBootstrapStatus = {
      status: 'idle',
      snapshot: null,
      error: null,
    };
    checkDiskSpace.mockResolvedValue({ free: 4096, size: 16384 });
    chainStorageManagerMock.resolveDiskSpaceCheckPath.mockResolvedValue(
      '/tmp/unused'
    );
    chainStorageCoordinatorMock.ensureManagedChainLayout.mockResolvedValue({
      managedChainPath: '/tmp/state/chain',
      isRecoveryFallback: false,
    });
    fsExtraMock.pathExists.mockResolvedValue(false);
    chainStorageCoordinatorMock.isManagedChainEmpty.mockResolvedValue(false);
    chainStorageCoordinatorMock.wipeChainAndSnapshots.mockResolvedValue(
      undefined
    );
    chainStorageCoordinatorMock.onDirectoryChanged.mockImplementation(
      (callback) => {
        directoryChangedHandler = callback;
      }
    );
    resetMithrilDecisionStateMock.mockReset();
    directoryChangedHandler = null;
  });

  afterEach(async () => {
    await flushPromises();
    jest.runOnlyPendingTimers();
    jest.useRealTimers();
    global.setInterval = realSetInterval;
    global.clearInterval = realClearInterval;
  });

  it('starts cardano-node after recovery when the default managed chain already has data', async () => {
    const { handleDiskSpace } =
      require('./handleDiskSpace') as typeof import('./handleDiskSpace');
    const cardanoNode = createCardanoNode();

    chainStorageCoordinatorMock.ensureManagedChainLayout.mockResolvedValue({
      managedChainPath: '/tmp/state/chain',
      isRecoveryFallback: true,
    });
    chainStorageCoordinatorMock.isManagedChainEmpty.mockResolvedValue(false);

    const handleCheckDiskSpace = handleDiskSpace(
      { webContents: {} } as never,
      cardanoNode as never
    );

    await handleCheckDiskSpace();

    expect(
      chainStorageCoordinatorMock.ensureManagedChainLayout
    ).toHaveBeenCalledWith('stopped');
    expect(checkDiskSpace).toHaveBeenCalledWith('/tmp/state/chain');
    expect(
      chainStorageManagerMock.resolveDiskSpaceCheckPath
    ).not.toHaveBeenCalled();
    expect(cardanoNode.start).toHaveBeenCalledTimes(1);
    expect(mithrilBootstrapStatusChannelMock.send).not.toHaveBeenCalledWith(
      expect.objectContaining({ status: 'decision' }),
      expect.anything()
    );
  });

  it('continues into the existing Mithril decision flow after recovery when the default chain is empty', async () => {
    const { handleDiskSpace } =
      require('./handleDiskSpace') as typeof import('./handleDiskSpace');
    const cardanoNode = createCardanoNode();

    chainStorageCoordinatorMock.ensureManagedChainLayout.mockResolvedValue({
      managedChainPath: '/tmp/state/chain',
      isRecoveryFallback: true,
    });
    chainStorageCoordinatorMock.isManagedChainEmpty.mockResolvedValue(true);

    const handleCheckDiskSpace = handleDiskSpace(
      { webContents: {} } as never,
      cardanoNode as never
    );

    await handleCheckDiskSpace();

    expect(cardanoNode.start).not.toHaveBeenCalled();
    expect(mithrilBootstrapStatusChannelMock.send).toHaveBeenCalledWith(
      expect.objectContaining({ status: 'decision' }),
      expect.anything()
    );
  });

  it('still propagates unrecoverable startup gate errors to direct callers', async () => {
    const { handleDiskSpace } =
      require('./handleDiskSpace') as typeof import('./handleDiskSpace');
    const cardanoNode = createCardanoNode();
    const layoutError = new Error('permission denied');

    chainStorageCoordinatorMock.ensureManagedChainLayout.mockRejectedValue(
      layoutError
    );

    const handleCheckDiskSpace = handleDiskSpace(
      { webContents: {} } as never,
      cardanoNode as never
    );

    await expect(handleCheckDiskSpace()).rejects.toMatchObject({
      message: 'permission denied',
      code: 'MANAGED_CHAIN_LAYOUT_ERROR',
    });
    await expect(handleCheckDiskSpace()).rejects.toMatchObject({
      message: 'permission denied',
      code: 'MANAGED_CHAIN_LAYOUT_ERROR',
    });
    expect(checkDiskSpace).not.toHaveBeenCalled();
    expect(cardanoNode.start).not.toHaveBeenCalled();
  });

  it('starts cardano-node immediately after a directory change when the new managed chain already has data', async () => {
    const { handleDiskSpace } =
      require('./handleDiskSpace') as typeof import('./handleDiskSpace');
    const cardanoNode = createCardanoNode();

    handleDiskSpace({ webContents: {} } as never, cardanoNode as never);

    expect(directoryChangedHandler).toEqual(expect.any(Function));

    directoryChangedHandler?.();
    await waitForAssertion(() => {
      expect(cardanoNode.start).toHaveBeenCalledTimes(1);
    });

    expect(mithrilBootstrapStatusChannelMock.send).toHaveBeenCalledWith(
      expect.objectContaining({ status: 'idle' }),
      expect.anything()
    );
    expect(mithrilBootstrapStatusChannelMock.send).not.toHaveBeenCalledWith(
      expect.objectContaining({ status: 'decision' }),
      expect.anything()
    );
    expect(
      mithrilBootstrapStatusChannelMock.send.mock.invocationCallOrder[0]
    ).toBeLessThan(cardanoNode.start.mock.invocationCallOrder[0]);
  });

  it('continues into the Mithril decision flow immediately after a directory change when the new managed chain is empty', async () => {
    const { handleDiskSpace } =
      require('./handleDiskSpace') as typeof import('./handleDiskSpace');
    const cardanoNode = createCardanoNode();

    chainStorageCoordinatorMock.isManagedChainEmpty.mockResolvedValue(true);

    handleDiskSpace({ webContents: {} } as never, cardanoNode as never);

    directoryChangedHandler?.();
    await waitForAssertion(() => {
      expect(mithrilBootstrapStatusChannelMock.send).toHaveBeenCalledWith(
        expect.objectContaining({ status: 'decision' }),
        expect.anything()
      );
    });

    expect(mithrilBootstrapStatusChannelMock.send).not.toHaveBeenCalledWith(
      expect.objectContaining({ status: 'idle' }),
      expect.anything()
    );
    expect(cardanoNode.start).not.toHaveBeenCalled();
  });

  it('abandons a stale in-flight check after a directory change during the startup gate', async () => {
    const { handleDiskSpace } =
      require('./handleDiskSpace') as typeof import('./handleDiskSpace');
    const initialLayout = createDeferred<{
      managedChainPath: string;
      isRecoveryFallback: boolean;
    }>();
    const cardanoNode = createCardanoNode();

    chainStorageCoordinatorMock.ensureManagedChainLayout
      .mockImplementationOnce(() => initialLayout.promise)
      .mockResolvedValueOnce({
        managedChainPath: '/tmp/state/fresh-chain',
        isRecoveryFallback: false,
      });

    const handleCheckDiskSpace = handleDiskSpace(
      { webContents: {} } as never,
      cardanoNode as never
    );

    const pendingCheck = handleCheckDiskSpace();

    expect(chainStorageCoordinatorMock.onDirectoryChanged).toHaveBeenCalledWith(
      expect.any(Function)
    );

    directoryChangedHandler?.();
    initialLayout.resolve({
      managedChainPath: '/tmp/state/stale-chain',
      isRecoveryFallback: false,
    });

    await pendingCheck;
    await waitForAssertion(() => {
      expect(cardanoNode.start).toHaveBeenCalledTimes(1);
    });

    expect(resetMithrilDecisionStateMock).toHaveBeenCalledTimes(2);
    expect(checkDiskSpace).toHaveBeenCalledTimes(1);
    expect(checkDiskSpace).toHaveBeenCalledWith('/tmp/state/fresh-chain');
  });

  it('re-evaluates immediately when resetToDefault emits the same directory-change callback', async () => {
    const { handleDiskSpace } =
      require('./handleDiskSpace') as typeof import('./handleDiskSpace');
    const cardanoNode = createCardanoNode();

    chainStorageCoordinatorMock.ensureManagedChainLayout
      .mockResolvedValueOnce({
        managedChainPath: '/tmp/state/custom-chain',
        isRecoveryFallback: false,
      })
      .mockResolvedValueOnce({
        managedChainPath: '/tmp/state/default-chain',
        isRecoveryFallback: false,
      });

    const handleCheckDiskSpace = handleDiskSpace(
      { webContents: {} } as never,
      cardanoNode as never
    );

    await handleCheckDiskSpace();
    cardanoNode.state = 'stopped';

    directoryChangedHandler?.();
    await waitForAssertion(() => {
      expect(cardanoNode.start).toHaveBeenCalledTimes(2);
    });

    expect(checkDiskSpace).toHaveBeenNthCalledWith(
      1,
      '/tmp/state/custom-chain'
    );
    expect(checkDiskSpace).toHaveBeenNthCalledWith(
      2,
      '/tmp/state/default-chain'
    );
  });

  it('coalesces overlapping triggers into one trailing rerun without double-starting the node', async () => {
    const { handleDiskSpace } =
      require('./handleDiskSpace') as typeof import('./handleDiskSpace');
    const initialLayout = createDeferred<{
      managedChainPath: string;
      isRecoveryFallback: boolean;
    }>();
    const cardanoNode = createCardanoNode();

    chainStorageCoordinatorMock.ensureManagedChainLayout
      .mockImplementationOnce(() => initialLayout.promise)
      .mockResolvedValueOnce({
        managedChainPath: '/tmp/state/trailing-chain',
        isRecoveryFallback: false,
      });

    const handleCheckDiskSpace = handleDiskSpace(
      { webContents: {} } as never,
      cardanoNode as never
    );

    const activeRun = handleCheckDiskSpace();

    directoryChangedHandler?.();
    directoryChangedHandler?.();

    initialLayout.resolve({
      managedChainPath: '/tmp/state/stale-chain',
      isRecoveryFallback: false,
    });

    await activeRun;
    await waitForAssertion(() => {
      expect(cardanoNode.start).toHaveBeenCalledTimes(1);
    });

    expect(
      chainStorageCoordinatorMock.ensureManagedChainLayout
    ).toHaveBeenCalledTimes(2);
    expect(checkDiskSpace).toHaveBeenCalledTimes(1);
  });

  it('preserves fresh-directory semantics when a later poll trigger overlaps a queued rerun', async () => {
    const { handleDiskSpace } =
      require('./handleDiskSpace') as typeof import('./handleDiskSpace');
    const initialLayout = createDeferred<{
      managedChainPath: string;
      isRecoveryFallback: boolean;
    }>();
    const cardanoNode = createCardanoNode();

    cardanoNode.state = 'running';

    chainStorageCoordinatorMock.ensureManagedChainLayout
      .mockImplementationOnce(() => initialLayout.promise)
      .mockResolvedValueOnce({
        managedChainPath: '/tmp/state/fresh-chain',
        isRecoveryFallback: false,
      });

    const handleCheckDiskSpace = handleDiskSpace(
      { webContents: {} } as never,
      cardanoNode as never
    );

    const activeRun = handleCheckDiskSpace();

    directoryChangedHandler?.();
    const overlappingPollRun = handleCheckDiskSpace(true);

    initialLayout.resolve({
      managedChainPath: '/tmp/state/stale-chain',
      isRecoveryFallback: false,
    });

    await activeRun;
    const trailingResponse = await overlappingPollRun;

    expect(
      chainStorageCoordinatorMock.ensureManagedChainLayout
    ).toHaveBeenCalledTimes(2);
    expect(checkDiskSpace).toHaveBeenCalledTimes(1);
    expect(checkDiskSpace).toHaveBeenCalledWith('/tmp/state/fresh-chain');
    expect(trailingResponse.hadNotEnoughSpaceLeft).toBe(false);
    expect(cardanoNode.start).not.toHaveBeenCalled();
    expect(cardanoNode.restart).not.toHaveBeenCalled();
  });

  it('logs fire-and-forget directory-change recheck failures instead of leaving rejections unhandled', async () => {
    const { handleDiskSpace } =
      require('./handleDiskSpace') as typeof import('./handleDiskSpace');
    const cardanoNode = createCardanoNode();
    const layoutError = new Error('permission denied');

    chainStorageCoordinatorMock.ensureManagedChainLayout.mockRejectedValue(
      layoutError
    );

    handleDiskSpace({ webContents: {} } as never, cardanoNode as never);

    expect(() => directoryChangedHandler?.()).not.toThrow();

    await flushPromises();

    expect(logger.error).toHaveBeenCalledWith(
      '[MITHRIL] Immediate disk-space recheck after directory change failed',
      expect.objectContaining({
        error: expect.objectContaining({
          code: 'MANAGED_CHAIN_LAYOUT_ERROR',
          message: 'permission denied',
        }),
      })
    );
  });

  it('logs and contains managed-layout failures from the background polling interval', async () => {
    const { handleDiskSpace } =
      require('./handleDiskSpace') as typeof import('./handleDiskSpace');
    const cardanoNode = createCardanoNode();
    const layoutError = new Error('permission denied');

    chainStorageCoordinatorMock.ensureManagedChainLayout.mockRejectedValue(
      layoutError
    );

    handleDiskSpace({ webContents: {} } as never, cardanoNode as never);

    const intervalCallback = (global.setInterval as jest.Mock).mock.calls[0][0];

    expect(() => intervalCallback()).not.toThrow();
    await flushPromises();

    expect(logger.error).toHaveBeenCalledWith(
      '[MITHRIL] Background disk-space poll failed',
      expect.objectContaining({
        error: expect.objectContaining({
          code: 'MANAGED_CHAIN_LAYOUT_ERROR',
          message: 'permission denied',
        }),
      })
    );
  });

  it('clears Mithril decision state and emits idle before starting on a non-empty managed chain', async () => {
    const { handleDiskSpace } =
      require('./handleDiskSpace') as typeof import('./handleDiskSpace');
    const cardanoNode = createCardanoNode();

    chainStorageCoordinatorMock.isManagedChainEmpty.mockResolvedValue(false);

    const handleCheckDiskSpace = handleDiskSpace(
      { webContents: {} } as never,
      cardanoNode as never
    );

    await handleCheckDiskSpace();

    expect(resetMithrilDecisionStateMock).toHaveBeenCalledTimes(1);
    expect(mithrilBootstrapStatusChannelMock.send).toHaveBeenCalledWith(
      expect.objectContaining({ status: 'idle' }),
      expect.anything()
    );
    expect(
      mithrilBootstrapStatusChannelMock.send.mock.invocationCallOrder[0]
    ).toBeLessThan(cardanoNode.start.mock.invocationCallOrder[0]);
  });

  it('still starts cardano-node when emitting idle status fails on a non-empty managed chain', async () => {
    const { handleDiskSpace } =
      require('./handleDiskSpace') as typeof import('./handleDiskSpace');
    const cardanoNode = createCardanoNode();

    chainStorageCoordinatorMock.isManagedChainEmpty.mockResolvedValue(false);
    mithrilBootstrapStatusChannelMock.send.mockRejectedValueOnce(
      new Error('webContents unavailable')
    );

    const handleCheckDiskSpace = handleDiskSpace(
      { webContents: {} } as never,
      cardanoNode as never
    );

    await handleCheckDiskSpace();

    expect(cardanoNode.start).toHaveBeenCalledTimes(1);
    expect(logger.warn).toHaveBeenCalledWith(
      '[MITHRIL] Failed to broadcast bootstrap status update',
      expect.objectContaining({
        context: 'emit-idle',
        error: expect.objectContaining({
          message: 'webContents unavailable',
        }),
        status: 'idle',
      })
    );
  });

  it('still starts cardano-node when emitting idle status never resolves on a non-empty managed chain', async () => {
    const { handleDiskSpace } =
      require('./handleDiskSpace') as typeof import('./handleDiskSpace');
    const cardanoNode = createCardanoNode();

    chainStorageCoordinatorMock.isManagedChainEmpty.mockResolvedValue(false);
    mithrilBootstrapStatusChannelMock.send.mockImplementationOnce(
      () => new Promise(() => {})
    );

    const handleCheckDiskSpace = handleDiskSpace(
      { webContents: {} } as never,
      cardanoNode as never
    );

    await handleCheckDiskSpace();

    expect(cardanoNode.start).toHaveBeenCalledTimes(1);
  });

  it('falls back to starting cardano-node when the non-empty chain check throws', async () => {
    const { handleDiskSpace } =
      require('./handleDiskSpace') as typeof import('./handleDiskSpace');
    const cardanoNode = createCardanoNode();

    chainStorageCoordinatorMock.isManagedChainEmpty.mockRejectedValueOnce(
      new Error('managed chain unavailable')
    );

    const handleCheckDiskSpace = handleDiskSpace(
      { webContents: {} } as never,
      cardanoNode as never
    );

    await handleCheckDiskSpace();

    expect(cardanoNode.start).toHaveBeenCalledTimes(1);
    expect(logger.error).toHaveBeenCalledWith(
      '[MITHRIL] Failed to handle bootstrap decision',
      expect.objectContaining({
        error: expect.objectContaining({
          message: 'managed chain unavailable',
        }),
      })
    );
  });

  it('handles cancelled Mithril bootstrap by declining and starting cardano-node when chain is empty', async () => {
    const { handleDiskSpace } =
      require('./handleDiskSpace') as typeof import('./handleDiskSpace');
    const {
      getPendingMithrilBootstrapDecision,
      getMithrilBootstrapStatus,
    } = require('../ipc/mithrilBootstrapChannel');
    const cardanoNode = createCardanoNode();

    getPendingMithrilBootstrapDecision.mockReturnValue('decline');
    getMithrilBootstrapStatus.mockReturnValue({
      status: 'cancelled',
      snapshot: null,
      error: null,
    });
    chainStorageCoordinatorMock.isManagedChainEmpty.mockResolvedValue(true);

    const handleCheckDiskSpace = handleDiskSpace(
      { webContents: {} } as never,
      cardanoNode as never
    );

    await handleCheckDiskSpace();

    expect(
      chainStorageCoordinatorMock.wipeChainAndSnapshots
    ).toHaveBeenCalled();
    expect(cardanoNode.start).toHaveBeenCalledTimes(1);
  });

  it('handles cancelled Mithril bootstrap by declining and starting cardano-node when chain has data', async () => {
    const { handleDiskSpace } =
      require('./handleDiskSpace') as typeof import('./handleDiskSpace');
    const {
      getPendingMithrilBootstrapDecision,
      getMithrilBootstrapStatus,
    } = require('../ipc/mithrilBootstrapChannel');
    const cardanoNode = createCardanoNode();

    getPendingMithrilBootstrapDecision.mockReturnValue('decline');
    getMithrilBootstrapStatus.mockReturnValue({
      status: 'cancelled',
      snapshot: null,
      error: null,
    });
    chainStorageCoordinatorMock.isManagedChainEmpty.mockResolvedValue(false);

    const handleCheckDiskSpace = handleDiskSpace(
      { webContents: {} } as never,
      cardanoNode as never
    );

    await handleCheckDiskSpace();

    expect(
      chainStorageCoordinatorMock.wipeChainAndSnapshots
    ).toHaveBeenCalled();
    expect(cardanoNode.start).toHaveBeenCalledTimes(1);
  });
});
