import fs from 'fs-extra';
import checkDiskSpace from 'check-disk-space';
import { CardanoNodeStates } from '../../common/types/cardano-node.types';
import { handleDiskSpace } from './handleDiskSpace';

const mockDecisionListeners: Array<(
  decision: 'accept' | 'decline'
) => void> = [];
const mockStatusListeners: Array<(status: any) => void> = [];
let mockPendingDecision: 'accept' | 'decline' | null = null;
let mockLastStatus = {
  status: 'idle',
  snapshot: null,
  error: null,
};
let mockPersistStatusUpdates = true;

const mockChainStorageManager = {
  resolveChainStoragePath: jest.fn(),
  resolveMithrilWorkDir: jest.fn(),
};

const mockMithrilBootstrapService = {
  setWorkDir: jest.fn(),
  wipeChainAndSnapshots: jest.fn(),
};

jest.mock('fs-extra', () => ({
  pathExists: jest.fn(),
  readdir: jest.fn(),
}));

jest.mock('check-disk-space', () => jest.fn());

jest.mock('../config', () => ({
  DISK_SPACE_CHECK_DONT_BOTHER_ME_INTERVAL: 60_000,
  DISK_SPACE_CHECK_LONG_INTERVAL: 60_000,
  DISK_SPACE_CHECK_MEDIUM_INTERVAL: 30_000,
  DISK_SPACE_CHECK_SHORT_INTERVAL: 5_000,
  DISK_SPACE_RECOMMENDED_PERCENTAGE: 25,
  DISK_SPACE_REQUIRED: 1024,
  DISK_SPACE_CHECK_TIMEOUT: 1_000,
  DISK_SPACE_REQUIRED_MARGIN_PERCENTAGE: 5,
  stateDirectoryPath: '/tmp/state',
  launcherConfig: {
    wipeChain: false,
  },
}));

jest.mock('../ipc/get-disk-space-status', () => ({
  getDiskSpaceStatusChannel: {
    send: jest.fn(),
    onReceive: jest.fn(),
  },
}));

jest.mock('../ipc/mithrilBootstrapChannel', () => ({
  getPendingMithrilBootstrapDecision: jest.fn(() => mockPendingDecision),
  getMithrilBootstrapStatus: jest.fn(() => mockLastStatus),
  onMithrilBootstrapDecision: jest.fn((handler) => {
    mockDecisionListeners.push(handler);
    return jest.fn();
  }),
  onMithrilBootstrapStatus: jest.fn((handler) => {
    mockStatusListeners.push(handler);
    return jest.fn();
  }),
  waitForMithrilBootstrapDecision: jest.fn(() =>
    mockPendingDecision
      ? Promise.resolve(mockPendingDecision)
      : new Promise(() => {})
  ),
  mithrilBootstrapStatusChannel: {
    send: jest.fn(),
  },
  setMithrilBootstrapStatus: jest.fn((update) => {
    if (mockPersistStatusUpdates) {
      mockLastStatus = {
        ...mockLastStatus,
        ...update,
      };
    }
    return {
      ...mockLastStatus,
      ...update,
    };
  }),
}));

jest.mock('../mithril/MithrilBootstrapService', () => ({
  MithrilBootstrapService: jest.fn(() => mockMithrilBootstrapService),
}));

jest.mock('./chainStorageManager', () => ({
  ChainStorageManager: jest.fn(() => mockChainStorageManager),
}));

jest.mock('./logging', () => ({
  logger: {
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

const flushPromises = () =>
  new Promise<void>((resolve) => process.nextTick(resolve));

const createDeferred = () => {
  let resolve!: () => void;
  let reject!: (error?: unknown) => void;
  const promise = new Promise<void>((res, rej) => {
    resolve = res;
    reject = rej;
  });

  return { promise, resolve, reject };
};

describe('handleDiskSpace', () => {
  const mainWindow = { webContents: {} } as any;
  let cardanoNode;
  let logger;
  let getDiskSpaceStatusChannel;
  let intervalSpy;
  let clearIntervalSpy;

  beforeEach(() => {
    jest.clearAllMocks();
    mockDecisionListeners.length = 0;
    mockStatusListeners.length = 0;
    mockPendingDecision = null;
    mockLastStatus = {
      status: 'idle',
      snapshot: null,
      error: null,
    };
    mockPersistStatusUpdates = true;

    intervalSpy = jest.spyOn(global, 'setInterval').mockReturnValue(1 as any);
    clearIntervalSpy = jest
      .spyOn(global, 'clearInterval')
      .mockImplementation(() => undefined);

    (checkDiskSpace as jest.Mock).mockResolvedValue({
      free: 4096,
      size: 8192,
    });

    (fs.pathExists as jest.Mock).mockImplementation((targetPath: string) =>
      Promise.resolve(!targetPath.endsWith('mithril-bootstrap.lock'))
    );
    (fs.readdir as jest.Mock).mockResolvedValue([]);

    mockChainStorageManager.resolveChainStoragePath.mockResolvedValue(
      '/tmp/state/chain'
    );
    mockChainStorageManager.resolveMithrilWorkDir.mockResolvedValue(
      '/tmp/state/mithril'
    );

    mockMithrilBootstrapService.setWorkDir.mockReset();
    mockMithrilBootstrapService.wipeChainAndSnapshots.mockReset();

    cardanoNode = {
      state: CardanoNodeStates.STOPPED,
      start: jest.fn().mockResolvedValue(undefined),
      stop: jest.fn().mockResolvedValue(undefined),
      restart: jest.fn().mockResolvedValue(undefined),
      _startupTries: 0,
    };

    ({ logger } = require('./logging'));
    ({ getDiskSpaceStatusChannel } = require('../ipc/get-disk-space-status'));
  });

  afterEach(() => {
    intervalSpy.mockRestore();
    clearIntervalSpy.mockRestore();
  });

  it('ignores overlapping cancelled-to-decline recovery and resets the guard after completion', async () => {
    mockPendingDecision = 'decline';
    mockLastStatus = {
      status: 'cancelled',
      snapshot: null,
      error: null,
    };
    mockPersistStatusUpdates = false;

    const firstWipe = createDeferred();
    mockMithrilBootstrapService.wipeChainAndSnapshots
      .mockReturnValueOnce(firstWipe.promise)
      .mockResolvedValue(undefined);

    const handleCheckDiskSpace = handleDiskSpace(mainWindow, cardanoNode);

    const firstRun = handleCheckDiskSpace();
    await flushPromises();

    expect(
      mockMithrilBootstrapService.wipeChainAndSnapshots
    ).toHaveBeenCalledTimes(1);

    mockDecisionListeners.forEach((listener) => listener('decline'));
    await flushPromises();

    expect(
      mockMithrilBootstrapService.wipeChainAndSnapshots
    ).toHaveBeenCalledTimes(1);
    expect(logger.info).toHaveBeenCalledWith(
      '[MITHRIL] Decline recovery after bootstrap cancel already in progress',
      {
        source: 'decision-listener',
      }
    );

    firstWipe.resolve();
    await firstRun;

    expect(cardanoNode.start).toHaveBeenCalledTimes(1);
    expect(getDiskSpaceStatusChannel.send).toHaveBeenCalledTimes(1);

    await handleCheckDiskSpace();

    expect(
      mockMithrilBootstrapService.wipeChainAndSnapshots
    ).toHaveBeenCalledTimes(2);
    expect(cardanoNode.start).toHaveBeenCalledTimes(2);
  });
});
