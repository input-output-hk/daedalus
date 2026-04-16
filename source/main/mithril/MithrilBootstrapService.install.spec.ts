import fs from 'fs-extra';
import { MithrilBootstrapService } from './MithrilBootstrapService';
import { ChainStorageManager } from '../utils/chainStorageManager';

jest.mock('fs-extra', () => ({
  pathExists: jest.fn(),
  realpath: jest.fn(),
  readdir: jest.fn(),
  move: jest.fn(),
  remove: jest.fn(),
  lstat: jest.fn(),
  emptyDir: jest.fn(),
  ensureDir: jest.fn(),
}));

jest.mock('../config', () => ({
  stateDirectoryPath: '/tmp/state',
}));

jest.mock('../environment', () => ({
  environment: {
    network: 'mainnet',
    nodeVersion: '1.35.0',
  },
}));

jest.mock('../utils/logging', () => ({
  logger: {
    warn: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
  },
}));

describe('MithrilBootstrapService _installSnapshot', () => {
  const createChainStorageManager = () =>
    (({
      installSnapshot: jest.fn().mockResolvedValue(undefined),
      emptyManagedContents: jest.fn().mockResolvedValue(undefined),
    } as unknown) as ChainStorageManager);

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('delegates snapshot installation to the shared chain storage manager', async () => {
    const chainStorageManager = createChainStorageManager();
    const service = new MithrilBootstrapService(
      '/tmp/state/chain',
      chainStorageManager
    );

    await service._installSnapshot('/mnt/custom-parent/chain/db');

    expect(chainStorageManager.installSnapshot).toHaveBeenCalledWith(
      '/mnt/custom-parent/chain/db'
    );
  });

  it('rejects wipeChainAndSnapshots when strict artifact cleanup fails', async () => {
    const chainStorageManager = createChainStorageManager();
    const service = new MithrilBootstrapService(
      '/tmp/state/chain',
      chainStorageManager
    );
    const cleanupError = new Error('cleanup failed');

    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    (fs.remove as jest.Mock).mockRejectedValue(cleanupError);

    await expect(service.wipeChainAndSnapshots('wipe-test')).rejects.toThrow(
      'cleanup failed'
    );

    expect(chainStorageManager.emptyManagedContents).toHaveBeenCalledTimes(1);
  });
});
