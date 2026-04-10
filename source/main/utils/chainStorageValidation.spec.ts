import fs from 'fs-extra';
import {
  isSubPath,
  isSamePath,
  validateChainStorageDirectory,
} from './chainStorageValidation';

jest.mock('fs-extra', () => ({
  pathExists: jest.fn(),
  realpath: jest.fn(),
  stat: jest.fn(),
  lstat: jest.fn(),
  access: jest.fn(),
  constants: {
    W_OK: 2,
  },
}));

jest.mock('check-disk-space', () => jest.fn());

jest.mock('../config', () => ({
  DISK_SPACE_REQUIRED: 1024,
}));

jest.mock('./logging', () => ({
  logger: {
    warn: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
  },
}));

describe('isSubPath', () => {
  it('returns true when child is nested under parent', () => {
    expect(isSubPath('/tmp/state', '/tmp/state/chain')).toBe(true);
  });

  it('returns true when child equals parent', () => {
    expect(isSubPath('/tmp/state', '/tmp/state')).toBe(true);
  });

  it('returns false when child is a sibling', () => {
    expect(isSubPath('/tmp/state', '/tmp/other')).toBe(false);
  });

  it('returns false when parent contains only a prefix of child but not a dir boundary', () => {
    expect(isSubPath('/tmp/state', '/tmp/state2/chain')).toBe(false);
  });
});

describe('isSamePath', () => {
  it('returns true for identical absolute paths', () => {
    expect(isSamePath('/tmp/chain', '/tmp/chain')).toBe(true);
  });

  it('returns false for different paths', () => {
    expect(isSamePath('/tmp/chain', '/tmp/other')).toBe(false);
  });

  it('resolves relative paths before comparison', () => {
    // Both resolve to the same absolute path
    expect(isSamePath('/tmp/state/..', '/tmp')).toBe(true);
  });
});

describe('validateChainStorageDirectory', () => {
  const REQUIRED_SPACE = 1024;
  const STATE_DIR = '/tmp/state';
  const CHAIN_PATH = '/tmp/state/chain';

  const makeGetDefaultConfig = (overrides?: object) =>
    jest.fn().mockResolvedValue({
      defaultPath: CHAIN_PATH,
      availableSpaceBytes: 4096,
      requiredSpaceBytes: REQUIRED_SPACE,
      ...overrides,
    });

  beforeEach(() => {
    jest.clearAllMocks();
    const checkDiskSpace = require('check-disk-space');
    checkDiskSpace.mockResolvedValue({ free: 4096 });
  });

  it('returns valid with null path when targetDir is null', async () => {
    const result = await validateChainStorageDirectory(
      null,
      STATE_DIR,
      makeGetDefaultConfig()
    );
    expect(result).toEqual({ isValid: true, path: null });
  });

  it('returns valid with null path when targetDir is empty string', async () => {
    const result = await validateChainStorageDirectory(
      '   ',
      STATE_DIR,
      makeGetDefaultConfig()
    );
    expect(result).toEqual({ isValid: true, path: null });
  });

  it('treats the default chain path as a valid default selection', async () => {
    const getDefaultConfig = makeGetDefaultConfig();
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(STATE_DIR);

    const result = await validateChainStorageDirectory(
      CHAIN_PATH,
      STATE_DIR,
      getDefaultConfig
    );

    expect(result).toEqual({
      isValid: true,
      path: null,
      resolvedPath: CHAIN_PATH,
      availableSpaceBytes: 4096,
      requiredSpaceBytes: REQUIRED_SPACE,
    });
    expect(getDefaultConfig).toHaveBeenCalled();
  });

  it('returns path-not-found when the target directory does not exist', async () => {
    (fs.pathExists as jest.Mock).mockResolvedValue(false);

    const result = await validateChainStorageDirectory(
      '/mnt/missing',
      STATE_DIR,
      makeGetDefaultConfig()
    );

    expect(result).toEqual(
      expect.objectContaining({
        isValid: false,
        path: '/mnt/missing',
        reason: 'path-not-found',
      })
    );
  });

  it('returns not-writable when the target path is not a directory', async () => {
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue('/mnt/file.txt');
    (fs.stat as jest.Mock).mockResolvedValue({ isDirectory: () => false });

    const result = await validateChainStorageDirectory(
      '/mnt/file.txt',
      STATE_DIR,
      makeGetDefaultConfig()
    );

    expect(result).toEqual(
      expect.objectContaining({
        isValid: false,
        reason: 'not-writable',
      })
    );
  });

  it('returns inside-state-dir when the target is nested under the state directory', async () => {
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    ((fs.realpath as unknown) as jest.Mock).mockImplementation(
      async (p: string) => p
    );
    (fs.stat as jest.Mock).mockResolvedValue({ isDirectory: () => true });

    const result = await validateChainStorageDirectory(
      '/tmp/state/subdir',
      STATE_DIR,
      makeGetDefaultConfig()
    );

    expect(result).toEqual(
      expect.objectContaining({
        isValid: false,
        reason: 'inside-state-dir',
      })
    );
  });

  it('returns insufficient-space when free space is below the threshold', async () => {
    const checkDiskSpace = require('check-disk-space');
    checkDiskSpace.mockResolvedValue({ free: 100 });
    (fs.pathExists as jest.Mock)
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(false);
    ((fs.realpath as unknown) as jest.Mock).mockImplementation(
      async (p: string) => p
    );
    (fs.stat as jest.Mock).mockResolvedValue({ isDirectory: () => true });
    (fs.access as jest.Mock).mockResolvedValue(undefined);

    const result = await validateChainStorageDirectory(
      '/mnt/external',
      STATE_DIR,
      makeGetDefaultConfig(),
      REQUIRED_SPACE
    );

    expect(result).toEqual(
      expect.objectContaining({
        isValid: false,
        reason: 'insufficient-space',
        availableSpaceBytes: 100,
        requiredSpaceBytes: REQUIRED_SPACE,
        chainSubdirectoryStatus: 'will-create',
      })
    );
  });

  it('returns valid when all checks pass', async () => {
    const checkDiskSpace = require('check-disk-space');
    checkDiskSpace.mockResolvedValue({ free: 4096 });
    (fs.pathExists as jest.Mock)
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(false);
    ((fs.realpath as unknown) as jest.Mock).mockImplementation(
      async (p: string) => p
    );
    (fs.stat as jest.Mock).mockResolvedValue({ isDirectory: () => true });
    (fs.access as jest.Mock).mockResolvedValue(undefined);

    const result = await validateChainStorageDirectory(
      '/mnt/external',
      STATE_DIR,
      makeGetDefaultConfig()
    );

    expect(result).toEqual({
      isValid: true,
      path: '/mnt/external',
      resolvedPath: '/mnt/external',
      availableSpaceBytes: 4096,
      requiredSpaceBytes: REQUIRED_SPACE,
      chainSubdirectoryStatus: 'will-create',
    });
  });

  it('succeeds when the state directory has not been created yet', async () => {
    const checkDiskSpace = require('check-disk-space');
    checkDiskSpace.mockResolvedValue({ free: 4096 });
    (fs.pathExists as jest.Mock)
      .mockResolvedValueOnce(true) // target dir exists
      .mockResolvedValueOnce(false) // state dir does not exist yet
      .mockResolvedValueOnce(false); // managed chain dir does not exist yet
    ((fs.realpath as unknown) as jest.Mock).mockImplementation(
      async (p: string) => p
    );
    (fs.stat as jest.Mock).mockResolvedValue({ isDirectory: () => true });
    (fs.access as jest.Mock).mockResolvedValue(undefined);

    const result = await validateChainStorageDirectory(
      '/mnt/external',
      STATE_DIR,
      makeGetDefaultConfig()
    );

    expect(result).toEqual({
      isValid: true,
      path: '/mnt/external',
      resolvedPath: '/mnt/external',
      availableSpaceBytes: 4096,
      requiredSpaceBytes: REQUIRED_SPACE,
      chainSubdirectoryStatus: 'will-create',
    });
  });

  it('reports an existing managed chain subdirectory when present', async () => {
    const checkDiskSpace = require('check-disk-space');
    checkDiskSpace.mockResolvedValue({ free: 4096 });
    (fs.pathExists as jest.Mock)
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(true);
    ((fs.realpath as unknown) as jest.Mock).mockImplementation(
      async (p: string) => p
    );
    (fs.stat as jest.Mock).mockResolvedValue({ isDirectory: () => true });
    (fs.lstat as jest.Mock).mockResolvedValue({
      isDirectory: () => true,
    });
    (fs.access as jest.Mock).mockResolvedValue(undefined);

    const result = await validateChainStorageDirectory(
      '/mnt/external',
      STATE_DIR,
      makeGetDefaultConfig()
    );

    expect(result).toEqual({
      isValid: true,
      path: '/mnt/external',
      resolvedPath: '/mnt/external',
      availableSpaceBytes: 4096,
      requiredSpaceBytes: REQUIRED_SPACE,
      chainSubdirectoryStatus: 'existing-directory',
    });
  });

  it('rejects selecting the already managed chain child directory', async () => {
    (fs.pathExists as jest.Mock)
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(true);
    ((fs.realpath as unknown) as jest.Mock).mockImplementation(
      async (targetPath: string) => targetPath
    );
    (fs.stat as jest.Mock).mockResolvedValue({ isDirectory: () => true });

    const result = await validateChainStorageDirectory(
      '/mnt/external/chain',
      STATE_DIR,
      makeGetDefaultConfig(),
      REQUIRED_SPACE,
      {
        currentCustomPath: '/mnt/external',
      }
    );

    expect(result).toEqual(
      expect.objectContaining({
        isValid: false,
        path: '/mnt/external/chain',
        resolvedPath: '/mnt/external/chain',
        reason: 'is-managed-child',
      })
    );
  });

  it('rejects a selected parent when the managed chain subdirectory is a file', async () => {
    (fs.pathExists as jest.Mock)
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(true);
    ((fs.realpath as unknown) as jest.Mock).mockImplementation(
      async (p: string) => p
    );
    (fs.stat as jest.Mock).mockResolvedValue({ isDirectory: () => true });
    (fs.lstat as jest.Mock).mockResolvedValue({
      isDirectory: () => false,
    });
    (fs.access as jest.Mock).mockResolvedValue(undefined);

    const result = await validateChainStorageDirectory(
      '/mnt/external',
      STATE_DIR,
      makeGetDefaultConfig()
    );

    expect(result).toEqual(
      expect.objectContaining({
        isValid: false,
        path: '/mnt/external',
        resolvedPath: '/mnt/external',
        reason: 'path-is-file',
        chainSubdirectoryStatus: 'path-is-file',
      })
    );
  });

  it('returns unknown reason when an unexpected error is thrown', async () => {
    (fs.pathExists as jest.Mock).mockRejectedValue(new Error('disk error'));

    const result = await validateChainStorageDirectory(
      '/mnt/external',
      STATE_DIR,
      makeGetDefaultConfig()
    );

    expect(result).toEqual(
      expect.objectContaining({
        isValid: false,
        reason: 'unknown',
      })
    );
  });
});
