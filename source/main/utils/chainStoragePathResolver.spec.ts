import fs from 'fs-extra';
import {
  resolveStateDirectoryPath,
  resolveChainStoragePath,
  resolveMithrilWorkDir,
} from './chainStoragePathResolver';

jest.mock('fs-extra', () => ({
  pathExists: jest.fn(),
  realpath: jest.fn(),
  lstat: jest.fn(),
  readlink: jest.fn(),
}));

jest.mock('./logging', () => ({
  logger: {
    warn: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
  },
}));

describe('resolveStateDirectoryPath', () => {
  beforeEach(() => jest.clearAllMocks());

  it('returns exists=true and realpath when directory exists', async () => {
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(
      '/resolved/state'
    );

    const result = await resolveStateDirectoryPath('/tmp/state');

    expect(result).toEqual({ exists: true, resolvedPath: '/resolved/state' });
  });

  it('returns exists=false and resolved path when directory does not exist', async () => {
    (fs.pathExists as jest.Mock).mockResolvedValue(false);

    const result = await resolveStateDirectoryPath('/tmp/state');

    expect(result).toEqual({ exists: false, resolvedPath: '/tmp/state' });
    expect(fs.realpath).not.toHaveBeenCalled();
  });
});

describe('resolveChainStoragePath', () => {
  beforeEach(() => jest.clearAllMocks());

  it('returns symlink target when chain entry point is a symlink', async () => {
    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => true,
      isDirectory: () => false,
    });
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(
      '/mnt/external/chain'
    );

    const result = await resolveChainStoragePath('/tmp/state');

    expect(result).toBe('/mnt/external/chain');
  });

  it('returns entry-point chain path when chain entry point is a plain directory', async () => {
    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => false,
      isDirectory: () => true,
    });

    const result = await resolveChainStoragePath('/tmp/state');

    expect(result).toBe('/tmp/state/chain');
  });

  it('returns entry-point chain path when chain entry point is missing', async () => {
    ((fs.lstat as unknown) as jest.Mock).mockRejectedValue(
      Object.assign(new Error('missing'), { code: 'ENOENT' })
    );

    const result = await resolveChainStoragePath('/tmp/state');

    expect(result).toBe('/tmp/state/chain');
  });

  it('resolves junction target on win32 when directory entry is a junction', async () => {
    const originalPlatform = process.platform;
    Object.defineProperty(process, 'platform', {
      value: 'win32',
      configurable: true,
    });

    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => false,
      isDirectory: () => true,
    });
    (fs.readlink as jest.Mock).mockResolvedValue('C:\\target\\chain');
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(
      'C:\\target\\chain'
    );

    const result = await resolveChainStoragePath('/tmp/state');

    expect(result).toBe('C:\\target\\chain');

    Object.defineProperty(process, 'platform', {
      value: originalPlatform,
      configurable: true,
    });
  });

  it('returns entry-point chain path on win32 when directory is not a junction', async () => {
    const originalPlatform = process.platform;
    Object.defineProperty(process, 'platform', {
      value: 'win32',
      configurable: true,
    });

    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => false,
      isDirectory: () => true,
    });
    ((fs.readlink as unknown) as jest.Mock).mockRejectedValue(
      Object.assign(new Error('not a link'), { code: 'EINVAL' })
    );

    const result = await resolveChainStoragePath('/tmp/state');

    expect(result).toBe('/tmp/state/chain');

    Object.defineProperty(process, 'platform', {
      value: originalPlatform,
      configurable: true,
    });
  });
});

describe('resolveMithrilWorkDir', () => {
  beforeEach(() => jest.clearAllMocks());

  it('returns symlink target when chain entry point is a symlink', async () => {
    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => true,
      isDirectory: () => false,
    });
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(
      '/mnt/external/chain'
    );

    const result = await resolveMithrilWorkDir('/tmp/state');

    expect(result).toBe('/mnt/external/chain');
  });

  it('returns entry-point chain path when chain entry point is a plain directory', async () => {
    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => false,
      isDirectory: () => true,
    });

    const result = await resolveMithrilWorkDir('/tmp/state');

    expect(result).toBe('/tmp/state/chain');
  });

  it('returns entry-point chain path when chain entry point is missing', async () => {
    ((fs.lstat as unknown) as jest.Mock).mockRejectedValue(
      Object.assign(new Error('missing'), { code: 'ENOENT' })
    );

    const result = await resolveMithrilWorkDir('/tmp/state');

    expect(result).toBe('/tmp/state/chain');
  });
});
