import fs from 'fs-extra';
import {
  captureChainPathState,
  createSymlink,
} from './chainStorageManagerShared';

jest.mock('fs-extra', () => ({
  readlink: jest.fn(),
  realpath: jest.fn(),
  symlink: jest.fn(),
  stat: jest.fn(),
  remove: jest.fn(),
}));

jest.mock('./logging', () => ({
  logger: {
    warn: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
  },
}));

describe('captureChainPathState', () => {
  const originalPlatform = process.platform;

  beforeEach(() => {
    jest.clearAllMocks();
    Object.defineProperty(process, 'platform', {
      value: originalPlatform,
      configurable: true,
    });
  });

  afterEach(() => {
    Object.defineProperty(process, 'platform', {
      value: originalPlatform,
      configurable: true,
    });
  });

  it('classifies Windows junction-backed chain paths as symlinks for layout detection', async () => {
    Object.defineProperty(process, 'platform', {
      value: 'win32',
      configurable: true,
    });
    (fs.readlink as jest.Mock).mockResolvedValue('/custom-parent/chain');
    (fs.realpath as unknown as jest.Mock).mockResolvedValue(
      '/custom-parent/chain'
    );

    const result = await captureChainPathState({
      _chainPath: '/state/chain',
      _safeLstat: jest.fn().mockResolvedValue({
        isSymbolicLink: () => false,
        isDirectory: () => true,
      }),
    } as never);

    expect(result).toEqual({
      type: 'symlink',
      linkTargetPath: '/custom-parent/chain',
      resolvedPath: '/custom-parent/chain',
    });
  });
});

describe('createSymlink', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    (fs.symlink as unknown as jest.Mock).mockResolvedValue(undefined);
    (fs.remove as unknown as jest.Mock).mockResolvedValue(undefined);
  });

  it('returns without removing anything when the link resolves', async () => {
    (fs.realpath as unknown as jest.Mock).mockResolvedValue('/mnt/target');
    (fs.stat as unknown as jest.Mock).mockResolvedValue({
      isDirectory: () => true,
    });

    await expect(
      createSymlink('/mnt/target', '/state/chain')
    ).resolves.toBeUndefined();
    expect(fs.remove).not.toHaveBeenCalled();
  });

  // `fs.symlink` succeeds against a target that does not exist, so a caller
  // that skipped creating it would otherwise get a dangling chain entry point
  // reported as success.
  it('removes the link and raises when it does not resolve', async () => {
    (fs.realpath as unknown as jest.Mock).mockRejectedValue(
      Object.assign(new Error('no such file or directory'), { code: 'ENOENT' })
    );

    await expect(createSymlink('/mnt/gone', '/state/chain')).rejects.toThrow(
      'does not resolve to a directory'
    );
    expect(fs.remove).toHaveBeenCalledWith('/state/chain');
  });

  it('removes the link and raises when the target is not a directory', async () => {
    (fs.realpath as unknown as jest.Mock).mockResolvedValue('/mnt/a-file');
    (fs.stat as unknown as jest.Mock).mockResolvedValue({
      isDirectory: () => false,
    });

    await expect(createSymlink('/mnt/a-file', '/state/chain')).rejects.toThrow(
      'does not resolve to a directory'
    );
    expect(fs.remove).toHaveBeenCalledWith('/state/chain');
  });
});
