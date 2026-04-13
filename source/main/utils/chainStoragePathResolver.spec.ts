import fs from 'fs-extra';
import {
  resolveStateDirectoryPath,
  resolveCurrentChainSource,
  resolveChainStoragePath,
  resolveMithrilWorkDir,
} from './chainStoragePathResolver';
import type { ChainStorageConfig } from '../../common/types/mithril-bootstrap.types';

jest.mock('fs-extra', () => ({
  pathExists: jest.fn(),
  realpath: jest.fn(),
  lstat: jest.fn(),
  readJson: jest.fn(),
}));

jest.mock('./logging', () => ({
  logger: {
    warn: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
  },
}));

const makeConfig = (customPath: string | null = null): ChainStorageConfig => ({
  customPath,
  defaultPath: '/tmp/state/chain',
  availableSpaceBytes: 4096,
  requiredSpaceBytes: 1024,
});

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

describe('resolveCurrentChainSource', () => {
  beforeEach(() => jest.clearAllMocks());

  it('returns null when chain path does not exist', async () => {
    (fs.pathExists as jest.Mock).mockResolvedValue(false);

    const result = await resolveCurrentChainSource('/tmp/state/chain');

    expect(result).toBeNull();
  });

  it('returns symlink target when chain path is a symlink', async () => {
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => true,
      isDirectory: () => false,
    });
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(
      '/mnt/external/chain'
    );

    const result = await resolveCurrentChainSource('/tmp/state/chain');

    expect(result).toBe('/mnt/external/chain');
  });

  it('returns null when symlink cannot be resolved', async () => {
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => true,
      isDirectory: () => false,
    });
    ((fs.realpath as unknown) as jest.Mock).mockRejectedValue(
      new Error('broken symlink')
    );

    const result = await resolveCurrentChainSource('/tmp/state/chain');

    expect(result).toBeNull();
  });

  it('returns the chain path itself when it is a plain directory', async () => {
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => false,
      isDirectory: () => true,
    });

    const result = await resolveCurrentChainSource('/tmp/state/chain');

    expect(result).toBe('/tmp/state/chain');
  });

  it('returns null when path is neither symlink nor directory', async () => {
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => false,
      isDirectory: () => false,
    });

    const result = await resolveCurrentChainSource('/tmp/state/chain');

    expect(result).toBeNull();
  });
});

describe('resolveChainStoragePath', () => {
  beforeEach(() => jest.clearAllMocks());

  it('returns realpath of chain path when it exists', async () => {
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(
      '/mnt/chain-target'
    );
    const getConfig = jest.fn().mockResolvedValue(makeConfig(null));

    const result = await resolveChainStoragePath('/tmp/state', getConfig);

    expect(result).toBe('/mnt/chain-target');
    expect(getConfig).not.toHaveBeenCalled();
  });

  it('falls back to custom path from config when chain path does not exist', async () => {
    (fs.pathExists as jest.Mock).mockResolvedValue(false);
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(
      '/mnt/custom-chain'
    );
    const getConfig = jest
      .fn()
      .mockResolvedValue(makeConfig('/mnt/custom-chain'));

    const result = await resolveChainStoragePath('/tmp/state', getConfig);

    expect(result).toBe('/mnt/custom-chain');
  });

  it('falls back to stateDir when chain path fails and no custom path configured', async () => {
    (fs.pathExists as jest.Mock).mockResolvedValue(false);
    const getConfig = jest.fn().mockResolvedValue(makeConfig(null));

    const result = await resolveChainStoragePath('/tmp/state', getConfig);

    expect(result).toBe('/tmp/state');
  });

  it('falls back to stateDir when custom path realpath fails', async () => {
    (fs.pathExists as jest.Mock).mockResolvedValue(false);
    ((fs.realpath as unknown) as jest.Mock).mockRejectedValue(
      new Error('no such path')
    );
    const getConfig = jest
      .fn()
      .mockResolvedValue(makeConfig('/mnt/missing-chain'));

    const result = await resolveChainStoragePath('/tmp/state', getConfig);

    expect(result).toBe('/tmp/state');
  });
});

describe('resolveMithrilWorkDir', () => {
  beforeEach(() => jest.clearAllMocks());

  it('returns stateDir when no custom path is configured', async () => {
    const getConfig = jest.fn().mockResolvedValue(makeConfig(null));

    const result = await resolveMithrilWorkDir('/tmp/state', getConfig);

    expect(result).toBe('/tmp/state');
    expect(fs.realpath).not.toHaveBeenCalled();
  });

  it('returns resolved custom path when configured', async () => {
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(
      '/mnt/custom-chain'
    );
    const getConfig = jest
      .fn()
      .mockResolvedValue(makeConfig('/mnt/custom-chain'));

    const result = await resolveMithrilWorkDir('/tmp/state', getConfig);

    expect(result).toBe('/mnt/custom-chain');
  });

  it('falls back to stateDir when realpath of custom path fails', async () => {
    ((fs.realpath as unknown) as jest.Mock).mockRejectedValue(
      new Error('path unavailable')
    );
    const getConfig = jest
      .fn()
      .mockResolvedValue(makeConfig('/mnt/missing-chain'));

    const result = await resolveMithrilWorkDir('/tmp/state', getConfig);

    expect(result).toBe('/tmp/state');
  });
});
