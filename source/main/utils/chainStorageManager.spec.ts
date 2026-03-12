import fs from 'fs-extra';
import { ChainStorageManager } from './chainStorageManager';

jest.mock('fs-extra', () => ({
  pathExists: jest.fn(),
  lstat: jest.fn(),
  realpath: jest.fn(),
  stat: jest.fn(),
  access: jest.fn(),
  readdir: jest.fn(),
  ensureDir: jest.fn(),
  move: jest.fn(),
  symlink: jest.fn(),
  copy: jest.fn(),
  remove: jest.fn(),
  writeJson: jest.fn(),
  readJson: jest.fn(),
  constants: {
    W_OK: 2,
  },
}));

jest.mock('check-disk-space', () => jest.fn());

jest.mock('../config', () => ({
  DISK_SPACE_REQUIRED: 1024,
  stateDirectoryPath: '/tmp/state',
}));

jest.mock('./logging', () => ({
  logger: {
    warn: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
  },
}));

describe('ChainStorageManager', () => {
  const createConfig = (customPath: string | null, setAt?: string) => ({
    customPath,
    defaultPath: '/tmp/state/chain',
    availableSpaceBytes: 4096,
    requiredSpaceBytes: 1024,
    setAt,
  });

  beforeEach(() => {
    jest.clearAllMocks();
    const checkDiskSpace = require('check-disk-space');
    checkDiskSpace.mockResolvedValue({ free: 4096 });
  });

  it('setDirectory returns validation response when invalid', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, 'validate').mockResolvedValue({
      isValid: false,
      path: '/tmp/custom-chain',
      reason: 'not-writable',
    });

    const result = await manager.setDirectory('/tmp/custom-chain');

    expect(result).toEqual(
      expect.objectContaining({
        isValid: false,
        reason: 'not-writable',
      })
    );
    expect(fs.symlink).not.toHaveBeenCalled();
    expect(fs.writeJson).not.toHaveBeenCalled();
  });

  it('setDirectory with null path resets to default chain storage', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    const resetSpy = jest
      .spyOn(manager, 'resetToDefault')
      .mockResolvedValue({ isValid: true, path: null });

    const result = await manager.setDirectory(null);

    expect(resetSpy).toHaveBeenCalled();
    expect(result).toEqual({ isValid: true, path: null });
  });

  it('setDirectory with explicit default path resets to default chain storage', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    const resetSpy = jest
      .spyOn(manager, 'resetToDefault')
      .mockResolvedValue({ isValid: true, path: null });

    const result = await manager.setDirectory('/tmp/state/chain');

    expect(resetSpy).toHaveBeenCalled();
    expect(result).toEqual({ isValid: true, path: null });
  });

  it('setDirectory creates a symlink and persists config without migrating data', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, 'validate').mockResolvedValue({
      isValid: true,
      path: '/tmp/custom-chain',
      resolvedPath: '/mnt/external/daedalus-chain',
    });
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'directory',
      resolvedPath: '/tmp/state/chain',
    });
    (fs.readdir as jest.Mock).mockResolvedValue([]);

    const result = await manager.setDirectory('/tmp/custom-chain');

    expect(fs.remove).toHaveBeenCalledWith('/tmp/state/chain');
    expect(fs.ensureDir).toHaveBeenCalledWith('/mnt/external/daedalus-chain');
    expect(fs.symlink).toHaveBeenCalledWith(
      '/mnt/external/daedalus-chain',
      '/tmp/state/chain',
      process.platform === 'win32' ? 'junction' : 'dir'
    );
    expect(fs.writeJson).toHaveBeenCalledWith(
      '/tmp/state/chain-storage-config.json',
      expect.objectContaining({
        customPath: '/tmp/custom-chain',
        setAt: expect.any(String),
      }),
      { spaces: 2 }
    );
    expect(result).toEqual(
      expect.objectContaining({
        isValid: true,
        path: '/tmp/custom-chain',
        resolvedPath: '/mnt/external/daedalus-chain',
      })
    );
  });

  it('setDirectory does not migrate when switching between custom locations', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, 'validate').mockResolvedValue({
      isValid: true,
      path: '/tmp/custom-chain',
      resolvedPath: '/mnt/external/daedalus-chain',
    });
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: '/mnt/old-chain',
    });
    const migrateSpy = jest
      .spyOn(manager, 'migrateData')
      .mockResolvedValue(undefined);

    await manager.setDirectory('/tmp/custom-chain');

    expect(migrateSpy).not.toHaveBeenCalled();
    expect(fs.symlink).toHaveBeenCalled();
  });

  it('setDirectory rejects switching when default chain data already exists locally', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, 'validate').mockResolvedValue({
      isValid: true,
      path: '/tmp/new-custom-chain',
      resolvedPath: '/mnt/new-chain',
    });
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'directory',
      resolvedPath: '/tmp/state/chain',
    });
    (fs.readdir as jest.Mock).mockResolvedValue(['immutable']);

    const result = await manager.setDirectory('/tmp/new-custom-chain');

    expect(result).toEqual(
      expect.objectContaining({
        isValid: false,
        path: '/tmp/new-custom-chain',
        reason: 'unknown',
      })
    );
    expect(fs.symlink).not.toHaveBeenCalled();
  });

  it('setDirectory triggers rollback when symlink creation fails', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, 'validate').mockResolvedValue({
      isValid: true,
      path: '/tmp/custom-chain',
      resolvedPath: '/mnt/external/daedalus-chain',
    });
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'directory',
      resolvedPath: '/tmp/state/chain',
    });
    (fs.readdir as jest.Mock).mockResolvedValue([]);
    jest.spyOn(manager, 'getConfig').mockResolvedValue(createConfig(null));
    const rollbackSpy = jest
      .spyOn(manager, '_rollbackSetDirectory')
      .mockResolvedValue(undefined);
    (fs.symlink as jest.Mock).mockRejectedValue(new Error('symlink failed'));

    await expect(manager.setDirectory('/tmp/custom-chain')).rejects.toThrow(
      'symlink failed'
    );

    expect(rollbackSpy).toHaveBeenCalledWith(
      expect.objectContaining({
        targetPath: '/mnt/external/daedalus-chain',
      })
    );
  });

  it('migrateData moves source entries into target and removes source directory', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    (fs.lstat as jest.Mock).mockResolvedValue({
      isDirectory: () => true,
    });
    (fs.readdir as jest.Mock).mockResolvedValue(['db', 'immutable']);

    await manager.migrateData('/tmp/state/chain', '/mnt/external/chain');

    expect(fs.ensureDir).toHaveBeenCalledWith('/mnt/external/chain');
    expect(fs.move).toHaveBeenCalledWith(
      '/tmp/state/chain/db',
      '/mnt/external/chain/db',
      { overwrite: true }
    );
    expect(fs.move).toHaveBeenCalledWith(
      '/tmp/state/chain/immutable',
      '/mnt/external/chain/immutable',
      { overwrite: true }
    );
    expect(fs.remove).toHaveBeenCalledWith('/tmp/state/chain');
  });

  it('migrateData uses copy+remove fallback for EXDEV move failures', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    (fs.lstat as jest.Mock).mockResolvedValue({
      isDirectory: () => true,
    });
    (fs.readdir as jest.Mock).mockResolvedValue(['db']);
    (fs.move as jest.Mock).mockRejectedValue(
      Object.assign(new Error('Cross-device link not permitted'), {
        code: 'EXDEV',
      })
    );

    await manager.migrateData('/tmp/state/chain', '/mnt/external/chain');

    expect(fs.copy).toHaveBeenCalledWith(
      '/tmp/state/chain/db',
      '/mnt/external/chain/db',
      { overwrite: true }
    );
    expect(fs.remove).toHaveBeenCalledWith('/tmp/state/chain/db');
    expect(fs.remove).toHaveBeenCalledWith('/tmp/state/chain');
  });

  it('resetToDefault removes the symlink and config without migrating data back', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => true,
    });
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue('/tmp/state');
    const migrateSpy = jest
      .spyOn(manager, 'migrateData')
      .mockResolvedValue(undefined);

    const result = await manager.resetToDefault();

    expect(fs.remove).toHaveBeenCalledWith('/tmp/state/chain');
    expect(fs.ensureDir).toHaveBeenCalledWith('/tmp/state/chain');
    expect(migrateSpy).not.toHaveBeenCalled();
    expect(fs.remove).toHaveBeenCalledWith(
      '/tmp/state/chain-storage-config.json'
    );
    expect(result).toEqual(
      expect.objectContaining({
        isValid: true,
        path: null,
        resolvedPath: '/tmp/state/chain',
      })
    );
  });

  it('verifySymlink returns invalid when symlink target does not match config', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, 'getConfig')
      .mockResolvedValue(createConfig('/mnt/configured-chain'));
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => true,
    });
    ((fs.realpath as unknown) as jest.Mock)
      .mockResolvedValueOnce('/mnt/actual-chain')
      .mockResolvedValueOnce('/mnt/configured-chain');

    const result = await manager.verifySymlink();

    expect(result).toEqual(
      expect.objectContaining({
        isValid: false,
        path: '/mnt/configured-chain',
        reason: 'unknown',
      })
    );
  });

  it('verifySymlink returns valid when symlink target matches config', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, 'getConfig')
      .mockResolvedValue(createConfig('/mnt/configured-chain'));
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => true,
    });
    ((fs.realpath as unknown) as jest.Mock)
      .mockResolvedValueOnce('/mnt/configured-chain')
      .mockResolvedValueOnce('/mnt/configured-chain');

    const result = await manager.verifySymlink();

    expect(result).toEqual({
      isValid: true,
      path: '/mnt/configured-chain',
      resolvedPath: '/mnt/configured-chain',
    });
  });

  it('rollback restores previous symlink target and config', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    (fs.symlink as jest.Mock).mockResolvedValue(undefined);
    const migrateSpy = jest
      .spyOn(manager, 'migrateData')
      .mockResolvedValue(undefined);

    await manager._rollbackSetDirectory({
      previousState: {
        type: 'symlink',
        resolvedPath: '/mnt/old-chain',
      },
      previousConfig: createConfig(
        '/mnt/old-chain',
        '2026-03-09T00:00:00.000Z'
      ),
      targetPath: '/mnt/new-chain',
    });

    expect(migrateSpy).not.toHaveBeenCalled();
    expect(fs.symlink).toHaveBeenCalledWith(
      '/mnt/old-chain',
      '/tmp/state/chain',
      process.platform === 'win32' ? 'junction' : 'dir'
    );
    expect(fs.writeJson).toHaveBeenCalledWith(
      '/tmp/state/chain-storage-config.json',
      {
        customPath: '/mnt/old-chain',
        setAt: '2026-03-09T00:00:00.000Z',
      },
      { spaces: 2 }
    );
  });

  it('migrateData can preserve the source root directory', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    (fs.lstat as jest.Mock).mockResolvedValue({
      isDirectory: () => true,
    });
    (fs.readdir as jest.Mock).mockResolvedValue(['db']);

    await manager.migrateData('/mnt/source-chain', '/mnt/target-chain', {
      preserveSourceRoot: true,
    });

    expect(fs.move).toHaveBeenCalledWith(
      '/mnt/source-chain/db',
      '/mnt/target-chain/db',
      { overwrite: true }
    );
    expect(fs.remove).not.toHaveBeenCalledWith('/mnt/source-chain');
  });

  it('resolveChainStoragePath prefers resolved chain symlink target', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(
      '/mnt/chain-target'
    );

    const result = await manager.resolveChainStoragePath();

    expect(result).toBe('/mnt/chain-target');
  });

  it('resolveMithrilWorkDir resolves configured custom path', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, 'getConfig')
      .mockResolvedValue(createConfig('/mnt/custom-chain'));
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(
      '/mnt/custom-chain'
    );

    const result = await manager.resolveMithrilWorkDir();

    expect(result).toBe('/mnt/custom-chain');
  });

  it('validate treats the default chain path as a valid default selection', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    const checkDiskSpace = require('check-disk-space');
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue('/tmp/state');
    checkDiskSpace.mockResolvedValue({ free: 4096 });

    const result = await manager.validate('/tmp/state/chain');

    expect(result).toEqual({
      isValid: true,
      path: null,
      resolvedPath: '/tmp/state/chain',
      availableSpaceBytes: 4096,
      requiredSpaceBytes: 1024,
    });
  });

  it('validate succeeds when the state directory has not been created yet', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    const checkDiskSpace = require('check-disk-space');
    (fs.pathExists as jest.Mock)
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(false);
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(
      '/mnt/custom-chain'
    );
    (fs.stat as jest.Mock).mockResolvedValue({
      isDirectory: () => true,
    });
    (fs.access as jest.Mock).mockResolvedValue(undefined);
    checkDiskSpace.mockResolvedValue({ free: 4096 });

    const result = await manager.validate('/mnt/custom-chain');

    expect(result).toEqual({
      isValid: true,
      path: '/mnt/custom-chain',
      resolvedPath: '/mnt/custom-chain',
      availableSpaceBytes: 4096,
      requiredSpaceBytes: 1024,
    });
  });

  it('getConfig falls back to unavailable disk space when default metadata cannot be resolved', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    const checkDiskSpace = require('check-disk-space');
    checkDiskSpace.mockRejectedValue(new Error('disk unavailable'));

    const result = await manager.getConfig();

    expect(result.customPath).toBeNull();
    expect(result.defaultPath).toBe('/tmp/state/chain');
    expect(Number.isNaN(result.availableSpaceBytes)).toBe(true);
    expect(result.requiredSpaceBytes).toBe(1024);
  });
});
