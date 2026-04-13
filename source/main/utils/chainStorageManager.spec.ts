import fs from 'fs-extra';
import { ChainStorageManager } from './chainStorageManager';

jest.mock('fs-extra', () => ({
  pathExists: jest.fn(),
  lstat: jest.fn(),
  readlink: jest.fn(),
  realpath: jest.fn(),
  stat: jest.fn(),
  access: jest.fn(),
  readdir: jest.fn(),
  ensureDir: jest.fn(),
  move: jest.fn(),
  symlink: jest.fn(),
  copy: jest.fn(),
  remove: jest.fn(),
  rename: jest.fn(),
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

  const createPathNotFoundError = (message = 'path unavailable') =>
    Object.assign(new Error(message), { code: 'ENOENT' });

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
      .spyOn(manager, '_resetToDefault')
      .mockResolvedValue({ isValid: true, path: null });

    const result = await manager.setDirectory(null);

    expect(resetSpy).toHaveBeenCalled();
    expect(result).toEqual({ isValid: true, path: null });
  });

  it('setDirectory with explicit default path resets to default chain storage', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    const resetSpy = jest
      .spyOn(manager, '_resetToDefault')
      .mockResolvedValue({ isValid: true, path: null });

    const result = await manager.setDirectory('/tmp/state/chain');

    expect(resetSpy).toHaveBeenCalled();
    expect(result).toEqual({ isValid: true, path: null });
  });

  it('setDirectory creates a symlink without persisting config state', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, 'validate').mockResolvedValue({
      isValid: true,
      path: '/tmp/custom-parent',
      resolvedPath: '/mnt/external/daedalus-parent',
      chainSubdirectoryStatus: 'will-create',
    });
    jest.spyOn(manager, 'getConfig').mockResolvedValue(createConfig(null));
    ((fs.realpath as unknown) as jest.Mock).mockRejectedValueOnce(
      createPathNotFoundError()
    );
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'directory',
      resolvedPath: '/tmp/state/chain',
    });
    (fs.readdir as jest.Mock).mockResolvedValue([]);

    const result = await manager.setDirectory('/tmp/custom-parent');

    expect(fs.remove).toHaveBeenCalledWith('/tmp/state/chain');
    expect(fs.ensureDir).toHaveBeenCalledWith('/tmp/custom-parent/chain');
    expect(fs.symlink).toHaveBeenCalledWith(
      '/tmp/custom-parent/chain',
      '/tmp/state/chain',
      process.platform === 'win32' ? 'junction' : 'dir'
    );
    expect(fs.writeJson).not.toHaveBeenCalled();
    expect(result).toEqual(
      expect.objectContaining({
        isValid: true,
        path: '/tmp/custom-parent',
        resolvedPath: '/mnt/external/daedalus-parent',
        chainSubdirectoryStatus: 'will-create',
      })
    );
  });

  it('setDirectory does not migrate when switching between custom locations', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, 'validate').mockResolvedValue({
      isValid: true,
      path: '/tmp/custom-parent',
      resolvedPath: '/mnt/external/daedalus-parent',
    });
    jest
      .spyOn(manager, 'getConfig')
      .mockResolvedValue(createConfig('/mnt/old-parent'));
    jest
      .spyOn(manager, '_resolveRealPathOrInput')
      .mockImplementation(async (targetPath: string) => targetPath);
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: '/mnt/old-parent/chain',
    });
    const migrateSpy = jest
      .spyOn(manager, 'migrateData')
      .mockResolvedValue(undefined);

    await manager.setDirectory('/tmp/custom-parent');

    expect(migrateSpy).not.toHaveBeenCalled();
    expect(fs.symlink).toHaveBeenCalled();
  });

  it('setDirectory canonicalizes selecting the current managed child back to the configured parent', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, 'getConfig')
      .mockResolvedValue(createConfig('/mnt/custom-parent'));
    jest
      .spyOn(manager, '_resolveRealPathOrInput')
      .mockImplementation(async (targetPath: string) => targetPath);
    const validateSpy = jest.spyOn(manager, 'validate').mockResolvedValue({
      isValid: true,
      path: '/mnt/custom-parent',
      resolvedPath: '/mnt/custom-parent',
      chainSubdirectoryStatus: 'existing-directory',
    });
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: '/mnt/custom-parent/chain',
    });

    await manager.setDirectory('/mnt/custom-parent/chain');

    expect(validateSpy).toHaveBeenCalledWith('/mnt/custom-parent');
    expect(fs.ensureDir).toHaveBeenCalledWith('/mnt/custom-parent/chain');
    expect(fs.ensureDir).not.toHaveBeenCalledWith(
      '/mnt/custom-parent/chain/chain'
    );
  });

  it('setDirectory rejects switching when default chain data already exists locally', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, 'validate').mockResolvedValue({
      isValid: true,
      path: '/tmp/new-custom-parent',
      resolvedPath: '/mnt/new-parent',
    });
    jest.spyOn(manager, 'getConfig').mockResolvedValue(createConfig(null));
    ((fs.realpath as unknown) as jest.Mock).mockRejectedValueOnce(
      createPathNotFoundError()
    );
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'directory',
      resolvedPath: '/tmp/state/chain',
    });
    (fs.readdir as jest.Mock).mockResolvedValue(['immutable']);

    const result = await manager.setDirectory('/tmp/new-custom-parent');

    expect(result).toEqual(
      expect.objectContaining({
        isValid: false,
        path: '/tmp/new-custom-parent',
        reason: 'unknown',
      })
    );
    expect(fs.symlink).not.toHaveBeenCalled();
  });

  it('setDirectory triggers rollback when symlink creation fails', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, 'validate').mockResolvedValue({
      isValid: true,
      path: '/tmp/custom-parent',
      resolvedPath: '/mnt/external/daedalus-parent',
    });
    jest.spyOn(manager, 'getConfig').mockResolvedValue(createConfig(null));
    ((fs.realpath as unknown) as jest.Mock).mockRejectedValueOnce(
      createPathNotFoundError()
    );
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'directory',
      resolvedPath: '/tmp/state/chain',
    });
    (fs.readdir as jest.Mock).mockResolvedValue([]);
    const rollbackSpy = jest
      .spyOn(manager, '_rollbackSetDirectory')
      .mockResolvedValue(undefined);
    (fs.symlink as jest.Mock).mockRejectedValue(new Error('symlink failed'));

    await expect(manager.setDirectory('/tmp/custom-parent')).rejects.toThrow(
      'symlink failed'
    );

    expect(rollbackSpy).toHaveBeenCalledWith(
      expect.objectContaining({
        targetPath: '/tmp/custom-parent/chain',
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

  it('resetToDefault removes the symlink without migrating data back', async () => {
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
    expect(result).toEqual(
      expect.objectContaining({
        isValid: true,
        path: null,
        resolvedPath: '/tmp/state/chain',
      })
    );
  });

  it('getConfig derives the active custom parent from the symlink target', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: '/mnt/actual-parent/chain',
    });
    manager._isRecoveryFallback = true;

    const result = await manager.getConfig();

    expect(result).toEqual(
      expect.objectContaining({
        customPath: '/mnt/actual-parent',
        defaultPath: '/tmp/state/chain',
        isRecoveryFallback: true,
      })
    );
  });

  it('getConfig ignores stale legacy config metadata and derives state from the live chain entry point', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: '/mnt/live-parent/chain',
    });
    (fs.readJson as jest.Mock).mockResolvedValue({
      customPath: '/mnt/stale-parent',
      setAt: '2026-04-01T00:00:00.000Z',
    });

    const result = await manager.getConfig();

    expect(result).toEqual(
      expect.objectContaining({
        customPath: '/mnt/live-parent',
      })
    );
    expect(fs.readJson).not.toHaveBeenCalled();
  });

  it('rollback restores the previous symlink target without rewriting config', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    (fs.symlink as jest.Mock).mockResolvedValue(undefined);
    const migrateSpy = jest
      .spyOn(manager, 'migrateData')
      .mockResolvedValue(undefined);

    await manager._rollbackSetDirectory({
      previousState: {
        type: 'symlink',
        resolvedPath: '/mnt/old-parent/chain',
      },
      targetPath: '/mnt/new-chain',
    });

    expect(migrateSpy).not.toHaveBeenCalled();
    expect(fs.symlink).toHaveBeenCalledWith(
      '/mnt/old-parent/chain',
      '/tmp/state/chain',
      process.platform === 'win32' ? 'junction' : 'dir'
    );
    expect(fs.writeJson).not.toHaveBeenCalled();
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
    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => true,
      isDirectory: () => false,
    });
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(
      '/mnt/chain-target'
    );

    const result = await manager.resolveChainStoragePath();

    expect(result).toBe('/mnt/chain-target');
  });

  it('resolveMithrilWorkDir resolves the active symlink target', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => true,
      isDirectory: () => false,
    });
    ((fs.realpath as unknown) as jest.Mock).mockResolvedValue(
      '/mnt/custom-parent/chain'
    );

    const result = await manager.resolveMithrilWorkDir();

    expect(result).toBe('/mnt/custom-parent/chain');
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
    jest.spyOn(manager, 'getConfig').mockResolvedValue(createConfig(null));
    (fs.pathExists as jest.Mock)
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(false)
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
      chainSubdirectoryStatus: 'will-create',
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

  it('_detectLayout identifies a legacy direct-target custom root', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, '_resolveExistingDirectory')
      .mockResolvedValue('/mnt/custom-parent');
    jest.spyOn(manager, '_safeLstat').mockResolvedValue(null);
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: '/mnt/custom-parent',
    });
    jest.spyOn(manager, '_listLegacyManagedEntries').mockResolvedValue({
      managedEntries: ['immutable'],
      ignoredEntries: ['note.txt'],
    });

    const result = await manager._detectLayout('/mnt/custom-parent');

    expect(result).toMatchObject({
      kind: 'legacy-custom-root',
      customPath: '/mnt/custom-parent',
      managedChainPath: '/mnt/custom-parent/chain',
      currentChainSource: '/mnt/custom-parent',
      managedLegacyEntries: ['immutable'],
      ignoredLegacyEntries: ['note.txt'],
    });
  });

  it('_detectLayout identifies an already managed custom root', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, '_resolveExistingDirectory')
      .mockResolvedValue('/mnt/custom-parent');
    jest.spyOn(manager, '_safeLstat').mockResolvedValue({
      isDirectory: () => true,
    } as fs.Stats);
    jest
      .spyOn(manager, '_resolveRealPathOrInput')
      .mockResolvedValue('/mnt/custom-parent/chain');
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: '/mnt/custom-parent/chain',
    });
    jest.spyOn(manager, '_listLegacyManagedEntries').mockResolvedValue({
      managedEntries: [],
      ignoredEntries: [],
    });

    const result = await manager._detectLayout('/mnt/custom-parent');

    expect(result).toMatchObject({
      kind: 'managed-custom-root',
      managedChainPath: '/mnt/custom-parent/chain',
      resolvedManagedChainPath: '/mnt/custom-parent/chain',
      currentChainSource: '/mnt/custom-parent/chain',
    });
  });

  it('_detectLayout resolves a symlinked custom parent before deriving the managed path', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, '_resolveExistingDirectory')
      .mockResolvedValue('/mnt/actual-parent');
    jest.spyOn(manager, '_safeLstat').mockResolvedValue({
      isDirectory: () => true,
    } as fs.Stats);
    jest
      .spyOn(manager, '_resolveRealPathOrInput')
      .mockResolvedValue('/mnt/actual-parent/chain');
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: '/mnt/actual-parent/chain',
    });
    jest.spyOn(manager, '_listLegacyManagedEntries').mockResolvedValue({
      managedEntries: [],
      ignoredEntries: [],
    });

    const result = await manager._detectLayout('/mnt/alias-parent');

    expect(result).toMatchObject({
      kind: 'managed-custom-root',
      customPath: '/mnt/alias-parent',
      resolvedCustomPath: '/mnt/actual-parent',
      managedChainPath: '/mnt/actual-parent/chain',
      resolvedManagedChainPath: '/mnt/actual-parent/chain',
      currentChainSource: '/mnt/actual-parent/chain',
    });
  });

  it('_detectLayout marks a local chain directory as inconsistent when the managed child already exists', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, '_resolveExistingDirectory')
      .mockResolvedValue('/mnt/custom-parent');
    jest.spyOn(manager, '_safeLstat').mockResolvedValue({
      isDirectory: () => true,
    } as fs.Stats);
    jest
      .spyOn(manager, '_resolveRealPathOrInput')
      .mockResolvedValue('/mnt/custom-parent/chain');
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'directory',
      resolvedPath: '/tmp/state/chain',
    });
    jest.spyOn(manager, '_listLegacyManagedEntries').mockResolvedValue({
      managedEntries: [],
      ignoredEntries: [],
    });

    const result = await manager._detectLayout('/mnt/custom-parent');

    expect(result).toMatchObject({
      kind: 'inconsistent',
      managedChainPath: '/mnt/custom-parent/chain',
      currentChainSource: '/tmp/state/chain',
    });
  });

  it('_detectLayout identifies a broken link when neither managed nor legacy data is present', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, '_resolveExistingDirectory')
      .mockResolvedValue('/mnt/custom-parent');
    jest.spyOn(manager, '_safeLstat').mockResolvedValue(null);
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
    });
    jest.spyOn(manager, '_listLegacyManagedEntries').mockResolvedValue({
      managedEntries: [],
      ignoredEntries: [],
    });

    const result = await manager._detectLayout('/mnt/custom-parent');

    expect(result).toMatchObject({
      kind: 'broken-link',
      managedChainPath: '/mnt/custom-parent/chain',
      currentChainSource: null,
      managedLegacyEntries: [],
    });
  });

  it('_migrateLegacyCustomLayout records journal progress through completion', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    const writtenStates: string[] = [];

    jest
      .spyOn(manager, '_preflightLegacyMigration')
      .mockResolvedValue(undefined);
    jest
      .spyOn(manager, '_writeMigrationJournal')
      .mockImplementation(async (journal) => {
        writtenStates.push(journal.state);
      });
    jest.spyOn(manager, '_movePath').mockResolvedValue(undefined);
    jest.spyOn(manager, '_createSymlink').mockResolvedValue(undefined);
    jest.spyOn(manager, '_pathExistsViaLstat').mockResolvedValue(false);

    await manager._migrateLegacyCustomLayout({
      kind: 'legacy-custom-root',
      customPath: '/mnt/custom-parent',
      resolvedCustomPath: '/mnt/custom-parent',
      managedChainPath: '/mnt/custom-parent/chain',
      resolvedManagedChainPath: undefined,
      currentChainSource: '/mnt/custom-parent',
      entryPointState: {
        type: 'symlink',
        resolvedPath: '/mnt/custom-parent',
      },
      managedChainExists: false,
      managedChainIsDirectory: false,
      managedLegacyEntries: ['immutable', 'ledger'],
      ignoredLegacyEntries: ['note.txt'],
    });

    expect(writtenStates).toEqual([
      'start',
      'progress',
      'progress',
      'cutover',
      'completion',
    ]);
    expect(fs.rename).toHaveBeenCalledWith(
      '/tmp/state/chain.managed-next',
      '/tmp/state/chain'
    );
    expect(fs.remove).toHaveBeenCalledWith(
      '/tmp/state/Logs/chain-storage-migration-journal.json'
    );
  });

  it('_recoverInterruptedMigration cleans up a completed cutover on restart', async () => {
    const manager = new ChainStorageManager('/tmp/state');

    jest.spyOn(manager, '_readMigrationJournal').mockResolvedValue({
      state: 'cutover',
      customPath: '/mnt/custom-parent',
      legacyRootPath: '/mnt/custom-parent',
      managedChainPath: '/mnt/custom-parent/chain',
      movedEntries: ['immutable'],
      ignoredEntries: [],
      backupEntryPointPath: '/tmp/state/chain.legacy-backup',
      tempEntryPointPath: '/tmp/state/chain.managed-next',
      createdAt: '2026-04-03T00:00:00.000Z',
      updatedAt: '2026-04-03T00:00:00.000Z',
    });
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: '/mnt/custom-parent/chain',
    });
    jest.spyOn(manager, '_pathExistsViaLstat').mockResolvedValue(true);

    await manager._recoverInterruptedMigration({});

    expect(fs.remove).toHaveBeenCalledWith('/tmp/state/chain.legacy-backup');
    expect(fs.remove).toHaveBeenCalledWith('/tmp/state/chain.managed-next');
    expect(fs.remove).toHaveBeenCalledWith(
      '/tmp/state/Logs/chain-storage-migration-journal.json'
    );
  });

  it('ensureManagedChainLayout falls back to default storage when a broken custom target cannot be recovered', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, '_recoverInterruptedMigration')
      .mockResolvedValue(undefined);
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      linkTargetPath: '/mnt/custom-parent/chain',
    });
    jest.spyOn(manager, '_detectLayout').mockResolvedValue({
      kind: 'broken-link',
      customPath: '/mnt/custom-parent',
      resolvedCustomPath: '/mnt/custom-parent',
      managedChainPath: '/mnt/custom-parent/chain',
      resolvedManagedChainPath: undefined,
      currentChainSource: null,
      entryPointState: {
        type: 'symlink',
        linkTargetPath: '/mnt/custom-parent/chain',
      },
      managedChainExists: false,
      managedChainIsDirectory: false,
      managedLegacyEntries: [],
      ignoredLegacyEntries: [],
    });
    (fs.pathExists as jest.Mock).mockResolvedValue(true);

    const result = await manager.ensureManagedChainLayout();

    expect(fs.remove).toHaveBeenCalledWith('/tmp/state/chain');
    expect(fs.ensureDir).toHaveBeenCalledWith('/tmp/state/chain');
    expect(result).toEqual({
      managedChainPath: '/tmp/state/chain',
      isRecoveryFallback: true,
    });
    expect(manager._isRecoveryFallback).toBe(true);
  });

  it('ensureManagedChainLayout keeps the recovery fallback flag set for the rest of the session', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, '_ensureManagedChainLayout')
      .mockResolvedValueOnce({
        managedChainPath: '/tmp/state/chain',
        isRecoveryFallback: true,
      })
      .mockResolvedValueOnce({
        managedChainPath: '/tmp/state/chain',
        isRecoveryFallback: false,
      });

    await manager.ensureManagedChainLayout();
    await manager.ensureManagedChainLayout();

    expect(manager._isRecoveryFallback).toBe(true);
  });

  it('setDirectory clears the recovery fallback flag after selecting a new directory', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    manager._isRecoveryFallback = true;
    jest
      .spyOn(manager, '_canonicalizeManagedChildSelection')
      .mockResolvedValue('/mnt/custom-parent');
    jest.spyOn(manager, 'getConfig').mockResolvedValue(createConfig(null));
    jest.spyOn(manager, 'validate').mockResolvedValue({
      isValid: true,
      path: '/mnt/custom-parent',
      resolvedPath: '/mnt/custom-parent',
      chainSubdirectoryStatus: 'existing-directory',
    });
    jest
      .spyOn(manager, '_resolveRealPathOrInput')
      .mockResolvedValue('/mnt/custom-parent/chain');
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'directory',
      resolvedPath: '/tmp/state/chain',
    });
    (fs.readdir as jest.Mock).mockResolvedValue([]);
    jest.spyOn(manager, '_replaceCustomChainEntryPoint').mockResolvedValue();

    await manager.setDirectory('/mnt/custom-parent');

    expect(manager._isRecoveryFallback).toBe(false);
  });

  it('resetToDefault clears the recovery fallback flag', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    manager._isRecoveryFallback = true;
    jest.spyOn(manager, '_resetToDefault').mockResolvedValue({
      isValid: true,
      path: null,
      resolvedPath: '/tmp/state/chain',
      availableSpaceBytes: 4096,
      requiredSpaceBytes: 1024,
    });

    await manager.resetToDefault();

    expect(manager._isRecoveryFallback).toBe(false);
  });

  it('unlinkChainEntryPoint removes only the chain entry point', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: '/mnt/custom-parent/chain',
    });

    await manager.unlinkChainEntryPoint();

    expect(fs.remove).toHaveBeenCalledWith('/tmp/state/chain');
  });

  it('removeManagedDirectory removes the managed chain subdirectory', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, 'getConfig')
      .mockResolvedValue(createConfig('/mnt/custom-parent'));
    jest.spyOn(manager, '_pathExistsViaLstat').mockResolvedValue(true);

    await manager.removeManagedDirectory();

    expect(fs.remove).toHaveBeenCalledWith('/mnt/custom-parent/chain');
  });

  it('emptyManagedContents removes managed entries without deleting the managed directory', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, 'getConfig')
      .mockResolvedValue(createConfig('/mnt/custom-parent'));
    (fs.readdir as jest.Mock).mockResolvedValue(['db', 'immutable']);

    await manager.emptyManagedContents({
      excludeTopLevelEntries: ['db'],
    });

    expect(fs.ensureDir).toHaveBeenCalledWith('/mnt/custom-parent/chain');
    expect(fs.remove).toHaveBeenCalledWith(
      '/mnt/custom-parent/chain/immutable'
    );
    expect(fs.remove).not.toHaveBeenCalledWith('/mnt/custom-parent/chain/db');
    expect(fs.remove).not.toHaveBeenCalledWith('/mnt/custom-parent/chain');
  });
});
