import path from 'path';
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

// Path fixtures are derived rather than written as separator-specific literals.
// The code under test builds paths with `path.join`, and turns a custom storage
// parent into a managed chain path with `path.join(path.resolve(parent), …)`.
// On Windows those produce backslashes, and `path.resolve` also qualifies the
// path with the current drive, so a POSIX literal for a managed chain path
// matches on POSIX and nowhere else. Building the expectation the same way the
// code does keeps these assertions about the path rather than about the
// platform.
const STATE_DIR = '/tmp/state';
const STATE_CHAIN = path.join(STATE_DIR, 'chain');
const CUSTOM_PARENT = path.resolve('/mnt/custom-parent');
const CUSTOM_CHAIN = path.join(CUSTOM_PARENT, 'chain');
const TMP_CUSTOM_CHAIN = path.join(path.resolve('/tmp/custom-parent'), 'chain');
const ACTUAL_PARENT_CHAIN = path.join(
  path.resolve('/mnt/actual-parent'),
  'chain'
);

describe('ChainStorageManager', () => {
  const createConfig = (customPath: string | null) => ({
    customPath,
    defaultPath: STATE_CHAIN,
    availableSpaceBytes: 4096,
    requiredSpaceBytes: 1024,
  });

  const createPathNotFoundError = (message = 'path unavailable') =>
    Object.assign(new Error(message), { code: 'ENOENT' });

  beforeEach(() => {
    jest.clearAllMocks();
    const checkDiskSpace = require('check-disk-space');
    checkDiskSpace.mockResolvedValue({ free: 4096 });

    // createSymlink verifies that the link it just created resolves through to
    // a directory, so the default here has to be a link that works. Tests that
    // care override these; the `...Once` rejections elsewhere still take
    // precedence for the call they target and then fall back to this.
    (fs.realpath as unknown as jest.Mock).mockImplementation(
      async (targetPath: string) => targetPath
    );
    (fs.stat as jest.Mock).mockResolvedValue({ isDirectory: () => true });
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

    const result = await manager.setDirectory(STATE_CHAIN);

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
    (fs.realpath as unknown as jest.Mock).mockRejectedValueOnce(
      createPathNotFoundError()
    );
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'directory',
      resolvedPath: STATE_CHAIN,
    });
    (fs.readdir as jest.Mock).mockResolvedValue([]);

    const result = await manager.setDirectory('/tmp/custom-parent');

    expect(fs.remove).toHaveBeenCalledWith(STATE_CHAIN);
    expect(fs.ensureDir).toHaveBeenCalledWith(TMP_CUSTOM_CHAIN);
    expect(fs.symlink).toHaveBeenCalledWith(
      TMP_CUSTOM_CHAIN,
      STATE_CHAIN,
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
    await manager.setDirectory('/tmp/custom-parent');

    expect(fs.move).not.toHaveBeenCalled();
    expect(fs.symlink).toHaveBeenCalled();
  });

  it('setDirectory canonicalizes selecting the current managed child back to the configured parent', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    const validateSpy = jest.spyOn(manager, 'validate').mockResolvedValue({
      isValid: true,
      path: CUSTOM_PARENT,
      resolvedPath: CUSTOM_PARENT,
      chainSubdirectoryStatus: 'existing-directory',
    });
    jest
      .spyOn(manager, 'getConfig')
      .mockResolvedValue(createConfig(CUSTOM_PARENT));
    jest
      .spyOn(manager, '_resolveRealPathOrInput')
      .mockImplementation(async (targetPath: string) => targetPath);
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: CUSTOM_CHAIN,
    });

    await manager.setDirectory(CUSTOM_CHAIN);

    expect(validateSpy).toHaveBeenCalledWith(CUSTOM_CHAIN);
    expect(fs.ensureDir).toHaveBeenCalledWith(CUSTOM_CHAIN);
    expect(fs.ensureDir).not.toHaveBeenCalledWith(
      path.join(CUSTOM_CHAIN, 'chain')
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
    (fs.realpath as unknown as jest.Mock).mockRejectedValueOnce(
      createPathNotFoundError()
    );
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'directory',
      resolvedPath: STATE_CHAIN,
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
    (fs.realpath as unknown as jest.Mock).mockRejectedValueOnce(
      createPathNotFoundError()
    );
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'directory',
      resolvedPath: STATE_CHAIN,
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
        targetPath: TMP_CUSTOM_CHAIN,
      })
    );
  });

  it('resetToDefault removes the symlink without migrating data back', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => true,
    });
    (fs.realpath as unknown as jest.Mock).mockResolvedValue('/tmp/state');

    const result = await manager.resetToDefault();

    expect(fs.remove).toHaveBeenCalledWith(STATE_CHAIN);
    expect(fs.ensureDir).toHaveBeenCalledWith(STATE_CHAIN);
    expect(fs.move).not.toHaveBeenCalled();
    expect(result).toEqual(
      expect.objectContaining({
        isValid: true,
        path: null,
        resolvedPath: STATE_CHAIN,
      })
    );
  });

  it('getConfig derives the active custom parent from the symlink target', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: ACTUAL_PARENT_CHAIN,
    });
    manager._isRecoveryFallback = true;

    const result = await manager.getConfig();

    expect(result).toEqual(
      expect.objectContaining({
        customPath: '/mnt/actual-parent',
        defaultPath: STATE_CHAIN,
        isRecoveryFallback: true,
      })
    );
  });

  it('getConfig ignores stale legacy config metadata and derives state from the live chain entry point', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: path.join(path.resolve('/mnt/live-parent'), 'chain'),
    });

    const result = await manager.getConfig();

    expect(result).toEqual(
      expect.objectContaining({
        customPath: '/mnt/live-parent',
      })
    );
  });

  it('getManagedChainPath derives the custom parent without probing disk space', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    const checkDiskSpace = require('check-disk-space');
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: CUSTOM_CHAIN,
    });

    const result = await manager.getManagedChainPath();

    expect(result).toBe(CUSTOM_CHAIN);
    expect(checkDiskSpace).not.toHaveBeenCalled();
  });

  it('getManagedChainPath falls back to the default chain path when the entry point is unreadable', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    const checkDiskSpace = require('check-disk-space');
    jest
      .spyOn(manager, '_captureChainPathState')
      .mockRejectedValue(
        Object.assign(new Error('denied'), { code: 'EACCES' })
      );

    const result = await manager.getManagedChainPath();

    expect(result).toBe(STATE_CHAIN);
    expect(checkDiskSpace).not.toHaveBeenCalled();
  });

  it('resolveDiskSpaceCheckPath returns the managed chain directory without probing disk space', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    const checkDiskSpace = require('check-disk-space');
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: CUSTOM_CHAIN,
    });
    (fs.pathExists as jest.Mock).mockResolvedValue(true);

    const result = await manager.resolveDiskSpaceCheckPath();

    expect(result).toBe(CUSTOM_CHAIN);
    expect(checkDiskSpace).not.toHaveBeenCalled();
  });

  it('resolveDiskSpaceCheckPath falls back to the parent when the managed chain is missing', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: CUSTOM_CHAIN,
    });
    (fs.pathExists as jest.Mock).mockResolvedValue(false);

    const result = await manager.resolveDiskSpaceCheckPath();

    expect(result).toBe(CUSTOM_PARENT);
  });

  it('rollback restores the previous symlink target without rewriting config', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    (fs.symlink as jest.Mock).mockResolvedValue(undefined);

    await manager._rollbackSetDirectory({
      previousState: {
        type: 'symlink',
        resolvedPath: '/mnt/old-parent/chain',
      },
      targetPath: '/mnt/new-chain',
    });

    expect(fs.move).not.toHaveBeenCalled();
    expect(fs.symlink).toHaveBeenCalledWith(
      '/mnt/old-parent/chain',
      STATE_CHAIN,
      process.platform === 'win32' ? 'junction' : 'dir'
    );
    expect(fs.writeJson).not.toHaveBeenCalled();
  });

  it('rollback restores a broken symlink using linkTargetPath when resolvedPath is undefined', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    (fs.symlink as jest.Mock).mockResolvedValue(undefined);
    (fs.ensureDir as jest.Mock).mockRejectedValueOnce(new Error('ENOENT'));

    await manager._rollbackSetDirectory({
      previousState: {
        type: 'symlink',
        linkTargetPath: '/nonexistent/mount/chain',
        resolvedPath: undefined,
      },
      targetPath: '/mnt/new-chain',
    });

    expect(fs.symlink).toHaveBeenCalledWith(
      '/nonexistent/mount/chain',
      STATE_CHAIN,
      process.platform === 'win32' ? 'junction' : 'dir'
    );
  });

  it('resolveMithrilWorkDir resolves the active symlink target', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    (fs.lstat as jest.Mock).mockResolvedValue({
      isSymbolicLink: () => true,
      isDirectory: () => false,
    });
    (fs.realpath as unknown as jest.Mock).mockResolvedValue(CUSTOM_CHAIN);

    const result = await manager.resolveMithrilWorkDir();

    expect(result).toBe(CUSTOM_CHAIN);
  });

  it('validate treats the default chain path as a valid default selection', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    const checkDiskSpace = require('check-disk-space');
    (fs.pathExists as jest.Mock).mockResolvedValue(true);
    (fs.realpath as unknown as jest.Mock).mockResolvedValue('/tmp/state');
    checkDiskSpace.mockResolvedValue({ free: 4096 });

    const result = await manager.validate(STATE_CHAIN);

    expect(result).toEqual({
      isValid: true,
      path: null,
      resolvedPath: STATE_CHAIN,
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
    (fs.realpath as unknown as jest.Mock).mockImplementation((p: string) =>
      Promise.resolve(p === '/mnt/custom-chain' ? '/mnt/custom-chain' : p)
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
    expect(result.defaultPath).toBe(STATE_CHAIN);
    expect(Number.isNaN(result.availableSpaceBytes)).toBe(true);
    expect(result.requiredSpaceBytes).toBe(1024);
  });

  it('_detectLayout identifies a legacy direct-target custom root', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, '_resolveExistingDirectory')
      .mockResolvedValue(CUSTOM_PARENT);
    jest.spyOn(manager, '_safeLstat').mockResolvedValue(null);
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: CUSTOM_PARENT,
    });
    jest.spyOn(manager, '_listLegacyManagedEntries').mockResolvedValue({
      managedEntries: ['immutable'],
      ignoredEntries: ['note.txt'],
    });

    const result = await manager._detectLayout(CUSTOM_PARENT);

    expect(result).toMatchObject({
      kind: 'legacy-custom-root',
      customPath: CUSTOM_PARENT,
      managedChainPath: CUSTOM_CHAIN,
      currentChainSource: CUSTOM_PARENT,
      managedLegacyEntries: ['immutable'],
      ignoredLegacyEntries: ['note.txt'],
    });
  });

  it('_detectLayout identifies an already managed custom root', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, '_resolveExistingDirectory')
      .mockResolvedValue(CUSTOM_PARENT);
    jest.spyOn(manager, '_safeLstat').mockResolvedValue({
      isDirectory: () => true,
    } as fs.Stats);
    jest
      .spyOn(manager, '_resolveRealPathOrInput')
      .mockResolvedValue(CUSTOM_CHAIN);
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: CUSTOM_CHAIN,
    });
    jest.spyOn(manager, '_listLegacyManagedEntries').mockResolvedValue({
      managedEntries: [],
      ignoredEntries: [],
    });

    const result = await manager._detectLayout(CUSTOM_PARENT);

    expect(result).toMatchObject({
      kind: 'managed-custom-root',
      managedChainPath: CUSTOM_CHAIN,
      resolvedManagedChainPath: CUSTOM_CHAIN,
      currentChainSource: CUSTOM_CHAIN,
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
      .mockResolvedValue(ACTUAL_PARENT_CHAIN);
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: ACTUAL_PARENT_CHAIN,
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
      managedChainPath: ACTUAL_PARENT_CHAIN,
      resolvedManagedChainPath: ACTUAL_PARENT_CHAIN,
      currentChainSource: ACTUAL_PARENT_CHAIN,
    });
  });

  it('_detectLayout marks a local chain directory as inconsistent when the managed child already exists', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, '_resolveExistingDirectory')
      .mockResolvedValue(CUSTOM_PARENT);
    jest.spyOn(manager, '_safeLstat').mockResolvedValue({
      isDirectory: () => true,
    } as fs.Stats);
    jest
      .spyOn(manager, '_resolveRealPathOrInput')
      .mockResolvedValue(CUSTOM_CHAIN);
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'directory',
      resolvedPath: STATE_CHAIN,
    });
    jest.spyOn(manager, '_listLegacyManagedEntries').mockResolvedValue({
      managedEntries: [],
      ignoredEntries: [],
    });

    const result = await manager._detectLayout(CUSTOM_PARENT);

    expect(result).toMatchObject({
      kind: 'inconsistent',
      managedChainPath: CUSTOM_CHAIN,
      currentChainSource: STATE_CHAIN,
    });
  });

  it('_detectLayout identifies a broken link when neither managed nor legacy data is present', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, '_resolveExistingDirectory')
      .mockResolvedValue(CUSTOM_PARENT);
    jest.spyOn(manager, '_safeLstat').mockResolvedValue(null);
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
    });
    jest.spyOn(manager, '_listLegacyManagedEntries').mockResolvedValue({
      managedEntries: [],
      ignoredEntries: [],
    });

    const result = await manager._detectLayout(CUSTOM_PARENT);

    expect(result).toMatchObject({
      kind: 'broken-link',
      managedChainPath: CUSTOM_CHAIN,
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
      customPath: CUSTOM_PARENT,
      resolvedCustomPath: CUSTOM_PARENT,
      managedChainPath: CUSTOM_CHAIN,
      resolvedManagedChainPath: undefined,
      currentChainSource: CUSTOM_PARENT,
      entryPointState: {
        type: 'symlink',
        resolvedPath: CUSTOM_PARENT,
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
      path.join(STATE_DIR, 'chain.managed-next'),
      STATE_CHAIN
    );
    expect(fs.remove).toHaveBeenCalledWith(
      path.join(STATE_DIR, 'Logs', 'chain-storage-migration-journal.json')
    );
  });

  it('_recoverInterruptedMigration cleans up a completed cutover on restart', async () => {
    const manager = new ChainStorageManager('/tmp/state');

    jest.spyOn(manager, '_readMigrationJournal').mockResolvedValue({
      state: 'cutover',
      customPath: CUSTOM_PARENT,
      legacyRootPath: CUSTOM_PARENT,
      managedChainPath: CUSTOM_CHAIN,
      movedEntries: ['immutable'],
      ignoredEntries: [],
      backupEntryPointPath: path.join(STATE_DIR, 'chain.legacy-backup'),
      tempEntryPointPath: path.join(STATE_DIR, 'chain.managed-next'),
      createdAt: '2026-04-03T00:00:00.000Z',
      updatedAt: '2026-04-03T00:00:00.000Z',
    });
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: CUSTOM_CHAIN,
    });
    jest.spyOn(manager, '_pathExistsViaLstat').mockResolvedValue(true);

    await manager._recoverInterruptedMigration({});

    expect(fs.remove).toHaveBeenCalledWith(
      path.join(STATE_DIR, 'chain.legacy-backup')
    );
    expect(fs.remove).toHaveBeenCalledWith(
      path.join(STATE_DIR, 'chain.managed-next')
    );
    expect(fs.remove).toHaveBeenCalledWith(
      path.join(STATE_DIR, 'Logs', 'chain-storage-migration-journal.json')
    );
  });

  it('ensureManagedChainLayout falls back to default storage when a broken custom target cannot be recovered', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, '_recoverInterruptedMigration')
      .mockResolvedValue(undefined);
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      linkTargetPath: CUSTOM_CHAIN,
    });
    jest.spyOn(manager, '_detectLayout').mockResolvedValue({
      kind: 'broken-link',
      customPath: CUSTOM_PARENT,
      resolvedCustomPath: CUSTOM_PARENT,
      managedChainPath: CUSTOM_CHAIN,
      resolvedManagedChainPath: undefined,
      currentChainSource: null,
      entryPointState: {
        type: 'symlink',
        linkTargetPath: CUSTOM_CHAIN,
      },
      managedChainExists: false,
      managedChainIsDirectory: false,
      managedLegacyEntries: [],
      ignoredLegacyEntries: [],
    });
    (fs.pathExists as jest.Mock).mockResolvedValue(true);

    const result = await manager.ensureManagedChainLayout();

    expect(fs.remove).toHaveBeenCalledWith(STATE_CHAIN);
    expect(fs.ensureDir).toHaveBeenCalledWith(STATE_CHAIN);
    expect(result).toEqual({
      managedChainPath: STATE_CHAIN,
      isRecoveryFallback: true,
    });
    expect(manager._isRecoveryFallback).toBe(true);
  });

  it('ensureManagedChainLayout keeps the recovery fallback flag set for the rest of the session', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, '_ensureManagedChainLayout')
      .mockResolvedValueOnce({
        managedChainPath: STATE_CHAIN,
        isRecoveryFallback: true,
      })
      .mockResolvedValueOnce({
        managedChainPath: STATE_CHAIN,
        isRecoveryFallback: false,
      });

    await manager.ensureManagedChainLayout();
    await manager.ensureManagedChainLayout();

    expect(manager._isRecoveryFallback).toBe(true);
  });

  it('setDirectory clears the recovery fallback flag after selecting a new directory', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    manager._isRecoveryFallback = true;
    jest.spyOn(manager, 'getConfig').mockResolvedValue(createConfig(null));
    jest.spyOn(manager, 'validate').mockResolvedValue({
      isValid: true,
      path: CUSTOM_PARENT,
      resolvedPath: CUSTOM_PARENT,
      chainSubdirectoryStatus: 'existing-directory',
    });
    jest
      .spyOn(manager, '_resolveRealPathOrInput')
      .mockResolvedValue(CUSTOM_CHAIN);
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'directory',
      resolvedPath: STATE_CHAIN,
    });
    (fs.readdir as jest.Mock).mockResolvedValue([]);
    jest.spyOn(manager, '_replaceCustomChainEntryPoint').mockResolvedValue();

    await manager.setDirectory(CUSTOM_PARENT);

    expect(manager._isRecoveryFallback).toBe(false);
  });

  it('resetToDefault clears the recovery fallback flag', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    manager._isRecoveryFallback = true;
    jest.spyOn(manager, '_resetToDefault').mockResolvedValue({
      isValid: true,
      path: null,
      resolvedPath: STATE_CHAIN,
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
      resolvedPath: CUSTOM_CHAIN,
    });

    await manager.unlinkChainEntryPoint();

    expect(fs.remove).toHaveBeenCalledWith(STATE_CHAIN);
  });

  it('removeManagedDirectory removes the managed chain subdirectory', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: CUSTOM_CHAIN,
    });
    jest.spyOn(manager, '_pathExistsViaLstat').mockResolvedValue(true);

    await manager.removeManagedDirectory();

    expect(fs.remove).toHaveBeenCalledWith(CUSTOM_CHAIN);
  });

  it('prepareForLocationChange resets to default and removes an empty custom managed chain directory', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, 'getConfig')
      .mockResolvedValue(createConfig(CUSTOM_PARENT));
    jest.spyOn(manager, '_safeReadDir').mockResolvedValue([]);
    jest.spyOn(manager, '_pathExistsViaLstat').mockResolvedValue(true);
    jest.spyOn(manager, '_resetToDefault').mockResolvedValue({
      isValid: true,
      path: null,
      resolvedPath: STATE_CHAIN,
      availableSpaceBytes: 4096,
      requiredSpaceBytes: 1024,
    });

    const result = await manager.prepareForLocationChange();

    expect(manager._resetToDefault).toHaveBeenCalledTimes(1);
    expect(fs.remove).toHaveBeenCalledWith(CUSTOM_CHAIN);
    expect(result).toEqual(
      expect.objectContaining({
        path: null,
        resolvedPath: STATE_CHAIN,
      })
    );
  });

  it('prepareForLocationChange keeps a non-empty custom managed chain directory intact', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest
      .spyOn(manager, 'getConfig')
      .mockResolvedValue(createConfig(CUSTOM_PARENT));
    jest.spyOn(manager, '_safeReadDir').mockResolvedValue(['immutable']);
    const resetSpy = jest.spyOn(manager, '_resetToDefault');

    const result = await manager.prepareForLocationChange();

    expect(resetSpy).not.toHaveBeenCalled();
    expect(fs.remove).not.toHaveBeenCalledWith(CUSTOM_CHAIN);
    expect(result).toBeNull();
  });

  it('emptyManagedContents removes managed entries without deleting the managed directory', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, '_captureChainPathState').mockResolvedValue({
      type: 'symlink',
      resolvedPath: CUSTOM_CHAIN,
    });
    (fs.readdir as jest.Mock).mockResolvedValue(['db', 'immutable']);

    await manager.emptyManagedContents({
      excludeTopLevelEntries: ['db'],
    });

    expect(fs.ensureDir).toHaveBeenCalledWith(CUSTOM_CHAIN);
    expect(fs.remove).toHaveBeenCalledWith(
      path.join(CUSTOM_CHAIN, 'immutable')
    );
    expect(fs.remove).not.toHaveBeenCalledWith(path.join(CUSTOM_CHAIN, 'db'));
    expect(fs.remove).not.toHaveBeenCalledWith(CUSTOM_CHAIN);
  });

  it('installValidatedPartialSyncSnapshot installs only the validated staged allowlist', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, '_ensureManagedChainLayout').mockResolvedValue({
      managedChainPath: CUSTOM_CHAIN,
      isRecoveryFallback: false,
    });
    jest.spyOn(manager, 'getManagedChainPath').mockResolvedValue(CUSTOM_CHAIN);
    jest
      .spyOn(manager, '_resolveRealPathOrInput')
      .mockImplementation(async (targetPath: string) => targetPath);
    jest
      .spyOn(manager, '_safeReadDir')
      .mockImplementation(async (targetPath: string) => {
        if (targetPath === '/tmp/staged/db') {
          return ['clean', 'immutable', 'ledger', 'lsm', 'protocolMagicId'];
        }

        if (targetPath === '/tmp/staged/db/immutable') {
          return ['26108.chunk', '26108.primary', '26108.secondary'];
        }

        return [];
      });
    const emptySpy = jest
      .spyOn(manager, '_emptyManagedContents')
      .mockResolvedValue(undefined);
    const moveSpy = jest
      .spyOn(manager, '_movePath')
      .mockResolvedValue(undefined);

    await manager.installValidatedPartialSyncSnapshot('/tmp/staged/db', {
      expectedTopLevelEntries: [
        'clean',
        'immutable',
        'ledger',
        'lsm',
        'protocolMagicId',
      ],
    });

    expect(emptySpy).toHaveBeenCalledWith(CUSTOM_CHAIN, {
      excludeTopLevelEntries: ['immutable'],
    });
    expect(moveSpy).toHaveBeenCalledTimes(7);
    expect(moveSpy).toHaveBeenNthCalledWith(
      1,
      '/tmp/staged/db/clean',
      path.join(CUSTOM_CHAIN, 'clean')
    );
    expect(moveSpy).toHaveBeenCalledWith(
      '/tmp/staged/db/immutable/26108.chunk',
      path.join(CUSTOM_CHAIN, 'immutable', '26108.chunk')
    );
    expect(moveSpy).toHaveBeenCalledWith(
      '/tmp/staged/db/immutable/26108.primary',
      path.join(CUSTOM_CHAIN, 'immutable', '26108.primary')
    );
    expect(moveSpy).toHaveBeenCalledWith(
      '/tmp/staged/db/immutable/26108.secondary',
      path.join(CUSTOM_CHAIN, 'immutable', '26108.secondary')
    );
    expect(fs.remove).toHaveBeenCalledWith('/tmp/staged/db/immutable');
    expect(fs.remove).toHaveBeenCalledWith('/tmp/staged/db');
  });

  it('installValidatedPartialSyncSnapshot preserves existing immutable history while merging staged immutable entries', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, '_ensureManagedChainLayout').mockResolvedValue({
      managedChainPath: CUSTOM_CHAIN,
      isRecoveryFallback: false,
    });
    jest.spyOn(manager, 'getManagedChainPath').mockResolvedValue(CUSTOM_CHAIN);
    jest
      .spyOn(manager, '_resolveRealPathOrInput')
      .mockImplementation(async (targetPath: string) => targetPath);
    jest
      .spyOn(manager, '_safeReadDir')
      .mockImplementation(async (targetPath: string) => {
        if (targetPath === '/tmp/staged/db') {
          return ['clean', 'immutable', 'ledger', 'lsm', 'protocolMagicId'];
        }

        if (targetPath === '/tmp/staged/db/immutable') {
          return ['26108.chunk', '26108.primary', '26108.secondary'];
        }

        return [];
      });
    const emptySpy = jest
      .spyOn(manager, '_emptyManagedContents')
      .mockResolvedValue(undefined);
    const moveSpy = jest
      .spyOn(manager, '_movePath')
      .mockResolvedValue(undefined);

    await manager.installValidatedPartialSyncSnapshot('/tmp/staged/db', {
      expectedTopLevelEntries: [
        'clean',
        'immutable',
        'ledger',
        'lsm',
        'protocolMagicId',
      ],
    });

    expect(emptySpy).toHaveBeenCalledWith(CUSTOM_CHAIN, {
      excludeTopLevelEntries: ['immutable'],
    });
    expect(fs.ensureDir).toHaveBeenCalledWith(
      path.join(CUSTOM_CHAIN, 'immutable')
    );
    expect(moveSpy).toHaveBeenCalledWith(
      '/tmp/staged/db/immutable/26108.chunk',
      path.join(CUSTOM_CHAIN, 'immutable', '26108.chunk')
    );
    expect(moveSpy).toHaveBeenCalledWith(
      '/tmp/staged/db/immutable/26108.primary',
      path.join(CUSTOM_CHAIN, 'immutable', '26108.primary')
    );
    expect(moveSpy).toHaveBeenCalledWith(
      '/tmp/staged/db/immutable/26108.secondary',
      path.join(CUSTOM_CHAIN, 'immutable', '26108.secondary')
    );
    expect(fs.remove).toHaveBeenCalledWith('/tmp/staged/db/immutable');
    expect(fs.remove).toHaveBeenCalledWith('/tmp/staged/db');
  });

  it('installValidatedPartialSyncSnapshot rejects unexpected staged entries before live cutover', async () => {
    const manager = new ChainStorageManager('/tmp/state');
    jest.spyOn(manager, '_ensureManagedChainLayout').mockResolvedValue({
      managedChainPath: CUSTOM_CHAIN,
      isRecoveryFallback: false,
    });
    jest.spyOn(manager, 'getManagedChainPath').mockResolvedValue(CUSTOM_CHAIN);
    jest
      .spyOn(manager, '_resolveRealPathOrInput')
      .mockImplementation(async (targetPath: string) => targetPath);
    jest
      .spyOn(manager, '_safeReadDir')
      .mockResolvedValue([
        'clean',
        'immutable',
        'ledger',
        'lsm',
        'protocolMagicId',
        'volatile',
      ]);
    const emptySpy = jest
      .spyOn(manager, '_emptyManagedContents')
      .mockResolvedValue(undefined);

    await expect(
      manager.installValidatedPartialSyncSnapshot('/tmp/staged/db', {
        expectedTopLevelEntries: [
          'clean',
          'immutable',
          'ledger',
          'lsm',
          'protocolMagicId',
        ],
      })
    ).rejects.toThrow(
      'Validated partial sync install requires exactly clean, immutable, ledger, lsm, protocolMagicId in staged db output.'
    );

    expect(emptySpy).not.toHaveBeenCalled();
  });
});
