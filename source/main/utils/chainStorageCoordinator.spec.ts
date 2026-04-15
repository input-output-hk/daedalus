import type {} from './chainStorageCoordinator';

const chainStorageManagerMock = {
  getConfig: jest.fn(),
  validate: jest.fn(),
  setDirectory: jest.fn(),
  resetToDefault: jest.fn(),
  prepareForLocationChange: jest.fn(),
  ensureManagedChainLayout: jest.fn(),
  resolveDiskSpaceCheckPath: jest.fn(),
  isManagedChainEmpty: jest.fn(),
  resolveMithrilWorkDir: jest.fn(),
};

const mithrilBootstrapServiceMock = {
  setWorkDir: jest.fn(),
  listSnapshots: jest.fn(),
  startBootstrap: jest.fn(),
  cancel: jest.fn(),
  wipeChainAndSnapshots: jest.fn(),
};

jest.mock('./chainStorageManager', () => ({
  ChainStorageManager: jest
    .fn()
    .mockImplementation(() => chainStorageManagerMock),
}));

jest.mock('../mithril/MithrilBootstrapService', () => ({
  MithrilBootstrapService: jest
    .fn()
    .mockImplementation(() => mithrilBootstrapServiceMock),
}));

const loadModule = () => {
  let moduleExports;

  jest.isolateModules(() => {
    moduleExports = require('./chainStorageCoordinator');
  });

  return moduleExports as typeof import('./chainStorageCoordinator');
};

const createDeferred = () => {
  let resolve: () => void;
  const promise = new Promise<void>((resolvePromise) => {
    resolve = resolvePromise;
  });

  return {
    promise,
    resolve: resolve!,
  };
};

const flushPromises = () => new Promise((resolve) => setTimeout(resolve, 0));

describe('chainStorageCoordinator', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    chainStorageManagerMock.resolveMithrilWorkDir.mockResolvedValue(
      '/mnt/custom-parent/chain'
    );
    chainStorageManagerMock.isManagedChainEmpty.mockResolvedValue(true);
    mithrilBootstrapServiceMock.listSnapshots.mockResolvedValue([]);
    mithrilBootstrapServiceMock.startBootstrap.mockResolvedValue(undefined);
    mithrilBootstrapServiceMock.cancel.mockResolvedValue(undefined);
    mithrilBootstrapServiceMock.wipeChainAndSnapshots.mockResolvedValue(
      undefined
    );
    chainStorageManagerMock.ensureManagedChainLayout.mockResolvedValue({
      managedChainPath: '/mnt/custom-parent/chain',
      isRecoveryFallback: false,
    });
  });

  it('exposes the singleton-backed manager and Mithril service accessors', () => {
    const moduleExports = loadModule();

    expect(moduleExports.getChainStorageManager()).toBe(
      chainStorageManagerMock
    );
    expect(moduleExports.getMithrilBootstrapService()).toBe(
      mithrilBootstrapServiceMock
    );
    expect(moduleExports.chainStorageCoordinator.getChainStorageManager()).toBe(
      chainStorageManagerMock
    );
    expect(
      moduleExports.chainStorageCoordinator.getMithrilBootstrapService()
    ).toBe(mithrilBootstrapServiceMock);
  });

  it('serializes directory mutations through the coordinator lock', async () => {
    const firstMutation = createDeferred();

    chainStorageManagerMock.setDirectory
      .mockImplementationOnce(async () => {
        await firstMutation.promise;
        return { isValid: true, path: '/mnt/one' };
      })
      .mockResolvedValueOnce({ isValid: true, path: '/mnt/two' });

    const moduleExports = loadModule();

    const firstCall = moduleExports.chainStorageCoordinator.setDirectory(
      '/mnt/one'
    );
    const secondCall = moduleExports.chainStorageCoordinator.setDirectory(
      '/mnt/two'
    );

    await flushPromises();

    expect(chainStorageManagerMock.setDirectory).toHaveBeenCalledTimes(1);
    expect(chainStorageManagerMock.setDirectory).toHaveBeenCalledWith(
      '/mnt/one'
    );

    firstMutation.resolve();

    await firstCall;
    await secondCall;

    expect(chainStorageManagerMock.setDirectory).toHaveBeenCalledTimes(2);
    expect(chainStorageManagerMock.setDirectory).toHaveBeenNthCalledWith(
      2,
      '/mnt/two'
    );
  });

  it('syncs the managed chain path before delegating to Mithril consumers', async () => {
    const moduleExports = loadModule();

    await moduleExports.chainStorageCoordinator.listSnapshots();

    expect(chainStorageManagerMock.resolveMithrilWorkDir).toHaveBeenCalled();
    expect(mithrilBootstrapServiceMock.setWorkDir).toHaveBeenCalledWith(
      '/mnt/custom-parent/chain'
    );
    expect(mithrilBootstrapServiceMock.listSnapshots).toHaveBeenCalledTimes(1);
    expect(
      mithrilBootstrapServiceMock.setWorkDir.mock.invocationCallOrder[0]
    ).toBeLessThan(
      mithrilBootstrapServiceMock.listSnapshots.mock.invocationCallOrder[0]
    );
  });

  it('re-evaluates startup only when returning to the picker resets an empty custom selection', async () => {
    chainStorageManagerMock.prepareForLocationChange.mockResolvedValueOnce({
      isValid: true,
      path: null,
      resolvedPath: '/tmp/state/chain',
      availableSpaceBytes: 4096,
      requiredSpaceBytes: 1024,
    });
    const moduleExports = loadModule();
    const directoryChanged = jest.fn();

    moduleExports.chainStorageCoordinator.onDirectoryChanged(directoryChanged);

    const result = await moduleExports.chainStorageCoordinator.prepareForLocationChange();

    expect(chainStorageManagerMock.prepareForLocationChange).toHaveBeenCalled();
    expect(chainStorageManagerMock.resolveMithrilWorkDir).toHaveBeenCalled();
    expect(directoryChanged).toHaveBeenCalledTimes(1);
    expect(result).toEqual(
      expect.objectContaining({
        path: null,
        resolvedPath: '/tmp/state/chain',
      })
    );
  });

  it('does not notify directory changes when returning to the picker keeps the current storage', async () => {
    chainStorageManagerMock.prepareForLocationChange.mockResolvedValueOnce(
      null
    );
    const moduleExports = loadModule();
    const directoryChanged = jest.fn();

    moduleExports.chainStorageCoordinator.onDirectoryChanged(directoryChanged);

    const result = await moduleExports.chainStorageCoordinator.prepareForLocationChange();

    expect(chainStorageManagerMock.prepareForLocationChange).toHaveBeenCalled();
    expect(
      chainStorageManagerMock.resolveMithrilWorkDir
    ).not.toHaveBeenCalled();
    expect(directoryChanged).not.toHaveBeenCalled();
    expect(result).toBeNull();
  });

  it('waits for an in-flight layout mutation before reading config', async () => {
    const layoutMutation = createDeferred();

    chainStorageManagerMock.ensureManagedChainLayout.mockImplementationOnce(
      async () => {
        await layoutMutation.promise;
        return {
          managedChainPath: '/tmp/state/chain',
          isRecoveryFallback: true,
        };
      }
    );
    chainStorageManagerMock.getConfig.mockResolvedValue({
      customPath: null,
      defaultPath: '/tmp/state/chain',
      availableSpaceBytes: 4096,
      requiredSpaceBytes: 1024,
      isRecoveryFallback: true,
    });

    const moduleExports = loadModule();

    const ensureLayoutCall = moduleExports.chainStorageCoordinator.ensureManagedChainLayout(
      'stopped'
    );

    await flushPromises();

    const getConfigCall = moduleExports.chainStorageCoordinator.getConfig();

    await flushPromises();

    expect(chainStorageManagerMock.getConfig).not.toHaveBeenCalled();

    layoutMutation.resolve();

    await ensureLayoutCall;
    const config = await getConfigCall;

    expect(chainStorageManagerMock.getConfig).toHaveBeenCalledTimes(1);
    expect(config).toEqual(
      expect.objectContaining({
        isRecoveryFallback: true,
        customPath: null,
      })
    );
  });

  it('ensures managed layout and synced work dir before starting bootstrap', async () => {
    chainStorageManagerMock.isManagedChainEmpty.mockResolvedValue(false);
    const moduleExports = loadModule();

    await moduleExports.chainStorageCoordinator.startBootstrap('digest-1', {
      wipeChain: true,
      nodeState: 'stopped',
    });

    expect(
      chainStorageManagerMock.ensureManagedChainLayout
    ).toHaveBeenCalledWith({ nodeState: 'stopped' });
    expect(mithrilBootstrapServiceMock.setWorkDir).toHaveBeenCalledWith(
      '/mnt/custom-parent/chain'
    );
    expect(
      mithrilBootstrapServiceMock.startBootstrap
    ).toHaveBeenCalledWith('digest-1', { wipeChain: true });
  });

  it('rejects startBootstrap when the managed chain is non-empty and wipeChain is false', async () => {
    chainStorageManagerMock.isManagedChainEmpty.mockResolvedValue(false);

    const moduleExports = loadModule();

    await expect(
      moduleExports.chainStorageCoordinator.startBootstrap('digest-guard', {
        nodeState: 'stopped',
      })
    ).rejects.toThrow(
      'Cannot start Mithril bootstrap on a non-empty managed chain without wipeChain.'
    );

    expect(
      chainStorageManagerMock.ensureManagedChainLayout
    ).toHaveBeenCalledWith({ nodeState: 'stopped' });
    expect(chainStorageManagerMock.isManagedChainEmpty).toHaveBeenCalledTimes(
      1
    );
    expect(mithrilBootstrapServiceMock.startBootstrap).not.toHaveBeenCalled();
  });

  it('blocks setDirectory while bootstrap is in progress', async () => {
    const bootstrapMutation = createDeferred();

    mithrilBootstrapServiceMock.startBootstrap.mockImplementationOnce(
      async () => {
        await bootstrapMutation.promise;
      }
    );
    chainStorageManagerMock.setDirectory.mockResolvedValue({
      isValid: true,
      path: '/mnt/next',
    });

    const moduleExports = loadModule();

    const bootstrapCall = moduleExports.chainStorageCoordinator.startBootstrap(
      'digest-2',
      {
        nodeState: 'stopped',
      }
    );

    await flushPromises();

    expect(
      chainStorageManagerMock.ensureManagedChainLayout
    ).toHaveBeenCalledWith({ nodeState: 'stopped' });
    expect(
      mithrilBootstrapServiceMock.startBootstrap
    ).toHaveBeenCalledWith('digest-2', { wipeChain: undefined });
    expect(chainStorageManagerMock.setDirectory).not.toHaveBeenCalled();

    await expect(
      moduleExports.chainStorageCoordinator.setDirectory('/mnt/next')
    ).rejects.toThrow(
      'Cannot change chain storage directory while Mithril bootstrap is in progress.'
    );

    bootstrapMutation.resolve();

    await bootstrapCall;
  });

  it('blocks wipeChainAndSnapshots while bootstrap is in progress', async () => {
    const bootstrapMutation = createDeferred();

    mithrilBootstrapServiceMock.startBootstrap.mockImplementationOnce(
      async () => {
        await bootstrapMutation.promise;
      }
    );

    const moduleExports = loadModule();

    const bootstrapCall = moduleExports.chainStorageCoordinator.startBootstrap(
      'digest-3',
      {
        nodeState: 'stopped',
      }
    );

    await flushPromises();

    expect(
      mithrilBootstrapServiceMock.wipeChainAndSnapshots
    ).not.toHaveBeenCalled();

    await expect(
      moduleExports.chainStorageCoordinator.wipeChainAndSnapshots(
        'wipe-reason',
        'stopped'
      )
    ).rejects.toThrow(
      'Cannot wipe chain storage and snapshot data while Mithril bootstrap is in progress.'
    );

    bootstrapMutation.resolve();

    await bootstrapCall;
  });

  it('cancels bootstrap immediately without waiting for the mutation queue', async () => {
    const bootstrapMutation = createDeferred();

    mithrilBootstrapServiceMock.startBootstrap.mockImplementationOnce(
      async () => {
        await bootstrapMutation.promise;
      }
    );

    const moduleExports = loadModule();

    const bootstrapCall = moduleExports.chainStorageCoordinator.startBootstrap(
      'digest-4',
      {
        nodeState: 'stopped',
      }
    );

    await flushPromises();

    const cancelCall = moduleExports.chainStorageCoordinator.cancelBootstrap();

    await flushPromises();

    expect(mithrilBootstrapServiceMock.cancel).toHaveBeenCalledTimes(1);
    expect(chainStorageManagerMock.resolveMithrilWorkDir).toHaveBeenCalledTimes(
      1
    );

    bootstrapMutation.resolve();

    await cancelCall;
    await bootstrapCall;
  });

  it('serializes setDirectory while wipeChainAndSnapshots is in progress', async () => {
    const wipeMutation = createDeferred();

    mithrilBootstrapServiceMock.wipeChainAndSnapshots.mockImplementationOnce(
      async () => {
        await wipeMutation.promise;
      }
    );
    chainStorageManagerMock.setDirectory.mockResolvedValue({
      isValid: true,
      path: '/mnt/next',
    });

    const moduleExports = loadModule();

    const wipeCall = moduleExports.chainStorageCoordinator.wipeChainAndSnapshots(
      'wipe-reason',
      'stopped'
    );

    await flushPromises();

    const setDirectoryCall = moduleExports.chainStorageCoordinator.setDirectory(
      '/mnt/next'
    );

    await flushPromises();

    expect(
      chainStorageManagerMock.ensureManagedChainLayout
    ).toHaveBeenCalledWith({ nodeState: 'stopped' });
    expect(
      mithrilBootstrapServiceMock.wipeChainAndSnapshots
    ).toHaveBeenCalledWith('wipe-reason');
    expect(chainStorageManagerMock.setDirectory).not.toHaveBeenCalled();

    wipeMutation.resolve();

    await wipeCall;
    await setDirectoryCall;

    expect(chainStorageManagerMock.setDirectory).toHaveBeenCalledWith(
      '/mnt/next'
    );
    expect(
      mithrilBootstrapServiceMock.wipeChainAndSnapshots.mock
        .invocationCallOrder[0]
    ).toBeLessThan(
      chainStorageManagerMock.setDirectory.mock.invocationCallOrder[0]
    );
  });

  it('emits directory-change callbacks after a successful custom directory mutation', async () => {
    chainStorageManagerMock.setDirectory.mockResolvedValue({
      isValid: true,
      path: '/mnt/next',
      resolvedPath: '/mnt/next/chain',
    });

    const moduleExports = loadModule();
    const onDirectoryChanged = jest.fn();

    moduleExports.chainStorageCoordinator.onDirectoryChanged(
      onDirectoryChanged
    );

    await moduleExports.chainStorageCoordinator.setDirectory('/mnt/next');

    expect(onDirectoryChanged).toHaveBeenCalledTimes(1);
    expect(mithrilBootstrapServiceMock.setWorkDir).toHaveBeenCalledWith(
      '/mnt/custom-parent/chain'
    );
    expect(
      mithrilBootstrapServiceMock.setWorkDir.mock.invocationCallOrder[0]
    ).toBeLessThan(onDirectoryChanged.mock.invocationCallOrder[0]);
  });

  it('emits directory-change callbacks after resetting to the default directory', async () => {
    chainStorageManagerMock.resetToDefault.mockResolvedValue({
      isValid: true,
      path: null,
    });

    const moduleExports = loadModule();
    const onDirectoryChanged = jest.fn();

    moduleExports.chainStorageCoordinator.onDirectoryChanged(
      onDirectoryChanged
    );

    await moduleExports.chainStorageCoordinator.setDirectory(null);

    expect(chainStorageManagerMock.resetToDefault).toHaveBeenCalledTimes(1);
    expect(onDirectoryChanged).toHaveBeenCalledTimes(1);
  });

  it('does not emit directory-change callbacks for rejected directory mutations', async () => {
    chainStorageManagerMock.setDirectory.mockResolvedValue({
      isValid: false,
      path: '/mnt/blocked',
      reason: 'unknown',
    });

    const moduleExports = loadModule();
    const onDirectoryChanged = jest.fn();

    moduleExports.chainStorageCoordinator.onDirectoryChanged(
      onDirectoryChanged
    );

    await expect(
      moduleExports.chainStorageCoordinator.setDirectory('/mnt/blocked')
    ).resolves.toEqual(
      expect.objectContaining({
        isValid: false,
        path: '/mnt/blocked',
      })
    );

    expect(onDirectoryChanged).not.toHaveBeenCalled();
    expect(mithrilBootstrapServiceMock.setWorkDir).not.toHaveBeenCalled();
  });

  it('rejects prepareForLocationChange when node is running', async () => {
    const moduleExports = loadModule();
    await expect(
      moduleExports.chainStorageCoordinator.prepareForLocationChange('running')
    ).rejects.toThrow('while cardano-node is stopped');
  });

  it('allows prepareForLocationChange when node is stopped', async () => {
    chainStorageManagerMock.prepareForLocationChange.mockResolvedValue(null);
    const moduleExports = loadModule();
    await expect(
      moduleExports.chainStorageCoordinator.prepareForLocationChange('stopped')
    ).resolves.toBeNull();
  });

  it('rejects setDirectory when node is running', async () => {
    const moduleExports = loadModule();
    await expect(
      moduleExports.chainStorageCoordinator.setDirectory(
        '/mnt/external',
        'running'
      )
    ).rejects.toThrow('while cardano-node is stopped');
  });

  it('allows setDirectory when node is stopped', async () => {
    chainStorageManagerMock.setDirectory.mockResolvedValue({
      isValid: true,
      path: '/mnt/external',
    });
    const moduleExports = loadModule();
    await expect(
      moduleExports.chainStorageCoordinator.setDirectory(
        '/mnt/external',
        'stopped'
      )
    ).resolves.toEqual(expect.objectContaining({ isValid: true }));
  });

  it('rejects prepareForLocationChange when node is stopping', async () => {
    const moduleExports = loadModule();
    await expect(
      moduleExports.chainStorageCoordinator.prepareForLocationChange('stopping')
    ).rejects.toThrow('while cardano-node is stopped');
  });

  it('rejects setDirectory when node is stopping', async () => {
    const moduleExports = loadModule();
    await expect(
      moduleExports.chainStorageCoordinator.setDirectory(
        '/mnt/external',
        'stopping'
      )
    ).rejects.toThrow('while cardano-node is stopped');
  });

  it('rejects wipeChainAndSnapshots when node is stopping', async () => {
    const moduleExports = loadModule();
    await expect(
      moduleExports.chainStorageCoordinator.wipeChainAndSnapshots(
        'wipe-reason',
        'stopping'
      )
    ).rejects.toThrow('while cardano-node is stopped');
  });

  it('allows wipeChainAndSnapshots when node is stopped', async () => {
    const moduleExports = loadModule();
    await expect(
      moduleExports.chainStorageCoordinator.wipeChainAndSnapshots(
        'wipe-reason',
        'stopped'
      )
    ).resolves.toBeUndefined();
  });
});
