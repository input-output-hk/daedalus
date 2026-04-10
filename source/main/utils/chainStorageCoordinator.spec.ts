const chainStorageManagerMock = {
  getConfig: jest.fn(),
  validate: jest.fn(),
  verifySymlink: jest.fn(),
  setDirectory: jest.fn(),
  resetToDefault: jest.fn(),
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
    mithrilBootstrapServiceMock.listSnapshots.mockResolvedValue([]);
    mithrilBootstrapServiceMock.startBootstrap.mockResolvedValue(undefined);
    mithrilBootstrapServiceMock.cancel.mockResolvedValue(undefined);
    mithrilBootstrapServiceMock.wipeChainAndSnapshots.mockResolvedValue(
      undefined
    );
    chainStorageManagerMock.ensureManagedChainLayout.mockResolvedValue(
      '/mnt/custom-parent/chain'
    );
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

  it('ensures managed layout and synced work dir before starting bootstrap', async () => {
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
});
