const restartMock = jest.fn();
const cardanoNodeConstructorMock = jest.fn();
const mithrilControllerMock = {
  isBootstrapNodeStartBlocked: jest.fn(() => false),
  isPartialSyncNodeStartBlocked: jest.fn(() => false),
  getBootstrapStatus: jest.fn(() => ({ status: 'idle' })),
  getPartialSyncStatus: jest.fn(() => ({
    status: 'idle',
    allowedRecoveryActions: [],
    transferProgress: {},
    progressItems: [],
    error: null,
  })),
};

jest.mock('./CardanoNode', () => ({
  CardanoNode: jest.fn().mockImplementation((...args) => {
    cardanoNodeConstructorMock(...args);
    return {
      startupTries: 0,
      restart: restartMock,
      status: null,
      state: 'stopped',
      tlsConfig: null,
      saveStatus: jest.fn(),
      expectNodeUpdate: jest.fn(),
      setFault: jest.fn(),
    };
  }),
}));

jest.mock('electron', () => ({
  BrowserWindow: jest.fn(),
}));

jest.mock('../utils/logging', () => ({
  logger: {
    info: jest.fn(),
    error: jest.fn(),
  },
}));

jest.mock('../config', () => ({
  NODE_KILL_TIMEOUT: 1,
  NODE_SHUTDOWN_TIMEOUT: 1,
  NODE_STARTUP_MAX_RETRIES: 3,
  NODE_STARTUP_TIMEOUT: 1,
  NODE_UPDATE_TIMEOUT: 1,
}));

jest.mock('../ipc/cardano.ipc', () => {
  const createChannel = () => ({
    send: jest.fn(),
    onRequest: jest.fn(),
    onReceive: jest.fn(),
  });

  return {
    cardanoAwaitUpdateChannel: createChannel(),
    cardanoFaultInjectionChannel: createChannel(),
    cardanoRestartChannel: createChannel(),
    cardanoStateChangeChannel: createChannel(),
    getCachedCardanoStatusChannel: createChannel(),
    cardanoTlsConfigChannel: createChannel(),
    setCachedCardanoStatusChannel: createChannel(),
    exportWalletsChannel: createChannel(),
  };
});

jest.mock('../mithril/MithrilController', () => ({
  getMithrilController: () => mithrilControllerMock,
}));

jest.mock('./utils', () => ({
  exportWallets: jest.fn(),
}));

jest.mock('../utils/safeExitWithCode', () => ({
  safeExitWithCode: jest.fn(),
}));

describe('setupCardanoNode', () => {
  const realSetTimeout = global.setTimeout;

  beforeEach(() => {
    jest.resetModules();
    jest.clearAllMocks();
    restartMock.mockResolvedValue(undefined);
    mithrilControllerMock.isBootstrapNodeStartBlocked.mockReturnValue(false);
    mithrilControllerMock.isPartialSyncNodeStartBlocked.mockReturnValue(false);
    mithrilControllerMock.getBootstrapStatus.mockReturnValue({ status: 'idle' });
    mithrilControllerMock.getPartialSyncStatus.mockReturnValue({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    global.setTimeout = jest.fn() as unknown as typeof global.setTimeout;
  });

  afterEach(() => {
    global.setTimeout = realSetTimeout;
  });

  const setup = () => {
    const { setupCardanoNode } = require('./setup') as typeof import('./setup');
    setupCardanoNode(
      {
        logsPrefix: '/tmp/logs',
        nodeImplementation: 'cardano',
        nodeConfig: 'mainnet_full',
        tlsPath: '/tmp/tls',
        stateDir: '/tmp/state',
        cluster: null,
        configPath: '/tmp/config',
        syncTolerance: 300,
        cliBin: '/tmp/cardano-cli',
        isStaging: false,
        metadataUrl: 'https://example.com',
      } as never,
      { isDestroyed: () => false } as never,
      []
    );

    return cardanoNodeConstructorMock.mock.calls[0][2];
  };

  it('suppresses automatic restart while Mithril partial sync is active', () => {
    mithrilControllerMock.isPartialSyncNodeStartBlocked.mockReturnValue(true);
    mithrilControllerMock.getPartialSyncStatus.mockReturnValue({
      status: 'downloading',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    const transitions = setup();

    transitions.onCrashed(23);

    expect(global.setTimeout).not.toHaveBeenCalled();
  });

  it('suppresses automatic restart during installed-awaiting-node-start cutover', () => {
    mithrilControllerMock.isPartialSyncNodeStartBlocked.mockReturnValue(true);
    mithrilControllerMock.getPartialSyncStatus.mockReturnValue({
      status: 'starting-node',
      allowedRecoveryActions: ['wipe-and-full-sync'],
      transferProgress: {},
      progressItems: [],
      error: null,
    });

    const transitions = setup();

    transitions.onCrashed(31);

    expect(global.setTimeout).not.toHaveBeenCalled();
  });

  it('preserves bootstrap restart suppression unchanged', () => {
    mithrilControllerMock.isBootstrapNodeStartBlocked.mockReturnValue(true);
    mithrilControllerMock.getBootstrapStatus.mockReturnValue({ status: 'verifying' });

    const transitions = setup();

    transitions.onCrashed(17);

    expect(global.setTimeout).not.toHaveBeenCalled();
  });

  it('still schedules restart when no Mithril operation is active', () => {
    const transitions = setup();

    transitions.onCrashed(9);

    expect(global.setTimeout).toHaveBeenCalledTimes(1);
    expect((global.setTimeout as unknown as jest.Mock).mock.calls[0][1]).toBe(1000);
  });
});
