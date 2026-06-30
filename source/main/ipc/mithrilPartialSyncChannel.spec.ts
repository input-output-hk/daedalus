import type {} from './mithrilPartialSyncChannel';
import type { MithrilPartialSyncStatusSnapshot } from '../../common/types/mithril-partial-sync.types';

const mockChannels: Array<{
  onRequest: jest.Mock;
  send: jest.Mock;
}> = [];

const idleStatus: MithrilPartialSyncStatusSnapshot = {
  status: 'idle' as const,
  allowedRecoveryActions: [],
  transferProgress: {},
  progressItems: [],
  error: null,
};

const availabilityEnabled = { isEnabled: true, isSignificantlyBehind: false };

const mithrilControllerMock = {
  setPartialSyncStatusSender: jest.fn(),
  initialize: jest.fn(),
  getPartialSyncStatus: jest.fn(() => idleStatus),
  isPartialSyncActive: jest.fn(() => false),
  setPartialSyncStatus: jest.fn((status) => status),
  onPartialSyncStatus: jest.fn(),
  configurePartialSyncRuntime: jest.fn(),
  startPartialSync: jest.fn(),
  cancelPartialSync: jest.fn(),
  restartNormalFromPartialSync: jest.fn(),
  wipeAndFullSyncFromPartialSync: jest.fn(),
  broadcastPartialSyncStatus: jest.fn(),
  getPartialSyncAvailability: jest.fn(() => availabilityEnabled),
  finalizePartialSync: jest.fn(),
};

jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn().mockImplementation(() => {
    const channel = {
      onRequest: jest.fn(),
      send: jest.fn().mockResolvedValue(undefined),
    };
    mockChannels.push(channel);
    return channel;
  }),
}));

jest.mock('../mithril/MithrilController', () => ({
  getMithrilController: () => mithrilControllerMock,
}));

const loadModule = () => {
  let moduleExports;

  jest.isolateModules(() => {
    moduleExports = require('./mithrilPartialSyncChannel');
  });

  return moduleExports as typeof import('./mithrilPartialSyncChannel');
};

describe('mithrilPartialSyncChannel', () => {
  beforeEach(() => {
    jest.resetModules();
    jest.clearAllMocks();
    mockChannels.length = 0;
    mithrilControllerMock.getPartialSyncStatus.mockReturnValue(idleStatus);
    mithrilControllerMock.isPartialSyncActive.mockReturnValue(false);
    mithrilControllerMock.startPartialSync.mockResolvedValue(undefined);
    mithrilControllerMock.cancelPartialSync.mockResolvedValue(undefined);
    mithrilControllerMock.restartNormalFromPartialSync.mockResolvedValue(
      undefined
    );
    mithrilControllerMock.wipeAndFullSyncFromPartialSync.mockResolvedValue(
      undefined
    );
    mithrilControllerMock.broadcastPartialSyncStatus.mockResolvedValue(
      undefined
    );
    mithrilControllerMock.getPartialSyncAvailability.mockReturnValue(
      availabilityEnabled
    );
    mithrilControllerMock.finalizePartialSync.mockResolvedValue(undefined);
  });

  it('binds status delivery to the latest window without duplicating request handlers', async () => {
    const moduleExports = loadModule();
    const firstWindow = { webContents: { id: 1 } };
    const secondWindow = { webContents: { id: 2 } };

    moduleExports.handleMithrilPartialSyncRequests(firstWindow as never);
    const requestHandlerCountAfterFirst = mockChannels.filter(
      (channel) => channel.onRequest.mock.calls.length > 0
    ).length;

    moduleExports.handleMithrilPartialSyncRequests(secondWindow as never);
    const requestHandlerCountAfterSecond = mockChannels.filter(
      (channel) => channel.onRequest.mock.calls.length > 0
    ).length;

    expect(requestHandlerCountAfterSecond).toBe(requestHandlerCountAfterFirst);

    const sender =
      mithrilControllerMock.setPartialSyncStatusSender.mock.calls[1][0];
    await sender({ ...idleStatus, status: 'downloading' });

    expect(mockChannels[1].send).toHaveBeenCalledWith(
      { ...idleStatus, status: 'downloading' },
      secondWindow.webContents
    );
  });

  it('registers thin request handlers that delegate to the controller', async () => {
    const moduleExports = loadModule();

    moduleExports.handleMithrilPartialSyncRequests({
      webContents: {},
    } as never);

    await expect(mockChannels[1].onRequest.mock.calls[0][0]()).resolves.toEqual(
      idleStatus
    );
    await expect(mockChannels[0].onRequest.mock.calls[0][0]()).resolves.toBe(
      undefined
    );
    await expect(mockChannels[2].onRequest.mock.calls[0][0]()).resolves.toBe(
      undefined
    );
    await expect(mockChannels[3].onRequest.mock.calls[0][0]()).resolves.toBe(
      undefined
    );
    await expect(mockChannels[4].onRequest.mock.calls[0][0]()).resolves.toBe(
      undefined
    );

    expect(mithrilControllerMock.startPartialSync).toHaveBeenCalledTimes(1);
    expect(mithrilControllerMock.cancelPartialSync).toHaveBeenCalledTimes(1);
    expect(
      mithrilControllerMock.restartNormalFromPartialSync
    ).toHaveBeenCalledTimes(1);
    expect(
      mithrilControllerMock.wipeAndFullSyncFromPartialSync
    ).toHaveBeenCalledTimes(1);
  });

  it('keeps compatibility exports as controller proxies', async () => {
    const moduleExports = loadModule();
    const status = { ...idleStatus, status: 'starting-node' as const };
    const listener = jest.fn();

    mithrilControllerMock.getPartialSyncStatus.mockReturnValue(status);
    mithrilControllerMock.isPartialSyncActive.mockReturnValue(true);
    moduleExports.configureMithrilPartialSyncRuntime({
      stopNode: jest.fn(),
      restartStartupFlow: jest.fn(),
    });
    moduleExports.setMithrilPartialSyncStatus(status);
    moduleExports.onMithrilPartialSyncStatus(listener);
    await moduleExports.emitMithrilPartialSyncStatus(status);

    expect(moduleExports.getMithrilPartialSyncStatus()).toBe(status);
    expect(moduleExports.isMithrilPartialSyncActive()).toBe(true);
    expect(
      mithrilControllerMock.configurePartialSyncRuntime
    ).toHaveBeenCalledTimes(1);
    expect(mithrilControllerMock.setPartialSyncStatus).toHaveBeenCalledWith(
      status
    );
    expect(mithrilControllerMock.onPartialSyncStatus).toHaveBeenCalledWith(
      listener
    );
    expect(
      mithrilControllerMock.broadcastPartialSyncStatus
    ).toHaveBeenCalledWith(status);
  });

  it('returns isEnabled reflecting launcher config for a renderer one-shot availability query', async () => {
    const moduleExports = loadModule();

    moduleExports.handleMithrilPartialSyncRequests({
      webContents: {},
    } as never);

    await expect(
      mockChannels[5].onRequest.mock.calls[0][0]()
    ).resolves.toEqual(availabilityEnabled);
    expect(
      mithrilControllerMock.getPartialSyncAvailability
    ).toHaveBeenCalledTimes(1);
  });

  it('exposes availability on a channel separate from the status snapshot channel', async () => {
    const moduleExports = loadModule();

    moduleExports.handleMithrilPartialSyncRequests({
      webContents: {},
    } as never);

    expect(mockChannels[5]).not.toBe(mockChannels[1]);

    await mockChannels[5].onRequest.mock.calls[0][0]();

    expect(
      mithrilControllerMock.getPartialSyncAvailability
    ).toHaveBeenCalledTimes(1);
    expect(mithrilControllerMock.getPartialSyncStatus).not.toHaveBeenCalled();
  });

  it('surfaces isEnabled false when the kill switch is off and true on this branch', async () => {
    const moduleExports = loadModule();

    moduleExports.handleMithrilPartialSyncRequests({
      webContents: {},
    } as never);

    mithrilControllerMock.getPartialSyncAvailability.mockReturnValue({
      isEnabled: false,
      isSignificantlyBehind: false,
    });
    await expect(
      mockChannels[5].onRequest.mock.calls[0][0]()
    ).resolves.toEqual({ isEnabled: false, isSignificantlyBehind: false });

    mithrilControllerMock.getPartialSyncAvailability.mockReturnValue({
      isEnabled: true,
      isSignificantlyBehind: false,
    });
    await expect(
      mockChannels[5].onRequest.mock.calls[0][0]()
    ).resolves.toEqual({ isEnabled: true, isSignificantlyBehind: false });
  });

  it('forwards certifiedEpoch on the availability payload (#16)', async () => {
    const moduleExports = loadModule();

    moduleExports.handleMithrilPartialSyncRequests({
      webContents: {},
    } as never);

    const availabilityWithEpoch = {
      isEnabled: true,
      isSignificantlyBehind: true,
      behindByImmutables: 30,
      certifiedEpoch: 320,
    };
    mithrilControllerMock.getPartialSyncAvailability.mockReturnValue(
      availabilityWithEpoch
    );

    await expect(
      mockChannels[5].onRequest.mock.calls[0][0]()
    ).resolves.toEqual(availabilityWithEpoch);
  });

  it('finalize channel (mockChannels[6]) delegates to controller.finalizePartialSync() and resolves to undefined', async () => {
    const moduleExports = loadModule();

    moduleExports.handleMithrilPartialSyncRequests({
      webContents: {},
    } as never);

    // The finalize channel is the 7th channel constructed (index 6)
    await expect(
      mockChannels[6].onRequest.mock.calls[0][0]()
    ).resolves.toBeUndefined();

    expect(mithrilControllerMock.finalizePartialSync).toHaveBeenCalledTimes(1);
  });

  it('finalize channel is on a distinct channel from the availability and status channels', async () => {
    const moduleExports = loadModule();

    moduleExports.handleMithrilPartialSyncRequests({
      webContents: {},
    } as never);

    expect(mockChannels[6]).not.toBe(mockChannels[5]); // not availability
    expect(mockChannels[6]).not.toBe(mockChannels[1]); // not status
  });
});
