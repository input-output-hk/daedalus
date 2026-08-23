import type { handleManageAppUpdateRequests as HandleManageAppUpdateRequests } from './manageAppUpdateChannel';

const channel = {
  onRequest: jest.fn(),
  send: jest.fn().mockResolvedValue(undefined),
};

jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn().mockImplementation(() => channel),
}));

jest.mock('../config', () => ({
  launcherConfig: {
    applicationUpdateMode: 'installer-managed',
    updateRunnerBin: '/dangerous/update-runner',
  },
}));

jest.mock('../environment', () => ({
  environment: { isLinux: true },
}));

jest.mock('fs', () => ({
  __esModule: true,
  default: {
    existsSync: jest.fn(),
    readFileSync: jest.fn(),
    chmodSync: jest.fn(),
  },
}));

jest.mock('shasum', () => ({
  __esModule: true,
  default: jest.fn(),
}));

jest.mock('child_process', () => ({
  spawn: jest.fn(),
}));

jest.mock('electron', () => ({
  app: { on: jest.fn(), quit: jest.fn() },
  shell: { openPath: jest.fn() },
}));

jest.mock('../utils/logging', () => ({
  logger: { error: jest.fn(), info: jest.fn() },
}));

jest.mock('./lib/currentWindowSender', () => ({
  consumeIpcResponse: jest.fn(),
  currentWindowSender: { sender: { webContents: {} } },
}));

const loadHandler = (): typeof HandleManageAppUpdateRequests => {
  // Jest loads the runtime value corresponding to the type-only import above.
  const manageAppUpdateChannelModule = require('./manageAppUpdateChannel') as {
    handleManageAppUpdateRequests: typeof HandleManageAppUpdateRequests;
  };
  return manageAppUpdateChannelModule.handleManageAppUpdateRequests;
};

const assertNoInstallerSideEffects = (window: { close: jest.Mock }) => {
  const fs = jest.requireMock('fs').default;
  const shasum = jest.requireMock('shasum').default;
  const { spawn } = jest.requireMock('child_process');
  const { app, shell } = jest.requireMock('electron');

  expect(fs.existsSync).not.toHaveBeenCalled();
  expect(fs.readFileSync).not.toHaveBeenCalled();
  expect(fs.chmodSync).not.toHaveBeenCalled();
  expect(shasum).not.toHaveBeenCalled();
  expect(spawn).not.toHaveBeenCalled();
  expect(app.on).not.toHaveBeenCalled();
  expect(app.quit).not.toHaveBeenCalled();
  expect(shell.openPath).not.toHaveBeenCalled();
  expect(window.close).not.toHaveBeenCalled();
};

describe('manageAppUpdateChannel', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    channel.onRequest.mockReset();
    const { launcherConfig } = jest.requireMock('../config');
    launcherConfig.applicationUpdateMode = 'installer-managed';
    launcherConfig.updateRunnerBin = '/dangerous/update-runner';
    jest.requireMock('../environment').environment.isLinux = true;
  });

  it('rejects Linux before file, process, shell, or window side effects', async () => {
    const handleManageAppUpdateRequests = loadHandler();
    const window = { close: jest.fn() };

    handleManageAppUpdateRequests(window);
    const request = channel.onRequest.mock.calls[0][0];
    const result = await request({
      filePath: '/missing/installer',
      hash: 'unused',
    });

    expect(result).toEqual(
      expect.objectContaining({
        status: 'error',
        data: expect.objectContaining({
          info: { reason: 'system-package-update-disabled' },
        }),
      })
    );
    assertNoInstallerSideEffects(window);
  });

  it('rejects system-package mode before installer side effects on any OS', async () => {
    const handleManageAppUpdateRequests = loadHandler();
    jest.requireMock('../environment').environment.isLinux = false;
    jest.requireMock('../config').launcherConfig.applicationUpdateMode =
      'system-package-disabled';
    const window = { close: jest.fn() };

    handleManageAppUpdateRequests(window);
    const request = channel.onRequest.mock.calls[0][0];
    const result = await request({
      filePath: '/missing/installer',
      hash: 'unused',
    });

    expect(result).toEqual(
      expect.objectContaining({
        status: 'error',
        data: expect.objectContaining({
          info: { reason: 'system-package-update-disabled' },
        }),
      })
    );
    assertNoInstallerSideEffects(window);
  });

  it('opens a verified macOS or Windows installer only after app quit', async () => {
    const handleManageAppUpdateRequests = loadHandler();
    const fs = jest.requireMock('fs').default;
    const shasum = jest.requireMock('shasum').default;
    const { spawn } = jest.requireMock('child_process');
    const { app, shell } = jest.requireMock('electron');
    const installer = Buffer.from('verified installer');
    const hash = 'verified-sha256';
    const window = { close: jest.fn() };

    jest.requireMock('../environment').environment.isLinux = false;
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue(installer);
    shasum.mockReturnValue(hash);

    handleManageAppUpdateRequests(window);
    const request = channel.onRequest.mock.calls[0][0];
    const result = await request({
      filePath: '/downloads/daedalus-installer',
      hash,
    });

    expect(result).toEqual(expect.objectContaining({ status: 'success' }));
    expect(fs.existsSync).toHaveBeenCalledWith('/downloads/daedalus-installer');
    expect(fs.readFileSync).toHaveBeenCalledWith(
      '/downloads/daedalus-installer'
    );
    expect(shasum).toHaveBeenCalledWith(installer, 'sha256');
    expect(app.on).toHaveBeenCalledWith('quit', expect.any(Function));
    expect(app.quit).toHaveBeenCalledTimes(1);
    expect(shell.openPath).not.toHaveBeenCalled();

    app.on.mock.calls[0][1]();

    expect(shell.openPath).toHaveBeenCalledWith(
      '/downloads/daedalus-installer'
    );
    expect(fs.chmodSync).not.toHaveBeenCalled();
    expect(spawn).not.toHaveBeenCalled();
    expect(window.close).not.toHaveBeenCalled();
  });
});
