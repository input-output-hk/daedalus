import type { handleManageAppUpdateRequests as HandleManageAppUpdateRequests } from './manageAppUpdateChannel';

const channel = {
  onRequest: jest.fn(),
  send: jest.fn().mockResolvedValue(undefined),
};

jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn().mockImplementation(() => channel),
}));

jest.mock('../config', () => ({
  updateMode: 'installer-managed',
  updateRunnerBin: '/dangerous/update-runner',
}));

jest.mock('../environment', () => ({
  environment: { isLinux: true },
}));

jest.mock('../BackendLifecycle', () => ({
  backendLifecycle: { installUpdate: jest.fn() },
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
  app: { quit: jest.fn() },
  shell: { openPath: jest.fn() },
}));

jest.mock('../utils/logging', () => ({
  logger: { error: jest.fn(), info: jest.fn(), warn: jest.fn() },
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
  const { backendLifecycle } = jest.requireMock('../BackendLifecycle');

  expect(fs.existsSync).not.toHaveBeenCalled();
  expect(fs.readFileSync).not.toHaveBeenCalled();
  expect(fs.chmodSync).not.toHaveBeenCalled();
  expect(shasum).not.toHaveBeenCalled();
  expect(spawn).not.toHaveBeenCalled();
  expect(backendLifecycle.installUpdate).not.toHaveBeenCalled();
  expect(app.quit).not.toHaveBeenCalled();
  expect(shell.openPath).not.toHaveBeenCalled();
  expect(window.close).not.toHaveBeenCalled();
};

describe('manageAppUpdateChannel', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    channel.onRequest.mockReset();
    const config = jest.requireMock('../config');
    config.updateMode = 'installer-managed';
    config.updateRunnerBin = '/dangerous/update-runner';
    jest.requireMock('../environment').environment.isLinux = true;
    jest
      .requireMock('../BackendLifecycle')
      .backendLifecycle.installUpdate.mockReset()
      .mockReturnValue(true);
  });

  it('rejects package-managed Linux before installer side effects', async () => {
    const handleManageAppUpdateRequests = loadHandler();
    jest.requireMock('../config').updateMode = 'system-package-disabled';
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
    jest.requireMock('../config').updateMode = 'system-package-disabled';
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

  it('hands a verified macOS or Windows installer to the watchdog before quitting', async () => {
    const handleManageAppUpdateRequests = loadHandler();
    const fs = jest.requireMock('fs').default;
    const shasum = jest.requireMock('shasum').default;
    const { spawn } = jest.requireMock('child_process');
    const { app, shell } = jest.requireMock('electron');
    const { backendLifecycle } = jest.requireMock('../BackendLifecycle');
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
    expect(backendLifecycle.installUpdate).toHaveBeenCalledWith(
      '/downloads/daedalus-installer'
    );
    expect(
      backendLifecycle.installUpdate.mock.invocationCallOrder[0]
    ).toBeLessThan(app.quit.mock.invocationCallOrder[0]);
    expect(app.quit).toHaveBeenCalledTimes(1);
    expect(shell.openPath).not.toHaveBeenCalled();

    expect(fs.chmodSync).not.toHaveBeenCalled();
    expect(spawn).not.toHaveBeenCalled();
    expect(window.close).not.toHaveBeenCalled();
  });

  it.each([
    ['missing installer', false, 'verified-sha256'],
    ['mismatched hash', true, 'wrong-sha256'],
    ['missing hash', true, undefined],
  ])(
    'rejects a %s without launching or quitting',
    async (_reason, exists, hash) => {
      const handleManageAppUpdateRequests = loadHandler();
      const fs = jest.requireMock('fs').default;
      const shasum = jest.requireMock('shasum').default;
      const { backendLifecycle } = jest.requireMock('../BackendLifecycle');
      const { app, shell } = jest.requireMock('electron');
      const window = { close: jest.fn() };

      jest.requireMock('../environment').environment.isLinux = false;
      fs.existsSync.mockReturnValue(exists);
      fs.readFileSync.mockReturnValue(Buffer.from('installer'));
      shasum.mockReturnValue('verified-sha256');

      handleManageAppUpdateRequests(window);
      const request = channel.onRequest.mock.calls[0][0];
      const result = await request({
        filePath: '/downloads/daedalus-installer',
        hash,
      });

      expect(result).toEqual(expect.objectContaining({ status: 'error' }));
      expect(backendLifecycle.installUpdate).not.toHaveBeenCalled();
      expect(shell.openPath).not.toHaveBeenCalled();
      expect(app.quit).not.toHaveBeenCalled();
      expect(window.close).not.toHaveBeenCalled();
    }
  );

  it('opens a verified installer directly only when no watchdog is available', async () => {
    const handleManageAppUpdateRequests = loadHandler();
    const fs = jest.requireMock('fs').default;
    const shasum = jest.requireMock('shasum').default;
    const { backendLifecycle } = jest.requireMock('../BackendLifecycle');
    const { app, shell } = jest.requireMock('electron');
    const window = { close: jest.fn() };
    const filePath = '/downloads/daedalus-installer';

    jest.requireMock('../environment').environment.isLinux = false;
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue(Buffer.from('installer'));
    shasum.mockReturnValue('verified-sha256');
    backendLifecycle.installUpdate.mockReturnValue(false);

    handleManageAppUpdateRequests(window);
    const request = channel.onRequest.mock.calls[0][0];
    const result = await request({ filePath, hash: 'verified-sha256' });

    expect(result).toEqual(expect.objectContaining({ status: 'success' }));
    expect(backendLifecycle.installUpdate).toHaveBeenCalledWith(filePath);
    expect(shell.openPath).toHaveBeenCalledWith(filePath);
    expect(app.quit).toHaveBeenCalledTimes(1);
    expect(window.close).not.toHaveBeenCalled();
  });
});
