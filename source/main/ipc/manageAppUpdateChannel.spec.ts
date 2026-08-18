import type {} from './manageAppUpdateChannel';

const channel = {
  onRequest: jest.fn(),
  send: jest.fn().mockResolvedValue(undefined),
};

jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn().mockImplementation(() => channel),
}));

jest.mock('../config', () => ({
  launcherConfig: {
    applicationUpdateMode: 'system-package-disabled',
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

describe('manageAppUpdateChannel system package mode', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    channel.onRequest.mockReset();
  });

  it('rejects before reading or executing portable installer bytes', async () => {
    const {
      handleManageAppUpdateRequests,
    } = require('./manageAppUpdateChannel') as typeof import('./manageAppUpdateChannel');
    const fs = jest.requireMock('fs').default;
    const { spawn } = jest.requireMock('child_process');
    const window = { close: jest.fn() };

    handleManageAppUpdateRequests(window as any);
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
    expect(fs.existsSync).not.toHaveBeenCalled();
    expect(fs.readFileSync).not.toHaveBeenCalled();
    expect(fs.chmodSync).not.toHaveBeenCalled();
    expect(spawn).not.toHaveBeenCalled();
    expect(window.close).not.toHaveBeenCalled();
  });
});
