jest.mock('electron', () => ({
  ipcMain: { on: jest.fn(), removeListener: jest.fn() },
}));
jest.mock('./trustedRendererIpcAuthority', () => ({
  authorizeTrustedRenderer: jest.fn(() => ({ isCurrent: () => true })),
  isTrustedRendererEvent: jest.fn(() => true),
  onTrustedRendererInvalidated: jest.fn(() => () => {}),
}));

import { IpcChannel } from '../../../common/ipc/lib/IpcChannel';
import { MainIpcChannel } from './MainIpcChannel';

const { ipcMain } = require('electron');
const mockedIpcMain = ipcMain as any;

describe('MainIpcChannel', () => {
  beforeEach(() => {
    IpcChannel._instances = {};
    mockedIpcMain.on.mockReset();
    mockedIpcMain.removeListener.mockReset();
  });

  it('normalizes BrowserWindow send targets to webContents', async () => {
    const channel = new MainIpcChannel<string, string>('main-test');
    const webContents = { send: jest.fn() };
    const promise = channel.send(
      'message',
      { webContents } as any,
      ipcMain as any
    );

    expect(webContents.send).toHaveBeenCalledWith(
      'main-test-broadcast',
      expect.objectContaining({ message: 'message' })
    );
    const responseListener = mockedIpcMain.on.mock.calls[0][1];
    const requestId = webContents.send.mock.calls[0][1].requestId;
    responseListener(
      { sender: webContents },
      {
        requestId,
        isOk: true,
        response: 'response',
      }
    );
    await expect(promise).resolves.toBe('response');
  });
});
