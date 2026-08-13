jest.mock('electron', () => ({
  ipcMain: { on: jest.fn(), removeListener: jest.fn() },
}));
jest.mock('./trustedRendererIpcAuthority', () => ({
  authorizeTrustedRenderer: jest.fn(() => ({ isCurrent: () => true })),
  isTrustedRendererEvent: jest.fn((event) => event.trusted === true),
  onTrustedRendererInvalidated: jest.fn(() => () => {}),
}));

import { IpcConversation } from '../../../common/ipc/lib/IpcConversation';
import { MainIpcConversation } from './MainIpcConversation';

const { ipcMain: mockedIpcMain } = require('electron');
const mockMain = mockedIpcMain as any;

describe('MainIpcConversation', () => {
  beforeEach(() => {
    IpcConversation._instances = {};
    mockMain.on.mockReset();
    mockMain.removeListener.mockReset();
  });

  it('normalizes BrowserWindow and authenticates correlated responses', async () => {
    const conversation = new MainIpcConversation<string, string>(
      'main-conversation'
    );
    const webContents = { send: jest.fn() };
    const promise = conversation.request(
      'message',
      { webContents } as any,
      mockMain
    );
    const envelope = webContents.send.mock.calls[0][1];
    const listener = mockMain.on.mock.calls[0][1];
    listener(
      { trusted: false },
      {
        ...envelope,
        isResponse: true,
        isOk: true,
        message: 'spoofed',
      }
    );
    listener(
      { trusted: true },
      {
        ...envelope,
        isResponse: true,
        isOk: true,
        message: 'trusted',
      }
    );

    await expect(promise).resolves.toBe('trusted');
    expect(mockMain.removeListener).toHaveBeenCalledTimes(1);
  });
});
