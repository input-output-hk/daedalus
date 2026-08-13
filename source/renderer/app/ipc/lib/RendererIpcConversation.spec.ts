import { EventEmitter } from 'events';
import { IpcConversation } from '../../../../common/ipc/lib/IpcConversation';
import { RendererIpcConversation } from './RendererIpcConversation';

class Receiver extends EventEmitter {
  send = jest.fn();
}

describe('RendererIpcConversation', () => {
  beforeEach(() => {
    IpcConversation._instances = {};
  });

  it('ignores spoofed response events and cleans up after the match', async () => {
    const ipcRenderer = new Receiver();
    global.ipcRenderer = ipcRenderer as any;
    const conversation = new RendererIpcConversation<string, string>(
      'renderer-conversation'
    );
    const promise = conversation.request('message');
    const envelope = ipcRenderer.send.mock.calls[0][1];
    ipcRenderer.emit(
      'renderer-conversation',
      { sender: {} },
      {
        ...envelope,
        isResponse: true,
        isOk: true,
        message: 'spoofed',
      }
    );
    ipcRenderer.emit(
      'renderer-conversation',
      { sender: ipcRenderer },
      {
        ...envelope,
        isResponse: true,
        isOk: true,
        message: 'trusted',
      }
    );

    await expect(promise).resolves.toBe('trusted');
    expect(ipcRenderer.listenerCount('renderer-conversation')).toBe(0);
  });
});
