import { EventEmitter } from 'events';
import { IpcChannel } from '../../../../common/ipc/lib/IpcChannel';
import { RendererIpcChannel } from './RendererIpcChannel';

class Receiver extends EventEmitter {
  send = jest.fn();
}

describe('RendererIpcChannel', () => {
  beforeEach(() => {
    IpcChannel._instances = {};
  });

  it('accepts only responses from its ipcRenderer endpoint', async () => {
    const ipcRenderer = new Receiver();
    global.ipcRenderer = ipcRenderer as any;
    const channel = new RendererIpcChannel<string, string>('renderer-test');
    const promise = channel.request('message');
    const requestId = ipcRenderer.send.mock.calls[0][1].requestId;

    ipcRenderer.emit(
      'renderer-test-response',
      { sender: {} },
      {
        requestId,
        isOk: true,
        response: 'spoofed',
      }
    );
    ipcRenderer.emit(
      'renderer-test-response',
      { sender: ipcRenderer },
      {
        requestId,
        isOk: true,
        response: 'trusted',
      }
    );

    await expect(promise).resolves.toBe('trusted');
  });
});
