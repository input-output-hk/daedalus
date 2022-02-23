import { IpcChannel } from '../../common/ipc/lib/IpcChannel';
import type { IpcReceiver, IpcSender } from '../../common/ipc/lib/IpcChannel';
import { ipcMain, ipcRenderer } from '../../spec/electron';

import { createChannels } from './createHardwareWalletIPCChannels';
import { handleHardwareWalletRequests } from './getHardwareWalletChannel';

class MockIpcChannel<Incoming, Outgoing> extends IpcChannel<
  Incoming,
  Outgoing
> {
  async send(
    message: Outgoing,
    sender: IpcSender = ipcRenderer,
    receiver: IpcReceiver = ipcRenderer
  ): Promise<Incoming> {
    return super.send(message, sender, receiver);
  }

  async request(
    message: Outgoing,
    sender: IpcSender = ipcMain,
    receiver: IpcReceiver = ipcMain
  ): Promise<Incoming> {
    return super.request(message, sender, receiver);
  }

  onReceive(
    handler: (message: Incoming) => Promise<Outgoing>,
    receiver: IpcReceiver = ipcMain
  ): void {
    super.onReceive(handler, receiver);
  }

  onRequest(
    handler: (arg0: Incoming) => Promise<Outgoing>,
    receiver: IpcReceiver = ipcMain
  ): void {
    super.onRequest(handler, receiver);
  }
}

describe('test', () => {
  it('the correct Url for TESTNET', async (done) => {
    const channels = createChannels(MockIpcChannel);
    handleHardwareWalletRequests(ipcRenderer, channels);

    const y = new MockIpcChannel('GET_HARDWARE_WALLET_CONNECTION_CHANNEL');
    y.onReceive((params) => {
      expect(params).toEqual({
        disconnected: expect.any(Boolean),
        deviceType: expect.any(String),
        deviceId: null,
        // Available only when Cardano APP opened
        deviceModel: expect.any(String),
        // e.g. nanoS
        deviceName: expect.any(String),
        // e.g. Test Name
        path: expect.any(String),
      });
      done();
    });

    const x = new MockIpcChannel('GET_INIT_LEDGER_CONNECT_CHANNEL');
    x.request({}, ipcRenderer, ipcMain);
  }, 10000000);
});
