import {
  IpcChannel,
  IpcReceiver,
  IpcSender,
} from '../../common/ipc/lib/IpcChannel';
import { ipcMain, ipcRenderer } from '../../__tests-utils__/electron';

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

const createAndRegisterHardwareWalletChannels = () =>
  // @ts-ignore fix-me later
  handleHardwareWalletRequests(ipcRenderer, createChannels(MockIpcChannel));

const initLedgerChannel = () => {
  const initLedgerConnectChannel = new MockIpcChannel(
    'GET_INIT_LEDGER_CONNECT_CHANNEL'
  );
  initLedgerConnectChannel.request({}, ipcRenderer, ipcMain);
};

const createCardanoAppChannel = () =>
  new MockIpcChannel('GET_CARDANO_ADA_APP_CHANNEL');

const createGetPublicKeyChannel = () =>
  new MockIpcChannel('GET_EXTENDED_PUBLIC_KEY_CHANNEL');

const createHardwareWalletConnectionChannel = () =>
  new MockIpcChannel('GET_HARDWARE_WALLET_CONNECTION_CHANNEL');

const pollCardarnoApp = (deviceId: string) =>
  new Promise((resolve) => {
    const cardanoAppChannel = createCardanoAppChannel();

    const interval = setInterval(async () => {
      try {
        const cardanoAppChannelReply = await cardanoAppChannel.request(
          { path: deviceId },
          ipcRenderer,
          ipcRenderer
        );
        clearInterval(interval);
        return resolve(cardanoAppChannelReply);
      } catch (err) {
        return null;
      }
    }, 2000);
  });

/**
 * 1- Remove this file from jest.config.js/testPathIgnorePatterns
 * 2- Tag the desired test with `.only`
 *  */
describe('Ledger Hardware Wallets Channel', () => {
  /**
   * 1- Plug Ledger Nano S to your computer
   * 2- Start Cardano APP on Nano S
   * 3- Start JEST
   * 4- Nano S will prompt to export the public key
   * 5- Export the public key
   */
  it('should export public key when Cardano APP is already connected', () => {
    expect.assertions(3);

    createAndRegisterHardwareWalletChannels();

    const cardanoAppChannel = createCardanoAppChannel();
    const publicKeyChannel = createGetPublicKeyChannel();
    const hardwareWalletConnectionChannel = createHardwareWalletConnectionChannel();

    return new Promise((resolve) => {
      hardwareWalletConnectionChannel.onReceive(
        async (params: { path: string }) => {
          expect(params).toEqual({
            disconnected: expect.any(Boolean),
            deviceType: expect.any(String),
            deviceId: null,
            deviceModel: expect.any(String),
            deviceName: expect.any(String),
            path: expect.any(String),
          });

          const cardanoAppChannelReply = await cardanoAppChannel.request(
            { path: params.path },
            ipcRenderer,
            ipcRenderer
          );

          expect(cardanoAppChannelReply).toEqual({
            minor: expect.any(Number),
            major: expect.any(Number),
            patch: expect.any(Number),
            deviceId: expect.any(String),
          });

          const extendedPublicKey = await publicKeyChannel.request(
            {
              path: "1852'/1815'/0'",
              // Shelley 1852 ADA 1815 indicator for account '0'
              isTrezor: false,
              devicePath: params.path,
            },
            ipcRenderer,
            ipcRenderer
          );

          expect(extendedPublicKey).toEqual({
            chainCodeHex: expect.any(String),
            publicKeyHex: expect.any(String),
            deviceId: expect.any(String),
          });

          resolve(null);
        }
      );

      initLedgerChannel();
    });
  }, 10000000);

  /**
   * 1- Start JEST
   * 2- Plug Ledger Nano S to your computer
   * 3- Start Cardano APP on Nano S
   * 4- Nano S will prompt to export the public key
   * 5- Export the public key
   */
  it('should export public key when Cardano APP is launched after JEST already has started', () => {
    expect.assertions(3);

    createAndRegisterHardwareWalletChannels();

    const publicKeyChannel = createGetPublicKeyChannel();
    const hardwareWalletConnectionChannel = createHardwareWalletConnectionChannel();

    return new Promise((resolve) => {
      hardwareWalletConnectionChannel.onReceive(
        async (params: { path: string }) => {
          expect(params).toEqual({
            disconnected: expect.any(Boolean),
            deviceType: expect.any(String),
            deviceId: null,
            deviceModel: expect.any(String),
            deviceName: expect.any(String),
            path: expect.any(String),
          });

          const cardanoAppChannelReply = await pollCardarnoApp(params.path);

          expect(cardanoAppChannelReply).toEqual({
            minor: expect.any(Number),
            major: expect.any(Number),
            patch: expect.any(Number),
            deviceId: expect.any(String),
          });

          const extendedPublicKey = await publicKeyChannel.request(
            {
              path: "1852'/1815'/0'",
              // Shelley 1852 ADA 1815 indicator for account '0'
              isTrezor: false,
              devicePath: params.path,
            },
            ipcRenderer,
            ipcRenderer
          );

          expect(extendedPublicKey).toEqual({
            chainCodeHex: expect.any(String),
            publicKeyHex: expect.any(String),
            deviceId: expect.any(String),
          });

          resolve(null);
        }
      );

      initLedgerChannel();
    });
  }, 10000000);
});
