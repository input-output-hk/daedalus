import {
  IpcChannel,
  IpcReceiver,
  IpcSender,
} from '../../common/ipc/lib/IpcChannel';
import { ipcMain, ipcRenderer } from '../../__tests-utils__/electron';

import { createChannels } from './createHardwareWalletIPCChannels';
import { handleHardwareWalletRequests } from './getHardwareWalletChannel';

describe('Listen Ledger devices', () => {
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
  });
});
