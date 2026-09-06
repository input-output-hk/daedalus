jest.mock('../utils/logging', () => ({ logger: { error: jest.fn() } }));
jest.mock('electron', () => ({
  ipcMain: new (require('events').EventEmitter)(),
}));
jest.mock('./lib/trustedRendererIpcAuthority', () => ({
  authorizeTrustedRenderer: () => ({ isCurrent: () => true }),
  isTrustedRendererEvent: () => true,
  onTrustedRendererInvalidated: () => () => {},
}));

import { EventEmitter } from 'events';
import { ipcMain } from 'electron';
import { IpcChannel } from '../../common/ipc/lib/IpcChannel';
import { DAPP_CIP30_WALLET_CHANNEL } from '../../common/ipc/api';
import { currentWindowSender } from './lib/currentWindowSender';
import { executeCip30WalletRequest } from './cip30Wallet';

it('settles a wallet executor request when the renderer reports a backend failure', async () => {
  const renderer = new EventEmitter();
  const webContents = {
    send: (name: string, envelope: unknown) =>
      renderer.emit(
        name,
        {
          sender: {
            send: (reply: string, response: unknown) =>
              ipcMain.emit(reply, { sender: webContents }, response),
          },
        },
        envelope
      ),
  };
  currentWindowSender.bind({ isDestroyed: () => false, webContents } as any);
  // The renderer has its own module registry in the real application.
  IpcChannel._instances = {};
  const endpoint = new IpcChannel(DAPP_CIP30_WALLET_CHANNEL);
  endpoint.onRequest(async () => {
    throw new Error('Backend snapshot unavailable');
  }, renderer);
  try {
    await expect(
      executeCip30WalletRequest({
        operation: 'context',
        walletId: 'ab'.repeat(20),
        network: {
          networkId: 1,
          networkMagic: 764824073,
          genesisHash: 'cd'.repeat(32),
        },
        sourceRevision: 'ef'.repeat(20),
      })
    ).rejects.toThrow('Backend snapshot unavailable');
  } finally {
    renderer.removeAllListeners();
    ipcMain.removeAllListeners();
  }
}, 1000);
