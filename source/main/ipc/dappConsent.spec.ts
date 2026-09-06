jest.mock('../utils/logging', () => ({ logger: { error: jest.fn() } }));
jest.mock('electron', () => ({
  ipcMain: new (require('events').EventEmitter)(),
}));
jest.mock('./lib/trustedRendererIpcAuthority', () => ({
  authorizeTrustedRenderer: () => ({ isCurrent: () => true }),
  isTrustedRendererEvent: () => true,
  onTrustedRendererInvalidated: () => () => {},
}));
jest.mock('./dappBrowser', () => ({
  setDappBrowserConsentPending: jest.fn(),
  setDappConsentLifecycleRevoker: jest.fn(),
}));

import { EventEmitter } from 'events';
import { ipcMain } from 'electron';
import { IpcChannel } from '../../common/ipc/lib/IpcChannel';
import { DAPP_CONSENT_RENDER_CHANNEL } from '../../common/ipc/api';
import type {
  DappConsentRenderMainRequest,
  DappConsentRenderRendererResponse,
} from '../../common/ipc/api';
import { currentWindowSender } from './lib/currentWindowSender';
import { consentCoordinator } from './dappConsent';

afterEach(() => {
  consentCoordinator.cancel(() => true);
  ipcMain.removeAllListeners();
});

it('presents consent and dismisses it after a renderer refusal without executing the operation', async () => {
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
  const endpoint = new IpcChannel<
    DappConsentRenderMainRequest,
    DappConsentRenderRendererResponse
  >(DAPP_CONSENT_RENDER_CHANNEL);
  let visible: string | undefined;
  let presentedOrigin: string | undefined;
  endpoint.onRequest(async (message) => {
    if (message.type === 'present') {
      visible = message.request.requestId;
      presentedOrigin = message.request.origin;
      return { requestId: visible, approved: false };
    }
    if (visible === message.requestId) visible = undefined;
  }, renderer);
  const execute = jest.fn(async () => 'must not execute');
  await expect(
    consentCoordinator.request({
      identity: {
        guestWebContentsId: 7,
        documentGeneration: 1,
        origin: 'https://example.test',
        connectionId: '',
        walletId: 'wallet',
        routeEpoch: 1,
        networkGenesis: 'genesis',
      },
      presentation: {
        kind: 'connection',
        origin: 'https://example.test',
        walletName: 'Wallet',
        networkName: 'Preview',
        scopes: ['connection', 'read'],
        extensions: [],
      },
      payload: {},
      declined: { type: 'api-error', value: { code: -3, info: 'Refused' } },
      execute,
    })
  ).rejects.toMatchObject({ type: 'api-error', value: { code: -3 } });
  expect(presentedOrigin).toBe('https://example.test');
  expect(visible).toBeUndefined();
  expect(execute).not.toHaveBeenCalled();
}, 1000);
