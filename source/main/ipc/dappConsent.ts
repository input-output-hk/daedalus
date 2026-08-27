import type { BrowserWindow } from 'electron';
import { DAPP_CONSENT_RENDER_CHANNEL } from '../../common/ipc/api';
import type {
  DappConsentRenderMainRequest,
  DappConsentRenderRendererResponse,
} from '../../common/ipc/api';
import { parseDappApprovalDecision } from '../../common/cip30/schemas';
import { ConsentCoordinator } from '../cip30/ConsentCoordinator';
import type { DappGuestRevocationReason } from '../dapp/DappBrowserManager';
import {
  setDappBrowserConsentPending,
  setDappConsentLifecycleRevoker,
} from './dappBrowser';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  awaitIpcResponse,
  currentWindowSender,
} from './lib/currentWindowSender';

const renderChannel = new MainIpcChannel<
  DappConsentRenderRendererResponse,
  DappConsentRenderMainRequest
>(DAPP_CONSENT_RENDER_CHANNEL);
const accountChange = Object.freeze({
  type: 'api-error' as const,
  value: Object.freeze({ code: -4, info: 'Account changed' }),
});
const changesAccount = (reason: DappGuestRevocationReason): boolean =>
  reason === 'replaced' ||
  reason === 'navigation' ||
  reason === 'origin-mismatch' ||
  reason === 'route-changed';

export const consentCoordinator = new ConsentCoordinator({
  present: async (request) => {
    const value = await awaitIpcResponse(
      renderChannel.send(
        { type: 'present', request },
        currentWindowSender.sender
      )
    );
    const decision = parseDappApprovalDecision(value);
    consentCoordinator.decide(
      decision.requestId,
      decision.approved,
      decision.passphrase
    );
  },
  terminal: async (requestId) => {
    await awaitIpcResponse(
      renderChannel.send(
        { type: 'terminal', requestId },
        currentWindowSender.sender
      )
    );
  },
  setGuestHidden: setDappBrowserConsentPending,
});

export const handleDappConsentRequests = (window: BrowserWindow): void => {
  setDappConsentLifecycleRevoker((reason) =>
    consentCoordinator.cancel(
      () => true,
      changesAccount(reason) ? accountChange : undefined
    )
  );
  window.webContents.on('before-input-event', () =>
    consentCoordinator.activity()
  );
};
