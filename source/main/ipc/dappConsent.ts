import type { BrowserWindow } from 'electron';
import { DAPP_CONSENT_RENDER_CHANNEL } from '../../common/ipc/api';
import type {
  DappConsentRenderMainRequest,
  DappConsentRenderRendererResponse,
} from '../../common/ipc/api';
import { parseDappApprovalDecision } from '../../common/cip30/schemas';
import { ConsentCoordinator } from '../cip30/ConsentCoordinator';
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

export const consentCoordinator = new ConsentCoordinator({
  present: async (request) => {
    const value = await awaitIpcResponse(
      renderChannel.send(
        { type: 'present', request },
        currentWindowSender.sender
      )
    );
    const decision = parseDappApprovalDecision(value);
    consentCoordinator.decide(decision.requestId, decision.approved);
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
  setDappConsentLifecycleRevoker(() => consentCoordinator.cancel());
  window.webContents.on('before-input-event', () =>
    consentCoordinator.activity()
  );
};
