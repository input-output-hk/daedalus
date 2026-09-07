import type { BrowserWindow } from 'electron';
import { WALLET_APPROVAL_RENDER_CHANNEL } from '../../common/ipc/api';
import type {
  WalletApprovalRenderMainRequest,
  WalletApprovalRenderRendererResponse,
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
  WalletApprovalRenderRendererResponse,
  WalletApprovalRenderMainRequest
>(WALLET_APPROVAL_RENDER_CHANNEL);
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
      renderChannel.request(
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
  progress: async (requestId, phase, itemIndex, submissionAuthorized) => {
    await awaitIpcResponse(
      renderChannel.request(
        {
          type: 'progress',
          requestId,
          phase,
          ...(itemIndex === undefined ? {} : { itemIndex }),
          submissionAuthorized,
        },
        currentWindowSender.sender
      )
    );
  },
  terminal: async (requestId) => {
    await awaitIpcResponse(
      renderChannel.request(
        { type: 'terminal', requestId },
        currentWindowSender.sender
      )
    );
  },
  setGuestHidden: setDappBrowserConsentPending,
});

export const handleWalletApprovalRequests = (window: BrowserWindow): void => {
  setDappConsentLifecycleRevoker((reason) =>
    consentCoordinator.cancel(
      (identity) => identity.kind === 'dapp',
      changesAccount(reason) ? accountChange : undefined
    )
  );
  window.webContents.on('before-input-event', () =>
    consentCoordinator.activity()
  );
};
