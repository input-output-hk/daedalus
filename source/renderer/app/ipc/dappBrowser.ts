import {
  DAPP_BROWSER_CLOSE_CHANNEL,
  DAPP_BROWSER_OPEN_CHANNEL,
  DAPP_BROWSER_STATE_CHANNEL,
  DAPP_BROWSER_STATUS_CHANNEL,
} from '../../../common/ipc/api';
import type {
  DappBrowserCloseMainResponse,
  DappBrowserCloseRendererRequest,
  DappBrowserOpenMainResponse,
  DappBrowserOpenRendererRequest,
  DappBrowserStateMainRequest,
  DappBrowserStateRendererResponse,
  DappBrowserStatusMainResponse,
  DappBrowserStatusRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const openDappBrowserChannel = new RendererIpcChannel<
  DappBrowserOpenMainResponse,
  DappBrowserOpenRendererRequest
>(DAPP_BROWSER_OPEN_CHANNEL);

export const closeDappBrowserChannel = new RendererIpcChannel<
  DappBrowserCloseMainResponse,
  DappBrowserCloseRendererRequest
>(DAPP_BROWSER_CLOSE_CHANNEL);

export const dappBrowserStatusChannel = new RendererIpcChannel<
  DappBrowserStatusMainResponse,
  DappBrowserStatusRendererRequest
>(DAPP_BROWSER_STATUS_CHANNEL);

const dappBrowserStateChannel = new RendererIpcChannel<
  DappBrowserStateMainRequest,
  DappBrowserStateRendererResponse
>(DAPP_BROWSER_STATE_CHANNEL);
let stateHandler: ((isOpen: boolean) => void) | undefined;
let stateRegistered = false;

export const bindDappBrowserState = (
  next: (isOpen: boolean) => void
): (() => void) => {
  stateHandler = next;
  if (!stateRegistered) {
    stateRegistered = true;
    dappBrowserStateChannel.onReceive(async (isOpen) => {
      if (typeof isOpen === 'boolean') stateHandler?.(isOpen);
    });
  }
  return () => {
    if (stateHandler === next) stateHandler = undefined;
  };
};
