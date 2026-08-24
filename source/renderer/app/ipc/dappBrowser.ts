import {
  DAPP_BROWSER_CLOSE_CHANNEL,
  DAPP_BROWSER_OPEN_CHANNEL,
} from '../../../common/ipc/api';
import type {
  DappBrowserCloseMainResponse,
  DappBrowserCloseRendererRequest,
  DappBrowserOpenMainResponse,
  DappBrowserOpenRendererRequest,
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
