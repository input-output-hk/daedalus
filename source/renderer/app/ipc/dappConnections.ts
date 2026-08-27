import {
  DAPP_CONNECTIONS_CHANNEL,
  DappConnectionsMainResponse,
  DappConnectionsRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const dappConnectionsChannel = new RendererIpcChannel<
  DappConnectionsMainResponse,
  DappConnectionsRendererRequest
>(DAPP_CONNECTIONS_CHANNEL);
