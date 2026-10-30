import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { GET_WALLET_CONNECTION_CHANNEL } from '../../../common/ipc/api';
import type {
  GetWalletConnectionRendererRequest,
  GetWalletConnectionMainResponse,
} from '../../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>
export const getWalletConnectionChannel: RendererIpcChannel<
  GetWalletConnectionMainResponse,
  GetWalletConnectionRendererRequest
> = new RendererIpcChannel(GET_WALLET_CONNECTION_CHANNEL);
