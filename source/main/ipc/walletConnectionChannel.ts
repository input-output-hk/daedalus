import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_WALLET_CONNECTION_CHANNEL } from '../../common/ipc/api';
import type {
  GetWalletConnectionRendererRequest,
  GetWalletConnectionMainResponse,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>
export const getWalletConnectionChannel: MainIpcChannel<
  GetWalletConnectionRendererRequest,
  GetWalletConnectionMainResponse
> = new MainIpcChannel(GET_WALLET_CONNECTION_CHANNEL);
