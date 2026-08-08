import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  MITHRIL_PROGRESS_CHANNEL,
  MITHRIL_STATUS_CHANNEL,
  WALLET_PORT_CHANNEL,
} from '../../common/ipc/api';
import type {
  MithrilProgressMainRequest,
  MithrilProgressRendererResponse,
  MithrilStatusMainRequest,
  MithrilStatusRendererResponse,
  WalletPortMainRequest,
  WalletPortRendererResponse,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>
// Push channels are send-only from main; type params are swapped so
// main's Outgoing is the event payload and Incoming is the void ack.
export const mithrilProgressChannel: MainIpcChannel<
  MithrilProgressRendererResponse,
  MithrilProgressMainRequest
> = new MainIpcChannel(MITHRIL_PROGRESS_CHANNEL);

export const mithrilStatusChannel: MainIpcChannel<
  MithrilStatusRendererResponse,
  MithrilStatusMainRequest
> = new MainIpcChannel(MITHRIL_STATUS_CHANNEL);

export const walletPortChannel: MainIpcChannel<
  WalletPortRendererResponse,
  WalletPortMainRequest
> = new MainIpcChannel(WALLET_PORT_CHANNEL);
