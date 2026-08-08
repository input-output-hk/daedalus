import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  MITHRIL_PROGRESS_CHANNEL,
  MITHRIL_STATUS_CHANNEL,
  WALLET_PORT_CHANNEL,
} from '../../../common/ipc/api';
import type {
  MithrilProgressMainRequest,
  MithrilProgressRendererResponse,
  MithrilStatusMainRequest,
  MithrilStatusRendererResponse,
  WalletPortMainRequest,
  WalletPortRendererResponse,
} from '../../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>
// Push channels receive from main; Incoming is the event payload, Outgoing is the void ack.
export const mithrilProgressChannel: RendererIpcChannel<
  MithrilProgressMainRequest,
  MithrilProgressRendererResponse
> = new RendererIpcChannel(MITHRIL_PROGRESS_CHANNEL);

export const mithrilStatusChannel: RendererIpcChannel<
  MithrilStatusMainRequest,
  MithrilStatusRendererResponse
> = new RendererIpcChannel(MITHRIL_STATUS_CHANNEL);

export const walletPortChannel: RendererIpcChannel<
  WalletPortMainRequest,
  WalletPortRendererResponse
> = new RendererIpcChannel(WALLET_PORT_CHANNEL);
