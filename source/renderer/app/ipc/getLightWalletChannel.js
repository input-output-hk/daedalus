// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  CREATE_WALLET_CHANNEL,
} from '../../../common/ipc/lightWalletApi';

import type {
  createWalletRendererRequest,
  createWalletMainResponse,
} from '../../../common/ipc/lightWalletApi';

// IpcChannel<Incoming, Outgoing>
export const createWalletChannel: RendererIpcChannel<
  createWalletMainResponse,
  createWalletRendererRequest
> = new RendererIpcChannel(CREATE_WALLET_CHANNEL);
