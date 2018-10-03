// @flow
import type { CardanoNodeState, TlsConfig } from '../../../common/types/cardanoNode.types';
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  CARDANO_AWAIT_UPDATE_CHANNEL,
  CARDANO_STATE_CHANGE_CHANNEL,
  CARDANO_TLS_CONFIG_CHANNEL,
  CARDANO_RESTART_CHANNEL
} from '../../../common/ipc/cardano.ipc';

// IpcChannel<Request, AwaitedResponse, ReceivedMessage, Response>

export const tlsConfigChannel: RendererIpcChannel<void, TlsConfig, TlsConfig, void> = (
  new RendererIpcChannel(CARDANO_TLS_CONFIG_CHANNEL)
);

export const restartCardanoNodeChannel: RendererIpcChannel<void, void, void, void> = (
  new RendererIpcChannel(CARDANO_RESTART_CHANNEL)
);

export const cardanoStateChangeChannel: RendererIpcChannel<void, void, CardanoNodeState, void> = (
  new RendererIpcChannel(CARDANO_STATE_CHANGE_CHANNEL)
);

export const awaitUpdateChannel: RendererIpcChannel<void, void, void, void> = (
  new RendererIpcChannel(CARDANO_AWAIT_UPDATE_CHANNEL)
);
