// @flow
import {
  CARDANO_AWAIT_UPDATE_CHANNEL,
  CARDANO_RESTART_CHANNEL,
  CARDANO_TLS_CONFIG_CHANNEL,
  CARDANO_STATE_CHANGE_CHANNEL
} from '../../common/ipc/cardano.ipc';
import type { CardanoNodeState, TlsConfig } from '../../common/types/cardanoNode.types';
import { MainIpcChannel } from './lib/MainIpcChannel';

// IpcChannel<Request, AwaitedResponse, ReceivedRequest, Response>

export const cardanoRestartChannel: MainIpcChannel<void, void, void, void> = (
  new MainIpcChannel(CARDANO_RESTART_CHANNEL)
);

export const cardanoTlsConfigChannel: MainIpcChannel<TlsConfig, void, void, TlsConfig> = (
  new MainIpcChannel(CARDANO_TLS_CONFIG_CHANNEL)
);

export const cardanoAwaitUpdateChannel: MainIpcChannel<void, void, void, void> = (
  new MainIpcChannel(CARDANO_AWAIT_UPDATE_CHANNEL)
);

export const cardanoStateChangeChannel: MainIpcChannel<CardanoNodeState, void, void, void> = (
  new MainIpcChannel(CARDANO_STATE_CHANGE_CHANNEL)
);
