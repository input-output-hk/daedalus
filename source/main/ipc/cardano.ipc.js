// @flow
import {
  CARDANO_AWAIT_UPDATE_CHANNEL,
  CARDANO_RESTART_CHANNEL,
  CARDANO_TLS_CONFIG_CHANNEL,
  CARDANO_STATE_CHANGE_CHANNEL,
  CARDANO_FAULT_INJECTION_CHANNEL
} from '../../common/ipc/cardano.ipc';
import type {
  CardanoNodeState,
  FaultInjectionParam,
  TlsConfig
} from '../../common/types/cardanoNode.types';
import { MainIpcChannel } from './lib/MainIpcChannel';

// IpcChannel<Incoming, Outgoing>

export const cardanoRestartChannel: MainIpcChannel<void, void> = (
  new MainIpcChannel(CARDANO_RESTART_CHANNEL)
);

export const cardanoTlsConfigChannel: MainIpcChannel<void, ?TlsConfig> = (
  new MainIpcChannel(CARDANO_TLS_CONFIG_CHANNEL)
);

export const cardanoAwaitUpdateChannel: MainIpcChannel<void, void> = (
  new MainIpcChannel(CARDANO_AWAIT_UPDATE_CHANNEL)
);

export const cardanoStateChangeChannel: MainIpcChannel<void, CardanoNodeState> = (
  new MainIpcChannel(CARDANO_STATE_CHANGE_CHANNEL)
);

export const cardanoFaultInjectionChannel: MainIpcChannel<FaultInjectionParam, void> = (
  new MainIpcChannel(CARDANO_FAULT_INJECTION_CHANNEL)
);
