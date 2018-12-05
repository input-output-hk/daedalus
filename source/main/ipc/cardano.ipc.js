// @flow
import type {
  CardanoNodeState, CardanoStatus,
  FaultInjectionIpcRequest,
  TlsConfig
} from '../../common/types/cardano-node.types';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  CARDANO_FAULT_INJECTION_CHANNEL,
  CARDANO_RESTART_CHANNEL,
  CARDANO_STATE_CHANGE_CHANNEL,
  CARDANO_STATUS_CHANNEL,
  CARDANO_TLS_CONFIG_CHANNEL,
  CARDANO_AWAIT_UPDATE_CHANNEL
} from '../../common/ipc/api';

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

export const cardanoFaultInjectionChannel: MainIpcChannel<FaultInjectionIpcRequest, void> = (
  new MainIpcChannel(CARDANO_FAULT_INJECTION_CHANNEL)
);

export const cardanoStatusChannel: MainIpcChannel<CardanoStatus, ?CardanoStatus> = (
  new MainIpcChannel(CARDANO_STATUS_CHANNEL)
);
