// @flow
import type {
  CardanoNodeState, CardanoStatus,
  FaultInjectionIpcRequest,
  TlsConfig
} from '../../../common/types/cardano-node.types';
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  CARDANO_AWAIT_UPDATE_CHANNEL} from '../../../common/ipc/api';
import {
  CARDANO_FAULT_INJECTION_CHANNEL, CARDANO_RESTART_CHANNEL, CARDANO_STATE_CHANGE_CHANNEL,
  CARDANO_STATUS_CHANNEL, CARDANO_TLS_CONFIG_CHANNEL
} from '../../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const tlsConfigChannel: RendererIpcChannel<?TlsConfig, void> = (
  new RendererIpcChannel(CARDANO_TLS_CONFIG_CHANNEL)
);

export const restartCardanoNodeChannel: RendererIpcChannel<void, void> = (
  new RendererIpcChannel(CARDANO_RESTART_CHANNEL)
);

export const cardanoStateChangeChannel: RendererIpcChannel<CardanoNodeState, void> = (
  new RendererIpcChannel(CARDANO_STATE_CHANGE_CHANNEL)
);

export const awaitUpdateChannel: RendererIpcChannel<void, void> = (
  new RendererIpcChannel(CARDANO_AWAIT_UPDATE_CHANNEL)
);

export const cardanoFaultInjectionChannel: RendererIpcChannel<void, FaultInjectionIpcRequest> = (
  new RendererIpcChannel(CARDANO_FAULT_INJECTION_CHANNEL)
);

export const cardanoStatusChannel: RendererIpcChannel<?CardanoStatus, CardanoStatus> = (
  new RendererIpcChannel(CARDANO_STATUS_CHANNEL)
);
