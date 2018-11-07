// @flow
import type {
  CardanoNodeState,
  FaultInjectionIpcRequest,
  TlsConfig
} from '../../../common/types/cardanoNode.types';
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  CARDANO_AWAIT_UPDATE_CHANNEL,
  CARDANO_STATE_CHANGE_CHANNEL,
  CARDANO_TLS_CONFIG_CHANNEL,
  CARDANO_RESTART_CHANNEL,
  CARDANO_FAULT_INJECTION_CHANNEL
} from '../../../common/ipc/cardano.ipc';

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
