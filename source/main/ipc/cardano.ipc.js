// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  CARDANO_FAULT_INJECTION_CHANNEL,
  CARDANO_RESTART_CHANNEL,
  CARDANO_STATE_CHANNEL,
  GET_CACHED_CARDANO_STATUS_CHANNEL,
  CARDANO_TLS_CONFIG_CHANNEL,
  CARDANO_AWAIT_UPDATE_CHANNEL,
  SET_CACHED_CARDANO_STATUS_CHANNEL
} from '../../common/ipc/api';
import type {
  CardanoAwaitUpdateResponse,
  CardanoAwaitUpdateRequest,
  CardanoFaultInjectionResponse,
  CardanoFaultInjectionRequest,
  CardanoRestartResponse,
  CardanoRestartRequest,
  CardanoStateResponse,
  CardanoStateRequest,
  CardanoTlsConfigResponse,
  CardanoTlsConfigRequest,
  GetCachedCardanoStatusRequest,
  GetCachedCardanoStatusResponse,
  SetCachedCardanoStatusRequest,
  SetCachedCardanoStatusResponse
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const cardanoRestartChannel: (
  MainIpcChannel<CardanoRestartRequest, CardanoRestartResponse>
) = new MainIpcChannel(CARDANO_RESTART_CHANNEL);

export const cardanoTlsConfigChannel: (
  MainIpcChannel<CardanoTlsConfigRequest, CardanoTlsConfigResponse>
) = new MainIpcChannel(CARDANO_TLS_CONFIG_CHANNEL);

export const cardanoAwaitUpdateChannel: (
  MainIpcChannel<CardanoAwaitUpdateRequest, CardanoAwaitUpdateResponse>
) = new MainIpcChannel(CARDANO_AWAIT_UPDATE_CHANNEL);

export const cardanoStateChangeChannel: (
  MainIpcChannel<CardanoStateRequest, CardanoStateResponse>
) = new MainIpcChannel(CARDANO_STATE_CHANNEL);

export const cardanoFaultInjectionChannel: (
  MainIpcChannel<CardanoFaultInjectionRequest, CardanoFaultInjectionResponse>
) = new MainIpcChannel(CARDANO_FAULT_INJECTION_CHANNEL);

export const getCachedCardanoStatusChannel: (
  MainIpcChannel<GetCachedCardanoStatusRequest, GetCachedCardanoStatusResponse>
) = new MainIpcChannel(GET_CACHED_CARDANO_STATUS_CHANNEL);

export const setCachedCardanoStatusChannel: (
  MainIpcChannel<SetCachedCardanoStatusRequest, SetCachedCardanoStatusResponse>
) = new MainIpcChannel(SET_CACHED_CARDANO_STATUS_CHANNEL);
