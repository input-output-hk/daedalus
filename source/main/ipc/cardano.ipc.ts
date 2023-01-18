import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  CARDANO_FAULT_INJECTION_CHANNEL,
  CARDANO_RESTART_CHANNEL,
  CARDANO_STATE_CHANNEL,
  GET_CACHED_CARDANO_STATUS_CHANNEL,
  CARDANO_TLS_CONFIG_CHANNEL,
  CARDANO_AWAIT_UPDATE_CHANNEL,
  SET_CACHED_CARDANO_STATUS_CHANNEL,
  EXPORT_WALLETS_CHANNEL,
} from '../../common/ipc/api';
import type {
  CardanoAwaitUpdateMainResponse,
  CardanoAwaitUpdateRendererRequest,
  CardanoFaultInjectionMainResponse,
  CardanoFaultInjectionRendererRequest,
  CardanoRestartMainResponse,
  CardanoRestartRendererRequest,
  CardanoStateRendererResponse,
  CardanoStateRendererRequest,
  CardanoTlsConfigMainResponse,
  CardanoTlsConfigRendererRequest,
  GetCachedCardanoStatusRendererRequest,
  GetCachedCardanoStatusMainResponse,
  SetCachedCardanoStatusRendererRequest,
  SetCachedCardanoStatusMainResponse,
  ExportWalletsRendererRequest,
  ExportWalletsMainResponse,
} from '../../common/ipc/api';
// IpcChannel<Incoming, Outgoing>
export const cardanoRestartChannel: MainIpcChannel<
  CardanoRestartRendererRequest,
  CardanoRestartMainResponse
> = new MainIpcChannel(CARDANO_RESTART_CHANNEL);
export const cardanoTlsConfigChannel: MainIpcChannel<
  CardanoTlsConfigRendererRequest,
  CardanoTlsConfigMainResponse
> = new MainIpcChannel(CARDANO_TLS_CONFIG_CHANNEL);
export const cardanoAwaitUpdateChannel: MainIpcChannel<
  CardanoAwaitUpdateRendererRequest,
  CardanoAwaitUpdateMainResponse
> = new MainIpcChannel(CARDANO_AWAIT_UPDATE_CHANNEL);
export const cardanoStateChangeChannel: MainIpcChannel<
  CardanoStateRendererRequest,
  CardanoStateRendererResponse
> = new MainIpcChannel(CARDANO_STATE_CHANNEL);
export const cardanoFaultInjectionChannel: MainIpcChannel<
  CardanoFaultInjectionRendererRequest,
  CardanoFaultInjectionMainResponse
> = new MainIpcChannel(CARDANO_FAULT_INJECTION_CHANNEL);
export const getCachedCardanoStatusChannel: MainIpcChannel<
  GetCachedCardanoStatusRendererRequest,
  GetCachedCardanoStatusMainResponse
> = new MainIpcChannel(GET_CACHED_CARDANO_STATUS_CHANNEL);
export const setCachedCardanoStatusChannel: MainIpcChannel<
  SetCachedCardanoStatusRendererRequest,
  SetCachedCardanoStatusMainResponse
> = new MainIpcChannel(SET_CACHED_CARDANO_STATUS_CHANNEL);
export const exportWalletsChannel: MainIpcChannel<
  ExportWalletsRendererRequest,
  ExportWalletsMainResponse
> = new MainIpcChannel(EXPORT_WALLETS_CHANNEL);
