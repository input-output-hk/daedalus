// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  GET_HARDWARE_WALLET_TRANSPORT_CHANNEL,
  GET_EXTENDED_PUBLIC_KEY_CHANNEL,
  GET_CARDANO_ADA_APP_CHANNEL,
  GET_HARDWARE_WALLET_CONNECTION_CHANNEL,
  SIGN_TRANSACTION_CHANNEL,
} from '../../../common/ipc/api';

import type {
  getHardwareWalletTransportRendererRequest,
  getHardwareWalletTransportMainResponse,
  getExtendedPublicKeyRendererRequest,
  getExtendedPublicKeyMainResponse,
  getHardwareWalletConnectiontMainRequest,
  getHardwareWalletConnectiontRendererResponse,
  signTransaMainResponse,
  signTransactionRendererRequest,
  getCardanoAdaAppMainResponse,
  getCardanoAdaAppRendererRequest,
} from '../../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>
export const getHardwareWalletTransportChannel: RendererIpcChannel<
  getHardwareWalletTransportMainResponse,
  getHardwareWalletTransportRendererRequest
> = new RendererIpcChannel(GET_HARDWARE_WALLET_TRANSPORT_CHANNEL);

// IpcChannel<Incoming, Outgoing>
export const getExtendedPublicKeyChannel: RendererIpcChannel<
  getExtendedPublicKeyMainResponse,
  getExtendedPublicKeyRendererRequest
> = new RendererIpcChannel(GET_EXTENDED_PUBLIC_KEY_CHANNEL);

// IpcChannel<Incoming, Outgoing>
export const getCardanoAdaAppChannel: RendererIpcChannel<
  getCardanoAdaAppMainResponse,
  getCardanoAdaAppRendererRequest
> = new RendererIpcChannel(GET_CARDANO_ADA_APP_CHANNEL);

// IpcChannel<Incoming, Outgoing>
export const getHardwareWalletConnectionChannel: RendererIpcChannel<
  getHardwareWalletConnectiontMainRequest,
  getHardwareWalletConnectiontRendererResponse
> = new RendererIpcChannel(GET_HARDWARE_WALLET_CONNECTION_CHANNEL);

// IpcChannel<Incoming, Outgoing>
export const signTransactionChannel: RendererIpcChannel<
  signTransaMainResponse,
  signTransactionRendererRequest
> = new RendererIpcChannel(SIGN_TRANSACTION_CHANNEL);
