// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  GET_HARDWARE_WALLET_TRANSPORT_CHANNEL,
  GET_EXTENDED_PUBLIC_KEY_CHANNEL,
  GET_CARDANO_ADA_APP_CHANNEL,
  GET_HARDWARE_WALLET_CONNECTION_CHANNEL,
  DERIVE_ADDRESS_CHANNEL,
  SHOW_ADDRESS_CHANNEL,
  ATTEST_UTXO_CHANNEL,
  SIGN_TRANSACTION_CHANNEL,
} from '../../../common/ipc/api';

import type {
  getHardwareWalletTransportRendererRequest,
  getHardwareWalletTransportMainResponse,
  getExtendedPublicKeytRendererRequest,
  getExtendedPublicKeytMainResponse,
  getHardwareWalletConnectiontMainRequest,
  getHardwareWalletConnectiontRendererResponse,
  deriveAddressMainResponse,
  deriveAddressRendererRequest,
  showAddresMainResponse,
  showAddressRendererRequest,
  attestUtxoMainResponse,
  attestUtxoRendererRequest,
  signTransaMainResponse,
  signTransactionRendererRequest,
} from '../../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>
export const getHardwareWalletTransportChannel: RendererIpcChannel<
  getHardwareWalletTransportMainResponse,
  getHardwareWalletTransportRendererRequest
> = new RendererIpcChannel(GET_HARDWARE_WALLET_TRANSPORT_CHANNEL);

// IpcChannel<Incoming, Outgoing>
export const getExtendedPublicKeyChannel: RendererIpcChannel<
  getExtendedPublicKeytMainResponse,
  getExtendedPublicKeytRendererRequest
> = new RendererIpcChannel(GET_EXTENDED_PUBLIC_KEY_CHANNEL);

// IpcChannel<Incoming, Outgoing>
export const getCardanoAdaAppChannel: RendererIpcChannel<
  getCardanoAdaApptMainResponse,
  getCardanoAdaApptRendererRequest
> = new RendererIpcChannel(GET_CARDANO_ADA_APP_CHANNEL);

// IpcChannel<Incoming, Outgoing>
export const getHardwareWalletConnectionChannel: RendererIpcChannel<
  getHardwareWalletConnectiontMainRequest,
  getHardwareWalletConnectiontRendererResponse
> = new RendererIpcChannel(GET_HARDWARE_WALLET_CONNECTION_CHANNEL);

// IpcChannel<Incoming, Outgoing>
export const deriveAddressChannel: RendererIpcChannel<
  deriveAddressMainResponse,
  deriveAddressRendererRequest
> = new RendererIpcChannel(DERIVE_ADDRESS_CHANNEL);

// IpcChannel<Incoming, Outgoing>
export const showAddressChannel: RendererIpcChannel<
  showAddresMainResponse,
  showAddressRendererRequest
> = new RendererIpcChannel(SHOW_ADDRESS_CHANNEL);

// IpcChannel<Incoming, Outgoing>
export const attestUtxoChannel: RendererIpcChannel<
  attestUtxoMainResponse,
  attestUtxoRendererRequest
> = new RendererIpcChannel(ATTEST_UTXO_CHANNEL);

// IpcChannel<Incoming, Outgoing>
export const signTransactionChannel: RendererIpcChannel<
  signTransaMainResponse,
  signTransactionRendererRequest
> = new RendererIpcChannel(SIGN_TRANSACTION_CHANNEL);
