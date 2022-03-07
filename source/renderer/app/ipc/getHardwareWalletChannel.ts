import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  GET_HARDWARE_WALLET_TRANSPORT_CHANNEL,
  GET_EXTENDED_PUBLIC_KEY_CHANNEL,
  GET_CARDANO_ADA_APP_CHANNEL,
  GET_HARDWARE_WALLET_CONNECTION_CHANNEL,
  SIGN_TRANSACTION_LEDGER_CHANNEL,
  SIGN_TRANSACTION_TREZOR_CHANNEL,
  GET_INIT_TREZOR_CONNECT_CHANNEL,
  GET_INIT_LEDGER_CONNECT_CHANNEL,
  RESET_ACTION_TREZOR_CHANNEL,
  DERIVE_XPUB_CHANNEL,
  DERIVE_ADDRESS_CHANNEL,
  SHOW_ADDRESS_CHANNEL,
} from '../../../common/ipc/api';
import type {
  getHardwareWalletTransportRendererRequest,
  getHardwareWalletTransportMainResponse,
  getExtendedPublicKeyRendererRequest,
  getExtendedPublicKeyMainResponse,
  getHardwareWalletConnectiontMainRequest,
  getHardwareWalletConnectiontRendererResponse,
  signTransactionLedgerMainResponse,
  signTransactionLedgerRendererRequest,
  signTransactionTrezorMainResponse,
  signTransactionTrezorRendererRequest,
  getCardanoAdaAppMainResponse,
  getCardanoAdaAppRendererRequest,
  handleInitTrezorConnectMainResponse,
  handleInitTrezorConnectRendererRequest,
  handleInitLedgerConnectMainResponse,
  handleInitLedgerConnectRendererRequest,
  deriveXpubMainResponse,
  deriveXpubRendererRequest,
  resetTrezorActionMainResponse,
  resetTrezorActionRendererRequest,
  deriveAddressMainResponse,
  deriveAddressRendererRequest,
  showAddressMainResponse,
  showAddressRendererRequest,
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
export const signTransactionLedgerChannel: RendererIpcChannel<
  signTransactionLedgerMainResponse,
  signTransactionLedgerRendererRequest
> = new RendererIpcChannel(SIGN_TRANSACTION_LEDGER_CHANNEL);
// IpcChannel<Incoming, Outgoing>
export const signTransactionTrezorChannel: RendererIpcChannel<
  signTransactionTrezorMainResponse,
  signTransactionTrezorRendererRequest
> = new RendererIpcChannel(SIGN_TRANSACTION_TREZOR_CHANNEL);
export const handleInitTrezorConnectChannel: RendererIpcChannel<
  handleInitTrezorConnectMainResponse,
  handleInitTrezorConnectRendererRequest
> = new RendererIpcChannel(GET_INIT_TREZOR_CONNECT_CHANNEL);
export const handleInitLedgerConnectChannel: RendererIpcChannel<
  handleInitLedgerConnectMainResponse,
  handleInitLedgerConnectRendererRequest
> = new RendererIpcChannel(GET_INIT_LEDGER_CONNECT_CHANNEL);
export const deriveXpubChannel: RendererIpcChannel<
  deriveXpubMainResponse,
  deriveXpubRendererRequest
> = new RendererIpcChannel(DERIVE_XPUB_CHANNEL);
export const resetTrezorActionChannel: RendererIpcChannel<
  resetTrezorActionMainResponse,
  resetTrezorActionRendererRequest
> = new RendererIpcChannel(RESET_ACTION_TREZOR_CHANNEL);
export const deriveAddressChannel: RendererIpcChannel<
  deriveAddressMainResponse,
  deriveAddressRendererRequest
> = new RendererIpcChannel(DERIVE_ADDRESS_CHANNEL);
export const showAddressChannel: RendererIpcChannel<
  showAddressMainResponse,
  showAddressRendererRequest
> = new RendererIpcChannel(SHOW_ADDRESS_CHANNEL);
