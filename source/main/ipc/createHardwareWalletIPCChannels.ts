import { IpcChannel } from '../../common/ipc/lib/IpcChannel';
import type {
  deriveAddressMainResponse,
  deriveAddressRendererRequest,
  deriveXpubMainResponse,
  deriveXpubRendererRequest,
  getCardanoAdaAppMainResponse,
  getCardanoAdaAppRendererRequest,
  getExtendedPublicKeyMainResponse,
  getExtendedPublicKeyRendererRequest,
  getHardwareWalletConnectiontMainRequest,
  getHardwareWalletConnectiontRendererResponse,
  getHardwareWalletTransportMainResponse,
  getHardwareWalletTransportRendererRequest,
  handleInitLedgerConnectMainResponse,
  handleInitLedgerConnectRendererRequest,
  handleInitTrezorConnectMainResponse,
  handleInitTrezorConnectRendererRequest,
  resetTrezorActionMainResponse,
  resetTrezorActionRendererRequest,
  showAddressMainResponse,
  showAddressRendererRequest,
  signTransactionLedgerMainResponse,
  signTransactionLedgerRendererRequest,
  signTransactionTrezorMainResponse,
  signTransactionTrezorRendererRequest,
} from '../../common/ipc/api';
import {
  DERIVE_ADDRESS_CHANNEL,
  DERIVE_XPUB_CHANNEL,
  GET_CARDANO_ADA_APP_CHANNEL,
  GET_EXTENDED_PUBLIC_KEY_CHANNEL,
  GET_HARDWARE_WALLET_CONNECTION_CHANNEL,
  GET_HARDWARE_WALLET_TRANSPORT_CHANNEL,
  GET_INIT_LEDGER_CONNECT_CHANNEL,
  GET_INIT_TREZOR_CONNECT_CHANNEL,
  RESET_ACTION_TREZOR_CHANNEL,
  SHOW_ADDRESS_CHANNEL,
  SIGN_TRANSACTION_LEDGER_CHANNEL,
  SIGN_TRANSACTION_TREZOR_CHANNEL,
} from '../../common/ipc/api';

export const createChannels = <T, K>(Channel: IpcChannel<T, K>) => {
  const getHardwareWalletTransportChannel: IpcChannel<
    getHardwareWalletTransportRendererRequest,
    getHardwareWalletTransportMainResponse
  > = new Channel(GET_HARDWARE_WALLET_TRANSPORT_CHANNEL);
  const getExtendedPublicKeyChannel: IpcChannel<
    getExtendedPublicKeyRendererRequest,
    getExtendedPublicKeyMainResponse
  > = new Channel(GET_EXTENDED_PUBLIC_KEY_CHANNEL);
  const getCardanoAdaAppChannel: IpcChannel<
    getCardanoAdaAppRendererRequest,
    getCardanoAdaAppMainResponse
  > = new Channel(GET_CARDANO_ADA_APP_CHANNEL);
  const getHardwareWalletConnectionChannel: IpcChannel<
    getHardwareWalletConnectiontMainRequest,
    getHardwareWalletConnectiontRendererResponse
  > = new Channel(GET_HARDWARE_WALLET_CONNECTION_CHANNEL);
  const signTransactionLedgerChannel: IpcChannel<
    signTransactionLedgerRendererRequest,
    signTransactionLedgerMainResponse
  > = new Channel(SIGN_TRANSACTION_LEDGER_CHANNEL);
  const signTransactionTrezorChannel: IpcChannel<
    signTransactionTrezorRendererRequest,
    signTransactionTrezorMainResponse
  > = new Channel(SIGN_TRANSACTION_TREZOR_CHANNEL);
  const resetTrezorActionChannel: IpcChannel<
    resetTrezorActionRendererRequest,
    resetTrezorActionMainResponse
  > = new Channel(RESET_ACTION_TREZOR_CHANNEL);
  const handleInitTrezorConnectChannel: IpcChannel<
    handleInitTrezorConnectRendererRequest,
    handleInitTrezorConnectMainResponse
  > = new Channel(GET_INIT_TREZOR_CONNECT_CHANNEL);
  const handleInitLedgerConnectChannel: IpcChannel<
    handleInitLedgerConnectRendererRequest,
    handleInitLedgerConnectMainResponse
  > = new Channel(GET_INIT_LEDGER_CONNECT_CHANNEL);
  const deriveXpubChannel: IpcChannel<
    deriveXpubRendererRequest,
    deriveXpubMainResponse
  > = new Channel(DERIVE_XPUB_CHANNEL);
  const deriveAddressChannel: IpcChannel<
    deriveAddressRendererRequest,
    deriveAddressMainResponse
  > = new Channel(DERIVE_ADDRESS_CHANNEL);
  const showAddressChannel: IpcChannel<
    showAddressRendererRequest,
    showAddressMainResponse
  > = new Channel(SHOW_ADDRESS_CHANNEL);

  return {
    getHardwareWalletTransportChannel,
    getExtendedPublicKeyChannel,
    getCardanoAdaAppChannel,
    getHardwareWalletConnectionChannel,
    signTransactionLedgerChannel,
    signTransactionTrezorChannel,
    resetTrezorActionChannel,
    handleInitTrezorConnectChannel,
    handleInitLedgerConnectChannel,
    deriveXpubChannel,
    deriveAddressChannel,
    showAddressChannel,
  };
};
