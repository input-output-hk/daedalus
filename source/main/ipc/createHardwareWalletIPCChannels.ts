import { IpcChannel } from '../../common/ipc/lib/IpcChannel';
import {
  deriveAddressMainResponse,
  deriveAddressRendererRequest,
  deriveXpubMainResponse,
  deriveXpubRendererRequest,
  getCardanoAdaAppMainResponse,
  getCardanoAdaAppRendererRequest,
  getExtendedPublicKeyMainResponse,
  getExtendedPublicKeyRendererRequest,
  getHardwareWalletConnectionMainRequest,
  getHardwareWalletConnectionRendererResponse,
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
  WAIT_FOR_LEDGER_DEVICES,
  waitForLedgerDevicesRequest,
  waitForLedgerDevicesResponse,
} from '../../common/ipc/api';

export interface HardwareWalletChannels {
  getHardwareWalletTransportChannel: IpcChannel<
    getHardwareWalletTransportRendererRequest,
    getHardwareWalletTransportMainResponse
  >;
  getExtendedPublicKeyChannel: IpcChannel<
    getExtendedPublicKeyRendererRequest,
    getExtendedPublicKeyMainResponse
  >;
  getCardanoAdaAppChannel: IpcChannel<
    getCardanoAdaAppRendererRequest,
    getCardanoAdaAppMainResponse
  >;

  getHardwareWalletConnectionChannel: IpcChannel<
    getHardwareWalletConnectionMainRequest,
    getHardwareWalletConnectionRendererResponse
  >;

  signTransactionLedgerChannel: IpcChannel<
    signTransactionLedgerRendererRequest,
    signTransactionLedgerMainResponse
  >;

  signTransactionTrezorChannel: IpcChannel<
    signTransactionTrezorRendererRequest,
    signTransactionTrezorMainResponse
  >;

  resetTrezorActionChannel: IpcChannel<
    resetTrezorActionRendererRequest,
    resetTrezorActionMainResponse
  >;

  handleInitTrezorConnectChannel: IpcChannel<
    handleInitTrezorConnectRendererRequest,
    handleInitTrezorConnectMainResponse
  >;

  handleInitLedgerConnectChannel: IpcChannel<
    handleInitLedgerConnectRendererRequest,
    handleInitLedgerConnectMainResponse
  >;

  deriveXpubChannel: IpcChannel<
    deriveXpubRendererRequest,
    deriveXpubMainResponse
  >;

  deriveAddressChannel: IpcChannel<
    deriveAddressRendererRequest,
    deriveAddressMainResponse
  >;

  showAddressChannel: IpcChannel<
    showAddressRendererRequest,
    showAddressMainResponse
  >;

  waitForLedgerDevicesToConnectChannel: IpcChannel<
    waitForLedgerDevicesRequest,
    waitForLedgerDevicesResponse
  >;
}

export const createChannels = (
  Channel: typeof IpcChannel
): HardwareWalletChannels => {
  return {
    getHardwareWalletTransportChannel: new Channel(
      GET_HARDWARE_WALLET_TRANSPORT_CHANNEL
    ),
    getExtendedPublicKeyChannel: new Channel(GET_EXTENDED_PUBLIC_KEY_CHANNEL),
    getCardanoAdaAppChannel: new Channel(GET_CARDANO_ADA_APP_CHANNEL),
    getHardwareWalletConnectionChannel: new Channel(
      GET_HARDWARE_WALLET_CONNECTION_CHANNEL
    ),
    signTransactionLedgerChannel: new Channel(SIGN_TRANSACTION_LEDGER_CHANNEL),
    signTransactionTrezorChannel: new Channel(SIGN_TRANSACTION_TREZOR_CHANNEL),
    resetTrezorActionChannel: new Channel(RESET_ACTION_TREZOR_CHANNEL),
    handleInitTrezorConnectChannel: new Channel(
      GET_INIT_TREZOR_CONNECT_CHANNEL
    ),
    handleInitLedgerConnectChannel: new Channel(
      GET_INIT_LEDGER_CONNECT_CHANNEL
    ),
    deriveXpubChannel: new Channel(DERIVE_XPUB_CHANNEL),
    deriveAddressChannel: new Channel(DERIVE_ADDRESS_CHANNEL),
    showAddressChannel: new Channel(SHOW_ADDRESS_CHANNEL),
    waitForLedgerDevicesToConnectChannel: new Channel(WAIT_FOR_LEDGER_DEVICES),
  };
};
