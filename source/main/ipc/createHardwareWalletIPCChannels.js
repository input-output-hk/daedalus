'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.createChannels = void 0;
const api_1 = require('../../common/ipc/api');
const createChannels = (Channel) => {
  return {
    getHardwareWalletTransportChannel: new Channel(
      api_1.GET_HARDWARE_WALLET_TRANSPORT_CHANNEL
    ),
    getExtendedPublicKeyChannel: new Channel(
      api_1.GET_EXTENDED_PUBLIC_KEY_CHANNEL
    ),
    getCardanoAdaAppChannel: new Channel(api_1.GET_CARDANO_ADA_APP_CHANNEL),
    getHardwareWalletConnectionChannel: new Channel(
      api_1.GET_HARDWARE_WALLET_CONNECTION_CHANNEL
    ),
    signTransactionLedgerChannel: new Channel(
      api_1.SIGN_TRANSACTION_LEDGER_CHANNEL
    ),
    signTransactionTrezorChannel: new Channel(
      api_1.SIGN_TRANSACTION_TREZOR_CHANNEL
    ),
    resetTrezorActionChannel: new Channel(api_1.RESET_ACTION_TREZOR_CHANNEL),
    handleInitTrezorConnectChannel: new Channel(
      api_1.GET_INIT_TREZOR_CONNECT_CHANNEL
    ),
    handleInitLedgerConnectChannel: new Channel(
      api_1.GET_INIT_LEDGER_CONNECT_CHANNEL
    ),
    deriveXpubChannel: new Channel(api_1.DERIVE_XPUB_CHANNEL),
    deriveAddressChannel: new Channel(api_1.DERIVE_ADDRESS_CHANNEL),
    showAddressChannel: new Channel(api_1.SHOW_ADDRESS_CHANNEL),
    waitForLedgerDevicesToConnectChannel: new Channel(
      api_1.WAIT_FOR_LEDGER_DEVICES
    ),
  };
};
exports.createChannels = createChannels;
//# sourceMappingURL=createHardwareWalletIPCChannels.js.map
