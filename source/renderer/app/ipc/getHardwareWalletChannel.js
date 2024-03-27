'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.waitForLedgerDevicesToConnectChannel = exports.showAddressChannel = exports.deriveAddressChannel = exports.resetTrezorActionChannel = exports.deriveXpubChannel = exports.handleInitLedgerConnectChannel = exports.handleInitTrezorConnectChannel = exports.signTransactionTrezorChannel = exports.signTransactionLedgerChannel = exports.getHardwareWalletConnectionChannel = exports.getCardanoAdaAppChannel = exports.getExtendedPublicKeyChannel = exports.getHardwareWalletTransportChannel = void 0;
const RendererIpcChannel_1 = require('./lib/RendererIpcChannel');
const api_1 = require('../../../common/ipc/api');
// IpcChannel<Incoming, Outgoing>
exports.getHardwareWalletTransportChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GET_HARDWARE_WALLET_TRANSPORT_CHANNEL
);
// IpcChannel<Incoming, Outgoing>
exports.getExtendedPublicKeyChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GET_EXTENDED_PUBLIC_KEY_CHANNEL
);
// IpcChannel<Incoming, Outgoing>
exports.getCardanoAdaAppChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GET_CARDANO_ADA_APP_CHANNEL
);
// IpcChannel<Incoming, Outgoing>
exports.getHardwareWalletConnectionChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GET_HARDWARE_WALLET_CONNECTION_CHANNEL
);
// IpcChannel<Incoming, Outgoing>
exports.signTransactionLedgerChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.SIGN_TRANSACTION_LEDGER_CHANNEL
);
// IpcChannel<Incoming, Outgoing>
exports.signTransactionTrezorChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.SIGN_TRANSACTION_TREZOR_CHANNEL
);
exports.handleInitTrezorConnectChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GET_INIT_TREZOR_CONNECT_CHANNEL
);
exports.handleInitLedgerConnectChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.GET_INIT_LEDGER_CONNECT_CHANNEL
);
exports.deriveXpubChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.DERIVE_XPUB_CHANNEL
);
exports.resetTrezorActionChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.RESET_ACTION_TREZOR_CHANNEL
);
exports.deriveAddressChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.DERIVE_ADDRESS_CHANNEL
);
exports.showAddressChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.SHOW_ADDRESS_CHANNEL
);
exports.waitForLedgerDevicesToConnectChannel = new RendererIpcChannel_1.RendererIpcChannel(
  api_1.WAIT_FOR_LEDGER_DEVICES
);
//# sourceMappingURL=getHardwareWalletChannel.js.map
