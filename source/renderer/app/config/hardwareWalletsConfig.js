'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getHardwareWalletsNetworkConfig = exports.isHardwareWalletIndicatorEnabled = exports.isHardwareWalletSupportEnabled = exports.isLedgerEnabled = exports.isTrezorEnabled = exports.MINIMAL_TREZOR_FIRMWARE_VERSION = exports.MINIMAL_LEDGER_FIRMWARE_VERSION = exports.MINIMAL_CARDANO_APP_VERSION = exports.AddressTypeNibbles = exports.HW_BYRON_CONFIG = exports.HW_SHELLEY_CONFIG = exports.DEFAULT_ADDRESS_INDEX = exports.ADA_COIN_TYPE = exports.BYRON_PURPOSE_INDEX = exports.SHELLEY_PURPOSE_INDEX = exports.HARDENED = exports.HARDENED_HEX = void 0;
const lodash_1 = require('lodash');
const cardano_node_types_1 = require('../../../common/types/cardano-node.types');
exports.HARDENED_HEX = 0x80000000;
exports.HARDENED = 2147483648;
exports.SHELLEY_PURPOSE_INDEX = 1852;
exports.BYRON_PURPOSE_INDEX = 44;
exports.ADA_COIN_TYPE = 1815;
exports.DEFAULT_ADDRESS_INDEX = 0;
const { isMainnet, isStaging, isSelfnode } = global.environment;
const hardwareWalletNetworksConfig = {};
(0, lodash_1.map)(
  cardano_node_types_1.NetworkMagics,
  (networkMagic, network) => {
    const isMainnetLikeNetwork = isMainnet || isSelfnode || isStaging;
    hardwareWalletNetworksConfig[network] = {
      networkId: isMainnetLikeNetwork ? 1 : networkMagic[1],
      protocolMagic: isMainnetLikeNetwork ? 764824073 : networkMagic[0],
    };
  }
);
exports.HW_SHELLEY_CONFIG = {
  NETWORK: hardwareWalletNetworksConfig,
  DEFAULT_DERIVATION_PATH: [
    exports.HARDENED + exports.SHELLEY_PURPOSE_INDEX,
    exports.HARDENED + exports.ADA_COIN_TYPE,
    exports.HARDENED + exports.DEFAULT_ADDRESS_INDEX,
    2,
    0,
  ],
  ABS_DERIVATION_PATH: [
    exports.HARDENED + exports.SHELLEY_PURPOSE_INDEX,
    exports.HARDENED + exports.ADA_COIN_TYPE,
    exports.HARDENED + exports.DEFAULT_ADDRESS_INDEX,
  ],
};
exports.HW_BYRON_CONFIG = {
  NETWORK: hardwareWalletNetworksConfig,
  DEFAULT_DERIVATION_PATH: [
    exports.HARDENED + exports.BYRON_PURPOSE_INDEX,
    exports.HARDENED + exports.ADA_COIN_TYPE,
    exports.HARDENED + exports.DEFAULT_ADDRESS_INDEX,
    2,
    0,
  ],
  ABS_DERIVATION_PATH: [
    exports.HARDENED + exports.BYRON_PURPOSE_INDEX,
    exports.HARDENED + exports.ADA_COIN_TYPE,
    exports.HARDENED + exports.DEFAULT_ADDRESS_INDEX,
  ],
};
exports.AddressTypeNibbles = {
  BASE: 0b0000,
  POINTER: 0b0100,
  ENTERPRISE: 0b0110,
  BYRON: 0b1000,
  REWARD: 0b1110,
};
// MINIMAL_CARDANO_APP_VERSION v2.3.2 - Catalyst Voting support with LedgerJs 3.1.0 version
// https://github.com/cardano-foundation/ledgerjs-hw-app-cardano/blob/master/CHANGELOG.md
exports.MINIMAL_CARDANO_APP_VERSION = '2.3.2';
exports.MINIMAL_LEDGER_FIRMWARE_VERSION = '2.0.0';
exports.MINIMAL_TREZOR_FIRMWARE_VERSION = '2.4.0';
exports.isTrezorEnabled = true;
exports.isLedgerEnabled = true;
exports.isHardwareWalletSupportEnabled =
  exports.isTrezorEnabled || exports.isLedgerEnabled;
exports.isHardwareWalletIndicatorEnabled = false;
const getHardwareWalletsNetworkConfig = (network) => {
  const networkConfig = (0, lodash_1.get)(
    exports.HW_SHELLEY_CONFIG,
    ['NETWORK', network],
    {}
  );
  return networkConfig;
};
exports.getHardwareWalletsNetworkConfig = getHardwareWalletsNetworkConfig;
//# sourceMappingURL=hardwareWalletsConfig.js.map
