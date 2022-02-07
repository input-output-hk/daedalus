import { get, map } from 'lodash';
import { NetworkMagics } from '../../../common/types/cardano-node.types';
import type { NetworkMagicType } from '../../../common/types/cardano-node.types';
import type { Network } from '../../../common/types/environment.types';

export const HARDENED_HEX = 0x80000000;
export const HARDENED = 2147483648;
export const SHELLEY_PURPOSE_INDEX = 1852;
export const BYRON_PURPOSE_INDEX = 44;
export const ADA_COIN_TYPE = 1815;
export const DEFAULT_ADDRESS_INDEX = 0;
const { isMainnet, isStaging, isSelfnode } = global.environment;
const hardwareWalletNetworksConfig = {};
map(NetworkMagics, (networkMagic: NetworkMagicType, network: Network) => {
  const isMainnetLikeNetwork = isMainnet || isSelfnode || isStaging;
  hardwareWalletNetworksConfig[network] = {
    networkId: isMainnetLikeNetwork ? 1 : networkMagic[1],
    protocolMagic: isMainnetLikeNetwork ? 764824073 : networkMagic[0],
  };
});
export const HW_SHELLEY_CONFIG = {
  NETWORK: hardwareWalletNetworksConfig,
  DEFAULT_DERIVATION_PATH: [
    HARDENED + SHELLEY_PURPOSE_INDEX,
    HARDENED + ADA_COIN_TYPE,
    HARDENED + DEFAULT_ADDRESS_INDEX,
    2,
    0,
  ],
  ABS_DERIVATION_PATH: [
    HARDENED + SHELLEY_PURPOSE_INDEX,
    HARDENED + ADA_COIN_TYPE,
    HARDENED + DEFAULT_ADDRESS_INDEX,
  ],
};
export const HW_BYRON_CONFIG = {
  NETWORK: hardwareWalletNetworksConfig,
  DEFAULT_DERIVATION_PATH: [
    HARDENED + BYRON_PURPOSE_INDEX,
    HARDENED + ADA_COIN_TYPE,
    HARDENED + DEFAULT_ADDRESS_INDEX,
    2,
    0,
  ],
  ABS_DERIVATION_PATH: [
    HARDENED + BYRON_PURPOSE_INDEX,
    HARDENED + ADA_COIN_TYPE,
    HARDENED + DEFAULT_ADDRESS_INDEX,
  ],
};
export const AddressTypeNibbles = {
  BASE: 0b0000,
  POINTER: 0b0100,
  ENTERPRISE: 0b0110,
  BYRON: 0b1000,
  REWARD: 0b1110,
};
// MINIMAL_CARDANO_APP_VERSION v2.3.2 - Catalyst Voting support with LedgerJs 3.1.0 version
// https://github.com/cardano-foundation/ledgerjs-hw-app-cardano/blob/master/CHANGELOG.md
export const MINIMAL_CARDANO_APP_VERSION = '2.3.2';
export const MINIMAL_LEDGER_FIRMWARE_VERSION = '2.0.0';
export const MINIMAL_TREZOR_FIRMWARE_VERSION = '2.4.0';
export const isTrezorEnabled = true;
export const isLedgerEnabled = true;
export const isHardwareWalletSupportEnabled =
  isTrezorEnabled || isLedgerEnabled;
export const isHardwareWalletIndicatorEnabled = false;
export const getHardwareWalletsNetworkConfig = (network: Network) => {
  const networkConfig = get(HW_SHELLEY_CONFIG, ['NETWORK', network], {});
  return networkConfig;
};
