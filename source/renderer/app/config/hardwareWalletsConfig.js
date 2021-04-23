const { isMainnet, isTestnet } = global.environment;

export const HARDENED_HEX = 0x80000000;
export const HARDENED = 2147483648;
export const SHELLEY_PURPOSE_INDEX = 1852;
export const BYRON_PURPOSE_INDEX = 44;
export const ADA_COIN_TYPE = 1815;
export const DEFAULT_ADDRESS_INDEX = 0;

export const HW_SHELLEY_CONFIG = {
  NETWORK: {
    MAINNET: {
      name: 'mainnet',
      networkId: 1,
      protocolMagic: 764824073,
      trezorProtocolMagic: 764824073,
      eraStartSlot: 4492800,
      ttl: 3600,
    },
    TESTNET: {
      name: 'testnet',
      networkId: 0,
      protocolMagic: 1097911063,
      trezorProtocolMagic: 1097911063,
      eraStartSlot: 4492800,
      ttl: 3600,
    },
  },
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
  NETWORK: {
    MAINNET: {
      name: 'mainnet',
      networkId: 1,
      protocolMagic: 764824073,
    },
  },
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

// JS Library supports Cardano APP version down to 2.0.4
// https://github.com/vacuumlabs/ledgerjs-cardano-shelley/blob/develop/CHANGELOG.md#220---february-8th-2020
export const MINIMAL_CARDANO_APP_VERSION = '2.2.0';
export const MINIMAL_LEDGER_FIRMWARE_VERSION = '1.6.1';
export const MINIMAL_TREZOR_FIRMWARE_VERSION = '2.3.6';

export const isTrezorEnabled = true;
export const isLedgerEnabled = true;

export const isHardwareWalletSupportEnabled =
  (isMainnet || isTestnet) && (isTrezorEnabled || isLedgerEnabled);

export const isHardwareWalletIndicatorEnabled = false;
