const { isMainnet } = global.environment;

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
      eraStartSlot: 4492800,
      ttl: 3600,
    },
  },
  ABD_DERIVATION_PATH: [
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
  ABD_DERIVATION_PATH: [
    HARDENED + BYRON_PURPOSE_INDEX,
    HARDENED + ADA_COIN_TYPE,
    HARDENED + DEFAULT_ADDRESS_INDEX,
  ],
};

export const MINIMAL_LEDGER_APP_VERSION = {
  major: 2,
  minor: 0,
  patch: 3,
};

export const RECOMMENDED_LEDGER_APP_VERSION = {
  major: 2,
  minor: 0,
  patch: 4,
};

export const MINIMAL_CARDANO_APP_VERSION = '2.0.3';
export const MINIMAL_LEDGER_FIRMWARE_VERSION = '1.6.1';
export const MINIMAL_TREZOR_FIRMWARE_VERSION = '2.3.3';

export const isTrezorEnabled = true;
export const isLedgerEnabled = true;

export const isHardwareWalletSupportEnabled =
  isMainnet && (isTrezorEnabled || isLedgerEnabled);

export const isHardwareWalletIndicatorEnabled = false;
