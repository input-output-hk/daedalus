const { isMainnet } = global.environment;

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
};

export const HW_BYRON_CONFIG = {
  NETWORK: {
    MAINNET: {
      name: 'mainnet',
      networkId: 1,
      protocolMagic: 764824073,
    },
  },
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

export const MINIMAL_LEDGER_FIRMWARE_VERSION = '1.6.1';
export const MINIMAL_TREZOR_FIRMWARE_VERSION = '2.3.3';

export const isTrezorEnabled = true;
export const isLedgerEnabled = true;

export const isHardwareWalletSupportEnabled =
  isMainnet && (isTrezorEnabled || isLedgerEnabled);
