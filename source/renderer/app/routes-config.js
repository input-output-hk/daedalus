// @flow
export const ROUTES = {
  ROOT: '/',
  PAPER_WALLET_CREATE_CERTIFICATE: '/paper-wallet/create-certificate',
  NETWORK_INFO: '/network-info',
  STAKING: {
    ROOT: '/staking',
    COUNTDOWN: '/staking/countdown',
    PAGE: '/staking/:page',
    DELEGATION_CENTER: '/staking/delegation-center',
    STAKE_POOLS: '/staking/stake-pools',
    REWARDS: '/staking/rewards',
    EPOCHS: '/staking/epochs',
    INFO: '/staking/info',
  },
  PROFILE: {
    INITIAL_SETTINGS: '/profile/initial-settings',
    TERMS_OF_USE: '/profile/terms-of-service',
    DATA_LAYER_MIGRATION: '/profile/data-layer-migration',
  },
  WALLETS: {
    ROOT: '/wallets',
    ADD: '/wallets/add',
    PAGE: '/wallets/:id/:page',
    SUMMARY: '/wallets/:id/summary',
    TRANSACTIONS: '/wallets/:id/transactions',
    SEND: '/wallets/:id/send',
    RECEIVE: '/wallets/:id/receive',
    SETTINGS: '/wallets/:id/settings',
    UTXO: '/wallets/:id/utxo',
  },
  SETTINGS: {
    ROOT: '/settings',
    GENERAL: '/settings/general',
    TERMS_OF_USE: '/settings/terms-of-service',
    SUPPORT: '/settings/support',
    DISPLAY: '/settings/display',
  },
};
