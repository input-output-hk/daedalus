// @flow
export const ROUTES = {
  ROOT: '/',
  STAKING: '/staking',
  ADA_REDEMPTION: '/ada-redemption',
  NETWORK_STATUS: '/network-status',
  EKG: '/network-status', // really, should go to http://localhost:8002
  PAPER_WALLET_CREATE_CERTIFICATE: '/paper-wallet/create-certificate',
  PROFILE: {
    LANGUAGE_SELECTION: '/profile/language-selection',
    TERMS_OF_USE: '/profile/terms-of-use',
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
  },
  SETTINGS: {
    ROOT: '/settings',
    GENERAL: '/settings/general',
    TERMS_OF_USE: '/settings/terms-of-use',
    SUPPORT: '/settings/support',
    DISPLAY: '/settings/display',
  },
};
