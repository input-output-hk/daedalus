// @flow
export const WALLET_NAV_IDS = {
  SUMMARY: 'summary',
  SEND: 'send',
  RECEIVE: 'receive',
  TRANSACTIONS: 'transactions',
  SETTINGS: 'settings',
};

export const ITN_LEGACY_WALLET_EXCLUDED_NAV_ITEMS = [
  WALLET_NAV_IDS.SEND,
  WALLET_NAV_IDS.RECEIVE,
];
