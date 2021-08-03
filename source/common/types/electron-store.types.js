// @flow

export type StorageType = 'get' | 'set' | 'delete' | 'reset';

export type StorageKey =
  | 'APP-AUTOMATIC-UPDATE-FAILED'
  | 'APP-UPDATE-COMPLETED'
  | 'ASSET-DATA'
  | 'ASSET-SETTINGS-DIALOG-WAS-OPENED'
  | 'CURRENCY-ACTIVE'
  | 'CURRENCY-SELECTED'
  | 'DATA-LAYER-MIGRATION-ACCEPTANCE'
  | 'DOWNLOAD-MANAGER'
  | 'HARDWARE-WALLET-DEVICES'
  | 'HARDWARE-WALLETS'
  | 'READ-NEWS'
  | 'RESET'
  | 'SMASH-SERVER'
  | 'STAKING-INFO-WAS-OPEN'
  | 'TERMS-OF-USE-ACCEPTANCE'
  | 'THEME'
  | 'USER-DATE-FORMAT-ENGLISH'
  | 'USER-DATE-FORMAT-JAPANESE'
  | 'USER-LOCALE'
  | 'USER-NUMBER-FORMAT'
  | 'USER-TIME-FORMAT'
  | 'WALLET-MIGRATION-STATUS'
  | 'WALLETS'
  | 'WINDOW-BOUNDS';

export type StoreMessage = {
  type: StorageType,
  key: StorageKey,
  data?: any,
  id?: string,
};
