// @flow

export type StorageType = 'get' | 'set' | 'delete' | 'reset';

export type StorageKey =
  | 'ALONZO-INFO-WAS-OPEN'
  | 'APP-AUTOMATIC-UPDATE-FAILED'
  | 'APP-UPDATE-COMPLETED'
  | 'ASSET-DATA'
  | 'ASSET-SETTINGS-DIALOG-WAS-OPENED'
  | 'CURRENCY-ACTIVE'
  | 'CURRENCY-SELECTED'
  | 'DATA-LAYER-MIGRATION-ACCEPTANCE'
  | 'DISCREET-MODE-ENABLED'
  | 'DOWNLOAD-MANAGER'
  | 'HARDWARE-WALLET-DEVICES'
  | 'HARDWARE-WALLETS'
  | 'READ-NEWS'
  | 'RESET'
  | 'SMASH-SERVER'
  | 'TERMS-OF-USE-ACCEPTANCE'
  | 'THEME'
  | 'TOKEN-FAVORITES'
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
