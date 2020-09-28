// @flow

export type StorageType = 'get' | 'set' | 'delete' | 'reset';

export type StorageKey =
  | 'RESET'
  | 'USER-LOCALE'
  | 'USER-NUMBER-FORMAT'
  | 'USER-DATE-FORMAT-ENGLISH'
  | 'USER-DATE-FORMAT-JAPANESE'
  | 'USER-TIME-FORMAT'
  | 'TERMS-OF-USE-ACCEPTANCE'
  | 'THEME'
  | 'DATA-LAYER-MIGRATION-ACCEPTANCE'
  | 'READ-NEWS'
  | 'WALLETS'
  | 'HARDWARE_WALLETS'
  | 'WALLET-MIGRATION-STATUS'
  | 'DOWNLOAD-MANAGER';

export type StoreMessage = {
  type: StorageType,
  key: StorageKey,
  data?: any,
  id?: string,
};
