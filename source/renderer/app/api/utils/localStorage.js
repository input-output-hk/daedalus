// @flow

/* eslint-disable consistent-return */

import { includes } from 'lodash';
import type { NewsTimestamp } from '../news/types';

const store = global.electronStore;

export type WalletLocalData = {
  id: string,
  recoveryPhraseVerificationDate?: ?Date,
  creationDate?: ?Date,
};

export type WalletsLocalData = {
  [key: string]: WalletLocalData,
};

type StorageKeys = {
  USER_LOCALE: string,
  TERMS_OF_USE_ACCEPTANCE: string,
  THEME: string,
  DATA_LAYER_MIGRATION_ACCEPTANCE: string,
  WALLETS: string,
  READ_NEWS: string,
};

/**
 * This api layer provides access to the electron local storage
 * for user settings that are not synced with any coin backend.
 */

export default class LocalStorageApi {
  storageKeys: StorageKeys;

  constructor(NETWORK: string) {
    this.storageKeys = {
      USER_LOCALE: `${NETWORK}-USER-LOCALE`,
      TERMS_OF_USE_ACCEPTANCE: `${NETWORK}-TERMS-OF-USE-ACCEPTANCE`,
      THEME: `${NETWORK}-THEME`,
      DATA_LAYER_MIGRATION_ACCEPTANCE: `${NETWORK}-DATA-LAYER-MIGRATION-ACCEPTANCE`,
      WALLETS: `${NETWORK}-WALLETS`,
      READ_NEWS: `${NETWORK}-READ_NEWS`,
    };
  }

  getUserLocale = (): Promise<string> =>
    new Promise((resolve, reject) => {
      try {
        const locale = store.get(this.storageKeys.USER_LOCALE);
        if (!locale) return resolve('');
        resolve(locale);
      } catch (error) {
        return reject(error);
      }
    });

  setUserLocale = (locale: string): Promise<void> =>
    new Promise((resolve, reject) => {
      try {
        store.set(this.storageKeys.USER_LOCALE, locale);
        resolve();
      } catch (error) {
        return reject(error);
      }
    });

  unsetUserLocale = (): Promise<void> =>
    new Promise(resolve => {
      try {
        store.delete(this.storageKeys.USER_LOCALE);
        resolve();
      } catch (error) {} // eslint-disable-line
    });

  getTermsOfUseAcceptance = (): Promise<boolean> =>
    new Promise((resolve, reject) => {
      try {
        const accepted = store.get(this.storageKeys.TERMS_OF_USE_ACCEPTANCE);
        if (!accepted) return resolve(false);
        resolve(accepted);
      } catch (error) {
        return reject(error);
      }
    });

  setTermsOfUseAcceptance = (): Promise<void> =>
    new Promise((resolve, reject) => {
      try {
        store.set(this.storageKeys.TERMS_OF_USE_ACCEPTANCE, true);
        resolve();
      } catch (error) {
        return reject(error);
      }
    });

  unsetTermsOfUseAcceptance = (): Promise<void> =>
    new Promise(resolve => {
      try {
        store.delete(this.storageKeys.TERMS_OF_USE_ACCEPTANCE);
        resolve();
      } catch (error) {} // eslint-disable-line
    });

  getUserTheme = (): Promise<string> =>
    new Promise((resolve, reject) => {
      try {
        const theme = store.get(this.storageKeys.THEME);
        if (!theme) return resolve('');
        resolve(theme);
      } catch (error) {
        return reject(error);
      }
    });

  setUserTheme = (theme: string): Promise<void> =>
    new Promise((resolve, reject) => {
      try {
        store.set(this.storageKeys.THEME, theme);
        resolve();
      } catch (error) {
        return reject(error);
      }
    });

  unsetUserTheme = (): Promise<void> =>
    new Promise(resolve => {
      try {
        store.delete(this.storageKeys.THEME);
        resolve();
      } catch (error) {} // eslint-disable-line
    });

  getDataLayerMigrationAcceptance = (): Promise<boolean> =>
    new Promise((resolve, reject) => {
      try {
        const accepted = store.get(
          this.storageKeys.DATA_LAYER_MIGRATION_ACCEPTANCE
        );
        if (!accepted) return resolve(false);
        resolve(true);
      } catch (error) {
        return reject(error);
      }
    });

  setDataLayerMigrationAcceptance = (): Promise<void> =>
    new Promise((resolve, reject) => {
      try {
        store.set(this.storageKeys.DATA_LAYER_MIGRATION_ACCEPTANCE, true);
        resolve();
      } catch (error) {
        return reject(error);
      }
    });

  unsetDataLayerMigrationAcceptance = (): Promise<void> =>
    new Promise(resolve => {
      try {
        store.delete(this.storageKeys.DATA_LAYER_MIGRATION_ACCEPTANCE);
        resolve();
      } catch (error) {} // eslint-disable-line
    });

  getWalletsLocalData = (): Promise<Object> =>
    new Promise((resolve, reject) => {
      try {
        const walletsLocalData = store.get(this.storageKeys.WALLETS);
        if (!walletsLocalData) return resolve({});
        return resolve(walletsLocalData);
      } catch (error) {
        return reject(error);
      }
    });

  getWalletLocalData = (walletId: string): Promise<WalletLocalData> =>
    new Promise((resolve, reject) => {
      try {
        const walletData = store.get(`${this.storageKeys.WALLETS}.${walletId}`);
        if (!walletData) {
          resolve({
            id: walletId,
          });
        }
        return resolve(walletData);
      } catch (error) {
        return reject(error);
      }
    });

  setWalletLocalData = (walletData: WalletLocalData): Promise<void> =>
    new Promise((resolve, reject) => {
      try {
        const walletId = walletData.id;
        store.set(`${this.storageKeys.WALLETS}.${walletId}`, walletData);
        return resolve();
      } catch (error) {
        return reject(error);
      }
    });

  updateWalletLocalData = (updatedWalletData: Object): Promise<Object> =>
    new Promise(async (resolve, reject) => {
      const walletId = updatedWalletData.id;
      const currentWalletData = await this.getWalletLocalData(walletId);
      const walletData = Object.assign(
        {},
        currentWalletData,
        updatedWalletData
      );
      try {
        store.set(`${this.storageKeys.WALLETS}.${walletId}`, walletData);
        return resolve(walletData);
      } catch (error) {
        return reject(error);
      }
    });

  unsetWalletLocalData = (walletId: string): Promise<void> =>
    new Promise((resolve, reject) => {
      try {
        store.delete(`${this.storageKeys.WALLETS}.${walletId}`);
        return resolve();
      } catch (error) {
        return reject(error);
      }
    });

  getReadNews = (): Promise<NewsTimestamp[]> =>
    new Promise((resolve, reject) => {
      try {
        const readNews = store.get(this.storageKeys.READ_NEWS);
        if (!readNews) return resolve([]);
        resolve(readNews);
      } catch (error) {
        return reject(error);
      }
    });

  markNewsAsRead = (
    newsTimestamps: NewsTimestamp[]
  ): Promise<NewsTimestamp[]> =>
    new Promise((resolve, reject) => {
      try {
        const readNews = store.get(this.storageKeys.READ_NEWS) || [];

        if (!includes(readNews, newsTimestamps[0])) {
          store.set(
            this.storageKeys.READ_NEWS,
            readNews.concat(newsTimestamps)
          );
        }

        resolve(readNews);
      } catch (error) {
        return reject(error);
      }
    });

  unsetReadNews = (): Promise<void> =>
    new Promise(resolve => {
      try {
        store.delete(this.storageKeys.READ_NEWS);
        resolve();
      } catch (error) {} // eslint-disable-line
    });

  reset = async () => {
    await this.unsetUserLocale();
    await this.unsetTermsOfUseAcceptance();
    await this.unsetUserTheme();
    await this.unsetDataLayerMigrationAcceptance();
    await this.unsetReadNews();
  };
}
