// @flow

/* eslint-disable consistent-return */

import type { NewsTimestamp } from '../news/types';

const store = global.electronStore;

type StorageKeys = {
  USER_LOCALE: string,
  TERMS_OF_USE_ACCEPTANCE: string,
  THEME: string,
  DATA_LAYER_MIGRATION_ACCEPTANCE: string,
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
        store.set(this.storageKeys.READ_NEWS, readNews.concat(newsTimestamps));
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
