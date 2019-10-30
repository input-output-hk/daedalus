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
  [key: string]: string,
};

/**
 * This api layer provides access to the electron local storage
 * for user settings that are not synced with any coin backend.
 */

export default class LocalStorageApi {
  static Getter = (key: string, fallbackValue: any): Promise<any> =>
    new Promise((resolve, reject) => {
      try {
        const value = store.get(key);
        if (!value) return resolve(fallbackValue);
        resolve(value);
      } catch (error) {
        return reject(error);
      }
    });

  static Setter = (key: string, value: any): Promise<void> =>
    new Promise((resolve, reject) => {
      try {
        store.set(key, value);
        resolve();
      } catch (error) {
        return reject(error);
      }
    });

  static Unsetter = (key: string): Promise<void> =>
    new Promise(resolve => {
      try {
        store.delete(key);
        resolve();
      } catch (error) {} // eslint-disable-line
    });

  storageKeys: StorageKeys;

  constructor(NETWORK: string) {
    const storageKeysRaw = [
      'USER_LOCALE',
      'USER_NUMBER_FORMAT',
      'USER_DATE_FORMAT_ENGLISH',
      'USER_DATE_FORMAT_JAPANESE',
      'USER_TIME_FORMAT',
      'TERMS_OF_USE_ACCEPTANCE',
      'THEME',
      'DATA_LAYER_MIGRATION_ACCEPTANCE',
      'READ_NEWS',
      'WALLETS',
    ];
    this.storageKeys = {};
    storageKeysRaw.forEach(key => {
      const keyStr = key.replace(new RegExp('_', 'g'), '-');
      this.storageKeys[key] = `${NETWORK}-${keyStr}`;
    });
  }

  getUserLocale = (): Promise<string> =>
    new LocalStorageApi.Getter(this.storageKeys.USER_LOCALE, '');

  setUserLocale = (locale: string): Promise<void> =>
    new LocalStorageApi.Setter(this.storageKeys.USER_LOCALE, locale);

  unsetUserLocale = (): Promise<void> =>
    new LocalStorageApi.Unsetter(this.storageKeys.USER_LOCALE);

  getUserNumberFormat = (): Promise<string> =>
    new LocalStorageApi.Getter(this.storageKeys.USER_NUMBER_FORMAT, '');

  setUserNumberFormat = (numberFormat: string): Promise<void> =>
    new LocalStorageApi.Setter(
      this.storageKeys.USER_NUMBER_FORMAT,
      numberFormat
    );

  unsetUserNumberFormat = (): Promise<void> =>
    new LocalStorageApi.Unsetter(this.storageKeys.USER_NUMBER_FORMAT);

  getUserDateFormatEnglish = (): Promise<string> =>
    new LocalStorageApi.Getter(this.storageKeys.USER_DATE_FORMAT_ENGLISH, '');

  setUserDateFormatEnglish = (dateFormat: string): Promise<void> =>
    new LocalStorageApi.Setter(
      this.storageKeys.USER_DATE_FORMAT_ENGLISH,
      dateFormat
    );

  unsetUserDateFormatEnglish = (): Promise<void> =>
    new LocalStorageApi.Unsetter(this.storageKeys.USER_DATE_FORMAT_ENGLISH);

  getUserDateFormatJapanese = (): Promise<string> =>
    new LocalStorageApi.Getter(this.storageKeys.USER_DATE_FORMAT_JAPANESE, '');

  setUserDateFormatJapanese = (dateFormat: string): Promise<void> =>
    new LocalStorageApi.Setter(
      this.storageKeys.USER_DATE_FORMAT_JAPANESE,
      dateFormat
    );

  unsetUserDateFormatJapanese = (): Promise<void> =>
    new LocalStorageApi.Unsetter(this.storageKeys.USER_DATE_FORMAT_JAPANESE);

  getUserTimeFormat = (): Promise<string> =>
    new LocalStorageApi.Getter(this.storageKeys.USER_TIME_FORMAT, '');

  setUserTimeFormat = (timeFormat: string): Promise<void> =>
    new LocalStorageApi.Setter(this.storageKeys.USER_TIME_FORMAT, timeFormat);

  unsetUserTimeFormat = (): Promise<void> =>
    new LocalStorageApi.Unsetter(this.storageKeys.USER_TIME_FORMAT);

  getTermsOfUseAcceptance = (): Promise<boolean> =>
    new LocalStorageApi.Getter(this.storageKeys.TERMS_OF_USE_ACCEPTANCE, false);

  setTermsOfUseAcceptance = (): Promise<void> =>
    new LocalStorageApi.Setter(this.storageKeys.TERMS_OF_USE_ACCEPTANCE, true);

  unsetTermsOfUseAcceptance = (): Promise<void> =>
    new LocalStorageApi.Unsetter(this.storageKeys.TERMS_OF_USE_ACCEPTANCE);

  getUserTheme = (): Promise<string> =>
    new LocalStorageApi.Getter(this.storageKeys.THEME, '');

  setUserTheme = (theme: string): Promise<void> =>
    new LocalStorageApi.Setter(this.storageKeys.THEME, theme);

  unsetUserTheme = (): Promise<void> =>
    new LocalStorageApi.Unsetter(this.storageKeys.THEME);

  getDataLayerMigrationAcceptance = (): Promise<boolean> =>
    new LocalStorageApi.Getter(
      this.storageKeys.DATA_LAYER_MIGRATION_ACCEPTANCE,
      false
    );

  setDataLayerMigrationAcceptance = (): Promise<void> =>
    new LocalStorageApi.Setter(
      this.storageKeys.DATA_LAYER_MIGRATION_ACCEPTANCE,
      true
    );

  unsetDataLayerMigrationAcceptance = (): Promise<void> =>
    new LocalStorageApi.Unsetter(
      this.storageKeys.DATA_LAYER_MIGRATION_ACCEPTANCE
    );

  getWalletsLocalData = (): Promise<Object> =>
    new LocalStorageApi.Getter(this.storageKeys.THEME, {});

  getWalletLocalData = (walletId: string): Promise<WalletLocalData> =>
    new LocalStorageApi.Getter(`${this.storageKeys.WALLETS}.${walletId}`, {
      id: walletId,
    });

  setWalletLocalData = (walletData: WalletLocalData): Promise<void> =>
    new LocalStorageApi.Setter(
      `${this.storageKeys.WALLETS}.${walletData.id}`,
      walletData
    );

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
    new LocalStorageApi.Unsetter(`${this.storageKeys.WALLETS}.${walletId}`);

  getReadNews = (): Promise<NewsTimestamp[]> =>
    new LocalStorageApi.Getter(this.storageKeys.READ_NEWS, []);

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
    new LocalStorageApi.Unsetter(this.storageKeys.READ_NEWS);

  reset = async () => {
    await this.unsetUserLocale();
    await this.unsetUserNumberFormat();
    await this.unsetUserDateFormatEnglish();
    await this.unsetUserDateFormatJapanese();
    await this.unsetUserTimeFormat();
    await this.unsetTermsOfUseAcceptance();
    await this.unsetUserTheme();
    await this.unsetDataLayerMigrationAcceptance();
    await this.unsetReadNews();
  };
}
