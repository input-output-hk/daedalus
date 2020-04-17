// @flow

/* eslint-disable consistent-return */

import { includes } from 'lodash';
import { electronStoreConversation } from '../../ipc/electronStoreConversation';
import type { NewsTimestamp } from '../news/types';
import type { WalletMigrationStatus } from '../../stores/WalletMigrationStore';
import { WalletMigrationStatuses } from '../../stores/WalletMigrationStore';

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
  static get = async (key: string, fallbackValue: any): Promise<any> => {
    const value = await electronStoreConversation.request({
      type: 'get',
      key,
    });
    if (!value) return fallbackValue;
    return value;
  };

  static set = async (key: string, value: any): Promise<void> => {
    await electronStoreConversation.request({
      type: 'set',
      key,
      data: value,
    });
  };

  static unset = async (key: string): Promise<void> => {
    await electronStoreConversation.request({
      type: 'delete',
      key,
    });
  };

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
      'WALLET_MIGRATION_STATUS',
    ];
    this.storageKeys = {};
    storageKeysRaw.forEach(key => {
      const keyStr = key.replace(new RegExp('_', 'g'), '-');
      this.storageKeys[key] = `${NETWORK}-${keyStr}`;
    });
  }

  getUserLocale = (): Promise<string> =>
    LocalStorageApi.get(this.storageKeys.USER_LOCALE, '');

  setUserLocale = (locale: string): Promise<void> =>
    LocalStorageApi.set(this.storageKeys.USER_LOCALE, locale);

  unsetUserLocale = (): Promise<void> =>
    LocalStorageApi.unset(this.storageKeys.USER_LOCALE);

  getUserNumberFormat = (): Promise<string> =>
    LocalStorageApi.get(this.storageKeys.USER_NUMBER_FORMAT, '');

  setUserNumberFormat = (numberFormat: string): Promise<void> =>
    LocalStorageApi.set(this.storageKeys.USER_NUMBER_FORMAT, numberFormat);

  unsetUserNumberFormat = (): Promise<void> =>
    LocalStorageApi.unset(this.storageKeys.USER_NUMBER_FORMAT);

  getUserDateFormatEnglish = (): Promise<string> =>
    LocalStorageApi.get(this.storageKeys.USER_DATE_FORMAT_ENGLISH, '');

  setUserDateFormatEnglish = (dateFormat: string): Promise<void> =>
    LocalStorageApi.set(this.storageKeys.USER_DATE_FORMAT_ENGLISH, dateFormat);

  unsetUserDateFormatEnglish = (): Promise<void> =>
    LocalStorageApi.unset(this.storageKeys.USER_DATE_FORMAT_ENGLISH);

  getUserDateFormatJapanese = (): Promise<string> =>
    LocalStorageApi.get(this.storageKeys.USER_DATE_FORMAT_JAPANESE, '');

  setUserDateFormatJapanese = (dateFormat: string): Promise<void> =>
    LocalStorageApi.set(this.storageKeys.USER_DATE_FORMAT_JAPANESE, dateFormat);

  unsetUserDateFormatJapanese = (): Promise<void> =>
    LocalStorageApi.unset(this.storageKeys.USER_DATE_FORMAT_JAPANESE);

  getUserTimeFormat = (): Promise<string> =>
    LocalStorageApi.get(this.storageKeys.USER_TIME_FORMAT, '');

  setUserTimeFormat = (timeFormat: string): Promise<void> =>
    LocalStorageApi.set(this.storageKeys.USER_TIME_FORMAT, timeFormat);

  unsetUserTimeFormat = (): Promise<void> =>
    LocalStorageApi.unset(this.storageKeys.USER_TIME_FORMAT);

  getTermsOfUseAcceptance = (): Promise<boolean> =>
    LocalStorageApi.get(this.storageKeys.TERMS_OF_USE_ACCEPTANCE, false);

  setTermsOfUseAcceptance = (): Promise<void> =>
    LocalStorageApi.set(this.storageKeys.TERMS_OF_USE_ACCEPTANCE, true);

  unsetTermsOfUseAcceptance = (): Promise<void> =>
    LocalStorageApi.unset(this.storageKeys.TERMS_OF_USE_ACCEPTANCE);

  getUserTheme = (): Promise<string> =>
    LocalStorageApi.get(this.storageKeys.THEME, '');

  setUserTheme = (theme: string): Promise<void> =>
    LocalStorageApi.set(this.storageKeys.THEME, theme);

  unsetUserTheme = (): Promise<void> =>
    LocalStorageApi.unset(this.storageKeys.THEME);

  getDataLayerMigrationAcceptance = (): Promise<boolean> =>
    LocalStorageApi.get(
      this.storageKeys.DATA_LAYER_MIGRATION_ACCEPTANCE,
      false
    );

  setDataLayerMigrationAcceptance = (): Promise<void> =>
    LocalStorageApi.set(this.storageKeys.DATA_LAYER_MIGRATION_ACCEPTANCE, true);

  unsetDataLayerMigrationAcceptance = (): Promise<void> =>
    LocalStorageApi.unset(this.storageKeys.DATA_LAYER_MIGRATION_ACCEPTANCE);

  getWalletsLocalData = (): Promise<Object> =>
    LocalStorageApi.get(this.storageKeys.THEME, {});

  getWalletLocalData = (walletId: string): Promise<WalletLocalData> =>
    LocalStorageApi.get(`${this.storageKeys.WALLETS}.${walletId}`, {
      id: walletId,
    });

  setWalletLocalData = (walletData: WalletLocalData): Promise<void> =>
    LocalStorageApi.set(
      `${this.storageKeys.WALLETS}.${walletData.id}`,
      walletData
    );

  updateWalletLocalData = async (
    updatedWalletData: Object
  ): Promise<Object> => {
    const walletId = updatedWalletData.id;
    const currentWalletData = await this.getWalletLocalData(walletId);
    const walletData = Object.assign({}, currentWalletData, updatedWalletData);
    await LocalStorageApi.set(
      `${this.storageKeys.WALLETS}.${walletId}`,
      walletData
    );
    return walletData;
  };

  unsetWalletLocalData = (walletId: string): Promise<void> =>
    LocalStorageApi.unset(`${this.storageKeys.WALLETS}.${walletId}`);

  getReadNews = (): Promise<NewsTimestamp[]> =>
    LocalStorageApi.get(this.storageKeys.READ_NEWS, []);

  markNewsAsRead = async (
    newsTimestamps: NewsTimestamp[]
  ): Promise<NewsTimestamp[]> => {
    const readNews =
      (await LocalStorageApi.get(this.storageKeys.READ_NEWS)) || [];
    if (!includes(readNews, newsTimestamps[0])) {
      await LocalStorageApi.set(
        this.storageKeys.READ_NEWS,
        readNews.concat(newsTimestamps)
      );
    }
    return readNews;
  };

  unsetReadNews = (): Promise<void> =>
    LocalStorageApi.unset(this.storageKeys.READ_NEWS);

  getWalletMigrationStatus = (): Promise<WalletMigrationStatus> =>
    LocalStorageApi.get(
      this.storageKeys.WALLET_MIGRATION_STATUS,
      WalletMigrationStatuses.UNSTARTED
    );

  setWalletMigrationStatus = (status: WalletMigrationStatus): Promise<void> =>
    LocalStorageApi.set(this.storageKeys.WALLET_MIGRATION_STATUS, status);

  unsetWalletMigrationStatus = (): Promise<void> =>
    LocalStorageApi.unset(this.storageKeys.WALLET_MIGRATION_STATUS);

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
    await this.unsetWalletMigrationStatus();
  };
}
