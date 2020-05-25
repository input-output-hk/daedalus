// @flow

/* eslint-disable consistent-return */

import { includes } from 'lodash';
import { electronStoreConversation } from '../../ipc/electronStoreConversation';
import { WalletMigrationStatuses } from '../../stores/WalletMigrationStore';
import { STORAGE_KEYS as keys } from '../../../../common/config/electron-store.config';
import type { NewsTimestamp } from '../news/types';
import type { WalletMigrationStatus } from '../../stores/WalletMigrationStore';
import type { StorageKey } from '../../../../common/types/electron-store.types';

export type WalletLocalData = {
  id: string,
  recoveryPhraseVerificationDate?: ?Date,
  creationDate: Date,
};

export type WalletsLocalData = {
  [key: StorageKey]: WalletLocalData,
};

/**
 * This api layer provides access to the electron local storage
 * for user settings that are not synced with any coin backend.
 */

export default class LocalStorageApi {
  static get = async (
    key: StorageKey,
    fallbackValue?: any,
    id?: string
  ): Promise<any> => {
    const value = await electronStoreConversation.request({
      type: 'get',
      key,
      id,
    });
    if (!value) return fallbackValue || '';
    return value;
  };

  static set = async (
    key: StorageKey,
    data: any,
    id?: string
  ): Promise<void> => {
    await electronStoreConversation.request({
      type: 'set',
      key,
      data,
      id,
    });
  };

  static unset = async (key: StorageKey, id?: string): Promise<void> => {
    await electronStoreConversation.request({
      type: 'delete',
      key,
      id,
    });
  };

  getUserLocale = (): Promise<string> => LocalStorageApi.get(keys.USER_LOCALE);

  setUserLocale = (locale: string): Promise<void> =>
    LocalStorageApi.set(keys.USER_LOCALE, locale);

  unsetUserLocale = (): Promise<void> =>
    LocalStorageApi.unset(keys.USER_LOCALE);

  getUserNumberFormat = (): Promise<string> =>
    LocalStorageApi.get(keys.USER_NUMBER_FORMAT);

  setUserNumberFormat = (numberFormat: string): Promise<void> =>
    LocalStorageApi.set(keys.USER_NUMBER_FORMAT, numberFormat);

  unsetUserNumberFormat = (): Promise<void> =>
    LocalStorageApi.unset(keys.USER_NUMBER_FORMAT);

  getUserDateFormatEnglish = (): Promise<string> =>
    LocalStorageApi.get(keys.USER_DATE_FORMAT_ENGLISH);

  setUserDateFormatEnglish = (dateFormat: string): Promise<void> =>
    LocalStorageApi.set(keys.USER_DATE_FORMAT_ENGLISH, dateFormat);

  unsetUserDateFormatEnglish = (): Promise<void> =>
    LocalStorageApi.unset(keys.USER_DATE_FORMAT_ENGLISH);

  getUserDateFormatJapanese = (): Promise<string> =>
    LocalStorageApi.get(keys.USER_DATE_FORMAT_JAPANESE);

  setUserDateFormatJapanese = (dateFormat: string): Promise<void> =>
    LocalStorageApi.set(keys.USER_DATE_FORMAT_JAPANESE, dateFormat);

  unsetUserDateFormatJapanese = (): Promise<void> =>
    LocalStorageApi.unset(keys.USER_DATE_FORMAT_JAPANESE);

  getUserTimeFormat = (): Promise<string> =>
    LocalStorageApi.get(keys.USER_TIME_FORMAT);

  setUserTimeFormat = (timeFormat: string): Promise<void> =>
    LocalStorageApi.set(keys.USER_TIME_FORMAT, timeFormat);

  unsetUserTimeFormat = (): Promise<void> =>
    LocalStorageApi.unset(keys.USER_TIME_FORMAT);

  getTermsOfUseAcceptance = (): Promise<boolean> =>
    LocalStorageApi.get(keys.TERMS_OF_USE_ACCEPTANCE, false);

  setTermsOfUseAcceptance = (): Promise<void> =>
    LocalStorageApi.set(keys.TERMS_OF_USE_ACCEPTANCE, true);

  unsetTermsOfUseAcceptance = (): Promise<void> =>
    LocalStorageApi.unset(keys.TERMS_OF_USE_ACCEPTANCE);

  getUserTheme = (): Promise<string> => LocalStorageApi.get(keys.THEME);

  setUserTheme = (theme: string): Promise<void> =>
    LocalStorageApi.set(keys.THEME, theme);

  unsetUserTheme = (): Promise<void> => LocalStorageApi.unset(keys.THEME);

  getDataLayerMigrationAcceptance = (): Promise<boolean> =>
    LocalStorageApi.get(keys.DATA_LAYER_MIGRATION_ACCEPTANCE, false);

  setDataLayerMigrationAcceptance = (): Promise<void> =>
    LocalStorageApi.set(keys.DATA_LAYER_MIGRATION_ACCEPTANCE, true);

  unsetDataLayerMigrationAcceptance = (): Promise<void> =>
    LocalStorageApi.unset(keys.DATA_LAYER_MIGRATION_ACCEPTANCE);

  getWalletsLocalData = (): Promise<Object> =>
    LocalStorageApi.get(keys.WALLETS, {});

  getWalletLocalData = (walletId: string): Promise<WalletLocalData> =>
    LocalStorageApi.get(
      keys.WALLETS,
      {
        id: walletId,
      },
      walletId
    );

  setWalletLocalData = async (
    walletId: string,
    updatedWalletData?: Object
  ): Promise<WalletLocalData> => {
    const currentWalletData = await this.getWalletLocalData(walletId);
    const defaultData = { creationDate: new Date() };
    const unmutableData = { id: walletId };
    const walletData = Object.assign(
      {},
      defaultData,
      currentWalletData,
      updatedWalletData,
      unmutableData
    );
    await LocalStorageApi.set(keys.WALLETS, walletData, walletId);
    return walletData;
  };

  unsetWalletLocalData = (walletId: string): Promise<void> =>
    LocalStorageApi.unset(keys.WALLETS, walletId);

  getReadNews = (): Promise<NewsTimestamp[]> =>
    LocalStorageApi.get(keys.READ_NEWS, []);

  markNewsAsRead = async (
    newsTimestamps: NewsTimestamp[]
  ): Promise<NewsTimestamp[]> => {
    const readNews = (await LocalStorageApi.get(keys.READ_NEWS)) || [];
    if (!includes(readNews, newsTimestamps[0])) {
      await LocalStorageApi.set(
        keys.READ_NEWS,
        readNews.concat(newsTimestamps)
      );
    }
    return readNews;
  };

  unsetReadNews = (): Promise<void> => LocalStorageApi.unset(keys.READ_NEWS);

  getWalletMigrationStatus = (): Promise<WalletMigrationStatus> =>
    LocalStorageApi.get(
      keys.WALLET_MIGRATION_STATUS,
      WalletMigrationStatuses.UNSTARTED
    );

  setWalletMigrationStatus = (status: WalletMigrationStatus): Promise<void> =>
    LocalStorageApi.set(keys.WALLET_MIGRATION_STATUS, status);

  unsetWalletMigrationStatus = (): Promise<void> =>
    LocalStorageApi.unset(keys.WALLET_MIGRATION_STATUS);

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
