// @flow

/* eslint-disable consistent-return */

import { includes, without } from 'lodash';
import { electronStoreConversation } from '../../ipc/electronStoreConversation';
import { WalletMigrationStatuses } from '../../stores/WalletMigrationStore';
import {
  STORAGE_TYPES as types,
  STORAGE_KEYS as keys,
} from '../../../../common/config/electron-store.config';

import type { NewsTimestamp } from '../news/types';
import type { WalletMigrationStatus } from '../../stores/WalletMigrationStore';
import type {
  TransportDevice,
  ExtendedPublicKey,
} from '../../stores/HardwareWalletsStore';
import type { StorageKey } from '../../../../common/types/electron-store.types';

export type WalletLocalData = {
  id: string,
  recoveryPhraseVerificationDate?: ?Date,
  creationDate: Date,
};
export type WalletsLocalData = {
  [key: StorageKey]: WalletLocalData,
};

export type SetHardwareWalletLocalDataRequestType = {
  walletId: string,
  data: {
    device?: TransportDevice,
    extendedPublicKey?: ExtendedPublicKey,
    disconnected?: boolean,
  },
};

export type HardwareWalletLocalData = {
  id: string,
  device: TransportDevice,
  extendedPublicKey: ExtendedPublicKey,
  disconnected: boolean,
};

export type HardwareWalletsLocalData = {
  [key: string]: HardwareWalletLocalData,
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
      type: types.GET,
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
      type: types.SET,
      key,
      data,
      id,
    });
  };

  static unset = async (key: StorageKey, id?: string): Promise<void> => {
    await electronStoreConversation.request({
      type: types.DELETE,
      key,
      id,
    });
  };

  static reset = async (): Promise<void> => {
    await electronStoreConversation.request({
      type: types.RESET,
      key: keys.RESET,
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

  getUserTheme = (): Promise<string> => LocalStorageApi.get(keys.USER_THEME);

  setUserTheme = (theme: string): Promise<void> =>
    LocalStorageApi.set(keys.USER_THEME, theme);

  unsetUserTheme = (): Promise<void> => LocalStorageApi.unset(keys.USER_THEME);

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

  markNewsAsUnread = async (
    newsTimestamp: NewsTimestamp
  ): Promise<NewsTimestamp[]> => {
    const readNews = (await LocalStorageApi.get(keys.READ_NEWS)) || [];
    if (includes(readNews, newsTimestamp)) {
      await LocalStorageApi.set(
        keys.READ_NEWS,
        without(readNews, newsTimestamp)
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

  getHardwareWalletsLocalData = (): Promise<HardwareWalletsLocalData> =>
    LocalStorageApi.get(keys.HARDWARE_WALLETS, {});

  getHardwareWalletLocalData = (
    walletId: string
  ): Promise<HardwareWalletLocalData> =>
    LocalStorageApi.get(
      keys.HARDWARE_WALLETS,
      {
        id: walletId,
      },
      walletId
    );

  setHardwareWalletLocalData = async (
    walletId: string,
    data?: Object
  ): Promise<HardwareWalletLocalData> => {
    const currentWalletData = await this.getHardwareWalletLocalData(walletId);
    const unmutableData = { id: walletId };
    const walletData = Object.assign(
      {},
      currentWalletData,
      data,
      unmutableData
    );
    await LocalStorageApi.set(keys.HARDWARE_WALLETS, walletData, walletId);
    return walletData;
  };

  unsetHardwareWalletLocalData = (walletId: string): Promise<void> =>
    LocalStorageApi.unset(`${keys.HARDWARE_WALLETS}.${walletId}`);

  unsetHardwareWalletLocalDataAll = (): Promise<void> =>
    LocalStorageApi.unset(keys.HARDWARE_WALLETS);

  reset = async () => {
    await LocalStorageApi.reset();
  };
}
