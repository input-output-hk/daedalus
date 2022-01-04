/* eslint-disable consistent-return */
import { includes, without, get } from 'lodash';
import { toJS } from '../../../../common/utils/helper';
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
  HardwareWalletExtendedPublicKeyResponse,
  DeviceType,
} from '../../../../common/types/hardware-wallets.types';
import type { StorageKey } from '../../../../common/types/electron-store.types';
import type { Currency, DeprecatedCurrency } from '../../types/currencyTypes';
import {
  CURRENCY_IS_ACTIVE_BY_DEFAULT,
  CURRENCY_DEFAULT_SELECTED,
} from '../../config/currencyConfig';

export type WalletLocalData = {
  id: string;
  recoveryPhraseVerificationDate?: Date | null | undefined;
  creationDate: Date;
  showUsedAddresses: boolean;
};
export type WalletsLocalData = Record<StorageKey, WalletLocalData>;
export type SetHardwareWalletLocalDataRequestType = {
  walletId: string;
  data: {
    device?: TransportDevice;
    extendedPublicKey?: HardwareWalletExtendedPublicKeyResponse;
    disconnected?: boolean;
  };
};
export type SetHardwareWalletDeviceRequestType = {
  deviceId: string | null | undefined;
  // @TODO - mark as mandatory parameter once Ledger improver
  data: {
    deviceType?: DeviceType;
    deviceModel?: string;
    deviceName?: string;
    path?: string | null | undefined;
    paired?: string | null | undefined;
    disconnected?: boolean;
  };
};
export type HardwareWalletLocalData = {
  id: string;
  deviceType: DeviceType;
  device: TransportDevice;
  extendedPublicKey: HardwareWalletExtendedPublicKeyResponse;
  disconnected: boolean;
};
export type HardwareWalletsLocalData = Record<string, HardwareWalletLocalData>;
export type HardwareWalletDevicesType = Record<string, TransportDevice>;
export type AssetLocalData = {
  decimals: number;
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
    if (value === undefined) return fallbackValue || '';
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
      data: toJS(data),
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
  getCurrencySelected = async (): Promise<string> => {
    const localCurrencySelected: Promise<
      Currency | DeprecatedCurrency | string
    > = await LocalStorageApi.get(
      keys.CURRENCY_SELECTED,
      CURRENCY_DEFAULT_SELECTED
    );
    if (typeof localCurrencySelected === 'string') return localCurrencySelected;

    /**
     *
     * Prior versions were storing the whole Currency object,
     * which could lead different formats (e.g. currency.code or currency.symbol)
     * It now stores only the currency code string,
     * but we need to account for users storing old formats
     *
     * In this case, we also set the correct local code value
     *
     */
    const localCurrencyCode: string =
      get(localCurrencySelected, 'code') ||
      get(localCurrencySelected, 'symbol');
    this.setCurrencySelected(localCurrencyCode);
    return localCurrencyCode;
  };
  setCurrencySelected = (currencyCode: string): Promise<void> =>
    LocalStorageApi.set(keys.CURRENCY_SELECTED, currencyCode);
  unsetCurrencySelected = (): Promise<void> =>
    LocalStorageApi.unset(keys.CURRENCY_SELECTED);
  getCurrencyIsActive = (): Promise<boolean> =>
    LocalStorageApi.get(keys.CURRENCY_ACTIVE, CURRENCY_IS_ACTIVE_BY_DEFAULT);
  setCurrencyIsActive = async (isActive: boolean): Promise<void> =>
    LocalStorageApi.set(keys.CURRENCY_ACTIVE, isActive);
  unsetCurrencyIsActive = (): Promise<void> =>
    LocalStorageApi.unset(keys.CURRENCY_ACTIVE);
  getWalletsLocalData = (): Promise<Record<string, any>> =>
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
    updatedWalletData?: Record<string, any>
  ): Promise<WalletLocalData> => {
    const currentWalletData = await this.getWalletLocalData(walletId);
    const defaultData = {
      creationDate: new Date(),
    };
    const unmutableData = {
      id: walletId,
    };
    const walletData = Object.assign(
      {},
      defaultData,
      currentWalletData,
      updatedWalletData,
      unmutableData
    );
    await LocalStorageApi.set(keys.WALLETS, toJS(walletData), walletId);
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
  getAppAutomaticUpdateFailed = (): Promise<boolean> =>
    LocalStorageApi.get(keys.APP_AUTOMATIC_UPDATE_FAILED, false);
  setAppAutomaticUpdateFailed = (): Promise<void> =>
    LocalStorageApi.set(keys.APP_AUTOMATIC_UPDATE_FAILED, true);
  unsetAppAutomaticUpdateFailed = (): Promise<void> =>
    LocalStorageApi.unset(keys.APP_AUTOMATIC_UPDATE_FAILED);
  getAppUpdateCompleted = (): Promise<string> =>
    LocalStorageApi.get(keys.APP_UPDATE_COMPLETED, false);
  setAppUpdateCompleted = (verstion: string): Promise<void> =>
    LocalStorageApi.set(keys.APP_UPDATE_COMPLETED, verstion, '');
  unsetAppUpdateCompleted = (): Promise<void> =>
    LocalStorageApi.unset(keys.APP_UPDATE_COMPLETED);
  getWalletTokenFavorites = (): Promise<Record<string, boolean>> =>
    LocalStorageApi.get(keys.TOKEN_FAVORITES, {});
  toggleWalletTokenFavorite = async (
    uniqueId: string,
    isFavorite: boolean
  ): Promise<boolean> => {
    const favorites = await this.getWalletTokenFavorites();
    const newFavorites = { ...favorites, [uniqueId]: isFavorite };
    await LocalStorageApi.set(keys.TOKEN_FAVORITES, newFavorites);
    return !isFavorite;
  };
  unsetWalletTokenFavorite = async (uniqueId: string): Promise<void> => {
    const favorites = await this.getWalletTokenFavorites();
    delete favorites[uniqueId];
    await LocalStorageApi.set(keys.TOKEN_FAVORITES, favorites);
  };
  unsetWalletTokenFavorites = async (): Promise<void> =>
    LocalStorageApi.unset(keys.TOKEN_FAVORITES);
  getAssetsLocalData = (): Promise<AssetLocalData> =>
    LocalStorageApi.get(keys.ASSET_DATA, []);
  unsetAssetsLocalData = (): Promise<void> =>
    LocalStorageApi.unset(keys.ASSET_DATA);
  getAssetLocalData = (
    policyId: string,
    assetName: string
  ): Promise<AssetLocalData> =>
    LocalStorageApi.get(keys.ASSET_DATA, {}, `${policyId}${assetName}`);
  setAssetLocalData = (
    policyId: string,
    assetName: string,
    assetLocalData: AssetLocalData
  ): Promise<void> =>
    LocalStorageApi.set(
      keys.ASSET_DATA,
      assetLocalData,
      `${policyId}${assetName}`
    );
  getAssetSettingsDialogWasOpened = (): Promise<boolean> =>
    LocalStorageApi.get(keys.ASSET_SETTINGS_DIALOG_WAS_OPENED, false);
  setAssetSettingsDialogWasOpened = (): Promise<void> =>
    LocalStorageApi.set(keys.ASSET_SETTINGS_DIALOG_WAS_OPENED, true);
  unsetAssetSettingsDialogWasOpened = (): Promise<void> =>
    LocalStorageApi.unset(keys.ASSET_SETTINGS_DIALOG_WAS_OPENED);
  getSmashServer = (): Promise<string> =>
    LocalStorageApi.get(keys.SMASH_SERVER);
  setSmashServer = (smashServerUrl: string): Promise<void> =>
    LocalStorageApi.set(keys.SMASH_SERVER, smashServerUrl);
  unsetSmashServer = (): Promise<void> =>
    LocalStorageApi.unset(keys.SMASH_SERVER);
  getStakingInfoWasOpen = (): Promise<boolean> =>
    LocalStorageApi.get(keys.STAKING_INFO_WAS_OPEN, false);
  setStakingInfoWasOpen = async (): Promise<void> =>
    LocalStorageApi.set(keys.STAKING_INFO_WAS_OPEN, true);
  unsetStakingInfoWasOpen = (): Promise<void> =>
    LocalStorageApi.unset(keys.STAKING_INFO_WAS_OPEN);
  // Paired Hardware wallets (software <-> hardware wallet / device)
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
    data?: Record<string, any>
  ): Promise<HardwareWalletLocalData> => {
    const currentWalletData = await this.getHardwareWalletLocalData(walletId);
    const unmutableData = {
      id: walletId,
    };
    const walletData = Object.assign(
      {},
      currentWalletData,
      data,
      unmutableData
    );
    await LocalStorageApi.set(
      keys.HARDWARE_WALLETS,
      toJS(walletData),
      walletId
    );
    return walletData;
  };
  unsetHardwareWalletLocalData = (walletId: string): Promise<void> =>
    LocalStorageApi.unset(keys.HARDWARE_WALLETS, walletId);
  unsetHardwareWalletLocalDataAll = async (): Promise<void> =>
    LocalStorageApi.unset(keys.HARDWARE_WALLETS);
  // Recognized Hardware wallet devices
  getHardwareWalletDevices = (): Promise<HardwareWalletsLocalData> =>
    LocalStorageApi.get(keys.HARDWARE_WALLET_DEVICES, {});
  getHardwareWalletDevice = (
    deviceId: string
  ): Promise<HardwareWalletLocalData> =>
    LocalStorageApi.get(
      keys.HARDWARE_WALLET_DEVICES,
      {
        id: deviceId,
      },
      deviceId
    );
  setHardwareWalletDevice = async (
    deviceId: string,
    data?: Record<string, any>
  ): Promise<HardwareWalletLocalData> => {
    const currentDeviceData = await this.getHardwareWalletDevice(deviceId);
    const unmutableData = {
      id: deviceId,
    };
    const deviceData = Object.assign(
      {},
      currentDeviceData,
      data,
      unmutableData
    );
    await LocalStorageApi.set(
      keys.HARDWARE_WALLET_DEVICES,
      toJS(deviceData),
      deviceId
    );
    return deviceData;
  };
  overrideHardwareWalletDevices = async (
    data: HardwareWalletDevicesType
  ): Promise<void> =>
    LocalStorageApi.set(keys.HARDWARE_WALLET_DEVICES, toJS(data));
  unsetHardwareWalletDevice = (deviceId: string): Promise<void> =>
    LocalStorageApi.unset(keys.HARDWARE_WALLET_DEVICES, deviceId);
  unsetHardwareWalletDevicesAll = async (): Promise<void> =>
    LocalStorageApi.unset(keys.HARDWARE_WALLET_DEVICES);
  reset = async () => {
    await LocalStorageApi.reset();
  };
}
