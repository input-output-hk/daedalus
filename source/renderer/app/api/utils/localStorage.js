'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
/* eslint-disable consistent-return */
const lodash_1 = require('lodash');
const uuid_1 = require('uuid');
const helper_1 = require('../../../../common/utils/helper');
const electronStoreConversation_1 = require('../../ipc/electronStoreConversation');
const WalletMigrationStore_1 = require('../../stores/WalletMigrationStore');
const electron_store_config_1 = require('../../../../common/config/electron-store.config');
const currencyConfig_1 = require('../../config/currencyConfig');
const analytics_1 = require('../../analytics');
/**
 * This api layer provides access to the electron local storage
 * for user settings that are not synced with any coin backend.
 */
class LocalStorageApi {
  static get = async (key, fallbackValue, id) => {
    const value = await electronStoreConversation_1.electronStoreConversation.request(
      {
        type: electron_store_config_1.STORAGE_TYPES.GET,
        key,
        id,
      }
    );
    if (value === undefined) return fallbackValue || '';
    return value;
  };
  static set = async (key, data, id) => {
    await electronStoreConversation_1.electronStoreConversation.request({
      type: electron_store_config_1.STORAGE_TYPES.SET,
      key,
      data: (0, helper_1.toJS)(data),
      id,
    });
  };
  static unset = async (key, id) => {
    await electronStoreConversation_1.electronStoreConversation.request({
      type: electron_store_config_1.STORAGE_TYPES.DELETE,
      key,
      id,
    });
  };
  static reset = async () => {
    await electronStoreConversation_1.electronStoreConversation.request({
      type: electron_store_config_1.STORAGE_TYPES.RESET,
      key: electron_store_config_1.STORAGE_KEYS.RESET,
    });
  };
  getUserLocale = () =>
    LocalStorageApi.get(electron_store_config_1.STORAGE_KEYS.USER_LOCALE);
  setUserLocale = (locale) =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.USER_LOCALE,
      locale
    );
  unsetUserLocale = () =>
    LocalStorageApi.unset(electron_store_config_1.STORAGE_KEYS.USER_LOCALE);
  getUserNumberFormat = () =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.USER_NUMBER_FORMAT
    );
  setUserNumberFormat = (numberFormat) =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.USER_NUMBER_FORMAT,
      numberFormat
    );
  unsetUserNumberFormat = () =>
    LocalStorageApi.unset(
      electron_store_config_1.STORAGE_KEYS.USER_NUMBER_FORMAT
    );
  getUserDateFormatEnglish = () =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.USER_DATE_FORMAT_ENGLISH
    );
  setUserDateFormatEnglish = (dateFormat) =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.USER_DATE_FORMAT_ENGLISH,
      dateFormat
    );
  unsetUserDateFormatEnglish = () =>
    LocalStorageApi.unset(
      electron_store_config_1.STORAGE_KEYS.USER_DATE_FORMAT_ENGLISH
    );
  getUserDateFormatJapanese = () =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.USER_DATE_FORMAT_JAPANESE
    );
  setUserDateFormatJapanese = (dateFormat) =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.USER_DATE_FORMAT_JAPANESE,
      dateFormat
    );
  unsetUserDateFormatJapanese = () =>
    LocalStorageApi.unset(
      electron_store_config_1.STORAGE_KEYS.USER_DATE_FORMAT_JAPANESE
    );
  getUserTimeFormat = () =>
    LocalStorageApi.get(electron_store_config_1.STORAGE_KEYS.USER_TIME_FORMAT);
  setUserTimeFormat = (timeFormat) =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.USER_TIME_FORMAT,
      timeFormat
    );
  unsetUserTimeFormat = () =>
    LocalStorageApi.unset(
      electron_store_config_1.STORAGE_KEYS.USER_TIME_FORMAT
    );
  getTermsOfUseAcceptance = () =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.TERMS_OF_USE_ACCEPTANCE,
      false
    );
  setTermsOfUseAcceptance = () =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.TERMS_OF_USE_ACCEPTANCE,
      true
    );
  unsetTermsOfUseAcceptance = () =>
    LocalStorageApi.unset(
      electron_store_config_1.STORAGE_KEYS.TERMS_OF_USE_ACCEPTANCE
    );
  getAnalyticsAcceptance = () =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.ANALYTICS_ACCEPTANCE,
      analytics_1.AnalyticsAcceptanceStatus.PENDING
    );
  setAnalyticsAcceptance = (status) =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.ANALYTICS_ACCEPTANCE,
      status
    );
  unsetAnalyticsAcceptance = () =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.ANALYTICS_ACCEPTANCE,
      analytics_1.AnalyticsAcceptanceStatus.PENDING
    );
  getUserID = async () => {
    let userId = await LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.USER_ID,
      null
    );
    if (!userId) {
      userId = (0, uuid_1.v4)();
      await LocalStorageApi.set(
        electron_store_config_1.STORAGE_KEYS.USER_ID,
        userId
      );
    }
    return userId;
  };
  getUserTheme = () =>
    LocalStorageApi.get(electron_store_config_1.STORAGE_KEYS.THEME);
  setUserTheme = (theme) =>
    LocalStorageApi.set(electron_store_config_1.STORAGE_KEYS.THEME, theme);
  unsetUserTheme = () =>
    LocalStorageApi.unset(electron_store_config_1.STORAGE_KEYS.THEME);
  getDataLayerMigrationAcceptance = () =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.DATA_LAYER_MIGRATION_ACCEPTANCE,
      false
    );
  setDataLayerMigrationAcceptance = () =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.DATA_LAYER_MIGRATION_ACCEPTANCE,
      true
    );
  unsetDataLayerMigrationAcceptance = () =>
    LocalStorageApi.unset(
      electron_store_config_1.STORAGE_KEYS.DATA_LAYER_MIGRATION_ACCEPTANCE
    );
  getCurrencySelected = async () => {
    const localCurrencySelected = await LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.CURRENCY_SELECTED,
      currencyConfig_1.CURRENCY_DEFAULT_SELECTED
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
    const localCurrencyCode =
      (0, lodash_1.get)(localCurrencySelected, 'code') ||
      (0, lodash_1.get)(localCurrencySelected, 'symbol');
    this.setCurrencySelected(localCurrencyCode);
    return localCurrencyCode;
  };
  setCurrencySelected = (currencyCode) =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.CURRENCY_SELECTED,
      currencyCode
    );
  unsetCurrencySelected = () =>
    LocalStorageApi.unset(
      electron_store_config_1.STORAGE_KEYS.CURRENCY_SELECTED
    );
  getCurrencyIsActive = () =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.CURRENCY_ACTIVE,
      currencyConfig_1.CURRENCY_IS_ACTIVE_BY_DEFAULT
    );
  setCurrencyIsActive = async (isActive) =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.CURRENCY_ACTIVE,
      isActive
    );
  unsetCurrencyIsActive = () =>
    LocalStorageApi.unset(electron_store_config_1.STORAGE_KEYS.CURRENCY_ACTIVE);
  getWalletsLocalData = () =>
    LocalStorageApi.get(electron_store_config_1.STORAGE_KEYS.WALLETS, {});
  getWalletLocalData = (walletId) =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.WALLETS,
      {
        id: walletId,
      },
      walletId
    );
  setWalletLocalData = async (walletId, updatedWalletData) => {
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
    await LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.WALLETS,
      (0, helper_1.toJS)(walletData),
      walletId
    );
    return walletData;
  };
  unsetWalletLocalData = (walletId) =>
    LocalStorageApi.unset(
      electron_store_config_1.STORAGE_KEYS.WALLETS,
      walletId
    );
  getReadNews = () =>
    LocalStorageApi.get(electron_store_config_1.STORAGE_KEYS.READ_NEWS, []);
  markNewsAsRead = async (newsTimestamps) => {
    const readNews =
      (await LocalStorageApi.get(
        electron_store_config_1.STORAGE_KEYS.READ_NEWS
      )) || [];
    if (!(0, lodash_1.includes)(readNews, newsTimestamps[0])) {
      await LocalStorageApi.set(
        electron_store_config_1.STORAGE_KEYS.READ_NEWS,
        readNews.concat(newsTimestamps)
      );
    }
    return readNews;
  };
  markNewsAsUnread = async (newsTimestamp) => {
    const readNews =
      (await LocalStorageApi.get(
        electron_store_config_1.STORAGE_KEYS.READ_NEWS
      )) || [];
    if ((0, lodash_1.includes)(readNews, newsTimestamp)) {
      await LocalStorageApi.set(
        electron_store_config_1.STORAGE_KEYS.READ_NEWS,
        (0, lodash_1.without)(readNews, newsTimestamp)
      );
    }
    return readNews;
  };
  unsetReadNews = () =>
    LocalStorageApi.unset(electron_store_config_1.STORAGE_KEYS.READ_NEWS);
  getWalletMigrationStatus = () =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.WALLET_MIGRATION_STATUS,
      WalletMigrationStore_1.WalletMigrationStatuses.UNSTARTED
    );
  setWalletMigrationStatus = (status) =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.WALLET_MIGRATION_STATUS,
      status
    );
  unsetWalletMigrationStatus = () =>
    LocalStorageApi.unset(
      electron_store_config_1.STORAGE_KEYS.WALLET_MIGRATION_STATUS
    );
  getAppAutomaticUpdateFailed = () =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.APP_AUTOMATIC_UPDATE_FAILED,
      false
    );
  setAppAutomaticUpdateFailed = () =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.APP_AUTOMATIC_UPDATE_FAILED,
      true
    );
  unsetAppAutomaticUpdateFailed = () =>
    LocalStorageApi.unset(
      electron_store_config_1.STORAGE_KEYS.APP_AUTOMATIC_UPDATE_FAILED
    );
  getAppUpdateCompleted = () =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.APP_UPDATE_COMPLETED,
      false
    );
  setAppUpdateCompleted = (verstion) =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.APP_UPDATE_COMPLETED,
      verstion,
      ''
    );
  unsetAppUpdateCompleted = () =>
    LocalStorageApi.unset(
      electron_store_config_1.STORAGE_KEYS.APP_UPDATE_COMPLETED
    );
  getWalletTokenFavorites = () =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.TOKEN_FAVORITES,
      {}
    );
  toggleWalletTokenFavorite = async (uniqueId, isFavorite) => {
    const favorites = await this.getWalletTokenFavorites();
    const newFavorites = { ...favorites, [uniqueId]: isFavorite };
    await LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.TOKEN_FAVORITES,
      newFavorites
    );
    return !isFavorite;
  };
  unsetWalletTokenFavorite = async (uniqueId) => {
    const favorites = await this.getWalletTokenFavorites();
    delete favorites[uniqueId];
    await LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.TOKEN_FAVORITES,
      favorites
    );
  };
  unsetWalletTokenFavorites = async () =>
    LocalStorageApi.unset(electron_store_config_1.STORAGE_KEYS.TOKEN_FAVORITES);
  getAssetsLocalData = () =>
    LocalStorageApi.get(electron_store_config_1.STORAGE_KEYS.ASSET_DATA, []);
  unsetAssetsLocalData = () =>
    LocalStorageApi.unset(electron_store_config_1.STORAGE_KEYS.ASSET_DATA);
  getAssetLocalData = (policyId, assetName) =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.ASSET_DATA,
      {},
      `${policyId}${assetName}`
    );
  setAssetLocalData = (policyId, assetName, assetLocalData) =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.ASSET_DATA,
      assetLocalData,
      `${policyId}${assetName}`
    );
  getSmashServer = () =>
    LocalStorageApi.get(electron_store_config_1.STORAGE_KEYS.SMASH_SERVER);
  setSmashServer = (smashServerUrl) =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.SMASH_SERVER,
      smashServerUrl
    );
  unsetSmashServer = () =>
    LocalStorageApi.unset(electron_store_config_1.STORAGE_KEYS.SMASH_SERVER);
  getStakingInfoWasOpen = () =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.STAKING_INFO_WAS_OPEN,
      false
    );
  setStakingInfoWasOpen = async () =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.STAKING_INFO_WAS_OPEN,
      true
    );
  unsetStakingInfoWasOpen = () =>
    LocalStorageApi.unset(
      electron_store_config_1.STORAGE_KEYS.STAKING_INFO_WAS_OPEN
    );
  // Paired Hardware wallets (software <-> hardware wallet / device)
  getHardwareWalletsLocalData = () =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.HARDWARE_WALLETS,
      {}
    );
  getHardwareWalletLocalData = (walletId) =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.HARDWARE_WALLETS,
      {
        id: walletId,
      },
      walletId
    );
  setHardwareWalletLocalData = async (walletId, data) => {
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
      electron_store_config_1.STORAGE_KEYS.HARDWARE_WALLETS,
      (0, helper_1.toJS)(walletData),
      walletId
    );
    return walletData;
  };
  unsetHardwareWalletLocalData = (walletId) =>
    LocalStorageApi.unset(
      electron_store_config_1.STORAGE_KEYS.HARDWARE_WALLETS,
      walletId
    );
  unsetHardwareWalletLocalDataAll = async () =>
    LocalStorageApi.unset(
      electron_store_config_1.STORAGE_KEYS.HARDWARE_WALLETS
    );
  // Recognized Hardware wallet devices
  getHardwareWalletDevices = () =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.HARDWARE_WALLET_DEVICES,
      {}
    );
  getHardwareWalletDevice = (deviceId) =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.HARDWARE_WALLET_DEVICES,
      {
        id: deviceId,
      },
      deviceId
    );
  setHardwareWalletDevice = async (deviceId, data) => {
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
      electron_store_config_1.STORAGE_KEYS.HARDWARE_WALLET_DEVICES,
      (0, helper_1.toJS)(deviceData),
      deviceId
    );
    return deviceData;
  };
  overrideHardwareWalletDevices = async (data) =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.HARDWARE_WALLET_DEVICES,
      (0, helper_1.toJS)(data)
    );
  unsetHardwareWalletDevice = (deviceId) =>
    LocalStorageApi.unset(
      electron_store_config_1.STORAGE_KEYS.HARDWARE_WALLET_DEVICES,
      deviceId
    );
  unsetHardwareWalletDevicesAll = async () =>
    LocalStorageApi.unset(
      electron_store_config_1.STORAGE_KEYS.HARDWARE_WALLET_DEVICES
    );
  setStakePoolsListViewTooltip = async (visited) =>
    LocalStorageApi.set(
      electron_store_config_1.STORAGE_KEYS.STAKE_POOLS_LIST_VIEW_TOOLTIP,
      visited
    );
  getStakePoolsListViewTooltip = async () =>
    LocalStorageApi.get(
      electron_store_config_1.STORAGE_KEYS.STAKE_POOLS_LIST_VIEW_TOOLTIP,
      true
    );
  unsetStakePoolsListViewTooltip = async () =>
    LocalStorageApi.unset(
      electron_store_config_1.STORAGE_KEYS.STAKE_POOLS_LIST_VIEW_TOOLTIP
    );
  reset = async () => {
    await LocalStorageApi.reset();
  };
}
exports.default = LocalStorageApi;
//# sourceMappingURL=localStorage.js.map
