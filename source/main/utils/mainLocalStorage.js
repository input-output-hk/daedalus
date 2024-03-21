'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.downloadManagerLocalStorage = void 0;
const lodash_1 = require('lodash');
const electron_store_config_1 = require('../../common/config/electron-store.config');
const downloadManagerConfig_1 = require('../../common/config/downloadManagerConfig');
const electronStoreConversation_1 = require('../ipc/electronStoreConversation');
exports.downloadManagerLocalStorage = {
  get: async (id) => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'info' does not exist on type '{}'.
    const { info, data } =
      (await (0, electronStoreConversation_1.requestElectronStore)({
        type: electron_store_config_1.STORAGE_TYPES.GET,
        key: electron_store_config_1.STORAGE_KEYS.DOWNLOAD_MANAGER,
        id,
      })) || {};
    return {
      info,
      data,
    };
  },
  getAll: async () => {
    const info = await (0, electronStoreConversation_1.requestElectronStore)({
      type: electron_store_config_1.STORAGE_TYPES.GET,
      key: electron_store_config_1.STORAGE_KEYS.DOWNLOAD_MANAGER,
    });
    return info || [];
  },
  setInfo: async (info, id) => {
    const data = downloadManagerConfig_1.DOWNLOAD_DATA_DEFAULT;
    (0, electronStoreConversation_1.requestElectronStore)({
      type: electron_store_config_1.STORAGE_TYPES.SET,
      key: electron_store_config_1.STORAGE_KEYS.DOWNLOAD_MANAGER,
      data: {
        info,
        data,
      },
      id,
    });
  },
  setData: async (newData, id) => {
    const {
      data: oldData,
      info,
    } = await exports.downloadManagerLocalStorage.get(id);
    const data = (0, lodash_1.mergeWith)(oldData, newData, (o, n, key) => {
      if (typeof n === 'number' && key !== 'remainingSize')
        return Math.max(o, n);
      return n;
    });
    await (0, electronStoreConversation_1.requestElectronStore)({
      type: electron_store_config_1.STORAGE_TYPES.SET,
      key: electron_store_config_1.STORAGE_KEYS.DOWNLOAD_MANAGER,
      data: {
        info,
        data,
      },
      id,
    });
    return data;
  },
  setAllPaused: async () => {
    const downloads = await exports.downloadManagerLocalStorage.getAll();
    const downloadsArray = Object.keys(downloads);
    for (let index = 0; index < downloadsArray.length; index++) {
      const downloadId = downloadsArray[index];
      const { data } = downloads[downloadId];
      const { state, progress } = data;
      if (
        state === downloadManagerConfig_1.DOWNLOAD_STATES.DOWNLOADING &&
        progress < 100
      ) {
        await exports.downloadManagerLocalStorage.setData(
          {
            state: downloadManagerConfig_1.DOWNLOAD_STATES.PAUSED,
          },
          downloadId
        );
      }
    }
  },
  unset: async (id) => {
    const localDownloadsData = await (0,
    electronStoreConversation_1.requestElectronStore)({
      type: electron_store_config_1.STORAGE_TYPES.GET,
      key: electron_store_config_1.STORAGE_KEYS.DOWNLOAD_MANAGER,
    });
    await (0, electronStoreConversation_1.requestElectronStore)({
      type: electron_store_config_1.STORAGE_TYPES.SET,
      key: electron_store_config_1.STORAGE_KEYS.DOWNLOAD_MANAGER,
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      data: (0, lodash_1.omit)(localDownloadsData, id),
    });
    await (0, electronStoreConversation_1.requestElectronStore)({
      type: electron_store_config_1.STORAGE_TYPES.DELETE,
      key: electron_store_config_1.STORAGE_KEYS.APP_AUTOMATIC_UPDATE_FAILED,
    });
  },
};
//# sourceMappingURL=mainLocalStorage.js.map
