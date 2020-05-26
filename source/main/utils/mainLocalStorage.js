// @flow
import {
  STORAGE_TYPES,
  STORAGE_KEYS,
} from '../../common/config/electron-store.config';
import { requestElectronStore } from '../ipc/electronStoreConversation';

export const downloadManagerLocalStorage = {
  get: async (id?: string) =>
    requestElectronStore({
      type: STORAGE_TYPES.GET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
      id,
    }),
  set: async (data: any, id?: string) =>
    requestElectronStore({
      type: STORAGE_TYPES.SET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
      data,
      id,
    }),
  update: async (updatedData: any, id?: string) => {
    const {
      downloadInfo: currentDownladInfo,
    } = await downloadManagerLocalStorage.get(id);
    const { downloadInfo: newDownladInfo } = updatedData;
    const downloadInfo = {
      ...currentDownladInfo,
      ...newDownladInfo,
    };
    const data = {
      ...updatedData,
      downloadInfo,
    };
    await requestElectronStore({
      type: STORAGE_TYPES.SET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
      data,
      id,
    });
  },
  unset: async () =>
    requestElectronStore({
      type: STORAGE_TYPES.GET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
    }),
};
