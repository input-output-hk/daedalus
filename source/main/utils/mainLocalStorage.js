// @flow
import {
  STORAGE_TYPES,
  STORAGE_KEYS,
} from '../../common/config/electron-store.config';
import { requestElectronStore } from '../ipc/electronStoreConversation';

const getIdFromFilename = (filename: string): string =>
  filename.replace(/./g, '-');

export const downloadManagerLocalStorage = {
  get: async (id: string) =>
    requestElectronStore({
      type: STORAGE_TYPES.GET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
      id: getIdFromFilename(id),
    }),
  set: async (data: any, id: string) =>
    requestElectronStore({
      type: STORAGE_TYPES.SET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
      data,
      id: getIdFromFilename(id),
    }),
  update: async (updatedData: any, id: string) => {
    const {
      downloadInfo: currentDownladInfo,
    } = await downloadManagerLocalStorage.get(getIdFromFilename(id));
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
      id: getIdFromFilename(id),
    });
  },
  unset: async () =>
    requestElectronStore({
      type: STORAGE_TYPES.GET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
    }),
};
