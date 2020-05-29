// @flow
import { mergeWith, omit } from 'lodash';
import {
  STORAGE_TYPES,
  STORAGE_KEYS,
} from '../../common/config/electron-store.config';
import type {
  DownloadData,
  DownloadProgress,
} from '../../common/types/download-manager.types';
import { DOWNLOAD_PROGRESS_DEFAULT } from '../../common/config/download-manager';
import { requestElectronStore } from '../ipc/electronStoreConversation';

export const downloadManagerLocalStorage = {
  get: async (id: string) => {
    const { data = {}, progress = {} } = await requestElectronStore({
      type: STORAGE_TYPES.GET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
      id,
    });
    return { data, progress };
  },
  getAll: async () =>
    requestElectronStore({
      type: STORAGE_TYPES.GET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
    }),
  setData: async (data: DownloadData, id: string) => {
    const progress: DownloadProgress = DOWNLOAD_PROGRESS_DEFAULT;
    requestElectronStore({
      type: STORAGE_TYPES.SET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
      data: { data, progress },
      id,
    });
  },
  setProgress: async (
    newProgres: DownloadProgress,
    id: string
  ): Promise<DownloadProgress> => {
    const {
      progress: oldProgress,
      data,
    } = await downloadManagerLocalStorage.get(id);
    const progress = mergeWith(oldProgress, newProgres, (o, n, key) => {
      if (typeof n === 'number' && key !== 'remainingSize')
        return Math.max(o, n);
      return n;
    });
    await requestElectronStore({
      type: STORAGE_TYPES.SET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
      data: { data, progress },
      id,
    });
    return progress;
  },
  unset: async (id: string) => {
    const localDownloadsData = await requestElectronStore({
      type: STORAGE_TYPES.GET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
    });
    await requestElectronStore({
      type: STORAGE_TYPES.SET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
      data: omit(localDownloadsData, id),
    });
  },
};
