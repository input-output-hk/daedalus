// @flow
import { mergeWith } from 'lodash';
import {
  STORAGE_TYPES,
  STORAGE_KEYS,
} from '../../common/config/electron-store.config';
import type {
  DownloadData,
  DownloadProgress,
} from '../../common/types/download-manager.types';
import { DOWNLOAD_STATES } from '../../common/config/download-manager';
import { requestElectronStore } from '../ipc/electronStoreConversation';

const getIdFromFilename = (filename: string): string =>
  filename.replace(/\./g, '-');

export const downloadManagerLocalStorage = {
  get: async (id: string) => {
    let request = await requestElectronStore({
      type: STORAGE_TYPES.GET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
      id: getIdFromFilename(id),
    });
    if (!request || !request.downloadInfo)
      request = {
        downloadInfo: {},
      };
    return request;
  },
  setData: async (data: DownloadData, id: string) => {
    const progress: DownloadProgress = {
      state: DOWNLOAD_STATES.IDLE,
      downloaded: 0,
      incomplete: false,
      isResumed: false,
      remainingSize: 0,
      serverFileSize: 0,
      diskFileSize: 0,
      downloadSize: 0,
      progress: 0,
      speed: 0,
    };
    requestElectronStore({
      type: STORAGE_TYPES.SET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
      data: { data, progress },
      id: getIdFromFilename(id),
    });
  },
  setProgress: async (newProgres: DownloadProgress, id: string) => {
    const {
      progress: oldProgress,
      data,
    } = await downloadManagerLocalStorage.get(getIdFromFilename(id));
    const progress = mergeWith(oldProgress, newProgres, (o, n) => {
      if (typeof n === 'number') return Math.max(o, n);
    });
    await requestElectronStore({
      type: STORAGE_TYPES.SET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
      data: { data, progress },
      id: getIdFromFilename(id),
    });
  },
  unset: async () =>
    requestElectronStore({
      type: STORAGE_TYPES.GET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
    }),
};
