// @flow
import { mergeWith, omit } from 'lodash';
import {
  STORAGE_TYPES,
  STORAGE_KEYS,
} from '../../common/config/electron-store.config';
import type {
  DownloadData,
  DownloadProgress,
  DownloadProgressUpdate,
  DownloadLocalDataResponse,
} from '../../common/types/downloadManager.types';
import {
  DOWNLOAD_PROGRESS_DEFAULT,
  DOWNLOAD_STATES,
} from '../../common/config/downloadManagerConfig';
import { requestElectronStore } from '../ipc/electronStoreConversation';

export const downloadManagerLocalStorage = {
  get: async (id: string): Promise<DownloadLocalDataResponse> => {
    const { data, progress } =
      (await requestElectronStore({
        type: STORAGE_TYPES.GET,
        key: STORAGE_KEYS.DOWNLOAD_MANAGER,
        id,
      })) || {};
    if (!data || !progress) throw new Error('Invalid download Id');
    return { data, progress };
  },
  getAll: async () => {
    const data = await requestElectronStore({
      type: STORAGE_TYPES.GET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
    });
    return data || [];
  },
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
    newProgres: DownloadProgressUpdate,
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
  setAllStopped: async () => {
    const downloads = await downloadManagerLocalStorage.getAll();
    const downloadsArray = Object.keys(downloads);
    for (let index = 0; index < downloadsArray.length; index++) {
      await downloadManagerLocalStorage.setProgress(
        {
          state: DOWNLOAD_STATES.STOPPED,
        },
        downloadsArray[index]
      );
    }
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
