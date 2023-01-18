import { mergeWith, omit } from 'lodash';
import {
  STORAGE_TYPES,
  STORAGE_KEYS,
} from '../../common/config/electron-store.config';
import type {
  DownloadInfo,
  DownloadData,
  DownloadDataUpdate,
  DownloadLocalDataResponse,
} from '../../common/types/downloadManager.types';
import {
  DOWNLOAD_DATA_DEFAULT,
  DOWNLOAD_STATES,
} from '../../common/config/downloadManagerConfig';
import { requestElectronStore } from '../ipc/electronStoreConversation';

export const downloadManagerLocalStorage = {
  get: async (id: string): Promise<DownloadLocalDataResponse> => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'info' does not exist on type '{}'.
    const { info, data } =
      (await requestElectronStore({
        type: STORAGE_TYPES.GET,
        key: STORAGE_KEYS.DOWNLOAD_MANAGER,
        id,
      })) || {};
    return {
      info,
      data,
    };
  },
  getAll: async () => {
    const info = await requestElectronStore({
      type: STORAGE_TYPES.GET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
    });
    return info || [];
  },
  setInfo: async (info: DownloadInfo, id: string) => {
    const data: DownloadData = DOWNLOAD_DATA_DEFAULT;
    requestElectronStore({
      type: STORAGE_TYPES.SET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
      data: {
        info,
        data,
      },
      id,
    });
  },
  setData: async (
    newData: DownloadDataUpdate,
    id: string
  ): Promise<DownloadData> => {
    const { data: oldData, info } = await downloadManagerLocalStorage.get(id);
    const data = mergeWith(oldData, newData, (o, n, key) => {
      if (typeof n === 'number' && key !== 'remainingSize')
        return Math.max(o, n);
      return n;
    });
    await requestElectronStore({
      type: STORAGE_TYPES.SET,
      key: STORAGE_KEYS.DOWNLOAD_MANAGER,
      data: {
        info,
        data,
      },
      id,
    });
    return data;
  },
  setAllPaused: async () => {
    const downloads = await downloadManagerLocalStorage.getAll();
    const downloadsArray = Object.keys(downloads);

    for (let index = 0; index < downloadsArray.length; index++) {
      const downloadId = downloadsArray[index];
      const { data } = downloads[downloadId];
      const { state, progress } = data;

      if (state === DOWNLOAD_STATES.DOWNLOADING && progress < 100) {
        await downloadManagerLocalStorage.setData(
          {
            state: DOWNLOAD_STATES.PAUSED,
          },
          downloadId
        );
      }
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
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      data: omit(localDownloadsData, id),
    });
    await requestElectronStore({
      type: STORAGE_TYPES.DELETE,
      key: STORAGE_KEYS.APP_AUTOMATIC_UPDATE_FAILED,
    });
  },
};
