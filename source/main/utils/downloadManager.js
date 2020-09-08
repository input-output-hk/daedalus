// @flow
import { app } from 'electron';
import fs from 'fs';
import type { BrowserWindow } from 'electron';
import { MainIpcChannel } from '../ipc/lib/MainIpcChannel';

import {
  ALLOWED_DOWNLOAD_DIRECTORIES,
  DOWNLOAD_DATA_DEFAULT,
  DOWNLOAD_EVENT_TYPES as types,
  DOWNLOAD_STATES as states,
} from '../../common/config/downloadManagerConfig';
import { extractFileNameFromPath } from '../../common/utils/files';
import { downloadManagerLocalStorage as localStorage } from './mainLocalStorage';
import type {
  DownloadRendererRequest,
  DownloadMainResponse,
} from '../../common/ipc/api';
import type {
  AllowedDownloadDirectories,
  DownloadInfoInit,
  DownloadInfoProgress,
  DownloadInfoEnd,
  DownloadInfoError,
  DownloadInfo,
  DownloadDataUpdate,
} from '../../common/types/downloadManager.types';
import { stateDirectoryPath } from '../config';

export const getIdFromFileName = (fileName: string): string =>
  fileName.replace(/\./g, '-');

export const getPathFromDirectoryName = (
  directoryName: AllowedDownloadDirectories
) => {
  const downloadsDirectory = `${stateDirectoryPath}/Downloads`;
  switch (directoryName) {
    case ALLOWED_DOWNLOAD_DIRECTORIES.DESKTOP:
      return app.getPath('desktop');
    case ALLOWED_DOWNLOAD_DIRECTORIES.DOWLOADS:
      return app.getPath('downloads');
    default:
      if (!fs.existsSync(downloadsDirectory)) fs.mkdirSync(downloadsDirectory);
      return downloadsDirectory;
  }
};

export const getOriginalFilename = ({
  fileUrl,
  options,
  resumeDownload,
}: DownloadRendererRequest): string => {
  let name = '';
  if (resumeDownload) name = resumeDownload.originalFilename;
  else if (options && typeof options.fileName === 'string')
    name = options.fileName;
  else name = extractFileNameFromPath(fileUrl);
  return name;
};

export const getEventActions = async (
  info: DownloadInfo,
  window: BrowserWindow,
  requestDownloadChannel: MainIpcChannel<
    DownloadRendererRequest,
    DownloadMainResponse
  >
): Promise<Object> => {
  const { downloadId } = info;
  await localStorage.setInfo(info, downloadId);
  return {
    start: async () => {
      const eventType = types.START;
      const data = DOWNLOAD_DATA_DEFAULT;
      requestDownloadChannel.send(
        {
          eventType,
          info,
          data,
        },
        window.webContents
      );
    },
    download: async ({
      totalSize: serverFileSize,
      downloadedSize: diskFileSize,
    }: DownloadInfoInit) => {
      const rawData: DownloadDataUpdate = {
        ...{
          serverFileSize,
          diskFileSize,
          remainingSize: serverFileSize,
        },
        state: states.DOWNLOADING,
      };
      const data = await localStorage.setData(rawData, downloadId);
      requestDownloadChannel.send(
        {
          eventType: types.DOWNLOAD,
          info,
          data,
        },
        window.webContents
      );
    },
    progress: async ({
      total,
      downloaded: downloadSize,
      progress,
      speed,
    }: DownloadInfoProgress) => {
      const rawData: DownloadDataUpdate = {
        ...{
          remainingSize: total - downloadSize,
          downloadSize,
          progress,
          speed,
        },
        state: states.DOWNLOADING,
      };
      const formattedData = await localStorage.setData(rawData, downloadId);
      requestDownloadChannel.send(
        {
          eventType: types.PROGRESS,
          info,
          data: formattedData,
        },
        window.webContents
      );
    },
    end: async ({
      totalSize: downloadSize,
      onDiskSize: diskFileSize,
      incomplete,
    }: DownloadInfoEnd) => {
      const rawData: DownloadDataUpdate = {
        ...{
          downloadSize,
          diskFileSize,
          incomplete,
        },
        state: states.FINISHED,
      };
      const formattedData = await localStorage.setData(rawData, downloadId);
      const { destinationPath, temporaryFilename, originalFilename } = info;
      const temporaryPath = `${destinationPath}/${temporaryFilename}`;
      const newPath = `${destinationPath}/${originalFilename}`;
      fs.renameSync(temporaryPath, newPath);
      requestDownloadChannel.send(
        {
          eventType: types.END,
          info,
          data: formattedData,
        },
        window.webContents
      );
      const { persistLocalData } = info.options;
      if (!persistLocalData) await localStorage.unset(downloadId);
    },
    pause: async () => {
      console.log('PAUSE EVENT RECEIVED');
      const newState: DownloadDataUpdate = {
        state: states.PAUSED,
      };
      const formattedData = await localStorage.setData(newState, downloadId);
      requestDownloadChannel.send(
        {
          eventType: types.PAUSE,
          info,
          data: formattedData,
        },
        window.webContents
      );
    },
    error: async ({ message }: DownloadInfoError) => {
      const rawData: DownloadDataUpdate = {
        ...{
          message,
        },
        state: states.FAILED,
      };
      const formattedData = await localStorage.setData(rawData, downloadId);
      requestDownloadChannel.send(
        {
          eventType: types.ERROR,
          info,
          data: formattedData,
        },
        window.webContents
      );
    },
  };
};
