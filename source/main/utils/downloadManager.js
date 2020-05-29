// @flow
import { app } from 'electron';
import fs from 'fs';
import type { BrowserWindow } from 'electron';
import { MainIpcChannel } from '../ipc/lib/MainIpcChannel';

import {
  ALLOWED_DOWNLOAD_DIRECTORIES,
  DOWNLOAD_PROGRESS_DEFAULT,
  // DEFAULT_DIRECTORY_NAME,
  // TEMPORARY_FILENAME,
  DOWNLOAD_EVENT_TYPES as types,
  DOWNLOAD_STATES as states,
} from '../../common/config/download-manager';
import { extractFileNameFromPath } from '../../common/utils/files';
import { downloadManagerLocalStorage as localStorage } from './mainLocalStorage';
import type {
  // PersistedDownloadStatusRendererRequest,
  // PersistedDownloadStatusMainResponse,
  // DownloadStatusRendererRequest,
  // DownloadStatusMainResponse,
  DownloadRendererRequest,
  DownloadMainResponse,
} from '../../common/ipc/api';
import type {
  AllowedDownloadDirectories,
  DownloadInfoInit,
  DownloadInfoProgress,
  DownloadInfoEnd,
  DownloadInfoError,
  DownloadData,
  DownloadProgress,
} from '../../common/types/download-manager.types';

const getIdFromFilename = (filename: string): string =>
  filename.replace(/\./g, '-');

export const getPathFromDirectoryName = (
  directoryName: AllowedDownloadDirectories
) => {
  switch (directoryName) {
    case ALLOWED_DOWNLOAD_DIRECTORIES.DESKTOP:
      return app.getPath('desktop');
    default:
      return app.getPath('downloads');
  }
};

export const getOriginalFilename = ({
  fileUrl,
  options,
}: DownloadRendererRequest) =>
  options && typeof options.fileName === 'string'
    ? options.fileName
    : extractFileNameFromPath(fileUrl);

export const getEventActions = async (
  data: DownloadData,
  window: BrowserWindow,
  requestDownloadChannel: MainIpcChannel<
    DownloadRendererRequest,
    DownloadMainResponse
  >
): Promise<Object> => {
  const id = getIdFromFilename(data.originalFilename);
  await localStorage.setData(data, id);
  return {
    start: async () => {
      const eventType = types.START;
      const progress = DOWNLOAD_PROGRESS_DEFAULT;
      requestDownloadChannel.send(
        {
          eventType,
          data,
          progress,
        },
        window.webContents
      );
    },
    download: async ({
      totalSize: serverFileSize,
      downloadedSize: diskFileSize,
    }: DownloadInfoInit) => {
      const rawProgress: DownloadProgress = {
        ...DOWNLOAD_PROGRESS_DEFAULT,
        ...{
          serverFileSize,
          diskFileSize,
          remainingSize: serverFileSize,
        },
        state: states.DOWNLOADING,
      };
      const progress = await localStorage.setProgress(rawProgress, id);
      requestDownloadChannel.send(
        {
          eventType: types.DOWNLOAD,
          data,
          progress,
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
      const rawProgress: DownloadProgress = {
        ...DOWNLOAD_PROGRESS_DEFAULT,
        ...{
          remainingSize: total - downloadSize,
          downloadSize,
          progress,
          speed,
        },
        state: states.DOWNLOADING,
      };
      const formattedProgress = await localStorage.setProgress(rawProgress, id);
      requestDownloadChannel.send(
        {
          eventType: types.PROGRESS,
          data,
          progress: formattedProgress,
        },
        window.webContents
      );
    },
    end: async ({
      totalSize: downloadSize,
      onDiskSize: diskFileSize,
      incomplete,
    }: DownloadInfoEnd) => {
      const rawProgress: DownloadProgress = {
        ...DOWNLOAD_PROGRESS_DEFAULT,
        ...{
          downloadSize,
          diskFileSize,
          incomplete,
        },
        state: states.FINISHED,
      };
      const formattedProgress = await localStorage.setProgress(rawProgress, id);
      const { destinationPath, temporaryFilename, originalFilename } = data;
      const temporaryPath = `${destinationPath}/${temporaryFilename}`;
      const newPath = `${destinationPath}/${originalFilename}`;
      fs.renameSync(temporaryPath, newPath);
      requestDownloadChannel.send(
        {
          eventType: types.END,
          data,
          progress: formattedProgress,
        },
        window.webContents
      );
    },
    error: async ({ message }: DownloadInfoError) => {
      const rawProgress: DownloadProgress = {
        ...DOWNLOAD_PROGRESS_DEFAULT,
        ...{
          message,
        },
        state: states.FAILED,
      };
      const formattedProgress = await localStorage.setProgress(rawProgress, id);
      requestDownloadChannel.send(
        {
          eventType: types.ERROR,
          data,
          progress: formattedProgress,
        },
        window.webContents
      );
    },
  };
};
