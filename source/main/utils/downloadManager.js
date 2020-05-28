// @flow

import { app } from 'electron';
import fs from 'fs';
import type { BrowserWindow } from 'electron';
import {
  ALLOWED_DOWNLOAD_DIRECTORIES,
  DOWNLOAD_INFO_DEFAULT,
  // DEFAULT_DIRECTORY_NAME,
  // TEMPORARY_FILENAME,
  DOWNLOAD_STATES as state,
} from '../../common/config/download-manager';
import { extractFileNameFromPath } from '../../common/utils/files';
import { downloadManagerLocalStorage } from './mainLocalStorage';
import type {
  // PersistedDownloadStatusRendererRequest,
  // PersistedDownloadStatusMainResponse,
  // DownloadStatusRendererRequest,
  // DownloadStatusMainResponse,
  DownloadRendererRequest,
  // DownloadMainResponse,
} from '../../common/ipc/api';
import type {
  AllowedDownloadDirectories,
  // DownloadInfo,
  // DownloadEvent,
  // DownloadEventType,
  // DownloadInfoInit,
  // DownloadInfoProgress,
  // DownloadInfoEnd,
  // DownloadInfoError,
} from '../../common/types/download-manager.types';

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

// DownloadInfoInit,
// DownloadInfoProgress,
// DownloadInfoEnd,
// DownloadInfoError,

export const downloadUpdateActions = async (
  fileUrl: string,
  destinationPath: string,
  temporaryFilename: string,
  originalFilename: string,
  options: DownloadRequestOptions,
  window: BrowserWindow
): Promise<Function> => {
  await downloadManagerLocalStorage.set(
    {
      downloadInfo: {
        fileUrl,
        originalFilename,
        temporaryFilename,
        options,
        status: state.IDLE,
      },
    },
    originalFilename
  );
  return async (status: DownloadEventType, downloadEvent: DownloadEvent) => {
    const {
      downloadInfo: localDownloadInfo,
    } = await downloadManagerLocalStorage.get(originalFilename);
    const downloadInfo: DownloadInfo = formatUpdate(status)(
      downloadEvent,
      localDownloadInfo
    );
    console.log('localDownloadInfo', localDownloadInfo);
    console.log('downloadInfo', downloadInfo);
    console.log('originalFilename', originalFilename);
    await downloadManagerLocalStorage.update(
      {
        status,
        downloadInfo,
      },
      originalFilename
    );
    if (status === state.STARTED) {
      // STARTED event doesn't have `downloadInfo`` response
      return;
    }
    if (status === state.TIMEOUT || status === state.ERROR) {
      throw new Error(downloadInfo.message);
    }
    if (status === state.FINISHED) {
      const temporaryPath = `${destinationPath}/${temporaryFilename}`;
      const newPath = `${destinationPath}/${originalFilename}`;
      fs.renameSync(temporaryPath, newPath);
    }
    requestDownloadChannel.send(
      {
        downloadInfo,
        progressstate: status,
      },
      window.webContents
    );
  };
};

export const formatUpdate = (status: DownloadEventType) => {
  switch (status) {
    case statuses.STARTED:
      return (
        update: DownloadEvent,
        localDownloadInfo: DownloadInfo
      ): DownloadInfo =>
        Object.assign({}, DOWNLOAD_INFO_DEFAULT, localDownloadInfo, update);
    case statuses.FINISHED:
      return (
        update: DownloadEvent,
        localDownloadInfo: DownloadInfo
      ): DownloadInfo =>
        Object.assign({}, DOWNLOAD_INFO_DEFAULT, localDownloadInfo, update);
    case statuses.ERROR:
      return (
        update: DownloadEvent,
        localDownloadInfo: DownloadInfo
      ): DownloadInfo =>
        Object.assign({}, DOWNLOAD_INFO_DEFAULT, localDownloadInfo, update);
    default:
      return (
        { total = 0, downloaded: downloadedSize, ...update }: DownloadEvent,
        { totalSize }: DownloadInfo
      ): DownloadInfo =>
        Object.assign({}, DOWNLOAD_INFO_DEFAULT, update, {
          downloadedSize,
          totalSize: Math.max(total, totalSize),
        });
  }
};
