// @flow
import { app } from 'electron';
import fs from 'fs';
import type { BrowserWindow } from 'electron';
import { MainIpcChannel } from '../ipc/lib/MainIpcChannel';
import { logger } from './logging';
import {
  ALLOWED_DOWNLOAD_DIRECTORIES,
  DOWNLOAD_DATA_DEFAULT,
  DOWNLOAD_EVENT_TYPES as types,
  DOWNLOAD_STATES as states,
  ERROR_TIME_AFTER_NO_END_EVENT,
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

export const downloads = {};

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

const getPath = (
  info: DownloadInfo
): { temporaryPath: string, newPath: string } => {
  const { destinationPath, temporaryFilename, originalFilename } = info;
  const temporaryPath = `${destinationPath}/${temporaryFilename}`;
  const newPath = `${destinationPath}/${originalFilename}`;
  return {
    temporaryPath,
    newPath,
  };
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
  let serverFileSize;
  let checkNoEndEvent: TimeoutID;

  const startEvent = async () => {
    logger.info('DownloadManager:startEvent.');
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
  };
  const downloadEvent = async ({
    totalSize,
    downloadedSize: diskFileSize,
  }: DownloadInfoInit) => {
    logger.info('DownloadManager:downloadEvent.');
    serverFileSize = totalSize;
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
  };
  const progressEvent = async ({
    total,
    downloaded: downloadSize,
    progress,
    speed,
  }: DownloadInfoProgress) => {
    const rawData: DownloadDataUpdate = {
      ...{
        remainingSize: total - downloadSize,
        serverFileSize,
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
    if (progress === 100) {
      // Checks if the file was delete while the download was in progress
      checkNoEndEvent = setTimeout(() => {
        const { temporaryPath, newPath } = getPath(info);
        if (!fs.existsSync(temporaryPath) || !fs.existsSync(newPath)) {
          errorEvent({ message: 'The download file was manually deleted' });
        }
      }, ERROR_TIME_AFTER_NO_END_EVENT);
    }
  };
  const endEvent = async ({
    totalSize: downloadSize,
    onDiskSize: diskFileSize,
    incomplete,
  }: DownloadInfoEnd) => {
    clearTimeout(checkNoEndEvent);
    delete downloads[downloadId];
    logger.info('DownloadManager:endEvent.');
    const rawData: DownloadDataUpdate = {
      ...{
        downloadSize,
        diskFileSize,
        incomplete,
      },
      state: states.FINISHED,
    };
    const formattedData = await localStorage.setData(rawData, downloadId);
    const { temporaryPath, newPath } = getPath(info);
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
  };
  const pauseEvent = async () => {
    logger.info('DownloadManager:pauseEvent.');
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
  };
  const errorEvent = async ({ message }: DownloadInfoError) => {
    logger.error('DownloadManager:errorEvent', { error: message });
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
        error: message,
      },
      window.webContents
    );
  };

  return {
    start: startEvent,
    download: downloadEvent,
    progress: progressEvent,
    end: endEvent,
    pause: pauseEvent,
    error: errorEvent,
  };
};
