// @flow
import { DownloaderHelper } from 'node-downloader-helper';
import { app } from 'electron';
import type { BrowserWindow } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  PERSISTED_DOWNLOAD_STATUS,
  DOWNLOAD_STATUS,
  REQUEST_DOWNLOAD,
} from '../../common/ipc/api';
import { ALLOWED_DOWNLOAD_DIRECTORIES } from '../../common/config/download-manager';
import type {
  PersistedDownloadStatusRendererRequest,
  PersistedDownloadStatusMainResponse,
  DownloadStatusRendererRequest,
  DownloadStatusMainResponse,
  DownloadRendererRequest,
  DownloadMainResponse,
} from '../../common/ipc/api';
import type { AllowedDownloadDirectories } from '../../common/types/download-manager.types';

const getPathFromDirectoryName = (dir: AllowedDownloadDirectories) => {
  switch (dir) {
    case ALLOWED_DOWNLOAD_DIRECTORIES.DESKTOP:
      return app.getPath('desktop');
    default:
      return app.getPath('downloads');
  }
};

const getPersistDownloadStatus = async ({
  file,
}: PersistedDownloadStatusRendererRequest): Promise<PersistedDownloadStatusMainResponse> => {
  const {
    hasPendingDownload,
    pendingUpdateFileName,
  } = await checkhasPendingDownload({ file });

  let downloadProgress;

  if (hasPendingDownload) {
    downloadProgress = await checkdownloadProgress();
  }

  downloadProgress = await checkdownloadProgress();
  return {
    hasPendingDownload,
    pendingUpdateFileName,
    downloadProgress,
  };
};

const getDownloadStatus = async (): Promise<DownloadStatusMainResponse> => {
  const isDownloading = await checkisDownloading();
  const downloadProgress = await checkdownloadProgress();
  return {
    isDownloading,
    downloadProgress,
  };
};

const checkisDownloading = async (): Promise<boolean> => false;

const checkdownloadProgress = async (): Promise<number> => 0;

const checkhasPendingDownload = async ({
  file,
}: PersistedDownloadStatusRendererRequest): Promise<PersistedDownloadStatusMainResponse> => {
  const {
    // fileNamePattern,
    // fileExtentionPattern,
    filePath,
    // fileName,
    // fileExtention,
  } = file;
  const pathName = filePath || ALLOWED_DOWNLOAD_DIRECTORIES.DOWNLOADS;
  console.log('pathName', pathName);
  // const path = getPathFromDirectoryName(pathName);
  const hasPendingDownload = false;
  const pendingUpdateFileName = '';
  return {
    hasPendingDownload,
    pendingUpdateFileName,
  };
};

const requestDownload = async (
  downloadRequest: DownloadRendererRequest,
  window: BrowserWindow
): Promise<DownloadMainResponse> => {
  const defaultDirectoryName = ALLOWED_DOWNLOAD_DIRECTORIES.DOWNLOADS;
  const {
    fileUrl,
    destinationDirectoryName = defaultDirectoryName,
    options,
  } = downloadRequest;
  const destinationPath = getPathFromDirectoryName(destinationDirectoryName);
  console.log('fileUrl', fileUrl);
  console.log('destinationPath', destinationPath);
  console.log('options', options);

  const update = (isDownloading: boolean, downloadProgress: number) =>
    requestDownloadChannel.send(
      { isDownloading, downloadProgress },
      window.webContents
    );

  const download = new DownloaderHelper(fileUrl, destinationPath, options);
  download.on('start', update(true, 0));
  download.on('download', update(true, 0));
  download.on('end', update(true, 0));
  download.on('error', update(true, 0));
  download.on('stateChanged', update(true, 0));
  download.on('timeout', update(true, 0));
  // download.on('progress', ({ progress }) => update(true, progress));
  download.start();
  return download;
  // return { isDownloading: false, downloadProgress: 0 };
};

const getPersistDownloadStatusChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  PersistedDownloadStatusRendererRequest,
  PersistedDownloadStatusMainResponse
> = new MainIpcChannel(PERSISTED_DOWNLOAD_STATUS);

const getDownloadStatusChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  DownloadStatusRendererRequest,
  DownloadStatusMainResponse
> = new MainIpcChannel(DOWNLOAD_STATUS);

const requestDownloadChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  DownloadRendererRequest,
  DownloadMainResponse
> = new MainIpcChannel(REQUEST_DOWNLOAD);

export default (window: BrowserWindow) => {
  getPersistDownloadStatusChannel.onRequest(getPersistDownloadStatus);
  getDownloadStatusChannel.onRequest(getDownloadStatus);
  requestDownloadChannel.onRequest((downloadRequest: DownloadRendererRequest) =>
    requestDownload(downloadRequest, window)
  );
};
