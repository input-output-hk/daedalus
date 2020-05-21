// @flow
import { DownloaderHelper } from 'node-downloader-helper';
import { app, ipcMain } from 'electron';
import { MainIpcConversation } from './lib/MainIpcConversation';
import {
  PERSISTED_DOWNLOAD_STATUS,
  DOWNLOAD_STATUS,
  REQUEST_DOWNLOAD,
} from '../../common/ipc/api';
import type {
  PersistedDownloadStatusRendererRequest,
  PersistedDownloadStatusMainResponse,
  DownloadStatusRendererRequest,
  DownloadStatusMainResponse,
  DownloadRendererRequest,
  DownloadMainResponse,
  DownloadMainAsyncResponse,
} from '../../common/ipc/api';

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

const sendDownloadStatus = (status: DownloadMainAsyncResponse) => {
  console.log('sendDownloadStatus: ', status);
};

const requestDownload = async (
  downloadRequest: DownloadRendererRequest
): Promise<void> => {
  console.log('downloadRequest -----', downloadRequest);
  const downloadsPath = app.getPath('downloads');
  const { url, destinationFolder = downloadsPath } = downloadRequest;

  const download = new DownloaderHelper(url, destinationFolder);
  download.on('start', () =>
    sendDownloadStatus({ isDownloading: true, downloadProgress: 0 })
  );
  download.on('end', () =>
    sendDownloadStatus({ isDownloading: true, downloadProgress: 100 })
  );
  // download.on('progress', (a, b, c) => {
  //   console.log('progress----');
  //   console.log('a', a);
  //   console.log('b', b);
  //   console.log('c', c);
  // })
  download.start();
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
  console.log('file', file);
  const path = filePath || app.getPath('downloads');
  console.log('path', path);
  const hasPendingDownload = false;
  const pendingUpdateFileName = '';
  return {
    hasPendingDownload,
    pendingUpdateFileName,
  };
};

const getPersistDownloadStatusChannel: // IpcChannel<Incoming, Outgoing>
MainIpcConversation<
  PersistedDownloadStatusRendererRequest,
  PersistedDownloadStatusMainResponse
> = new MainIpcConversation(PERSISTED_DOWNLOAD_STATUS);

const getDownloadStatusChannel: // IpcChannel<Incoming, Outgoing>
MainIpcConversation<
  DownloadStatusRendererRequest,
  DownloadStatusMainResponse
> = new MainIpcConversation(DOWNLOAD_STATUS);

const requestDownloadChannel: // IpcChannel<Incoming, Outgoing>
MainIpcConversation<
  DownloadRendererRequest,
  DownloadMainResponse
> = new MainIpcConversation(REQUEST_DOWNLOAD);

export default () => {
  getPersistDownloadStatusChannel.onReceive(getPersistDownloadStatus);
  getDownloadStatusChannel.onReceive(getDownloadStatus);
  requestDownloadChannel.onReceive(requestDownload);
};
