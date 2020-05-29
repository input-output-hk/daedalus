// @flow
import { DownloaderHelper } from 'node-downloader-helper';
import type { BrowserWindow } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  getOriginalFilename,
  getPathFromDirectoryName,
  getEventActions,
  getIdFromFilename,
} from '../utils/downloadManager';
import {
  // PERSISTED_DOWNLOAD_STATUS,
  // DOWNLOAD_STATUS,
  REQUEST_DOWNLOAD,
} from '../../common/ipc/api';
import {
  DEFAULT_DIRECTORY_NAME,
  TEMPORARY_FILENAME,
} from '../../common/config/download-manager';
import { generateFileNameWithTimestamp } from '../../common/utils/files.js';
import type {
  // PersistedDownloadStatusRendererRequest,
  // PersistedDownloadStatusMainResponse,
  // DownloadStatusRendererRequest,
  // DownloadStatusMainResponse,
  DownloadRendererRequest,
  DownloadMainResponse,
} from '../../common/ipc/api';

// const getPersistDownloadStatus = async ({
//   file,
// }: PersistedDownloadStatusRendererRequest): Promise<PersistedDownloadStatusMainResponse> => {
//   const {
//     hasPendingDownload,
//     pendingUpdateFileName,
//   } = await checkhasPendingDownload({ file });

//   let downloadProgress;

//   if (hasPendingDownload) {
//     downloadProgress = await checkdownloadProgress();
//   }

//   downloadProgress = await checkdownloadProgress();
//   return {
//     hasPendingDownload,
//     pendingUpdateFileName,
//     downloadProgress,
//   };
// };

// const getDownloadStatus = async (): Promise<DownloadStatusMainResponse> => {
//   const isDownloading = await checkisDownloading();
//   const downloadProgress = await checkdownloadProgress();
//   return {
//     isDownloading,
//     downloadProgress,
//   };
// };

// const checkisDownloading = async (): Promise<boolean> => false;

// const checkdownloadProgress = async (): Promise<number> => 0;

// const checkhasPendingDownload = async ({
//   file,
// }: PersistedDownloadStatusRendererRequest): Promise<PersistedDownloadStatusMainResponse> => {
//   const {
//     // fileNamePattern,
//     // fileExtentionPattern,
//     filePath,
//     // fileName,
//     // fileExtention,
//   } = file;
//   const pathName = filePath || DEFAULT_DIRECTORY_NAME;
//   // const path = getPathFromDirectoryName(pathName);
//   const hasPendingDownload = false;
//   const pendingUpdateFileName = '';
//   return {
//     hasPendingDownload,destinationDirectoryName
//     pendingUpdateFileName,
//   };
// };

const requestDownload = async (
  downloadRequestPayload: DownloadRendererRequest,
  window: BrowserWindow
): Promise<any> => {
  const temporaryFilename = generateFileNameWithTimestamp(TEMPORARY_FILENAME);
  const originalFilename = getOriginalFilename(downloadRequestPayload);
  const {
    fileUrl,
    destinationDirectoryName = DEFAULT_DIRECTORY_NAME,
    options: _options,
  } = downloadRequestPayload;
  const destinationPath = getPathFromDirectoryName(destinationDirectoryName);
  const options = {
    ..._options,
    fileName: temporaryFilename,
  };
  const downloadId = getIdFromFilename(originalFilename);
  const data = {
    downloadId,
    fileUrl,
    destinationPath,
    destinationDirectoryName,
    temporaryFilename,
    originalFilename,
    options,
  };
  const eventActions = await getEventActions(
    data,
    window,
    requestDownloadChannel
  );

  const download = new DownloaderHelper(fileUrl, destinationPath, options);
  download.on('start', eventActions.start);
  download.on('download', eventActions.download);
  download.on('progress.throttled', eventActions.progress);
  download.on('end', eventActions.end);
  download.on('error', eventActions.error);
  download.start();
};

// const getPersistDownloadStatusChannel: // IpcChannel<Incoming, Outgoing>
// MainIpcChannel<
//   PersistedDownloadStatusRendererRequest,
//   PersistedDownloadStatusMainResponse
// > = new MainIpcChannel(PERSISTED_DOWNLOAD_STATUS);

// const getDownloadStatusChannel: // IpcChannel<Incoming, Outgoing>
// MainIpcChannel<
//   DownloadStatusRendererRequest,
//   DownloadStatusMainResponse
// > = new MainIpcChannel(DOWNLOAD_STATUS);

const requestDownloadChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  DownloadRendererRequest,
  DownloadMainResponse
> = new MainIpcChannel(REQUEST_DOWNLOAD);

export default (window: BrowserWindow) => {
  // getPersistDownloadStatusChannel.onRequest(getPersistDownloadStatus);
  // getDownloadStatusChannel.onRequest(getDownloadStatus);
  requestDownloadChannel.onRequest(
    (downloadRequestPayload: DownloadRendererRequest) =>
      requestDownload(downloadRequestPayload, window)
  );
};
