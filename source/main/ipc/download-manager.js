// @flow
import { DownloaderHelper } from 'node-downloader-helper';
import { throttle } from 'lodash';
import fs from 'fs';
import type { BrowserWindow } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  getOriginalFilename,
  getPathFromDirectoryName,
} from '../utils/downloadManager';
import {
  // PERSISTED_DOWNLOAD_STATUS,
  // DOWNLOAD_STATUS,
  REQUEST_DOWNLOAD,
} from '../../common/ipc/api';
import {
  DEFAULT_DIRECTORY_NAME,
  TEMPORARY_FILENAME,
  DOWNLOAD_PROGRESS_STATUSES as statusType,
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
import type {
  DownloadInfo,
  DownloadProgressStatuses,
} from '../../common/types/download-manager.types';

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
//     hasPendingDownload,
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
    options,
  } = downloadRequestPayload;
  const destinationPath = getPathFromDirectoryName(destinationDirectoryName);
  const _options = {
    ...options,
    fileName: temporaryFilename,
  };
  const update = (
    progressStatusType: DownloadProgressStatuses,
    downloadInfo: DownloadInfo
  ) => {
    if (progressStatusType === statusType.FINISHED) {
      const temporaryPath = `${destinationPath}/${temporaryFilename}`;
      const newPath = `${destinationPath}/${originalFilename}`;
      fs.renameSync(temporaryPath, newPath);
    }
    requestDownloadChannel.send(
      {
        downloadInfo,
        progressStatusType,
      },
      window.webContents
    );
  };

  const updateThrottle = throttle(update, 1000, {
    leading: true,
    trailing: true,
  });

  const download = new DownloaderHelper(fileUrl, destinationPath, _options);
  download.on('download', update.bind(this, statusType.DOWNLOAD));
  download.on('end', update.bind(this, statusType.FINISHED));
  download.on('timeout', update.bind(this, statusType.TIMEOUT));
  download.on('progress', updateThrottle.bind(this, statusType.PROGRESS));
  download.on('error', update.bind(this, statusType.ERROR));
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
