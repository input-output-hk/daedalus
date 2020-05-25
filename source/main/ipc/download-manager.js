// @flow
import { DownloaderHelper } from 'node-downloader-helper';
import { throttle } from 'lodash';
import { app } from 'electron';
import type { BrowserWindow } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  // PERSISTED_DOWNLOAD_STATUS,
  // DOWNLOAD_STATUS,
  REQUEST_DOWNLOAD,
} from '../../common/ipc/api';
import {
  ALLOWED_DOWNLOAD_DIRECTORIES,
  DOWNLOAD_PROGRESS_STATUSES as statusType,
} from '../../common/config/download-manager';
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
  DownloadInfo,
  DownloadProgressStatuses,
} from '../../common/types/download-manager.types';

const getPathFromDirectoryName = (dir: AllowedDownloadDirectories) => {
  switch (dir) {
    case ALLOWED_DOWNLOAD_DIRECTORIES.DESKTOP:
      return app.getPath('desktop');
    default:
      return app.getPath('downloads');
  }
};

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
//   const pathName = filePath || ALLOWED_DOWNLOAD_DIRECTORIES.DOWNLOADS;
//   // const path = getPathFromDirectoryName(pathName);
//   const hasPendingDownload = false;
//   const pendingUpdateFileName = '';
//   return {
//     hasPendingDownload,
//     pendingUpdateFileName,
//   };
// };

const requestDownload = async (
  downloadRequest: DownloadRendererRequest,
  window: BrowserWindow
): Promise<any> => {
  // const defaultDirectoryName = ALLOWED_DOWNLOAD_DIRECTORIES.DOWNLOADS;
  const defaultDirectoryName = ALLOWED_DOWNLOAD_DIRECTORIES.DESKTOP;
  const {
    fileUrl,
    destinationDirectoryName = defaultDirectoryName,
    options,
  } = downloadRequest;
  const destinationPath = getPathFromDirectoryName(destinationDirectoryName);

  const update = (
    progressStatusType: DownloadProgressStatuses,
    downloadInfo: DownloadInfo
  ) =>
    // crdownload
    // Unconfirmed 242990.crdownload
    // https://update-cardano-mainnet.iohk.io/daedalus-1.1.0-mainnet-12849.pkg
    requestDownloadChannel.send(
      {
        ...downloadInfo,
        progressStatusType,
      },
      window.webContents
    );

  const updateThrottle = throttle(update, 1000, {
    leading: true,
    trailing: true,
  });

  const download = new DownloaderHelper(fileUrl, destinationPath, options);
  download.on('download', update.bind(this, statusType.DOWNLOAD));
  download.on('end', update.bind(this, statusType.END));
  download.on('error', update.bind(this, statusType.ERROR));
  download.on('timeout', update.bind(this, statusType.TIMEOUT));
  download.on('progress', updateThrottle.bind(this, statusType.PROGRESS));
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
  requestDownloadChannel.onRequest((downloadRequest: DownloadRendererRequest) =>
    requestDownload(downloadRequest, window)
  );
};
