'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.pauseActiveDownloads = exports.downloadManagerChannel = void 0;
const node_downloader_helper_1 = require('node-downloader-helper');
const fs_1 = __importDefault(require('fs'));
const lodash_1 = require('lodash');
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const logging_1 = require('../utils/logging');
const downloadManager_1 = require('../utils/downloadManager');
const api_1 = require('../../common/ipc/api');
const downloadManagerConfig_1 = require('../../common/config/downloadManagerConfig');
const files_1 = require('../../common/utils/files');
const mainLocalStorage_1 = require('../utils/mainLocalStorage');
mainLocalStorage_1.downloadManagerLocalStorage.setAllPaused();
const requestDownload = async (downloadRequestPayload, window) => {
  const {
    fileUrl,
    destinationDirectoryName = downloadManagerConfig_1.DEFAULT_DIRECTORY_NAME,
    options: _options,
    id,
    resumeDownload,
  } = downloadRequestPayload;
  const temporaryFilename = resumeDownload
    ? resumeDownload.temporaryFilename
    : (0, files_1.generateFileNameWithTimestamp)(
        downloadManagerConfig_1.TEMPORARY_FILENAME
      );
  const originalFilename = (0, downloadManager_1.getOriginalFilename)(
    downloadRequestPayload
  );
  const destinationPath = (0, downloadManager_1.getPathFromDirectoryName)(
    destinationDirectoryName
  );
  const options = { ..._options, fileName: temporaryFilename };
  const downloadId = (0, downloadManager_1.getIdFromFileName)(
    id || originalFilename
  );
  const info = {
    downloadId,
    fileUrl,
    destinationPath,
    destinationDirectoryName,
    temporaryFilename,
    originalFilename,
    options,
  };
  if (downloadManager_1.downloads[downloadId]) {
    logging_1.logger.info(
      `DownloadManager: Preventing download "${downloadId}" duplicity`,
      {
        downloadId,
      }
    );
    return false;
  }
  const eventActions = await (0, downloadManager_1.getEventActions)(
    info,
    window,
    requestDownloadChannel
  );
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ fileName: string; method?: "GE... Remove this comment to see the full error message
  const download = new node_downloader_helper_1.DownloaderHelper(
    fileUrl,
    destinationPath,
    options
  );
  downloadManager_1.downloads[downloadId] = download;
  if (resumeDownload) {
    const { total: downloadSize } = await download.getTotalSize(); // get the total size from the server
    // @ts-ignore ts-migrate(2339) FIXME: Property '__total' does not exist on type 'Downloa... Remove this comment to see the full error message
    download.__total = downloadSize;
    // @ts-ignore ts-migrate(2339) FIXME: Property '__filePath' does not exist on type 'Down... Remove this comment to see the full error message
    download.__filePath = `${info.destinationPath}/${info.temporaryFilename}`;
    // @ts-ignore ts-migrate(2339) FIXME: Property '__downloaded' does not exist on type 'Do... Remove this comment to see the full error message
    download.__downloaded = download.__getFilesizeInBytes(download.__filePath);
    // @ts-ignore ts-migrate(2551) FIXME: Property '__isResumable' does not exist on type 'D... Remove this comment to see the full error message
    download.__isResumable = true;
  }
  let currentDownloadData = 0;
  const progressType =
    options.progressIsThrottled === false ? 'progress' : 'progress.throttled';
  download.on('start', eventActions.start);
  download.on('download', eventActions.download);
  download.on(progressType, (evt) => {
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
    if (!evt || parseInt(evt.progress, 10) === currentDownloadData) return;
    currentDownloadData++;
    eventActions.progress(evt);
  });
  download.on('pause', eventActions.pause);
  download.on('end', eventActions.end);
  download.on('error', eventActions.error);
  if (resumeDownload) {
    download.resume();
  } else {
    download.start();
  }
  return download;
};
const requestResumeDownload = async (resumeDownloadRequestPayload, window) => {
  const downloadLocalData = await getDownloadLocalData(
    resumeDownloadRequestPayload
  );
  const {
    temporaryFilename,
    originalFilename,
    downloadId: id,
    fileUrl,
    destinationDirectoryName,
    destinationPath,
    options,
  } = downloadLocalData.info || {};
  if (!id) throw new Error('Invalid download ID');
  const filePath = `${destinationPath}/${temporaryFilename}`;
  let requestDownloadPayload = {
    id,
    fileUrl,
    destinationDirectoryName,
    options,
  };
  // Check if the file to be resumed still exists
  if (fs_1.default.existsSync(filePath)) {
    requestDownloadPayload = {
      ...requestDownloadPayload,
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ resumeDownload: { temporaryFilename: strin... Remove this comment to see the full error message
      resumeDownload: {
        temporaryFilename,
        originalFilename,
      },
    };
  } else {
    // Otherwise:
    // * New download request
    // * The previous download data is removed
    // * `fileName` is removed from options
    requestDownloadPayload = {
      ...requestDownloadPayload,
      options: (0, lodash_1.omit)(options, 'fileName'),
    };
    await mainLocalStorage_1.downloadManagerLocalStorage.unset(id);
  }
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ override: boolean; id: string;... Remove this comment to see the full error message
  return requestDownload({ ...requestDownloadPayload, override: true }, window);
};
const deleteDownloadedFile = async ({ id }) => {
  const downloadLocalData = await getDownloadLocalData({
    id,
  });
  if (!downloadLocalData) throw new Error('Download data not found');
  const { destinationPath, originalFilename, temporaryFilename } =
    downloadLocalData.info || {};
  const originalFilePath = `${destinationPath}/${originalFilename}`;
  const temporaryFilePath = `${destinationPath}/${temporaryFilename}`;
  if (fs_1.default.existsSync(originalFilePath))
    fs_1.default.unlinkSync(originalFilePath);
  if (fs_1.default.existsSync(temporaryFilePath))
    fs_1.default.unlinkSync(temporaryFilePath);
};
const getDownloadLocalData = async ({ fileName, id = fileName }) => {
  if (!id) throw new Error('Requires `id` or `fileName`');
  const downloadId = (0, downloadManager_1.getIdFromFileName)(String(id));
  return mainLocalStorage_1.downloadManagerLocalStorage.get(downloadId);
};
const getDownloadsLocalData = async () => {
  return mainLocalStorage_1.downloadManagerLocalStorage.getAll();
};
const clearDownloadLocalData = async ({ fileName, id = fileName }) => {
  if (!id) throw new Error('Requires `id` or `fileName`');
  const downloadId = (0, downloadManager_1.getIdFromFileName)(String(id));
  return mainLocalStorage_1.downloadManagerLocalStorage.unset(downloadId);
};
const checkFileExists = async ({ id }) => {
  const downloadLocalData = await getDownloadLocalData({
    id,
  });
  if (!downloadLocalData) throw new Error('Download data not found');
  const { destinationPath, originalFilename, temporaryFilename } =
    downloadLocalData.info || {};
  const { state } = downloadLocalData.data || {};
  const fileName =
    state === downloadManagerConfig_1.DOWNLOAD_STATES.FINISHED
      ? originalFilename
      : temporaryFilename;
  const filePath = `${destinationPath}/${fileName}`;
  return fs_1.default.existsSync(filePath);
};
const requestDownloadChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.REQUEST_DOWNLOAD
);
const requestResumeDownloadChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.RESUME_DOWNLOAD
);
const deleteDownloadedFileChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.DELETE_DOWNLOADED_FILE
);
const getDownloadLocalDataChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.GET_DOWNLOAD_LOCAL_DATA
);
const getDownloadsLocalDataChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.GET_DOWNLOADS_LOCAL_DATA
);
const clearDownloadLocalDataChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.CLEAR_DOWNLOAD_LOCAL_DATA
);
const checkFileExistsChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.CHECK_FILE_EXISTS
);
const downloadManagerChannel = (window) => {
  requestDownloadChannel.onRequest((downloadRequestPayload) =>
    requestDownload(downloadRequestPayload, window)
  );
  requestResumeDownloadChannel.onRequest((resumeDownloadRequestPayload) =>
    requestResumeDownload(resumeDownloadRequestPayload, window)
  );
  deleteDownloadedFileChannel.onRequest(deleteDownloadedFile);
  getDownloadLocalDataChannel.onRequest(getDownloadLocalData);
  getDownloadsLocalDataChannel.onRequest(getDownloadsLocalData);
  clearDownloadLocalDataChannel.onRequest(clearDownloadLocalData);
  checkFileExistsChannel.onRequest(checkFileExists);
};
exports.downloadManagerChannel = downloadManagerChannel;
const pauseActiveDownloads = () => {
  (0, lodash_1.forEach)(downloadManager_1.downloads, (download, downloadId) => {
    try {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'state' does not exist on type 'never'.
      if (
        download &&
        download.state === downloadManagerConfig_1.DOWNLOAD_STATES.DOWNLOADING
      )
        // @ts-ignore ts-migrate(2339) FIXME: Property 'pause' does not exist on type 'never'.
        download.pause();
      logging_1.logger.info(
        `DownloadManager:PauseDownloads download "${downloadId}" was paused`,
        {
          downloadId,
        }
      );
    } catch (error) {
      logging_1.logger.error(
        `DownloadManager:PauseDownloads download "${downloadId}" could not be paused`,
        {
          error,
        }
      );
    }
  });
};
exports.pauseActiveDownloads = pauseActiveDownloads;
//# sourceMappingURL=downloadManagerChannel.js.map
