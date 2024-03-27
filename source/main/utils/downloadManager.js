'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.getEventActions = exports.getOriginalFilename = exports.getPathFromDirectoryName = exports.getIdFromFileName = exports.downloads = void 0;
const electron_1 = require('electron');
const fs_1 = __importDefault(require('fs'));
const logging_1 = require('./logging');
const downloadManagerConfig_1 = require('../../common/config/downloadManagerConfig');
const files_1 = require('../../common/utils/files');
const mainLocalStorage_1 = require('./mainLocalStorage');
const config_1 = require('../config');
exports.downloads = {};
const getIdFromFileName = (fileName) => fileName.replace(/\./g, '-');
exports.getIdFromFileName = getIdFromFileName;
const getPathFromDirectoryName = (directoryName) => {
  const downloadsDirectory = `${config_1.stateDirectoryPath}/Downloads`;
  switch (directoryName) {
    case downloadManagerConfig_1.ALLOWED_DOWNLOAD_DIRECTORIES.DESKTOP:
      return electron_1.app.getPath('desktop');
    case downloadManagerConfig_1.ALLOWED_DOWNLOAD_DIRECTORIES.DOWNLOADS:
      return electron_1.app.getPath('downloads');
    default:
      if (!fs_1.default.existsSync(downloadsDirectory))
        fs_1.default.mkdirSync(downloadsDirectory);
      return downloadsDirectory;
  }
};
exports.getPathFromDirectoryName = getPathFromDirectoryName;
const getOriginalFilename = ({ fileUrl, options, resumeDownload }) => {
  let name = '';
  if (resumeDownload) name = resumeDownload.originalFilename;
  else if (options && typeof options.fileName === 'string')
    name = options.fileName;
  else name = (0, files_1.extractFileNameFromPath)(fileUrl);
  return name;
};
exports.getOriginalFilename = getOriginalFilename;
const getPath = (info) => {
  const { destinationPath, temporaryFilename, originalFilename } = info;
  const temporaryPath = `${destinationPath}/${temporaryFilename}`;
  const newPath = `${destinationPath}/${originalFilename}`;
  return {
    temporaryPath,
    newPath,
  };
};
const getEventActions = async (info, window, requestDownloadChannel) => {
  const { downloadId } = info;
  await mainLocalStorage_1.downloadManagerLocalStorage.setInfo(
    info,
    downloadId
  );
  let serverFileSize;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'TimeoutID'.
  let checkNoEndEvent;
  const startEvent = async () => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('DownloadManager:startEvent.');
    const eventType = downloadManagerConfig_1.DOWNLOAD_EVENT_TYPES.START;
    const data = downloadManagerConfig_1.DOWNLOAD_DATA_DEFAULT;
    requestDownloadChannel.send(
      {
        eventType,
        info,
        data,
      },
      window.webContents
    );
  };
  const downloadEvent = async ({ totalSize, downloadedSize: diskFileSize }) => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('DownloadManager:downloadEvent.');
    serverFileSize = totalSize;
    const rawData = {
      ...{
        serverFileSize,
        diskFileSize,
        remainingSize: serverFileSize,
      },
      state: downloadManagerConfig_1.DOWNLOAD_STATES.DOWNLOADING,
    };
    const data = await mainLocalStorage_1.downloadManagerLocalStorage.setData(
      rawData,
      downloadId
    );
    requestDownloadChannel.send(
      {
        eventType: downloadManagerConfig_1.DOWNLOAD_EVENT_TYPES.DOWNLOAD,
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
  }) => {
    const rawData = {
      ...{
        remainingSize: total - downloadSize,
        serverFileSize,
        downloadSize,
        progress,
        speed,
      },
      state: downloadManagerConfig_1.DOWNLOAD_STATES.DOWNLOADING,
    };
    const formattedData = await mainLocalStorage_1.downloadManagerLocalStorage.setData(
      rawData,
      downloadId
    );
    requestDownloadChannel.send(
      {
        eventType: downloadManagerConfig_1.DOWNLOAD_EVENT_TYPES.PROGRESS,
        info,
        data: formattedData,
      },
      window.webContents
    );
    if (progress === 100) {
      // Checks if the file was delete while the download was in progress
      checkNoEndEvent = setTimeout(() => {
        const { temporaryPath, newPath } = getPath(info);
        if (
          !fs_1.default.existsSync(temporaryPath) ||
          !fs_1.default.existsSync(newPath)
        ) {
          errorEvent({
            message: 'The download file was manually deleted',
          });
        }
      }, downloadManagerConfig_1.ERROR_TIME_AFTER_NO_END_EVENT);
    }
  };
  const endEvent = async ({
    totalSize: downloadSize,
    onDiskSize: diskFileSize,
    incomplete,
  }) => {
    clearTimeout(checkNoEndEvent);
    delete exports.downloads[downloadId];
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('DownloadManager:endEvent.');
    const rawData = {
      ...{
        downloadSize,
        diskFileSize,
        incomplete,
      },
      state: downloadManagerConfig_1.DOWNLOAD_STATES.FINISHED,
    };
    const formattedData = await mainLocalStorage_1.downloadManagerLocalStorage.setData(
      rawData,
      downloadId
    );
    const { temporaryPath, newPath } = getPath(info);
    fs_1.default.renameSync(temporaryPath, newPath);
    requestDownloadChannel.send(
      {
        eventType: downloadManagerConfig_1.DOWNLOAD_EVENT_TYPES.END,
        info,
        data: formattedData,
      },
      window.webContents
    );
    const { persistLocalData } = info.options;
    if (!persistLocalData)
      await mainLocalStorage_1.downloadManagerLocalStorage.unset(downloadId);
  };
  const pauseEvent = async () => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('DownloadManager:pauseEvent.');
    const newState = {
      state: downloadManagerConfig_1.DOWNLOAD_STATES.PAUSED,
    };
    const formattedData = await mainLocalStorage_1.downloadManagerLocalStorage.setData(
      newState,
      downloadId
    );
    requestDownloadChannel.send(
      {
        eventType: downloadManagerConfig_1.DOWNLOAD_EVENT_TYPES.PAUSE,
        info,
        data: formattedData,
      },
      window.webContents
    );
  };
  const errorEvent = async ({ message }) => {
    logging_1.logger.error('DownloadManager:errorEvent', {
      error: message,
    });
    const rawData = {
      ...{
        message,
      },
      state: downloadManagerConfig_1.DOWNLOAD_STATES.FAILED,
    };
    const formattedData = await mainLocalStorage_1.downloadManagerLocalStorage.setData(
      rawData,
      downloadId
    );
    requestDownloadChannel.send(
      {
        eventType: downloadManagerConfig_1.DOWNLOAD_EVENT_TYPES.ERROR,
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
exports.getEventActions = getEventActions;
//# sourceMappingURL=downloadManager.js.map
