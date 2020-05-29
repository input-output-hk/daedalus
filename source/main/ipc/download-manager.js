// @flow
import { DownloaderHelper } from 'node-downloader-helper';
import type { BrowserWindow } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  getOriginalFilename,
  getPathFromDirectoryName,
  getEventActions,
  getIdFromFileName,
} from '../utils/downloadManager';
import {
  GET_DOWNLOAD_LOCAL_DATA,
  GET_DOWNLOADS_LOCAL_DATA,
  REQUEST_DOWNLOAD,
} from '../../common/ipc/api';
import {
  DEFAULT_DIRECTORY_NAME,
  TEMPORARY_FILENAME,
} from '../../common/config/download-manager';
import { generateFileNameWithTimestamp } from '../../common/utils/files.js';
import { downloadManagerLocalStorage as localStorage } from '../utils/mainLocalStorage';
import type {
  DownloadRendererRequest,
  DownloadMainResponse,
  DownloadLocalDataRendererRequest,
  DownloadLocalDataMainResponse,
  DownloadsLocalDataRendererRequest,
  DownloadsLocalDataMainResponse,
} from '../../common/ipc/api';

localStorage.setAllStopped();

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
  const downloadId = getIdFromFileName(originalFilename);
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

const getDownloadLocalData = async ({
  fileName,
}: DownloadLocalDataRendererRequest): Promise<DownloadLocalDataMainResponse> => {
  const downloadId = getIdFromFileName(fileName);
  return localStorage.get(downloadId);
};

const getDownloadsLocalData = async (): Promise<DownloadsLocalDataMainResponse> =>
  localStorage.getAll();

const requestDownloadChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  DownloadRendererRequest,
  DownloadMainResponse
> = new MainIpcChannel(REQUEST_DOWNLOAD);

const getDownloadLocalDataChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  DownloadLocalDataRendererRequest,
  DownloadLocalDataMainResponse
> = new MainIpcChannel(GET_DOWNLOAD_LOCAL_DATA);

const getDownloadsLocalDataChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  DownloadsLocalDataRendererRequest,
  DownloadsLocalDataMainResponse
> = new MainIpcChannel(GET_DOWNLOADS_LOCAL_DATA);

export default (window: BrowserWindow) => {
  requestDownloadChannel.onRequest(
    (downloadRequestPayload: DownloadRendererRequest) =>
      requestDownload(downloadRequestPayload, window)
  );
  getDownloadLocalDataChannel.onRequest(getDownloadLocalData);
  getDownloadsLocalDataChannel.onRequest(getDownloadsLocalData);
};
