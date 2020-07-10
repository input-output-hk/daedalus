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
  RESUME_DOWNLOAD,
} from '../../common/ipc/api';
import {
  DEFAULT_DIRECTORY_NAME,
  TEMPORARY_FILENAME,
} from '../../common/config/downloadManagerConfig';
import { generateFileNameWithTimestamp } from '../../common/utils/files.js';
import { downloadManagerLocalStorage as localStorage } from '../utils/mainLocalStorage';
import type {
  DownloadRendererRequest,
  DownloadMainResponse,
  DownloadLocalDataRendererRequest,
  DownloadLocalDataMainResponse,
  DownloadsLocalDataRendererRequest,
  DownloadsLocalDataMainResponse,
  ResumeDownloadRendererRequest,
  ResumeDownloadMainResponse,
} from '../../common/ipc/api';

localStorage.setAllStopped();

const requestDownload = async (
  downloadRequestPayload: DownloadRendererRequest,
  window: BrowserWindow
): Promise<any> => {
  const {
    fileUrl,
    destinationDirectoryName = DEFAULT_DIRECTORY_NAME,
    // options,
    options: _options,
    id,
    resumeDownload,
  } = downloadRequestPayload;

  const temporaryFilename = resumeDownload
    ? resumeDownload.temporaryFilename
    : generateFileNameWithTimestamp(TEMPORARY_FILENAME);
  const originalFilename = getOriginalFilename(downloadRequestPayload);
  const destinationPath = getPathFromDirectoryName(destinationDirectoryName);
  const options = {
    ..._options,
    fileName: temporaryFilename,
  };
  const downloadId = getIdFromFileName(id || originalFilename);
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

  if (resumeDownload) {
    const { total: downloadSize } = await download.getTotalSize(); // get the total size from the server
    download.__total = downloadSize;
    download.__filePath = `${data.destinationPath}/${data.temporaryFilename}`;
    download.__downloaded = download.__getFilesizeInBytes(download.__filePath);
    download.__isResumable = true;
  }

  let currentDownloadProgress = 0;

  const progressType =
    options.progressIsThrottled === false ? 'progress' : 'progress.throttled';

  download.on('start', eventActions.start);
  download.on('download', eventActions.download);
  download.on(progressType, evt => {
    if (!evt || parseInt(evt.progress, 10) === currentDownloadProgress) return;
    currentDownloadProgress++;
    eventActions.progress(evt);
  });
  download.on('end', eventActions.end);
  download.on('error', eventActions.error);
  if (resumeDownload) download.resume();
  else download.start();
  return download;
};

const getDownloadLocalData = async ({
  fileName,
  id = fileName,
}: DownloadLocalDataRendererRequest): Promise<DownloadLocalDataMainResponse> => {
  if (!id) throw new Error('Requires `id` or `fileName`');
  const downloadId: string = getIdFromFileName(String(id));
  return localStorage.get(downloadId);
};

const getDownloadsLocalData = async (): Promise<DownloadsLocalDataMainResponse> =>
  localStorage.getAll();

const requestResumeDownload = async (
  resumeDownloadRequestPayload: ResumeDownloadRendererRequest,
  window: BrowserWindow
): Promise<any> => {
  const downloadLocalData = await getDownloadLocalData(
    resumeDownloadRequestPayload
  );
  const { temporaryFilename, originalFilename } = downloadLocalData.data || {};
  const { downloadId: id, fileUrl, destinationDirectoryName, options } =
    downloadLocalData.data || {};
  if (!id) throw new Error('Invalid download ID 2');
  const requestDownloadPayload = {
    id,
    fileUrl,
    destinationDirectoryName,
    options,
    resumeDownload: { temporaryFilename, originalFilename },
  };
  return requestDownload(
    {
      ...requestDownloadPayload,
      override: true,
    },
    window
  );
};

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

const requestResumeDownloadChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  ResumeDownloadRendererRequest,
  ResumeDownloadMainResponse
> = new MainIpcChannel(RESUME_DOWNLOAD);

export const downloadManagerChannel = (window: BrowserWindow) => {
  requestDownloadChannel.onRequest(
    (downloadRequestPayload: DownloadRendererRequest) =>
      requestDownload(downloadRequestPayload, window)
  );
  requestResumeDownloadChannel.onRequest(
    (resumeDownloadRequestPayload: ResumeDownloadRendererRequest) =>
      requestResumeDownload(resumeDownloadRequestPayload, window)
  );
  getDownloadLocalDataChannel.onRequest(getDownloadLocalData);
  getDownloadsLocalDataChannel.onRequest(getDownloadsLocalData);
};
