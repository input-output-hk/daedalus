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
    options: _options,
    id,
    resumeDownload,
  } = downloadRequestPayload;
  console.log('resumeDownload', resumeDownload);
  const temporaryFilename = resumeDownload
    ? resumeDownload.data.temporaryFilename
    : generateFileNameWithTimestamp(TEMPORARY_FILENAME);
  console.log('temporaryFilename', temporaryFilename);
  const originalFilename = getOriginalFilename(downloadRequestPayload);
  const destinationPath = getPathFromDirectoryName(destinationDirectoryName);
  const options = {
    ..._options,
    fileName: temporaryFilename,
  };
  const downloadId = getIdFromFileName(id || originalFilename);
  let data = {
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
    const { data: resumeDownloadData = {}, progress = {} } = resumeDownload;
    data = resumeDownloadData;
    const { downloadSize, progress: progressPerc } = progress;
    download.__total = downloadSize;
    download.__filePath = data.destinationPath + data.temporaryFilename;
    download.__progress = progressPerc;
    // download.__isResumed = true;
    download.__isResumable = true;
  }

  download.on('start', eventActions.start);
  download.on('download', eventActions.download);
  download.on('progress.throttled', (...p) => {
    console.log('progress:', p);
    eventActions.progress(...p);
  });
  download.on('end', eventActions.end);
  download.on('error', eventActions.error);
  if (resumeDownload) download.resume();
  else download.start();
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
  const { downloadId: id, fileUrl, destinationDirectoryName, options } =
    downloadLocalData.data || {};
  if (!id) throw new Error('Invalid download ID');
  const requestDownloadPayload = {
    id,
    fileUrl,
    destinationDirectoryName,
    options,
    resumeDownload: downloadLocalData,
  };
  requestDownload(
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

export default (window: BrowserWindow) => {
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
