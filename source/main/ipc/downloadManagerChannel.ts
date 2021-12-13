import { DownloaderHelper } from 'node-downloader-helper';
import fs from 'fs';
import { forEach, omit } from 'lodash';
import type { BrowserWindow } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { logger } from '../utils/logging';
import {
  getOriginalFilename,
  getPathFromDirectoryName,
  getEventActions,
  getIdFromFileName,
  downloads,
} from '../utils/downloadManager';
import {
  REQUEST_DOWNLOAD,
  RESUME_DOWNLOAD,
  DELETE_DOWNLOADED_FILE,
  GET_DOWNLOAD_LOCAL_DATA,
  GET_DOWNLOADS_LOCAL_DATA,
  CLEAR_DOWNLOAD_LOCAL_DATA,
  CHECK_FILE_EXISTS,
} from '../../common/ipc/api';
import {
  DEFAULT_DIRECTORY_NAME,
  TEMPORARY_FILENAME,
  DOWNLOAD_STATES,
} from '../../common/config/downloadManagerConfig';
import { generateFileNameWithTimestamp } from '../../common/utils/files';
import { downloadManagerLocalStorage as localStorage } from '../utils/mainLocalStorage';
import type {
  DownloadRendererRequest,
  DownloadMainResponse,
  ResumeDownloadRendererRequest,
  ResumeDownloadMainResponse,
  DeleteDownloadedFileRendererRequest,
  DeleteDownloadedFileMainResponse,
  DownloadLocalDataRendererRequest,
  DownloadLocalDataMainResponse,
  DownloadsLocalDataRendererRequest,
  DownloadsLocalDataMainResponse,
  ClearDownloadLocalDataRendererRequest,
  ClearDownloadLocalDataMainResponse,
  CheckFileExistsMainResponse,
  CheckFileExistsRendererRequest,
} from '../../common/ipc/api';

localStorage.setAllPaused();

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
  const temporaryFilename = resumeDownload
    ? resumeDownload.temporaryFilename
    : generateFileNameWithTimestamp(TEMPORARY_FILENAME);
  const originalFilename = getOriginalFilename(downloadRequestPayload);
  const destinationPath = getPathFromDirectoryName(destinationDirectoryName);
  const options = { ..._options, fileName: temporaryFilename };
  const downloadId = getIdFromFileName(id || originalFilename);
  const info = {
    downloadId,
    fileUrl,
    destinationPath,
    destinationDirectoryName,
    temporaryFilename,
    originalFilename,
    options,
  };

  if (downloads[downloadId]) {
    logger.info(
      `DownloadManager: Preventing download "${downloadId}" duplicity`,
      {
        downloadId,
      }
    );
    return false;
  }

  const eventActions = await getEventActions(
    info,
    window,
    requestDownloadChannel
  );
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ fileName: string; method?: "GE... Remove this comment to see the full error message
  const download = new DownloaderHelper(fileUrl, destinationPath, options);
  downloads[downloadId] = download;

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

const requestResumeDownload = async (
  resumeDownloadRequestPayload: ResumeDownloadRendererRequest,
  window: BrowserWindow
): Promise<any> => {
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
  if (fs.existsSync(filePath)) {
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
      options: omit(options, 'fileName'),
    };
    await localStorage.unset(id);
  }

  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ override: boolean; id: string;... Remove this comment to see the full error message
  return requestDownload({ ...requestDownloadPayload, override: true }, window);
};

const deleteDownloadedFile = async ({
  id,
}: DeleteDownloadedFileRendererRequest): Promise<
  DeleteDownloadedFileMainResponse
> => {
  const downloadLocalData = await getDownloadLocalData({
    id,
  });
  if (!downloadLocalData) throw new Error('Download data not found');
  const { destinationPath, originalFilename, temporaryFilename } =
    downloadLocalData.info || {};
  const originalFilePath = `${destinationPath}/${originalFilename}`;
  const temporaryFilePath = `${destinationPath}/${temporaryFilename}`;
  if (fs.existsSync(originalFilePath)) fs.unlinkSync(originalFilePath);
  if (fs.existsSync(temporaryFilePath)) fs.unlinkSync(temporaryFilePath);
};

const getDownloadLocalData = async ({
  fileName,
  id = fileName,
}: DownloadLocalDataRendererRequest): Promise<
  DownloadLocalDataMainResponse
> => {
  if (!id) throw new Error('Requires `id` or `fileName`');
  const downloadId: string = getIdFromFileName(String(id));
  return localStorage.get(downloadId);
};

const getDownloadsLocalData = async (): Promise<
  DownloadsLocalDataMainResponse
  // @ts-ignore ts-migrate(2322) FIXME: Type 'unknown' is not assignable to type 'Download... Remove this comment to see the full error message
> => localStorage.getAll();

const clearDownloadLocalData = async ({
  fileName,
  id = fileName,
}: ClearDownloadLocalDataRendererRequest): Promise<
  ClearDownloadLocalDataMainResponse
> => {
  if (!id) throw new Error('Requires `id` or `fileName`');
  const downloadId: string = getIdFromFileName(String(id));
  return localStorage.unset(downloadId);
};

const checkFileExists = async ({
  id,
}: CheckFileExistsRendererRequest): Promise<CheckFileExistsMainResponse> => {
  const downloadLocalData = await getDownloadLocalData({
    id,
  });
  if (!downloadLocalData) throw new Error('Download data not found');
  const { destinationPath, originalFilename, temporaryFilename } =
    downloadLocalData.info || {};
  const { state } = downloadLocalData.data || {};
  const fileName =
    state === DOWNLOAD_STATES.FINISHED ? originalFilename : temporaryFilename;
  const filePath = `${destinationPath}/${fileName}`;
  return fs.existsSync(filePath);
};

const requestDownloadChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  DownloadRendererRequest,
  DownloadMainResponse
> = new MainIpcChannel(REQUEST_DOWNLOAD);
const requestResumeDownloadChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  ResumeDownloadRendererRequest,
  ResumeDownloadMainResponse
> = new MainIpcChannel(RESUME_DOWNLOAD);
const deleteDownloadedFileChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  DeleteDownloadedFileRendererRequest,
  DeleteDownloadedFileMainResponse
> = new MainIpcChannel(DELETE_DOWNLOADED_FILE);
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
const clearDownloadLocalDataChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  ClearDownloadLocalDataRendererRequest,
  ClearDownloadLocalDataMainResponse
> = new MainIpcChannel(CLEAR_DOWNLOAD_LOCAL_DATA);
const checkFileExistsChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  CheckFileExistsRendererRequest,
  CheckFileExistsMainResponse
> = new MainIpcChannel(CHECK_FILE_EXISTS);
export const downloadManagerChannel = (window: BrowserWindow) => {
  requestDownloadChannel.onRequest(
    (downloadRequestPayload: DownloadRendererRequest) =>
      requestDownload(downloadRequestPayload, window)
  );
  requestResumeDownloadChannel.onRequest(
    (resumeDownloadRequestPayload: ResumeDownloadRendererRequest) =>
      requestResumeDownload(resumeDownloadRequestPayload, window)
  );
  deleteDownloadedFileChannel.onRequest(deleteDownloadedFile);
  getDownloadLocalDataChannel.onRequest(getDownloadLocalData);
  getDownloadsLocalDataChannel.onRequest(getDownloadsLocalData);
  clearDownloadLocalDataChannel.onRequest(clearDownloadLocalData);
  checkFileExistsChannel.onRequest(checkFileExists);
};
export const pauseActiveDownloads = () => {
  forEach(downloads, (download, downloadId) => {
    try {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'state' does not exist on type 'never'.
      if (download && download.state === DOWNLOAD_STATES.DOWNLOADING)
        // @ts-ignore ts-migrate(2339) FIXME: Property 'pause' does not exist on type 'never'.
        download.pause();
      logger.info(
        `DownloadManager:PauseDownloads download "${downloadId}" was paused`,
        {
          downloadId,
        }
      );
    } catch (error) {
      logger.error(
        `DownloadManager:PauseDownloads download "${downloadId}" could not be paused`,
        {
          error,
        }
      );
    }
  });
};
