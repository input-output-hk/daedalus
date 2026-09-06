import {
  REQUEST_DOWNLOAD,
  RESUME_DOWNLOAD,
  DELETE_DOWNLOADED_FILE,
  GET_DOWNLOAD_LOCAL_DATA,
  GET_DOWNLOADS_LOCAL_DATA,
  CLEAR_DOWNLOAD_LOCAL_DATA,
  CHECK_FILE_EXISTS,
} from '../../../common/ipc/api';
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
  CheckFileExistsRendererRequest,
  CheckFileExistsMainResponse,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const requestDownloadChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  DownloadMainResponse,
  DownloadRendererRequest
> = new RendererIpcChannel(REQUEST_DOWNLOAD);
export const requestResumeDownloadChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  ResumeDownloadMainResponse,
  ResumeDownloadRendererRequest
> = new RendererIpcChannel(RESUME_DOWNLOAD);
export const deleteDownloadedFile: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  DeleteDownloadedFileMainResponse,
  DeleteDownloadedFileRendererRequest
> = new RendererIpcChannel(DELETE_DOWNLOADED_FILE);
export const getDownloadLocalDataChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  DownloadLocalDataMainResponse,
  DownloadLocalDataRendererRequest
> = new RendererIpcChannel(GET_DOWNLOAD_LOCAL_DATA);
export const getDownloadsLocalDataChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  DownloadsLocalDataMainResponse,
  DownloadsLocalDataRendererRequest
> = new RendererIpcChannel(GET_DOWNLOADS_LOCAL_DATA);
export const clearDownloadLocalDataChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  ClearDownloadLocalDataMainResponse,
  ClearDownloadLocalDataRendererRequest
> = new RendererIpcChannel(CLEAR_DOWNLOAD_LOCAL_DATA);
export const checkFileExistsChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  CheckFileExistsMainResponse,
  CheckFileExistsRendererRequest
> = new RendererIpcChannel(CHECK_FILE_EXISTS);
