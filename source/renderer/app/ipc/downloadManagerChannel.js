// @flow
import {
  REQUEST_DOWNLOAD,
  GET_DOWNLOAD_LOCAL_DATA,
  GET_DOWNLOADS_LOCAL_DATA,
  RESUME_DOWNLOAD,
} from '../../../common/ipc/api';
import type {
  DownloadRendererRequest,
  DownloadMainResponse,
  DownloadLocalDataRendererRequest,
  DownloadLocalDataMainResponse,
  DownloadsLocalDataRendererRequest,
  DownloadsLocalDataMainResponse,
  ResumeDownloadRendererRequest,
  ResumeDownloadMainResponse,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const requestDownloadChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  DownloadMainResponse,
  DownloadRendererRequest
> = new RendererIpcChannel(REQUEST_DOWNLOAD);

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

export const requestResumeDownloadChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  ResumeDownloadMainResponse,
  ResumeDownloadRendererRequest
> = new RendererIpcChannel(RESUME_DOWNLOAD);
