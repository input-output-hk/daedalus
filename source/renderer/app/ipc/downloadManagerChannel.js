// @flow
import {
  GET_DOWNLOAD_LOCAL_DATA,
  REQUEST_DOWNLOAD,
} from '../../../common/ipc/api';
import type {
  DownloadLocalDataRendererRequest,
  DownloadLocalDataMainResponse,
  DownloadRendererRequest,
  DownloadMainResponse,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getDownloadLocalDataChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  DownloadLocalDataMainResponse,
  DownloadLocalDataRendererRequest
> = new RendererIpcChannel(GET_DOWNLOAD_LOCAL_DATA);

export const requestDownloadChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  DownloadMainResponse,
  DownloadRendererRequest
> = new RendererIpcChannel(REQUEST_DOWNLOAD);
