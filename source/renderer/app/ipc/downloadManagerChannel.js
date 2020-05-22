// @flow
import {
  PERSISTED_DOWNLOAD_STATUS,
  DOWNLOAD_STATUS,
  REQUEST_DOWNLOAD,
} from '../../../common/ipc/api';
import type {
  PersistedDownloadStatusRendererRequest,
  PersistedDownloadStatusMainResponse,
  DownloadStatusRendererRequest,
  DownloadStatusMainResponse,
  DownloadRendererRequest,
  DownloadMainResponse,
} from '../../../common/ipc/api';
// import { RendererIpcConversation as RendererIpcChannel } from './lib/RendererIpcConversation';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getPersistedDownloadStatusChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  PersistedDownloadStatusMainResponse,
  PersistedDownloadStatusRendererRequest
> = new RendererIpcChannel(PERSISTED_DOWNLOAD_STATUS);

export const getDownloadStatusChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  DownloadStatusMainResponse,
  DownloadStatusRendererRequest
> = new RendererIpcChannel(DOWNLOAD_STATUS);

export const requestDownloadChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  DownloadMainResponse,
  DownloadRendererRequest
> = new RendererIpcChannel(REQUEST_DOWNLOAD);
