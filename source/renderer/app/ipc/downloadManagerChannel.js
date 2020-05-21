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
import { RendererIpcConversation } from './lib/RendererIpcConversation';

export const getPersistedDownloadStatusChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcConversation<
  PersistedDownloadStatusMainResponse,
  PersistedDownloadStatusRendererRequest
> = new RendererIpcConversation(PERSISTED_DOWNLOAD_STATUS);

export const getDownloadStatusChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcConversation<
  DownloadStatusMainResponse,
  DownloadStatusRendererRequest
> = new RendererIpcConversation(DOWNLOAD_STATUS);

export const requestDownloadChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcConversation<
  DownloadMainResponse,
  DownloadRendererRequest
> = new RendererIpcConversation(REQUEST_DOWNLOAD);
