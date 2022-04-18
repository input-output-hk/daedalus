import { RendererIpcChannel } from './lib/RendererIpcChannel';
import type {
  CompressLogsRendererRequest,
  CompressLogsMainResponse,
  DownloadLogsRendererRequest,
  DownloadLogsMainResponse,
  GetLogsRendererRequest,
  GetLogsMainResponse,
} from '../../../common/ipc/api';
import {
  COMPRESS_LOGS_CHANNEL,
  DOWNLOAD_LOGS_CHANNEL,
  GET_LOGS_CHANNEL,
} from '../../../common/ipc/api';
// IpcChannel<Incoming, Outgoing>
export const getLogsChannel: RendererIpcChannel<
  GetLogsMainResponse,
  GetLogsRendererRequest
> = new RendererIpcChannel(GET_LOGS_CHANNEL);
export const compressLogsChannel: RendererIpcChannel<
  CompressLogsMainResponse,
  CompressLogsRendererRequest
> = new RendererIpcChannel(COMPRESS_LOGS_CHANNEL);
export const downloadLogsChannel: RendererIpcChannel<
  DownloadLogsMainResponse,
  DownloadLogsRendererRequest
> = new RendererIpcChannel(DOWNLOAD_LOGS_CHANNEL);
