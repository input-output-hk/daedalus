// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import type {
  CompressLogsRequest,
  CompressLogsResponse,
  DownloadLogsRequest,
  DownloadLogsResponse,
  GetLogsRequest,
  GetLogsResponse,
} from '../../../common/ipc/api';
import {
  COMPRESS_LOGS_CHANNEL,
  DOWNLOAD_LOGS_CHANNEL,
  GET_LOGS_CHANNEL,
} from '../../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const getLogsChannel: RendererIpcChannel<
  GetLogsResponse,
  GetLogsRequest
> = new RendererIpcChannel(GET_LOGS_CHANNEL);

export const compressLogsChannel: RendererIpcChannel<
  CompressLogsResponse,
  CompressLogsRequest
> = new RendererIpcChannel(COMPRESS_LOGS_CHANNEL);

export const downloadLogsChannel: RendererIpcChannel<
  DownloadLogsResponse,
  DownloadLogsRequest
> = new RendererIpcChannel(DOWNLOAD_LOGS_CHANNEL);
