import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  GET_CACHED_BACKEND_STATUS_CHANNEL,
} from '../../common/ipc/api';
import type {
  GetCachedBackendStatusRendererRequest,
  GetCachedBackendStatusMainResponse,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>
export const getCachedBackendStatusChannel: MainIpcChannel<
  GetCachedBackendStatusRendererRequest,
  GetCachedBackendStatusMainResponse
> = new MainIpcChannel(GET_CACHED_BACKEND_STATUS_CHANNEL);
