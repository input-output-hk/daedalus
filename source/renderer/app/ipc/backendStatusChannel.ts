import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  GET_CACHED_BACKEND_STATUS_CHANNEL,
} from '../../../common/ipc/api';
import type {
  GetCachedBackendStatusRendererRequest,
  GetCachedBackendStatusMainResponse,
} from '../../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>
export const getCachedBackendStatusChannel: RendererIpcChannel<
  GetCachedBackendStatusMainResponse,
  GetCachedBackendStatusRendererRequest
> = new RendererIpcChannel(GET_CACHED_BACKEND_STATUS_CHANNEL);
