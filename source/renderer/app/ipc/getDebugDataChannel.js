// @flow
import { GET_DEBUG_DATA_CHANNEL } from '../../../common/ipc/api';
import type {
  GetDebugDataRendererRequest,
  GetDebugDataMainResponse,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getDebugDataChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GetDebugDataMainResponse,
  GetDebugDataRendererRequest
> = new RendererIpcChannel(GET_DEBUG_DATA_CHANNEL);
