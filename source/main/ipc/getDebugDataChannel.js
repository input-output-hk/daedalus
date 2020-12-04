// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_DEBUG_DATA_CHANNEL } from '../../common/ipc/api';
import type {
  GetDebugDataRendererRequest,
  GetDebugDataMainResponse,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const getDebugDataChannel: MainIpcChannel<
  GetDebugDataRendererRequest,
  GetDebugDataMainResponse
> = new MainIpcChannel(GET_DEBUG_DATA_CHANNEL);
