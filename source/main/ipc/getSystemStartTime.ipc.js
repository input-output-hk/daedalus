// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_SYSTEM_START_TIME_CHANNEL } from '../../common/ipc/api';
import type {
  GetSystemStartTimeMainResponse,
  GetSystemStartTimeRendererRequest,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const getSystemStartTimeChannel: MainIpcChannel<
  GetSystemStartTimeRendererRequest,
  GetSystemStartTimeMainResponse
> = new MainIpcChannel(GET_SYSTEM_START_TIME_CHANNEL);
