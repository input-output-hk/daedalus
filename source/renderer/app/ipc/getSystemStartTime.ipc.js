// @flow
import { GET_SYSTEM_START_TIME_CHANNEL } from '../../../common/ipc/api';
import type { GetSystemStartTimeResponse } from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getSystemStartTimeChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<GetSystemStartTimeResponse, void>
) = (
  new RendererIpcChannel(GET_SYSTEM_START_TIME_CHANNEL)
);
