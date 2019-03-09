// @flow
import { GetSystemStartTimeChannel } from '../../../common/ipc/api';
import type { GetSystemStartTimeResponse } from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getSystemStartTimeChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<GetSystemStartTimeResponse, void>
) = (
  new RendererIpcChannel(GetSystemStartTimeChannel)
);
