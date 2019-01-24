// @flow
import { GetSystemStartTimeChannel } from '../../../common/ipc/getSystemStartTime.ipc';
import type { GetSystemStartTimeResponse } from '../../../common/types/getSystemStartTime.types';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getSystemStartTimeChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<GetSystemStartTimeResponse, void>
) = (
  new RendererIpcChannel(GetSystemStartTimeChannel)
);
