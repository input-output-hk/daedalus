// @flow
import { GetSystemStartTimeChannel } from '../../../common/ipc/get-system-start-time.ipc';
import type { GetSystemStartTimeResponse } from '../../../common/types/get-system-start-time.types';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getSystemStartTimeChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<GetSystemStartTimeResponse, void>
) = (
  new RendererIpcChannel(GetSystemStartTimeChannel)
);
