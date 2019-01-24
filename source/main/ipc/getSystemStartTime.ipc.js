// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GetSystemStartTimeChannel } from '../../common/ipc/getSystemStartTime.ipc';
import type { GetSystemStartTimeResponse } from '../../common/types/getSystemStartTime.types';

// IpcChannel<Incoming, Outgoing>

export const getSystemStartTimeChannel: (
  MainIpcChannel<void, GetSystemStartTimeResponse>
) = (
  new MainIpcChannel(GetSystemStartTimeChannel)
);

