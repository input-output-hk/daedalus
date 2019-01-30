// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GetSystemStartTimeChannel } from '../../common/ipc/api';
import type { GetSystemStartTimeResponse } from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const getSystemStartTimeChannel: (
  MainIpcChannel<void, GetSystemStartTimeResponse>
) = (
  new MainIpcChannel(GetSystemStartTimeChannel)
);
