// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GetSystemStartTimeChannel } from '../../common/ipc/get-system-start-time.ipc';
import type { GetSystemStartTimeResponse } from '../../common/types/get-system-start-time.types';

// IpcChannel<Incoming, Outgoing>

export const getSystemStartTimeChannel: (
  MainIpcChannel<void, GetSystemStartTimeResponse>
) = (
  new MainIpcChannel(GetSystemStartTimeChannel)
);

