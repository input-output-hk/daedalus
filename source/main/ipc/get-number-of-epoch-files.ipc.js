// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GetNumberOfEpochFilesChannel } from '../../common/ipc/epochs.ipc';
import type { GetNumberOfEpochFilesChannelResponse } from '../../common/types/epochs.types';

// IpcChannel<Incoming, Outgoing>

export const getNumberOfEpochFilesChannel: (
  MainIpcChannel<void, GetNumberOfEpochFilesChannelResponse>
) = (
  new MainIpcChannel(GetNumberOfEpochFilesChannel)
);

