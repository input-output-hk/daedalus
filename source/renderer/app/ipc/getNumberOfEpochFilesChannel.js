// @flow
import { GetNumberOfEpochFilesChannel } from '../../../common/ipc/epochs.ipc';
import type { GetNumberOfEpochFilesChannelResponse } from '../../../common/types/epochs.types';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getNumberOfEpochFilesChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<GetNumberOfEpochFilesChannelResponse, void>
) = (
  new RendererIpcChannel(GetNumberOfEpochFilesChannel)
);
