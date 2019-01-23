// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GetNumberOfEpochsConsolidatedChannel } from '../../common/ipc/epochs.ipc';
import type { GetNumberOfEpochsConsolidatedChannelResponse } from '../../common/types/epochs.types';

// IpcChannel<Incoming, Outgoing>

export const getNumberOfEpochsConsolidatedChannel: (
  MainIpcChannel<void, GetNumberOfEpochsConsolidatedChannelResponse>
) = (
  new MainIpcChannel(GetNumberOfEpochsConsolidatedChannel)
);

