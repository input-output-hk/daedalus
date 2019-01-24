// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GetNumberOfEpochsConsolidatedChannel } from '../../common/ipc/api';
import type { GetNumberOfEpochsConsolidatedChannelResponse } from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const getNumberOfEpochsConsolidatedChannel: (
  MainIpcChannel<void, GetNumberOfEpochsConsolidatedChannelResponse>
) = (
  new MainIpcChannel(GetNumberOfEpochsConsolidatedChannel)
);
