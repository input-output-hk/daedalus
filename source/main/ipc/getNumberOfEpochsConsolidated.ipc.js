// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GetNumberOfEpochsConsolidatedChannel } from '../../common/ipc/getNumberOfEpochsConsolidated.ipc';
import type { GetNumberOfEpochsConsolidatedChannelResponse } from '../../common/types/getNumberOfEpochsConsolidated.types';

// IpcChannel<Incoming, Outgoing>

export const getNumberOfEpochsConsolidatedChannel: (
  MainIpcChannel<void, GetNumberOfEpochsConsolidatedChannelResponse>
) = (
  new MainIpcChannel(GetNumberOfEpochsConsolidatedChannel)
);
