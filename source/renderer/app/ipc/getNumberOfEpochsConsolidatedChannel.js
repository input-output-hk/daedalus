// @flow
import { GetNumberOfEpochsConsolidatedChannel } from '../../../common/ipc/epochs.ipc';
import type { GetNumberOfEpochsConsolidatedChannelResponse } from '../../../common/types/epochs.types';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getNumberOfEpochsConsolidatedChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<GetNumberOfEpochsConsolidatedChannelResponse, void>
) = (
  new RendererIpcChannel(GetNumberOfEpochsConsolidatedChannel)
);
