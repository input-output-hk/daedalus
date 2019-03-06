// @flow
import { GET_CONSOLIDATED_EPOCHS_COUNT_CHANNEL } from '../../../common/ipc/api';
import type { GetConsolidatedEpochsCountResponse } from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getNumberOfEpochsConsolidatedChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<GetConsolidatedEpochsCountResponse, void>
) = (
  new RendererIpcChannel(GET_CONSOLIDATED_EPOCHS_COUNT_CHANNEL)
);
