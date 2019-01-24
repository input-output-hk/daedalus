// @flow
import { GetNumberOfEpochsConsolidatedChannel } from '../../../common/ipc/getNumberOfEpochsConsolidated.ipc';
import type { GetNumberOfEpochsConsolidatedChannelResponse } from '../../../common/types/getNumberOfEpochsConsolidated.types';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getNumberOfEpochsConsolidatedChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<GetNumberOfEpochsConsolidatedChannelResponse, void>
) = (
  new RendererIpcChannel(GetNumberOfEpochsConsolidatedChannel)
);
