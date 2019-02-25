// @flow
import { GetNumberOfEpochsConsolidatedChannel } from '../../../common/ipc/api';
import type { GetNumberOfEpochsConsolidatedChannelResponse } from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getNumberOfEpochsConsolidatedChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<GetNumberOfEpochsConsolidatedChannelResponse, void>
) = (
  new RendererIpcChannel(GetNumberOfEpochsConsolidatedChannel)
);
