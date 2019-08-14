// @flow
import { GET_CONSOLIDATED_EPOCHS_COUNT_CHANNEL } from '../../../common/ipc/api';
import type {
  GetConsolidatedEpochsCountMainResponse,
  GetConsolidatedEpochsCountRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getNumberOfEpochsConsolidatedChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GetConsolidatedEpochsCountMainResponse,
  GetConsolidatedEpochsCountRendererRequest
> = new RendererIpcChannel(GET_CONSOLIDATED_EPOCHS_COUNT_CHANNEL);
