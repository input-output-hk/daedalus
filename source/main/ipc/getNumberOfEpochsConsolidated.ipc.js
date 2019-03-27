// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_CONSOLIDATED_EPOCHS_COUNT_CHANNEL } from '../../common/ipc/api';
import type {
  GetConsolidatedEpochsCountMainResponse,
  GetConsolidatedEpochsCountRendererRequest,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const getNumberOfEpochsConsolidatedChannel: MainIpcChannel<
  GetConsolidatedEpochsCountRendererRequest,
  GetConsolidatedEpochsCountMainResponse
> = new MainIpcChannel(GET_CONSOLIDATED_EPOCHS_COUNT_CHANNEL);
