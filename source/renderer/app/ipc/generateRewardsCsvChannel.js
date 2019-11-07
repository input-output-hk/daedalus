// @flow
import { GENERATE_REWARDS_CSV_CHANNEL } from '../../../common/ipc/api';
import type {
  GenerateRewardsCsvMainResponse,
  GenerateRewardsCsvRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const generateRewardsCsvChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GenerateRewardsCsvMainResponse,
  GenerateRewardsCsvRendererRequest
> = new RendererIpcChannel(GENERATE_REWARDS_CSV_CHANNEL);
