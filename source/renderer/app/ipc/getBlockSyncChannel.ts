import { GET_BLOCK_REPLAY_STATUS_CHANNEL } from '../../../common/ipc/api';
import type {
  GetBlockReplayProgressRendererRequest,
  GetBlockReplayProgressMainResponse,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getBlockReplayProgressChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GetBlockReplayProgressMainResponse,
  GetBlockReplayProgressRendererRequest
> = new RendererIpcChannel(GET_BLOCK_REPLAY_STATUS_CHANNEL);
