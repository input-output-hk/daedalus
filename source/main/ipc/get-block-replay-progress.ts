import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_BLOCK_REPLAY_STATUS_CHANNEL } from '../../common/ipc/api';
import type {
  GetBlockReplayProgressRendererRequest,
  GetBlockReplayProgressMainResponse,
} from '../../common/ipc/api';
// IpcChannel<Incoming, Outgoing>
export const getBlockReplayProgressChannel: MainIpcChannel<
  GetBlockReplayProgressRendererRequest,
  GetBlockReplayProgressMainResponse
> = new MainIpcChannel(GET_BLOCK_REPLAY_STATUS_CHANNEL);
