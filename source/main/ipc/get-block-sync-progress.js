// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_BLOCK_SYNC_STATUS_CHANNEL } from '../../common/ipc/api';
import type {
  GetBlockSyncProgressRendererRequest,
  GetBlockSyncProgressMainResponse,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const getBlockReplayProgressChannel: MainIpcChannel<
  GetBlockSyncProgressRendererRequest,
  GetBlockSyncProgressMainResponse
> = new MainIpcChannel(GET_BLOCK_SYNC_STATUS_CHANNEL);
