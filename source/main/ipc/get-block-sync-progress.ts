import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_BLOCK_SYNC_PROGRESS_CHANNEL } from '../../common/ipc/api';
import type {
  GetBlockSyncProgressRendererRequest,
  GetBlockSyncProgressMainResponse,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const getBlockSyncProgressChannel: MainIpcChannel<
  GetBlockSyncProgressRendererRequest,
  GetBlockSyncProgressMainResponse
> = new MainIpcChannel(GET_BLOCK_SYNC_PROGRESS_CHANNEL);
