import { GET_BLOCK_SYNC_PROGRESS_CHANNEL } from '../../../common/ipc/api';
import type {
  GetBlockSyncProgressRendererRequest,
  GetBlockSyncProgressMainResponse,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getBlockSyncProgressChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GetBlockSyncProgressMainResponse,
  GetBlockSyncProgressRendererRequest
> = new RendererIpcChannel(GET_BLOCK_SYNC_PROGRESS_CHANNEL);
