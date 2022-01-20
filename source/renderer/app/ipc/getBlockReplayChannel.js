// @flow
import { GET_BLOCK_SYNC_STATUS_CHANNEL } from '../../../common/ipc/api';
import type {
  GetBlockSyncProgressRendererRequest,
  GetBlockSyncProgressMainResponse,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getBlockReplayProgressChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GetBlockSyncProgressMainResponse,
  GetBlockSyncProgressRendererRequest
> = new RendererIpcChannel(GET_BLOCK_SYNC_STATUS_CHANNEL);
