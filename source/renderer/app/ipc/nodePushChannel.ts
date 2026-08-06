import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  NODE_STARTUP_STATUS_CHANNEL,
  NODE_BLOCK_SYNC_PROGRESS_CHANNEL,
} from '../../../common/ipc/api';
import type {
  NodeStartupStatusMainRequest,
  NodeStartupStatusRendererResponse,
  NodeBlockSyncProgressMainRequest,
  NodeBlockSyncProgressRendererResponse,
} from '../../../common/ipc/api';

export const nodeStartupStatusChannel: RendererIpcChannel<
  NodeStartupStatusMainRequest,
  NodeStartupStatusRendererResponse
> = new RendererIpcChannel(NODE_STARTUP_STATUS_CHANNEL);

export const nodeBlockSyncProgressChannel: RendererIpcChannel<
  NodeBlockSyncProgressMainRequest,
  NodeBlockSyncProgressRendererResponse
> = new RendererIpcChannel(NODE_BLOCK_SYNC_PROGRESS_CHANNEL);
