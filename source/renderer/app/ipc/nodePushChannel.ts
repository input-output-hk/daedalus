import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  NODE_STARTUP_STATUS_CHANNEL,
  NODE_BLOCK_SYNC_PROGRESS_CHANNEL,
  WATCHDOG_STOPPED_CHANNEL,
} from '../../../common/ipc/api';
import type {
  NodeStartupStatusMainRequest,
  NodeStartupStatusRendererResponse,
  NodeBlockSyncProgressMainRequest,
  NodeBlockSyncProgressRendererResponse,
  WatchdogStoppedMainRequest,
  WatchdogStoppedRendererResponse,
} from '../../../common/ipc/api';

export const nodeStartupStatusChannel: RendererIpcChannel<
  NodeStartupStatusMainRequest,
  NodeStartupStatusRendererResponse
> = new RendererIpcChannel(NODE_STARTUP_STATUS_CHANNEL);

export const nodeBlockSyncProgressChannel: RendererIpcChannel<
  NodeBlockSyncProgressMainRequest,
  NodeBlockSyncProgressRendererResponse
> = new RendererIpcChannel(NODE_BLOCK_SYNC_PROGRESS_CHANNEL);

export const watchdogStoppedChannel: RendererIpcChannel<
  WatchdogStoppedMainRequest,
  WatchdogStoppedRendererResponse
> = new RendererIpcChannel(WATCHDOG_STOPPED_CHANNEL);
