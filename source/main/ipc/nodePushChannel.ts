import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  NODE_STARTUP_STATUS_CHANNEL,
  NODE_BLOCK_SYNC_PROGRESS_CHANNEL,
  WATCHDOG_STOPPED_CHANNEL,
} from '../../common/ipc/api';
import type {
  NodeStartupStatusMainRequest,
  NodeStartupStatusRendererResponse,
  NodeBlockSyncProgressMainRequest,
  NodeBlockSyncProgressRendererResponse,
  WatchdogStoppedMainRequest,
  WatchdogStoppedRendererResponse,
} from '../../common/ipc/api';

export const nodeStartupStatusChannel: MainIpcChannel<
  NodeStartupStatusRendererResponse,
  NodeStartupStatusMainRequest
> = new MainIpcChannel(NODE_STARTUP_STATUS_CHANNEL);

export const nodeBlockSyncProgressChannel: MainIpcChannel<
  NodeBlockSyncProgressRendererResponse,
  NodeBlockSyncProgressMainRequest
> = new MainIpcChannel(NODE_BLOCK_SYNC_PROGRESS_CHANNEL);

export const watchdogStoppedChannel: MainIpcChannel<
  WatchdogStoppedRendererResponse,
  WatchdogStoppedMainRequest
> = new MainIpcChannel(WATCHDOG_STOPPED_CHANNEL);
