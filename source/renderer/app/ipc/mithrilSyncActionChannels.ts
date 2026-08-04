import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  MITHRIL_SYNC_START_CHANNEL,
  MITHRIL_SYNC_RESTART_NODE_CHANNEL,
} from '../../../common/ipc/api';
import type {
  MithrilSyncStartRendererRequest,
  MithrilSyncStartMainResponse,
  MithrilSyncRestartNodeRendererRequest,
  MithrilSyncRestartNodeMainResponse,
} from '../../../common/ipc/api';

export { mithrilSyncCancelChannel } from './mithrilSyncChannel';

export const mithrilSyncStartChannel: RendererIpcChannel<
  MithrilSyncStartMainResponse,
  MithrilSyncStartRendererRequest
> = new RendererIpcChannel(MITHRIL_SYNC_START_CHANNEL);

export const mithrilSyncRestartNodeChannel: RendererIpcChannel<
  MithrilSyncRestartNodeMainResponse,
  MithrilSyncRestartNodeRendererRequest
> = new RendererIpcChannel(MITHRIL_SYNC_RESTART_NODE_CHANNEL);
