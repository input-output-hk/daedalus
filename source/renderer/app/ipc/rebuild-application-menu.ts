import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { REBUILD_APP_MENU_CHANNEL } from '../../../common/ipc/api';
import type {
  RebuildAppMenuMainResponse,
  RebuildAppMenuRendererRequest,
} from '../../../common/ipc/api';
// IpcChannel<Incoming, Outgoing>
export type RebuildApplicationMenu = RendererIpcChannel<
  RebuildAppMenuMainResponse,
  RebuildAppMenuRendererRequest
>;
export const rebuildApplicationMenu: RebuildApplicationMenu = new RendererIpcChannel(
  REBUILD_APP_MENU_CHANNEL
);
