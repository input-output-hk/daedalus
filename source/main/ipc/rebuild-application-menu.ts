import { MainIpcChannel } from './lib/MainIpcChannel';
import { REBUILD_APP_MENU_CHANNEL } from '../../common/ipc/api';
import type {
  RebuildAppMenuRendererRequest,
  RebuildAppMenuMainResponse,
} from '../../common/ipc/api';

export const rebuildApplicationMenu: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  RebuildAppMenuRendererRequest,
  RebuildAppMenuMainResponse
> = new MainIpcChannel(REBUILD_APP_MENU_CHANNEL);
