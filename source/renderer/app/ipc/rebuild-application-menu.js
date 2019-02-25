// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { REBUILD_APP_MENU_CHANNEL } from '../../../common/ipc/api';

export const rebuildApplicationMenu: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<void, void>
) = (
  new RendererIpcChannel(REBUILD_APP_MENU_CHANNEL)
);
