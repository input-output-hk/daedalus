// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { REBUILD_APP_MENU_CHANNEL } from '../../common/ipc/api';

export const rebuildApplicationMenu: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<void, void> = new MainIpcChannel(REBUILD_APP_MENU_CHANNEL);
