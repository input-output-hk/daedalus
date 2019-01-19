// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { RebuildApplicationMenu } from '../../common/ipc/api';

export const rebuildApplicationMenu: (
  // IpcChannel<Incoming, Outgoing>
  MainIpcChannel<void, void>
) = (
  new MainIpcChannel(RebuildApplicationMenu)
);
