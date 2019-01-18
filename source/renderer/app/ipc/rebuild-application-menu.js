// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { RebuildApplicationMenu } from '../../../common/ipc/api';

export const rebuildApplicationMenu: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<void, void>
) = (
  new RendererIpcChannel(RebuildApplicationMenu)
);
