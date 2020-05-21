// @flow
import {
  UPDATE_MANAGER_INIT,
  UPDATE_MANAGER_STATUS,
  UPDATE_MANAGER_REQUEST_DOWNLOAD,
} from '../../../common/ipc/api';
import type { UpdateManagerStatusResponse } from '../../../common/types/update-manager.types';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const updateManagerInitChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<UpdateManagerStatusResponse, void> = new RendererIpcChannel(
  UPDATE_MANAGER_INIT
);

export const updateManagerStatusChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<UpdateManagerStatusResponse, void> = new RendererIpcChannel(
  UPDATE_MANAGER_STATUS
);

export const updateManagerRequestDownloadChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<UpdateManagerStatusResponse, void> = new RendererIpcChannel(
  UPDATE_MANAGER_REQUEST_DOWNLOAD
);
