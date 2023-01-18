import { GET_STATE_DIRECTORY_PATH_CHANNEL } from '../../../common/ipc/api';
import type {
  GetStateDirectoryPathRendererRequest,
  GetStateDirectoryPathMainResponse,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getStateDirectoryPathChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GetStateDirectoryPathMainResponse,
  GetStateDirectoryPathRendererRequest
> = new RendererIpcChannel(GET_STATE_DIRECTORY_PATH_CHANNEL);
