import { GET_DESKTOP_DIRECTORY_PATH_CHANNEL } from '../../../common/ipc/api';
import type {
  GetDesktopDirectoryPathRendererRequest,
  GetDesktopDirectoryPathMainResponse,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getDesktopDirectoryPathChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GetDesktopDirectoryPathMainResponse,
  GetDesktopDirectoryPathRendererRequest
> = new RendererIpcChannel(GET_DESKTOP_DIRECTORY_PATH_CHANNEL);
