import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_DESKTOP_DIRECTORY_PATH_CHANNEL } from '../../common/ipc/api';
import type {
  GetDesktopDirectoryPathRendererRequest,
  GetDesktopDirectoryPathMainResponse,
} from '../../common/ipc/api';
// IpcChannel<Incoming, Outgoing>
export const getDesktopDirectoryPathChannel: MainIpcChannel<
  GetDesktopDirectoryPathRendererRequest,
  GetDesktopDirectoryPathMainResponse
> = new MainIpcChannel(GET_DESKTOP_DIRECTORY_PATH_CHANNEL);
