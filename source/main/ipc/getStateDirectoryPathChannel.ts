import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_STATE_DIRECTORY_PATH_CHANNEL } from '../../common/ipc/api';
import type {
  GetStateDirectoryPathRendererRequest,
  GetStateDirectoryPathMainResponse,
} from '../../common/ipc/api';
// IpcChannel<Incoming, Outgoing>
export const getStateDirectoryPathChannel: MainIpcChannel<
  GetStateDirectoryPathRendererRequest,
  GetStateDirectoryPathMainResponse
> = new MainIpcChannel(GET_STATE_DIRECTORY_PATH_CHANNEL);
