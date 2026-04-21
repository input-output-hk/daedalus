import { shell } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { OPEN_LOCAL_DIRECTORY_CHANNEL } from '../../common/ipc/api';
import type {
  OpenLocalDirectoryMainResponse,
  OpenLocalDirectoryRendererRequest,
} from '../../common/ipc/api';
// IpcChannel<Incoming, Outgoing>
export const openLocalDirectoryChannel: MainIpcChannel<
  OpenLocalDirectoryRendererRequest,
  OpenLocalDirectoryMainResponse
> = new MainIpcChannel(OPEN_LOCAL_DIRECTORY_CHANNEL);
openLocalDirectoryChannel.onReceive(
  async (path: OpenLocalDirectoryRendererRequest) => {
    const error = await shell.openPath(path);
    if (error) throw new Error(error);
  }
);
