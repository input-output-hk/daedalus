import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { OPEN_LOCAL_DIRECTORY_CHANNEL } from '../../../common/ipc/api';
import type {
  OpenLocalDirectoryMainResponse,
  OpenLocalDirectoryRendererRequest,
} from '../../../common/ipc/api';
// IpcChannel<Incoming, Outgoing>
export const openLocalDirectoryChannel: RendererIpcChannel<
  OpenLocalDirectoryMainResponse,
  OpenLocalDirectoryRendererRequest
> = new RendererIpcChannel(OPEN_LOCAL_DIRECTORY_CHANNEL);
