import { GET_DISK_SPACE_STATUS_CHANNEL } from '../../../common/ipc/api';
import type {
  GetDiskSpaceStatusRendererRequest,
  GetDiskSpaceStatusMainResponse,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getDiskSpaceStatusChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GetDiskSpaceStatusMainResponse,
  GetDiskSpaceStatusRendererRequest
> = new RendererIpcChannel(GET_DISK_SPACE_STATUS_CHANNEL);
