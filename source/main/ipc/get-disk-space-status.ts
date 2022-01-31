import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_DISK_SPACE_STATUS_CHANNEL } from '../../common/ipc/api';
import type {
  GetDiskSpaceStatusRendererRequest,
  GetDiskSpaceStatusMainResponse,
} from '../../common/ipc/api';
// IpcChannel<Incoming, Outgoing>
export const getDiskSpaceStatusChannel: MainIpcChannel<
  GetDiskSpaceStatusRendererRequest,
  GetDiskSpaceStatusMainResponse
> = new MainIpcChannel(GET_DISK_SPACE_STATUS_CHANNEL);
