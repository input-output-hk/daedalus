// @flow
import { GetDiskSpaceStatusChannelName } from '../../../common/ipc/api';
import type {
  GetDiskSpaceStatusRendererRequest,
  GetDiskSpaceStatusMainResponse
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getDiskSpaceStatusChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<GetDiskSpaceStatusMainResponse, GetDiskSpaceStatusRendererRequest>
) = (
  new RendererIpcChannel(GetDiskSpaceStatusChannelName)
);
