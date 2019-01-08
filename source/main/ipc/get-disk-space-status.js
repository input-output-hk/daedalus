// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GetDiskSpaceStatusChannelName } from '../../common/ipc/api';
import type {
  GetDiskSpaceStatusRendererRequest,
  GetDiskSpaceStatusMainResponse,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const openExternalUrlChannel: (
  MainIpcChannel<GetDiskSpaceStatusRendererRequest, GetDiskSpaceStatusMainResponse>
) = (
  new MainIpcChannel(GetDiskSpaceStatusChannelName)
);
