// @flow
import { DownloadKeyFileChannelName } from '../../../common/ipc/api';
import type {
  DownloadKeyFileRendererRequest,
  DownloadKeyFileMainResponse
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const downloadKeyFileChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<DownloadKeyFileMainResponse, DownloadKeyFileRendererRequest>
) = (
  new RendererIpcChannel(DownloadKeyFileChannelName)
);
