// @flow
import { DeleteKeyFileChannelName } from '../../../common/ipc/api';
import type {
  DeleteKeyFileRendererRequest,
  DeleteKeyFileMainResponse
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const deleteKeyFileChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<DeleteKeyFileMainResponse, DeleteKeyFileRendererRequest>
) = (
  new RendererIpcChannel(DeleteKeyFileChannelName)
);
