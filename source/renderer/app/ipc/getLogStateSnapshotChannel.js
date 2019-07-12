// @flow
import { GET_LOG_STATE_SNAPSHOT_CHANNEL } from '../../../common/ipc/api';
import type {
  GetLogStateSnapshotRendererRequest,
  GetLogStateSnapshotMainResponse,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getLogStateSnapshotChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GetLogStateSnapshotMainResponse,
  GetLogStateSnapshotRendererRequest
> = new RendererIpcChannel(GET_LOG_STATE_SNAPSHOT_CHANNEL);
