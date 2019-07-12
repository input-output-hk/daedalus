// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { SET_LOG_STATE_SNAPSHOT_CHANNEL } from '../../../common/ipc/api';
import type {
  SetLogStateSnapshotRendererRequest,
  SetLogStateSnapshotMainResponse,
} from '../../../common/ipc/api';

export const setLogStateSnapshotChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  SetLogStateSnapshotMainResponse,
  SetLogStateSnapshotRendererRequest
> = new RendererIpcChannel(SET_LOG_STATE_SNAPSHOT_CHANNEL);
