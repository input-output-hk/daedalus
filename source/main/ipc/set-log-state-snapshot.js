// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { SET_LOG_STATE_SNAPSHOT_CHANNEL } from '../../common/ipc/api';
import type {
  SetLogStateSnapshotRendererRequest,
  SetLogStateSnapshotMainResponse,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const setLogStateSnapshotChannel: MainIpcChannel<
  SetLogStateSnapshotMainResponse,
  SetLogStateSnapshotRendererRequest
> = new MainIpcChannel(SET_LOG_STATE_SNAPSHOT_CHANNEL);
