import { MainIpcChannel } from './lib/MainIpcChannel';
import { SET_STATE_SNAPSHOT_LOG_CHANNEL } from '../../common/ipc/api';
import type {
  SetStateSnapshotLogRendererRequest,
  SetStateSnapshotLogMainResponse,
} from '../../common/ipc/api';
// IpcChannel<Incoming, Outgoing>
export const setStateSnapshotLogChannel: MainIpcChannel<
  SetStateSnapshotLogMainResponse,
  SetStateSnapshotLogRendererRequest
> = new MainIpcChannel(SET_STATE_SNAPSHOT_LOG_CHANNEL);
