import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { SET_STATE_SNAPSHOT_LOG_CHANNEL } from '../../../common/ipc/api';
import type {
  SetStateSnapshotLogRendererRequest,
  SetStateSnapshotLogMainResponse,
} from '../../../common/ipc/api';

export const setStateSnapshotLogChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  SetStateSnapshotLogMainResponse,
  SetStateSnapshotLogRendererRequest
> = new RendererIpcChannel(SET_STATE_SNAPSHOT_LOG_CHANNEL);
