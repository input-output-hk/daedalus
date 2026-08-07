import { MainIpcChannel } from './lib/MainIpcChannel';
import { MITHRIL_COMMAND_CHANNEL } from '../../common/ipc/api';
import type {
  MithrilCommandRendererRequest,
  MithrilCommandMainResponse,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>
export const mithrilCommandChannel: MainIpcChannel<
  MithrilCommandRendererRequest,
  MithrilCommandMainResponse
> = new MainIpcChannel(MITHRIL_COMMAND_CHANNEL);
