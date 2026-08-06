import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  MITHRIL_COMMAND_CHANNEL,
} from '../../../common/ipc/api';
import type {
  MithrilCommandRendererRequest,
  MithrilCommandMainResponse,
} from '../../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>
export const mithrilCommandChannel: RendererIpcChannel<
  MithrilCommandMainResponse,
  MithrilCommandRendererRequest
> = new RendererIpcChannel(MITHRIL_COMMAND_CHANNEL);
