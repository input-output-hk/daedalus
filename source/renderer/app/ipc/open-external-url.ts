import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { OPEN_EXTERNAL_URL_CHANNEL } from '../../../common/ipc/api';
import type {
  OpenExternalUrlMainResponse,
  OpenExternalUrlRendererRequest,
} from '../../../common/ipc/api';

export const openExternalUrlChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  OpenExternalUrlMainResponse,
  OpenExternalUrlRendererRequest
> = new RendererIpcChannel(OPEN_EXTERNAL_URL_CHANNEL);
