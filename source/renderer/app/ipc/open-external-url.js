// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { OPEN_EXTERNAL_URL_CHANNEL } from '../../../common/ipc/api';
import type {
  OpenExternalUrlResponse,
  OpenExternalUrlRequest,
} from '../../../common/ipc/api';

export const openExternalUrlChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  OpenExternalUrlResponse,
  OpenExternalUrlRequest
> = new RendererIpcChannel(OPEN_EXTERNAL_URL_CHANNEL);
