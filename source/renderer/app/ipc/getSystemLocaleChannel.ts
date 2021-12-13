import { GET_SYSTEM_LOCALE_CHANNEL } from '../../../common/ipc/api';
import type {
  GetSystemLocaleRendererRequest,
  GetSystemLocaleMainResponse,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getSystemLocaleChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GetSystemLocaleMainResponse,
  GetSystemLocaleRendererRequest
> = new RendererIpcChannel(GET_SYSTEM_LOCALE_CHANNEL);
