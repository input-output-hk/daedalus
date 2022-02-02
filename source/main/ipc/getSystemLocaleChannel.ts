import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_SYSTEM_LOCALE_CHANNEL } from '../../common/ipc/api';
import type {
  GetSystemLocaleRendererRequest,
  GetSystemLocaleMainResponse,
} from '../../common/ipc/api';
// IpcChannel<Incoming, Outgoing>
export const getSystemLocaleChannel: MainIpcChannel<
  GetSystemLocaleRendererRequest,
  GetSystemLocaleMainResponse
> = new MainIpcChannel(GET_SYSTEM_LOCALE_CHANNEL);
