import { shell } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { OPEN_EXTERNAL_URL_CHANNEL } from '../../common/ipc/api';
import type {
  OpenExternalUrlMainResponse,
  OpenExternalUrlRendererRequest,
} from '../../common/ipc/api';
// IpcChannel<Incoming, Outgoing>
export const openExternalUrlChannel: MainIpcChannel<
  OpenExternalUrlRendererRequest,
  OpenExternalUrlMainResponse
> = new MainIpcChannel(OPEN_EXTERNAL_URL_CHANNEL);
openExternalUrlChannel.onReceive((url: OpenExternalUrlRendererRequest) =>
  shell.openExternal(url) ? Promise.resolve() : Promise.reject()
);
