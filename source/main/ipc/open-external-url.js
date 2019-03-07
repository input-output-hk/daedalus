// @flow
import { shell } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { OPEN_EXTERNAL_URL_CHANNEL } from '../../common/ipc/api';
import type {
  OpenExternalUrlResponse,
  OpenExternalUrlRequest,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const openExternalUrlChannel: (
  MainIpcChannel<OpenExternalUrlRequest, OpenExternalUrlResponse>
) = (
  new MainIpcChannel(OPEN_EXTERNAL_URL_CHANNEL)
);

openExternalUrlChannel.onReceive((url: OpenExternalUrlRequest) => (
  shell.openExternal(url) ? Promise.resolve() : Promise.reject()
));
