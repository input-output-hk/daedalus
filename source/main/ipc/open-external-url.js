// @flow
import { shell } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { OPEN_EXTERNAL_URL_CHANNEL } from '../../common/ipc/open-external-url';
import type { Url } from '../../common/ipc/open-external-url';

// IpcChannel<Incoming, Outgoing>

export const openExternalUrlChannel: MainIpcChannel<Url, void> = (
  new MainIpcChannel(OPEN_EXTERNAL_URL_CHANNEL)
);

openExternalUrlChannel.onReceive((url: Url) => (
  shell.openExternal(url) ? Promise.resolve() : Promise.reject()
));
