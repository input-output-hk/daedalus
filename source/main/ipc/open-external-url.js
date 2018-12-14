// @flow
import { shell } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { OpenExternalUrlChannelName } from '../../common/ipc/api';
import type {
  OpenExternalUrlMainResponse,
  OpenExternalUrlRendererRequest,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const openExternalUrlChannel: (
  MainIpcChannel<OpenExternalUrlRendererRequest, OpenExternalUrlMainResponse>
) = (
  new MainIpcChannel(OpenExternalUrlChannelName)
);

openExternalUrlChannel.onReceive((url: OpenExternalUrlRendererRequest) => (
  shell.openExternal(url) ? Promise.resolve() : Promise.reject()
));
