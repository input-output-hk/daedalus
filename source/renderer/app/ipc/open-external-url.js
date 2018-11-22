// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { OpenExternalUrlChannelName } from '../../../common/ipc/api';
import type {
  OpenExternalUrlMainResponse,
  OpenExternalUrlRendererRequest,
} from '../../../common/ipc/api';


export const openExternalUrlChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<OpenExternalUrlMainResponse, OpenExternalUrlRendererRequest>
) = (
  new RendererIpcChannel(OpenExternalUrlChannelName)
);
