// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { OPEN_EXTERNAL_URL_CHANNEL } from '../../../common/ipc/channels';
import type { Url } from '../../../common/ipc/contracts';

// IpcChannel<Incoming, Outgoing>

export const openExternalUrlChannel: RendererIpcChannel<void, Url> = (
  new RendererIpcChannel(OPEN_EXTERNAL_URL_CHANNEL)
);
