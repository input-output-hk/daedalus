// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { ELECTRON_LOG_CHANNEL } from '../../../common/ipc/api';
import type {
  ElectronLogMainResponse,
  ElectronLogRenderRequest,
} from '../../../common/ipc/api';

export const electronLogChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  ElectronLogMainResponse,
  ElectronLogRenderRequest
> = new RendererIpcChannel(ELECTRON_LOG_CHANNEL);
