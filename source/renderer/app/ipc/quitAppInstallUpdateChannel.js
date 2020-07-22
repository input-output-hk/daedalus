// @flow
import { QUIT_APP_INSTALL_UPDATE } from '../../../common/ipc/api';
import type {
  QuitAppInstallUpdateRendererRequest,
  QuitAppInstallUpdateMainResponse,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const quitAppInstallUpdateChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  QuitAppInstallUpdateMainResponse,
  QuitAppInstallUpdateRendererRequest
> = new RendererIpcChannel(QUIT_APP_INSTALL_UPDATE);
