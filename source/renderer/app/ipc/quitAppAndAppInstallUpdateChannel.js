// @flow
import { QUIT_APP_AND_INSTALL_UPDATE } from '../../../common/ipc/api';
import type {
  QuitAppAndAppInstallUpdateRendererRequest,
  QuitAppAndAppInstallUpdateMainResponse,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const quitAppAndAppInstallUpdateChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  QuitAppAndAppInstallUpdateMainResponse,
  QuitAppAndAppInstallUpdateRendererRequest
> = new RendererIpcChannel(QUIT_APP_AND_INSTALL_UPDATE);
