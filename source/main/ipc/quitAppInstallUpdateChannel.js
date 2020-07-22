// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { QUIT_APP_INSTALL_UPDATE } from '../../common/ipc/api';
import type {
  QuitAppInstallUpdateRendererRequest,
  QuitAppInstallUpdateMainResponse,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const quitAppInstallUpdateChannel: MainIpcChannel<
  QuitAppInstallUpdateRendererRequest,
  QuitAppInstallUpdateMainResponse
> = new MainIpcChannel(QUIT_APP_INSTALL_UPDATE);
