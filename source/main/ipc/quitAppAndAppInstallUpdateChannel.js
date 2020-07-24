// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { QUIT_APP_AND_INSTALL_UPDATE } from '../../common/ipc/api';
import type {
  QuitAppAndAppInstallUpdateRendererRequest,
  QuitAppAndAppInstallUpdateMainResponse,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const quitAppAndAppInstallUpdateChannel: MainIpcChannel<
  QuitAppAndAppInstallUpdateRendererRequest,
  QuitAppAndAppInstallUpdateMainResponse
> = new MainIpcChannel(QUIT_APP_AND_INSTALL_UPDATE);
