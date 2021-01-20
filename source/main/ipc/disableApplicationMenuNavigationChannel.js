// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { DISABLE_APPLICATION_MENU_NAVIGATION_CHANNEL } from '../../common/ipc/api';
import type {
  DisableApplicationMenuNavigationMainResponse,
  DisableApplicationMenuNavigationRendererRequest,
} from '../../common/ipc/api';

export const disableApplicationMenuNavigationChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  DisableApplicationMenuNavigationRendererRequest,
  DisableApplicationMenuNavigationMainResponse
> = new MainIpcChannel(DISABLE_APPLICATION_MENU_NAVIGATION_CHANNEL);
