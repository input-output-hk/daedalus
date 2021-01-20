// @flow
import { DISABLE_APPLICATION_MENU_NAVIGATION_CHANNEL } from '../../../common/ipc/api';
import type {
  DisableApplicationMenuNavigationMainResponse,
  DisableApplicationMenuNavigationRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const disableApplicationMenuNavigationChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  DisableApplicationMenuNavigationMainResponse,
  DisableApplicationMenuNavigationRendererRequest
> = new RendererIpcChannel(DISABLE_APPLICATION_MENU_NAVIGATION_CHANNEL);
