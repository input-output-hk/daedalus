import { ENABLE_APPLICATION_MENU_NAVIGATION_CHANNEL } from '../../../common/ipc/api';
import type {
  EnableApplicationMenuNavigationMainResponse,
  EnableApplicationMenuNavigationRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const enableApplicationMenuNavigationChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  EnableApplicationMenuNavigationMainResponse,
  EnableApplicationMenuNavigationRendererRequest
> = new RendererIpcChannel(ENABLE_APPLICATION_MENU_NAVIGATION_CHANNEL);
