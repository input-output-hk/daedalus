import { MainIpcChannel } from './lib/MainIpcChannel';
import { ENABLE_APPLICATION_MENU_NAVIGATION_CHANNEL } from '../../common/ipc/api';
import type {
  EnableApplicationMenuNavigationMainResponse,
  EnableApplicationMenuNavigationRendererRequest,
} from '../../common/ipc/api';

export const enableApplicationMenuNavigationChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  EnableApplicationMenuNavigationRendererRequest,
  EnableApplicationMenuNavigationMainResponse
> = new MainIpcChannel(ENABLE_APPLICATION_MENU_NAVIGATION_CHANNEL);
