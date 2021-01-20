// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { DISABLE_TERMS_AGREED_USER_MENU_ITEMS_CHANNEL } from '../../common/ipc/api';
import type {
  DisableTermsAgreedUserMenuItemsMainResponse,
  DisableTermsAgreedUserMenuItemsRendererRequest,
} from '../../common/ipc/api';

export const disableTermsAgreedUserMenuItemsChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  DisableTermsAgreedUserMenuItemsRendererRequest,
  DisableTermsAgreedUserMenuItemsMainResponse
> = new MainIpcChannel(DISABLE_TERMS_AGREED_USER_MENU_ITEMS_CHANNEL);
