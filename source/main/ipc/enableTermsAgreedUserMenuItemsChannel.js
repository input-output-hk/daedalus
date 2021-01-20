// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { ENABLE_TERMS_AGREED_USER_MENU_ITEMS_CHANNEL } from '../../common/ipc/api';
import type {
  EnableTermsAgreedUserMenuItemsMainResponse,
  EnableTermsAgreedUserMenuItemsRendererRequest,
} from '../../common/ipc/api';

export const enableTermsAgreedUserMenuItemsChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  EnableTermsAgreedUserMenuItemsRendererRequest,
  EnableTermsAgreedUserMenuItemsMainResponse
> = new MainIpcChannel(ENABLE_TERMS_AGREED_USER_MENU_ITEMS_CHANNEL);
