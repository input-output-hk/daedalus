// @flow
import { ENABLE_TERMS_AGREED_USER_MENU_ITEMS_CHANNEL } from '../../../common/ipc/api';
import type {
  EnableTermsAgreedUserMenuItemsMainResponse,
  EnableTermsAgreedUserMenuItemsRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const enableTermsAgreedUserMenuItemsChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  EnableTermsAgreedUserMenuItemsMainResponse,
  EnableTermsAgreedUserMenuItemsRendererRequest
> = new RendererIpcChannel(ENABLE_TERMS_AGREED_USER_MENU_ITEMS_CHANNEL);
