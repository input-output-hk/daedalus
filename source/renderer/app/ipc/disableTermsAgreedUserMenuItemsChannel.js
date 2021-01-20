// @flow
import { DISABLE_TERMS_AGREED_USER_MENU_ITEMS_CHANNEL } from '../../../common/ipc/api';
import type {
  DisableTermsAgreedUserMenuItemsMainResponse,
  DisableTermsAgreedUserMenuItemsRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const disableTermsAgreedUserMenuItemsChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  DisableTermsAgreedUserMenuItemsMainResponse,
  DisableTermsAgreedUserMenuItemsRendererRequest
> = new RendererIpcChannel(DISABLE_TERMS_AGREED_USER_MENU_ITEMS_CHANNEL);
