// @flow
import { ENTER_TERMS_OF_USE_CHANNEL } from '../../../common/ipc/api';
import type {
  EnterTermsOfUseMainResponse,
  EnterTermsOfUseRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const enterTermsOfUseChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  EnterTermsOfUseMainResponse,
  EnterTermsOfUseRendererRequest
> = new RendererIpcChannel(ENTER_TERMS_OF_USE_CHANNEL);
