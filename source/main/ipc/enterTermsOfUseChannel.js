// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { ENTER_TERMS_OF_USE_CHANNEL } from '../../common/ipc/api';
import type {
  EnterTermsOfUseMainResponse,
  EnterTermsOfUseRendererRequest,
} from '../../common/ipc/api';

export const enterTermsOfUseChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  EnterTermsOfUseRendererRequest,
  EnterTermsOfUseMainResponse
> = new MainIpcChannel(ENTER_TERMS_OF_USE_CHANNEL);
