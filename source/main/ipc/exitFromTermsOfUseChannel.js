// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { EXIT_FROM_TERMS_OF_USE_CHANNEL } from '../../common/ipc/api';
import type {
  ExitFromTermsOfUseMainResponse,
  ExitFromTermsOfUseRendererRequest,
} from '../../common/ipc/api';

export const exitFromTermsOfUseChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  ExitFromTermsOfUseRendererRequest,
  ExitFromTermsOfUseMainResponse
> = new MainIpcChannel(EXIT_FROM_TERMS_OF_USE_CHANNEL);
