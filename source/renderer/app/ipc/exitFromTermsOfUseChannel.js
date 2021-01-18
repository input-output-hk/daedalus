// @flow
import { EXIT_FROM_TERMS_OF_USE_CHANNEL } from '../../../common/ipc/api';
import type {
  ExitFromTermsOfUseMainResponse,
  ExitFromTermsOfUseRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const exitFromTermsOfUseChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  ExitFromTermsOfUseMainResponse,
  ExitFromTermsOfUseRendererRequest
> = new RendererIpcChannel(EXIT_FROM_TERMS_OF_USE_CHANNEL);
