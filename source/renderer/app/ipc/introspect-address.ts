import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { INTROSPECT_ADDRESS_CHANNEL } from '../../../common/ipc/api';
import type {
  IntrospectAddressRendererRequest,
  IntrospectAddressMainResponse,
} from '../../../common/ipc/api';
// IpcChannel<Incoming, Outgoing>
export const introspectAddressChannel: RendererIpcChannel<
  IntrospectAddressMainResponse,
  IntrospectAddressRendererRequest
> = new RendererIpcChannel(INTROSPECT_ADDRESS_CHANNEL);
