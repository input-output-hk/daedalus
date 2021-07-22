// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { GET_CUSTOM_PROTOCOL_CHANNEL } from '../../../common/ipc/api';

import type {
  GetCustomProtocolMainRequest,
  GetCustomProtocolRendererResponse,
} from '../../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>
export const getCustomProtocolChannel: RendererIpcChannel<
  GetCustomProtocolMainRequest,
  GetCustomProtocolRendererResponse
> = new RendererIpcChannel(GET_CUSTOM_PROTOCOL_CHANNEL);
