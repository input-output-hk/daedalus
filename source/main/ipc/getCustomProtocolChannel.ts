// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_CUSTOM_PROTOCOL_CHANNEL } from '../../common/ipc/api';
import type {
  GetCustomProtocolMainRequest,
  GetCustomProtocolRendererResponse,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const getCustomProtocolChannel: MainIpcChannel<
  GetCustomProtocolRendererResponse,
  GetCustomProtocolMainRequest
> = new MainIpcChannel(GET_CUSTOM_PROTOCOL_CHANNEL);
