// @flow
import { GET_BLOCK_REPLY_STATUS_CHANNEL } from '../../../common/ipc/api';
import type {
  GetBlockReplyProgressRendererRequest,
  GetBlockReplyProgressMainResponse,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getBlockReplyProgressChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GetBlockReplyProgressMainResponse,
  GetBlockReplyProgressRendererRequest
> = new RendererIpcChannel(GET_BLOCK_REPLY_STATUS_CHANNEL);
