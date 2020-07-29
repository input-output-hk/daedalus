// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_BLOCK_REPLY_STATUS_CHANNEL } from '../../common/ipc/api';
import type {
  GetBlockReplyProgressRendererRequest,
  GetBlockReplyProgressMainResponse,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const getBlockReplyProgressChannel: MainIpcChannel<
  GetBlockReplyProgressRendererRequest,
  GetBlockReplyProgressMainResponse
> = new MainIpcChannel(GET_BLOCK_REPLY_STATUS_CHANNEL);
