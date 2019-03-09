// @flow
import { ExtractWalletsChannelName } from '../../../common/ipc/api';
import type {
  ExtractWalletsRendererRequest,
  ExtractWalletsMainResponse
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const extractWalletsChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<ExtractWalletsMainResponse, ExtractWalletsRendererRequest>
) = (
  new RendererIpcChannel(ExtractWalletsChannelName)
);
