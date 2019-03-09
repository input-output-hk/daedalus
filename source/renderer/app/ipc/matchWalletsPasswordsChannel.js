// @flow
import { MatchWalletsPasswordsChannelName } from '../../../common/ipc/api';
import type {
  MatchWalletsPasswordsRendererRequest,
  MatchWalletsPasswordsMainResponse
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const matchWalletsPasswordsChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<MatchWalletsPasswordsMainResponse, MatchWalletsPasswordsRendererRequest>
) = (
  new RendererIpcChannel(MatchWalletsPasswordsChannelName)
);
