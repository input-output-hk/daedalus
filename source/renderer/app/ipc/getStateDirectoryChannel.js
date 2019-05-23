// @flow
import { GET_STATE_DIRECTORY_CHANNEL } from '../../../common/ipc/api';
import type {
  GetStateDirectoryRendererRequest,
  GetStateDirectoryMainResponse,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const getStateDirectoryChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GetStateDirectoryMainResponse,
  GetStateDirectoryRendererRequest
> = new RendererIpcChannel(GET_STATE_DIRECTORY_CHANNEL);
