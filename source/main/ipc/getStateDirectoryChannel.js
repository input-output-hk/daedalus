// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_STATE_DIRECTORY_CHANNEL } from '../../common/ipc/api';
import type {
  GetStateDirectoryRendererRequest,
  GetStateDirectoryMainResponse,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const getStateDirectoryChannel: MainIpcChannel<
  GetStateDirectoryRendererRequest,
  GetStateDirectoryMainResponse
> = new MainIpcChannel(GET_STATE_DIRECTORY_CHANNEL);
