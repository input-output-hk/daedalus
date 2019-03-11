// @flow
import { GenerateKeyFileChannelName } from '../../../common/ipc/api';
import type {
  GenerateKeyFileRendererRequest,
  GenerateKeyFileMainResponse
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const generateKeyFileChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<GenerateKeyFileMainResponse, GenerateKeyFileRendererRequest>
) = (
  new RendererIpcChannel(GenerateKeyFileChannelName)
);
