// @flow
import { GenerateRawSecretChannelName } from '../../../common/ipc/api';
import type {
  GenerateRawSecretRendererRequest,
  GenerateRawSecretMainResponse
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const generateRawSecretChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<GenerateRawSecretMainResponse, GenerateRawSecretRendererRequest>
) = (
  new RendererIpcChannel(GenerateRawSecretChannelName)
);
