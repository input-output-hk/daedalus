// @flow
import { GENERATE_FILE_BLOB_CHANNEL } from '../../../common/ipc/api';
import type {
  GenerateFileContentMainResponse,
  GenerateFileContentRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const generateFileContentChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GenerateFileContentMainResponse,
  GenerateFileContentRendererRequest
> = new RendererIpcChannel(GENERATE_FILE_BLOB_CHANNEL);
