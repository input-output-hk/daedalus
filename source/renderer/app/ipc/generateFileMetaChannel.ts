import { GENERATE_FILE_META_CHANNEL } from '../../../common/ipc/api';
import type {
  GenerateFileMetaMainResponse,
  GenerateFileMetaRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const generateFileMetaChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GenerateFileMetaMainResponse,
  GenerateFileMetaRendererRequest
> = new RendererIpcChannel(GENERATE_FILE_META_CHANNEL);
