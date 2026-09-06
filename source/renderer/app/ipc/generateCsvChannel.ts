import { GENERATE_CSV_CHANNEL } from '../../../common/ipc/api';
import type {
  GenerateCsvMainResponse,
  GenerateCsvRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const generateCsvChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GenerateCsvMainResponse,
  GenerateCsvRendererRequest
> = new RendererIpcChannel(GENERATE_CSV_CHANNEL);
