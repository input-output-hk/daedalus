import { GENERATE_ADDRESS_PDF_CHANNEL } from '../../../common/ipc/api';
import type {
  GenerateAddressPDFMainResponse,
  GenerateAddressPDFRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const generateAddressPDFChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GenerateAddressPDFMainResponse,
  GenerateAddressPDFRendererRequest
> = new RendererIpcChannel(GENERATE_ADDRESS_PDF_CHANNEL);
