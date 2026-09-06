import { GENERATE_QRCODE_CHANNEL } from '../../../common/ipc/api';
import type {
  GenerateQRCodeMainResponse,
  GenerateQRCodeRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const saveQRCodeImageChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GenerateQRCodeMainResponse,
  GenerateQRCodeRendererRequest
> = new RendererIpcChannel(GENERATE_QRCODE_CHANNEL);
