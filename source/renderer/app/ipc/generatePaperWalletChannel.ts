import { GENERATE_PAPER_WALLET_CHANNEL } from '../../../common/ipc/api';
import type {
  GeneratePaperWalletMainResponse,
  GeneratePaperWalletRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const generatePaperWalletChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GeneratePaperWalletMainResponse,
  GeneratePaperWalletRendererRequest
> = new RendererIpcChannel(GENERATE_PAPER_WALLET_CHANNEL);
