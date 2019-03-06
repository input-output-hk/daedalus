// @flow
import { GENERATE_PAPER_WALLET_CHANNEL } from '../../../common/ipc/api';
import type {
  GeneratePaperWalletResponse,
  GeneratePaperWalletRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const generatePaperWalletChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<GeneratePaperWalletResponse, GeneratePaperWalletRequest>
) = (
  new RendererIpcChannel(GENERATE_PAPER_WALLET_CHANNEL)
);
