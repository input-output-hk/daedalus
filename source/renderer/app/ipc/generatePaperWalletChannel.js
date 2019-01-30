// @flow
import { GeneratePaperWalletChannelName } from '../../../common/ipc/api';
import type {
  GeneratePaperWalletMainResponse,
  GeneratePaperWalletRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const generatePaperWalletChannel: (
  // IpcChannel<Incoming, Outgoing>
  RendererIpcChannel<GeneratePaperWalletMainResponse, GeneratePaperWalletRendererRequest>
) = (
  new RendererIpcChannel(GeneratePaperWalletChannelName)
);
