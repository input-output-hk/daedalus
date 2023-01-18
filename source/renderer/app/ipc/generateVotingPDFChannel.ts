import { GENERATE_VOTING_PDF_CHANNEL } from '../../../common/ipc/api';
import type {
  GenerateVotingPDFMainResponse,
  GenerateVotingPDFRendererRequest,
} from '../../../common/ipc/api';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

export const generateVotingPDFChannel: // IpcChannel<Incoming, Outgoing>
RendererIpcChannel<
  GenerateVotingPDFMainResponse,
  GenerateVotingPDFRendererRequest
> = new RendererIpcChannel(GENERATE_VOTING_PDF_CHANNEL);
