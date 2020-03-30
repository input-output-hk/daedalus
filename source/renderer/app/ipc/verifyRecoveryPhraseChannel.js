// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { VERIFY_RECOVERY_PHRASE_CHANNEL } from '../../../common/ipc/api';
import type {
  VerifyRecoveryPhraseRendererRequest,
  VerifyRecoveryPhraseMainResponse,
} from '../../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>
export const verifyRecoveryPhraseChannel: RendererIpcChannel<
  VerifyRecoveryPhraseMainResponse,
  VerifyRecoveryPhraseRendererRequest
> = new RendererIpcChannel(VERIFY_RECOVERY_PHRASE_CHANNEL);
