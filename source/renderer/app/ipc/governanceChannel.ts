import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { GOVERNANCE_DREP_ANCHOR_CHANNEL } from '../../../common/ipc/api';
import type {
  GovernanceDRepAnchorMainResponse,
  GovernanceDRepAnchorRendererRequest,
} from '../../../common/ipc/api';

export const governanceDRepAnchorChannel: RendererIpcChannel<
  GovernanceDRepAnchorMainResponse,
  GovernanceDRepAnchorRendererRequest
> = new RendererIpcChannel(GOVERNANCE_DREP_ANCHOR_CHANNEL);
