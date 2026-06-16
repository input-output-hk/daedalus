import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { GOVERNANCE_DREP_LIST_CHANNEL } from '../../../common/ipc/api';
import type {
  GovernanceDRepListMainResponse,
  GovernanceDRepListRendererRequest,
} from '../../../common/ipc/api';

export const governanceDRepListChannel: RendererIpcChannel<
  GovernanceDRepListMainResponse,
  GovernanceDRepListRendererRequest
> = new RendererIpcChannel(GOVERNANCE_DREP_LIST_CHANNEL);
