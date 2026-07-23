import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  GOVERNANCE_DREP_LIST_CHANNEL,
  GOVERNANCE_DREP_STAKE_CHANNEL,
} from '../../../common/ipc/api';
import type {
  GovernanceDRepListMainResponse,
  GovernanceDRepListRendererRequest,
  GovernanceDRepStakeMainResponse,
  GovernanceDRepStakeRendererRequest,
} from '../../../common/ipc/api';

export const governanceDRepListChannel: RendererIpcChannel<
  GovernanceDRepListMainResponse,
  GovernanceDRepListRendererRequest
> = new RendererIpcChannel(GOVERNANCE_DREP_LIST_CHANNEL);

export const governanceDRepStakeChannel: RendererIpcChannel<
  GovernanceDRepStakeMainResponse,
  GovernanceDRepStakeRendererRequest
> = new RendererIpcChannel(GOVERNANCE_DREP_STAKE_CHANNEL);
