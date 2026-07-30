import { MainIpcChannel } from './lib/MainIpcChannel';
import { GOVERNANCE_DREP_ANCHOR_CHANNEL } from '../../common/ipc/api';
import type {
  GovernanceDRepAnchorRendererRequest,
  GovernanceDRepAnchorMainResponse,
} from '../../common/ipc/api';
import { AnchorFetchErrorType } from '../../common/types/governance.types';
import { resolveVerifiedAnchor } from '../governance/AnchorVerificationService';
import { logger } from '../utils/logging';

const governanceDRepAnchorChannel: MainIpcChannel<
  GovernanceDRepAnchorRendererRequest,
  GovernanceDRepAnchorMainResponse
> = new MainIpcChannel(GOVERNANCE_DREP_ANCHOR_CHANNEL);

// The request carries an anchor URL, so nothing from it may be logged; the
// response is enum-shaped and the handler never rejects.
export const handleGovernanceAnchorRequests = () => {
  governanceDRepAnchorChannel.onRequest(async (anchor) => {
    let result: GovernanceDRepAnchorMainResponse;
    try {
      result = await resolveVerifiedAnchor(anchor);
    } catch {
      result = {
        status: 'unavailable',
        reason: AnchorFetchErrorType.InvalidRequest,
      };
    }
    logger.info('Governance IPC: anchor resolution finished', {
      status: result.status,
      reason: result.status === 'unavailable' ? result.reason : undefined,
    });
    return result;
  });
};
