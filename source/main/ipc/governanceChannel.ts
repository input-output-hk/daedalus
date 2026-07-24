import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  GOVERNANCE_DREP_LIST_CHANNEL,
  GOVERNANCE_DREP_STAKE_CHANNEL,
} from '../../common/ipc/api';
import type {
  GovernanceDRepListRendererRequest,
  GovernanceDRepListMainResponse,
  GovernanceDRepStakeRendererRequest,
  GovernanceDRepStakeMainResponse,
} from '../../common/ipc/api';
import { GovernanceQueryService } from '../governance/GovernanceQueryService';
import { logger } from '../utils/logging';
import { logDRepStateSnapshot } from '../utils/setupLogging';

const governanceDRepListChannel: MainIpcChannel<
  GovernanceDRepListRendererRequest,
  GovernanceDRepListMainResponse
> = new MainIpcChannel(GOVERNANCE_DREP_LIST_CHANNEL);

const governanceDRepStakeChannel: MainIpcChannel<
  GovernanceDRepStakeRendererRequest,
  GovernanceDRepStakeMainResponse
> = new MainIpcChannel(GOVERNANCE_DREP_STAKE_CHANNEL);

// Re-throw a marked PLAIN OBJECT (not an Error) so the structured error
// survives Electron structured clone intact. IpcChannel.onRequest forwards
// the raw thrown value via event.sender.send(responseChannel, false, error)
// with no re-wrap, and request() rejects the renderer promise with the
// structured-cloned value. Error instances flatten to { name, message } and
// would lose `details`; a plain object keeps every property.
const toGovernanceIpcError = (error: unknown) => {
  const queryErr = error as {
    queryErrorType?: string;
    message?: string;
    details?: string;
  };
  return {
    __governanceError: true,
    type: queryErr.queryErrorType ?? 'UNKNOWN',
    message:
      queryErr.message ?? 'An unknown error occurred while querying DRep data.',
    details: queryErr.details,
  };
};

export const handleGovernanceRequests = () => {
  governanceDRepListChannel.onRequest(async (_request) => {
    logger.info('Governance IPC: DRep list requested from renderer');
    try {
      const payload =
        await GovernanceQueryService.getInstance().fetchDRepRegistrations();
      // Support-bundle snapshot only; a write failure must never fail the
      // directory response.
      try {
        logDRepStateSnapshot(payload);
      } catch (snapshotError) {
        logger.error('Governance IPC: DRep-state snapshot write failed', {
          error: snapshotError,
        });
      }
      return payload;
    } catch (error) {
      logger.error('Governance IPC: DRep list query failed', { error });
      // eslint-disable-next-line
      throw toGovernanceIpcError(error);
    }
  });

  governanceDRepStakeChannel.onRequest(async (_request) => {
    logger.info(
      'Governance IPC: DRep stake distribution requested from renderer'
    );
    try {
      return await GovernanceQueryService.getInstance().fetchDRepStake();
    } catch (error) {
      logger.error('Governance IPC: DRep stake query failed', { error });
      // eslint-disable-next-line
      throw toGovernanceIpcError(error);
    }
  });
};
