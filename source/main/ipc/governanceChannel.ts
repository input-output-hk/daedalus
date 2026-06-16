import { MainIpcChannel } from './lib/MainIpcChannel';
import { GOVERNANCE_DREP_LIST_CHANNEL } from '../../common/ipc/api';
import type {
  GovernanceDRepListRendererRequest,
  GovernanceDRepListMainResponse,
} from '../../common/ipc/api';
import { GovernanceQueryService } from '../governance/GovernanceQueryService';
import { logger } from '../utils/logging';

const governanceDRepListChannel: MainIpcChannel<
  GovernanceDRepListRendererRequest,
  GovernanceDRepListMainResponse
> = new MainIpcChannel(GOVERNANCE_DREP_LIST_CHANNEL);

export const handleGovernanceRequests = () => {
  governanceDRepListChannel.onRequest(async (_request) => {
    logger.info('Governance IPC: DRep list requested from renderer');
    try {
      const result = await GovernanceQueryService.getInstance().fetchDRepList();
      return result;
    } catch (error) {
      logger.error('Governance IPC: DRep list query failed', { error });
      // Re-throw a marked PLAIN OBJECT (not an Error) so the structured error
      // survives Electron structured clone intact. IpcChannel.onRequest forwards
      // the raw thrown value via event.sender.send(responseChannel, false, error)
      // with no re-wrap, and request() rejects the renderer promise with the
      // structured-cloned value. Error instances flatten to { name, message } and
      // would lose `details`; a plain object keeps every property.
      const queryErr = error as {
        queryErrorType?: string;
        message?: string;
        details?: string;
      };
      // eslint-disable-next-line
      throw {
        __governanceError: true,
        type: queryErr.queryErrorType ?? 'UNKNOWN',
        message:
          queryErr.message ??
          'An unknown error occurred while querying DRep data.',
        details: queryErr.details,
      };
    }
  });
};
