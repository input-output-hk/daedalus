import type { RequestConfig } from '../../common/types';
import type { JoinStakePoolRequest } from '../types';
import type { Transaction } from '../../transactions/types';
import { request } from '../../utils/request';
import { walletInputSelectionChannel } from '../../../ipc/collateral';

export const joinStakePool = async (
  config: RequestConfig,
  { walletId, stakePoolId, passphrase }: JoinStakePoolRequest
): Promise<Transaction> =>
  request(
    {
      method: 'PUT',
      path: `/v2/stake-pools/${stakePoolId}/wallets/${walletId}`,
      ...config,
    },
    {},
    {
      passphrase,
      ...(await walletInputSelectionChannel.request({ walletId })),
    }
  );
