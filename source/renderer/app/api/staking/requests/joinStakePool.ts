import type { RequestConfig } from '../../common/types';
import type { JoinStakePoolRequest } from '../types';
import type { Transaction } from '../../transactions/types';
import { request } from '../../utils/request';

export const joinStakePool = (
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
    }
  );
