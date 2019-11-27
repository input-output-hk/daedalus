// @flow
import type { RequestConfig } from '../../common/types';
import type { StakePool, JoinStakePoolRequest } from '../types';
import { request } from '../../utils/request';

export const joinStakePool = (
  config: RequestConfig,
  { walletId, stakePoolId, passphrase }: JoinStakePoolRequest
): Promise<StakePool> =>
  request(
    {
      method: 'PUT',
      path: `/v2/stake-pools/${stakePoolId}/wallets/${walletId}`,
      ...config,
    },
    {},
    { passphrase }
  );
