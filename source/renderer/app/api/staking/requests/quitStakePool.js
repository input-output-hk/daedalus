// @flow
import type { RequestConfig } from '../../common/types';
import type { QuitStakePoolRequest } from '../types';
import type { Transaction } from '../../transactions/types';
import { request } from '../../utils/request';

export const quitStakePool = (
  config: RequestConfig,
  { stakePoolId, walletId, passphrase }: QuitStakePoolRequest
): Promise<Transaction> =>
  request(
    {
      method: 'DELETE',
      path: `/v2/stake-pools/${stakePoolId}/wallets/${walletId}`,
      ...config,
    },
    {},
    { passphrase }
  );
