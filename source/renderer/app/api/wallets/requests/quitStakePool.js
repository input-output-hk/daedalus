// @flow
import type { RequestConfig } from '../../common/types';
import type { QuitStakePoolRequest, QuitStakePoolResponse } from '../types';
import { request } from '../../utils/request';

export const quitStakePool = (
  config: RequestConfig,
  { stakePoolId, walletId, passphrase }: QuitStakePoolRequest
): Promise<QuitStakePoolResponse> =>
  request(
    {
      method: 'DELETE',
      path: `/v2/stake-pools/${stakePoolId}/wallets/${walletId}`,
      ...config,
    },
    {},
    { passphrase }
  );
