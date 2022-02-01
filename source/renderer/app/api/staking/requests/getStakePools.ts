import type { RequestConfig } from '../../common/types';
import type { AdaApiStakePools } from '../types';
import { request } from '../../utils/request';

export const getStakePools = (
  config: RequestConfig,
  stake: number
): Promise<AdaApiStakePools> =>
  request(
    {
      method: 'GET',
      path: '/v2/stake-pools',
      ...config,
    },
    {
      stake,
    }
  );
