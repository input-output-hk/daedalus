// @flow
import type { RequestConfig } from '../../common/types';
import type { AdaApiStakePools } from '../types';
import { request } from '../../utils/request';

export const getSmashSettings = (
  config: RequestConfig
): Promise<AdaApiStakePools> =>
  request({
    method: 'GET',
    path: '/v2/settings',
    ...config,
  });
