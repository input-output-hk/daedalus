import type { RequestConfig } from '../../common/types';
import type { NetworkInfoResponse } from '../types';
import { request } from '../../utils/request';

export const getNetworkInfo = (
  config: RequestConfig
): Promise<NetworkInfoResponse> =>
  request({
    method: 'GET',
    path: '/v2/network/information',
    ...config,
  });
