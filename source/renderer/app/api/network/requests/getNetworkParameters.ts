import type { RequestConfig } from '../../common/types';
import type { GetNetworkParametersApiResponse } from '../types';
import { request } from '../../utils/request';

export const getNetworkParameters = (
  config: RequestConfig
): Promise<GetNetworkParametersApiResponse> =>
  request({
    method: 'GET',
    path: '/v2/network/parameters',
    ...config,
  });
