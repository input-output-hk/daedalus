// @flow
import type { RequestConfig } from '../../common/types';
import type { NetworkParametersResponse } from '../types';
import { request } from '../../utils/request';

export const getNetworkParameters = (
  epochId: string,
  config: RequestConfig
): Promise<NetworkParametersResponse> =>
  request({
    method: 'GET',
    path: `/v2/network/parameters/${epochId}`,
    ...config,
  });
