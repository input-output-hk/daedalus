// @flow
import type { RequestConfig } from '../../common/types';
import type { NetworkInfoResponse } from '../types';
import { request } from '../../utils/request';

export type NetworkInfoQueryParams = {
  force_ntp_check: boolean,
};

export const getNetworkInfo = (
  config: RequestConfig,
  queryInfoParams?: NetworkInfoQueryParams
): Promise<NetworkInfoResponse> =>
  request(
    {
      method: 'GET',
      path: '/v2/network/information',
      ...config,
    },
    queryInfoParams
  );
