// @flow
import type { RequestConfig } from '../../common/types';
import type { NetworkClockResponse } from '../types';
import { request } from '../../utils/request';

export const getNetworkClock = (
  config: RequestConfig
): Promise<NetworkClockResponse> =>
  request({
    method: 'GET',
    path: '/v2/network/clock',
    ...config,
  });
