import type { RequestConfig } from '../../common/types';
import type { NetworkClockResponse } from '../types';
import { request } from '../../utils/request';

export const getNetworkClock = (
  config: RequestConfig,
  isForceCheck: boolean
): Promise<NetworkClockResponse> =>
  request(
    {
      method: 'GET',
      path: '/v2/network/clock',
      ...config,
    },
    {
      forceNtpCheck: isForceCheck,
    }
  );
