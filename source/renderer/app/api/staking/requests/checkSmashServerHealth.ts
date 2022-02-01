import type { RequestConfig } from '../../common/types';
import type { CheckSmashServerHealthApiResponse } from '../types';
import { request } from '../../utils/request';

export const checkSmashServerHealth = (
  config: RequestConfig,
  url?: string
): Promise<CheckSmashServerHealthApiResponse> =>
  request(
    {
      method: 'GET',
      path: '/v2/smash/health',
      ...config,
    },
    {
      url,
    }
  );
