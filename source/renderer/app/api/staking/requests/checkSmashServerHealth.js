// @flow
import type { RequestConfig } from '../../common/types';
import type { CheckSmashServerHealthResponse } from '../types';
import { request } from '../../utils/request';

export const checkSmashServerHealth = (
  config: RequestConfig,
  url?: string
): Promise<CheckSmashServerHealthResponse> =>
  request(
    {
      method: 'GET',
      path: '/v2/smash/health',
      ...config,
    },
    { url }
  );
