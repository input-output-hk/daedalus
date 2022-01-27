import type { RequestConfig } from '../../common/types';
import type { GetSmashSettingsResponse } from '../types';
import { request } from '../../utils/request';

export const getSmashSettings = (
  config: RequestConfig
): Promise<GetSmashSettingsResponse> =>
  request({
    method: 'GET',
    path: '/v2/settings',
    ...config,
  });
