// @flow
// import type { RequestConfig } from '../../common/types';
import type { NodeSettingsResponse } from '../types';
import { request } from '../../utils/request';

export const getCurrentEpoch = (): // config: RequestConfig
Promise<NodeSettingsResponse> =>
  request({
    host: 'cardanoexplorer.com',
    href: 'https://cardanoexplorer.com/api/blocks/pages',
    origin: 'https://cardanoexplorer.com',
    path: '/api/blocks/pages',
    protocol: 'https:',
    method: 'GET',
  });
