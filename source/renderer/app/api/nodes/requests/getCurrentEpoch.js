// @flow
// import type { RequestConfig } from '../../common/types';
import type { NodeSettingsResponse } from '../types';
import { externalRequest } from '../../utils/externalRequest';

export const getCurrentEpoch = (): // config: RequestConfig
Promise<NodeSettingsResponse> =>
  externalRequest({
    hostname: 'cardanoexplorer.com',
    path: '/api/blocks/pages',
    method: 'GET',
  });
